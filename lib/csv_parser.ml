open Parser
open Basic_parsers

type csv_row = string list
type csv_data = csv_row list

type csv_config = {
  delimiter : char;
  quote_char : char;
  escape_char : char option;
}

let default_config = { delimiter = ','; quote_char = '"'; escape_char = None }

let eol : unit parser =
 fun input ->
  (char_parser '\n' >|= (fun _ -> ()) <|> (string_parser "\r\n" >|= fun _ -> ()))
    input

let eof : unit parser =
 fun input ->
  match get_input_head input with
  | None -> Ok (input, ())
  | Some (c, _) ->
      Error
        (ParserError
           (input.loc, Printf.sprintf "Expected end of file, but found '%c'" c))

let escaped_char (config : csv_config) : char parser =
  match config.escape_char with
  | Some esc ->
      let* _ = char_parser esc in
      char_parser config.quote_char
      >|= (fun _ -> config.quote_char)
      <|> (char_parser esc >|= fun _ -> esc)
      <|> (char_parser 'n' >|= fun _ -> '\n')
      <|> (char_parser 'r' >|= fun _ -> '\r')
      <|> (char_parser 't' >|= fun _ -> '\t')
  | None ->
      let* _ = char_parser config.quote_char in
      let* _ = char_parser config.quote_char in
      return config.quote_char

let quoted_char (config : csv_config) : char parser =
  escaped_char config
  <|> parse_if "quoted char" (fun c ->
          c <> config.quote_char && c <> '\n' && c <> '\r')

let quoted_field (config : csv_config) : string parser =
  let* _ = char_parser config.quote_char in
  let* chars = many (quoted_char config) in
  let* _ = char_parser config.quote_char in
  return (String.concat "" (List.map (String.make 1) chars))

let unquoted_field (config : csv_config) : string parser =
  span_parser (fun c ->
      c <> config.delimiter && c <> '\n' && c <> '\r' && c <> config.quote_char)

let csv_field (config : csv_config) : string parser =
  quoted_field config <|> unquoted_field config

let csv_row (config : csv_config) : csv_row parser =
  let* fields = sep_by (char_parser config.delimiter) (csv_field config) in
  return fields

let csv_rows (config : csv_config) : csv_data parser =
 fun input ->
  let rec parse_rows acc input =
    match get_input_head input with
    | None -> Ok (input, List.rev acc)
    | Some _ -> (
        match csv_row config input with
        | Ok (new_input, row) -> (
            match (eol <|> eof) new_input with
            | Ok (final_input, _) -> parse_rows (row :: acc) final_input
            | Error _ -> Ok (new_input, List.rev (row :: acc)))
        | Error e -> if acc = [] then Error e else Ok (input, List.rev acc))
  in
  parse_rows [] input

let parse_csv ?(config = default_config) (s : string) :
    (csv_data, parser_error) result =
  match csv_rows config { loc = 0; str = s } with
  | Ok (_, data) -> Ok data
  | Error e -> Error e

let parse_csv_with_headers ?(config = default_config) (s : string) :
    (string list * csv_data, parser_error) result =
  let parser =
    let* header = csv_row config in
    let* _ = eol in
    let* rows = csv_rows config in
    return (header, rows)
  in
  match parser { loc = 0; str = s } with
  | Ok (_, (header, rows)) -> Ok (header, rows)
  | Error e -> Error e

type csv_stream = {
  current_input : input;
  finished : bool;
  config : csv_config;
}

let create_csv_stream ?(config = default_config) (s : string) : csv_stream =
  { current_input = { loc = 0; str = s }; finished = false; config }

let next_row (stream : csv_stream) :
    (csv_stream * csv_row option, parser_error) result =
  if stream.finished then Ok (stream, None)
  else
    match get_input_head stream.current_input with
    | None ->
        let new_stream = { stream with finished = true } in
        Ok (new_stream, None)
    | Some _ -> (
        let parser =
          let* row = csv_row stream.config in
          let* _ = eol <|> eof in
          return row
        in
        match parser stream.current_input with
        | Ok (new_input, row) ->
            let new_stream =
              {
                stream with
                current_input = new_input;
                finished = new_input.str = "";
              }
            in
            Ok (new_stream, Some row)
        | Error e -> Error e)

let fold_csv_stream ?(config = default_config) (s : string)
    (f : 'acc -> csv_row -> 'acc) (init : 'acc) : ('acc, parser_error) result =
  let initial_stream = create_csv_stream ~config s in
  let rec aux stream acc =
    match next_row stream with
    | Ok (new_stream, Some row) -> aux new_stream (f acc row)
    | Ok (_, None) -> Ok acc
    | Error e -> Error e
  in
  aux initial_stream init

let iter_csv_stream ?(config = default_config) (s : string)
    (f : csv_row -> unit) : (unit, parser_error) result =
  fold_csv_stream ~config s (fun () row -> f row) ()

let show_csv_row (row : csv_row) : string =
  String.concat "," (List.map (fun field -> Printf.sprintf "\"%s\"" field) row)

let show_csv_data (data : csv_data) : string =
  String.concat "\n" (List.map show_csv_row data)

let show_csv_with_headers (header : string list) (data : csv_data) : string =
  show_csv_row header ^ "\n" ^ show_csv_data data
