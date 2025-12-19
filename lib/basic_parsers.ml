open Parser

let char_parser (c : char) : char parser =
 fun input ->
  match get_input_head input with
  | Some (x, rest) when x = c -> Ok (rest, x)
  | Some (x, _) ->
      Error
        (ParserError
           (input.loc, Printf.sprintf "Expected '%c', but found '%c'" c x))
  | None ->
      Error
        (ParserError
           ( input.loc,
             Printf.sprintf "Expected '%c', but reached end of string" c ))

let string_parser (s : string) : string parser =
 fun input ->
  let rec aux i input =
    if i >= String.length s then Ok (input, s)
    else
      match char_parser (String.get s i) input with
      | Ok (rest, _) -> aux (i + 1) rest
      | Error _ ->
          Error
            (ParserError
               ( input.loc,
                 Printf.sprintf "Expected \"%s\", but found \"%s\"" s input.str
               ))
  in
  aux 0 input

let span_parser (pred : char -> bool) : string parser =
 fun input ->
  let rec aux acc input =
    match get_input_head input with
    | Some (c, rest) when pred c -> aux (c :: acc) rest
    | _ -> Ok (input, String.concat "" (List.map (String.make 1) (List.rev acc)))
  in
  aux [] input

let parse_if (desc : string) (pred : char -> bool) : char parser =
 fun input ->
  match get_input_head input with
  | Some (c, rest) when pred c -> Ok (rest, c)
  | Some (c, _) ->
      Error
        (ParserError
           (input.loc, Printf.sprintf "Expected %s, but found '%c'" desc c))
  | None ->
      Error
        (ParserError
           ( input.loc,
             Printf.sprintf "Expected %s, but reached end of string" desc ))

let rec many (p : 'a parser) : 'a list parser =
 fun input ->
  ((let* x = p in
    let* xs = many p in
    return (x :: xs))
  <|> return [])
    input

let rec sequence (ps : 'a parser list) : 'a list parser =
 fun input ->
  match ps with
  | [] -> return [] input
  | p :: ps' ->
      (let* x = p in
       let* xs = sequence ps' in
       return (x :: xs))
        input

let sep_by (sep : 'a parser) (elem : 'b parser) : 'b list parser =
 fun input ->
  ((let* x = elem in
    let* xs = many (sep *> elem) in
    return (x :: xs))
  <|> return [])
    input
