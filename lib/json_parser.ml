open Parser
open Basic_parsers

type json_value =
  | JsonNull
  | JsonBool of bool
  | JsonNumber of float
  | JsonString of string
  | JsonArray of json_value list
  | JsonObject of (string * json_value) list

let ws_parser : string parser =
  span_parser (fun c -> c = ' ' || c = '\n' || c = '\t' || c = '\r')

let json_null : json_value parser = string_parser "null" >|= fun _ -> JsonNull

let json_bool : json_value parser =
  string_parser "true"
  >|= (fun _ -> JsonBool true)
  <|> (string_parser "false" >|= fun _ -> JsonBool false)

let json_number : json_value parser =
  let is_digit c = c >= '0' && c <= '9' in
  let digits =
    let* first = parse_if "digit" is_digit in
    let* rest = span_parser is_digit in
    return (String.make 1 first ^ rest)
  in
  let* sign = char_parser '-' >|= (fun _ -> -1.) <|> return 1. in
  let* int_part = digits >|= float_of_string in
  let* frac_part =
    (let* _ = char_parser '.' in
     let* frac = digits in
     return (float_of_string ("0." ^ frac)))
    <|> return 0.
  in
  let* exp_part =
    (let* _ = char_parser 'e' <|> char_parser 'E' in
     let* sign =
       char_parser '+'
       >|= (fun _ -> 1)
       <|> (char_parser '-' >|= fun _ -> -1)
       <|> return 1
     in
     let* exp = digits >|= int_of_string in
     return (10. ** float_of_int (sign * exp)))
    <|> return 1.
  in
  return (JsonNumber (sign *. (int_part +. frac_part) *. exp_part))

let escape_char : char parser =
  let* _ = char_parser '\\' in
  char_parser '"'
  >|= (fun _ -> '"')
  <|> (char_parser '\\' >|= fun _ -> '\\')
  <|> (char_parser '/' >|= fun _ -> '/')
  <|> (char_parser 'b' >|= fun _ -> '\b')
  <|> (char_parser 'f' >|= fun _ -> '\012')
  <|> (char_parser 'n' >|= fun _ -> '\n')
  <|> (char_parser 'r' >|= fun _ -> '\r')
  <|> (char_parser 't' >|= fun _ -> '\t')

let normal_char : char parser =
  parse_if "normal char" (fun c -> c <> '"' && c <> '\\')

let string_literal : string parser =
  let* _ = char_parser '"' in
  let* chars = many (escape_char <|> normal_char) in
  let* _ = char_parser '"' in
  return (String.concat "" (List.map (String.make 1) chars))

let json_string : json_value parser = string_literal >|= fun s -> JsonString s

let rec json_value input =
  (ws_parser
   *> (json_null <|> json_bool <|> json_number <|> json_string <|> json_array
     <|> json_object)
  <* ws_parser)
    input

and json_array input =
  (let* _ = char_parser '[' *> ws_parser in
   let* elements =
     sep_by (ws_parser *> char_parser ',' <* ws_parser) json_value
   in
   let* _ = ws_parser *> char_parser ']' in
   return (JsonArray elements))
    input

and json_object input =
  (let* _ = char_parser '{' *> ws_parser in
   let pair =
     let* key = string_literal <* ws_parser <* char_parser ':' <* ws_parser in
     let* value = json_value in
     return (key, value)
   in
   let* pairs = sep_by (ws_parser *> char_parser ',' <* ws_parser) pair in
   let* _ = ws_parser *> char_parser '}' in
   return (JsonObject pairs))
    input

let parse_json (s : string) : (json_value, parser_error) result =
  match json_value { loc = 0; str = s } with
  | Ok (_, value) -> Ok value
  | Error e -> Error e

let rec show_json_value = function
  | JsonNull -> "null"
  | JsonBool b -> string_of_bool b
  | JsonNumber n -> string_of_float n
  | JsonString s ->
      let escaped =
        String.concat ""
          (List.map
             (function '\n' -> "\\n" | c -> String.make 1 c)
             (List.init (String.length s) (String.get s)))
      in
      Printf.sprintf "\"%s\"" escaped
  | JsonArray vs -> "[" ^ String.concat ", " (List.map show_json_value vs) ^ "]"
  | JsonObject pairs ->
      let show_pair (k, v) =
        Printf.sprintf "\"%s\": %s" k (show_json_value v)
      in
      "{" ^ String.concat ", " (List.map show_pair pairs) ^ "}"
