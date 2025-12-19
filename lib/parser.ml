type input = { loc : int; str : string }
type parser_error = ParserError of int * string
type 'a parser = input -> (input * 'a, parser_error) result

let get_input_head input =
  if input.str = "" then None
  else
    Some
      ( String.get input.str 0,
        {
          loc = input.loc + 1;
          str = String.sub input.str 1 (String.length input.str - 1);
        } )

let return x input = Ok (input, x)

let ( >>= ) (p : 'a parser) (f : 'a -> 'b parser) : 'b parser =
 fun input ->
  match p input with Ok (input', x) -> f x input' | Error e -> Error e

let ( let* ) = ( >>= )

let ( <|> ) (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
 fun input -> match p1 input with Ok result -> Ok result | Error _ -> p2 input

let map (f : 'a -> 'b) (p : 'a parser) : 'b parser =
 fun input ->
  match p input with Ok (input', x) -> Ok (input', f x) | Error e -> Error e

let ( >|= ) p f = map f p

let ( <*> ) (p1 : ('a -> 'b) parser) (p2 : 'a parser) : 'b parser =
 fun input ->
  match p1 input with
  | Ok (input', f) -> (
      match p2 input' with
      | Ok (input'', x) -> Ok (input'', f x)
      | Error e -> Error e)
  | Error e -> Error e

let ( <$> ) = map

let ( *> ) p1 p2 =
  let* _ = p1 in
  p2

let ( <* ) p1 p2 =
  let* x = p1 in
  let* _ = p2 in
  return x
