open Lab4.Json_parser
open Lab4.Csv_parser

let demo_json () =
  print_endline "=== JSON Parser Demo ===\n";

  let examples =
    [
      ("Null", {|null|});
      ("Boolean", {|true|});
      ("Number", {|42.5|});
      ("String", {|"Hello, OCaml!"|});
      ("Array", {|[1, 2, 3, "four", null]|});
      ("Object", {|{"name": "Alice", "age": 30, "active": true}|});
      ( "Nested",
        {|{
      "users": [
        {"id": 1, "name": "Bob"},
        {"id": 2, "name": "Carol"}
      ],
      "count": 2
    }|}
      );
    ]
  in

  List.iter
    (fun (name, input) ->
      Printf.printf "Example: %s\n" name;
      Printf.printf "Input: %s\n" input;
      match parse_json input with
      | Ok value -> Printf.printf "Parsed: %s\n\n" (show_json_value value)
      | Error (Lab4.Parser.ParserError (loc, msg)) ->
          Printf.printf "Error at position %d: %s\n\n" loc msg)
    examples

let demo_csv () =
  print_endline "=== CSV Parser Demo ===\n";

  print_endline "Example 1: Simple CSV";
  let csv1 = "name,age,city\nAlice,30,Paris\nBob,25,London" in
  Printf.printf "Input:\n%s\n\n" csv1;
  (match parse_csv_with_headers csv1 with
  | Ok (header, rows) ->
      Printf.printf "Header: %s\n" (String.concat ", " header);
      Printf.printf "Rows:\n";
      List.iter
        (fun row -> Printf.printf "  [%s]\n" (String.concat ", " row))
        rows;
      print_newline ()
  | Error (Lab4.Parser.ParserError (loc, msg)) ->
      Printf.printf "Error at position %d: %s\n\n" loc msg);

  print_endline "Example 2: CSV with quotes";
  let csv2 =
    {|"Product","Price","Description"
"Apple",1.50,"Fresh red apples"
"Banana",0.80,"Organic ""premium"" bananas"|}
  in
  Printf.printf "Input:\n%s\n\n" csv2;
  (match parse_csv csv2 with
  | Ok rows ->
      List.iter
        (fun row ->
          Printf.printf "  [%s]\n"
            (String.concat " | " (List.map (Printf.sprintf "'%s'") row)))
        rows;
      print_newline ()
  | Error (Lab4.Parser.ParserError (loc, msg)) ->
      Printf.printf "Error at position %d: %s\n\n" loc msg);

  print_endline "Example 3: Streaming CSV (processing row by row)";
  let csv3 = "id,value\n1,100\n2,200\n3,300" in
  Printf.printf "Input:\n%s\n\n" csv3;
  Printf.printf "Processing with fold (sum of 'value' column):\n";
  (match
     fold_csv_stream csv3
       (fun (is_header, count, sum) row ->
         if is_header then (
           Printf.printf "  Header: [%s]\n" (String.concat ", " row);
           (false, count, sum))
         else
           match row with
           | [ id; value ] ->
               let v = int_of_string value in
               Printf.printf "  Row %d: id=%s, value=%d\n" (count + 1) id v;
               (false, count + 1, sum + v)
           | _ -> (false, count, sum))
       (true, 0, 0)
   with
  | Ok (_, _count, total) -> Printf.printf "Total sum: %d\n\n" total
  | Error (Lab4.Parser.ParserError (loc, msg)) ->
      Printf.printf "Error at position %d: %s\n\n" loc msg);

  print_endline "Example 4: Custom delimiter (semicolon)";
  let config = { delimiter = ';'; quote_char = '"'; escape_char = None } in
  let csv4 = "a;b;c\n1;2;3" in
  Printf.printf "Input:\n%s\n\n" csv4;
  match parse_csv ~config csv4 with
  | Ok rows ->
      List.iter
        (fun row -> Printf.printf "  [%s]\n" (String.concat ", " row))
        rows;
      print_newline ()
  | Error (Lab4.Parser.ParserError (loc, msg)) ->
      Printf.printf "Error at position %d: %s\n\n" loc msg

let parse_file_as_json filename =
  try
    let ic = open_in filename in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    print_endline ("=== Parsing JSON from file: " ^ filename ^ " ===\n");
    match parse_json content with
    | Ok value ->
        Printf.printf "Successfully parsed JSON:\n%s\n\n"
          (show_json_value value);
        Ok ()
    | Error (Lab4.Parser.ParserError (loc, msg)) ->
        Printf.eprintf "JSON Parse Error at position %d: %s\n\n" loc msg;
        Error ()
  with
  | Sys_error err ->
      Printf.eprintf "File Error: %s\n" err;
      Error ()
  | e ->
      Printf.eprintf "Unexpected Error: %s\n" (Printexc.to_string e);
      Error ()

let parse_file_as_csv filename =
  try
    let ic = open_in filename in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    print_endline ("=== Parsing CSV from file: " ^ filename ^ " ===\n");
    match parse_csv_with_headers content with
    | Ok (header, rows) ->
        Printf.printf "Header: %s\n" (String.concat ", " header);
        Printf.printf "Number of rows: %d\n" (List.length rows);
        Printf.printf "\nData:\n";
        List.iteri
          (fun i row ->
            Printf.printf "  Row %d: [%s]\n" (i + 1) (String.concat " | " row))
          rows;
        print_newline ();
        Ok ()
    | Error (Lab4.Parser.ParserError (loc, msg)) ->
        Printf.eprintf "CSV Parse Error at position %d: %s\n\n" loc msg;
        Error ()
  with
  | Sys_error err ->
      Printf.eprintf "File Error: %s\n" err;
      Error ()
  | e ->
      Printf.eprintf "Unexpected Error: %s\n" (Printexc.to_string e);
      Error ()

let print_usage () =
  print_endline "Usage:";
  print_endline "  lab4                    - Run demo examples";
  print_endline "  lab4 json <file>        - Parse JSON from file";
  print_endline "  lab4 csv <file>         - Parse CSV from file";
  print_newline ()

let () =
  let args = Array.to_list Sys.argv in
  match args with
  | [ _ ] ->
      (* No arguments - run demo *)
      demo_json ();
      print_endline (String.make 60 '=');
      print_newline ();
      demo_csv ();
      print_endline "Demo complete!"
  | [ _; "json"; filename ] ->
      let _ = parse_file_as_json filename in
      ()
  | [ _; "csv"; filename ] ->
      let _ = parse_file_as_csv filename in
      ()
  | [ _; "--help" ] | [ _; "-h" ] -> print_usage ()
  | _ ->
      print_endline "Invalid arguments.\n";
      print_usage ();
      exit 1
