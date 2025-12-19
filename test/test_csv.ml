open Lab4.Parser
open Lab4.Csv_parser

let test_simple_csv () =
  let input = "a,b,c\n1,2,3\n4,5,6" in
  match parse_csv input with
  | Ok data ->
      Alcotest.(check int) "number of rows" 3 (List.length data);
      Alcotest.(check (list string))
        "first row" [ "a"; "b"; "c" ] (List.nth data 0);
      Alcotest.(check (list string))
        "second row" [ "1"; "2"; "3" ] (List.nth data 1);
      Alcotest.(check (list string))
        "third row" [ "4"; "5"; "6" ] (List.nth data 2)
  | Error (ParserError (loc, msg)) ->
      Alcotest.failf "Parse error at position %d: %s" loc msg

let test_quoted_csv () =
  let input =
    {|"name","age","city"
"John Doe",30,"New York"
"Jane Smith",25,"Los Angeles"|}
  in
  match parse_csv input with
  | Ok data ->
      Alcotest.(check int) "number of rows" 3 (List.length data);
      Alcotest.(check (list string))
        "first row" [ "name"; "age"; "city" ] (List.nth data 0);
      Alcotest.(check (list string))
        "second row"
        [ "John Doe"; "30"; "New York" ]
        (List.nth data 1)
  | Error (ParserError (loc, msg)) ->
      Alcotest.failf "Parse error at position %d: %s" loc msg

let test_empty_fields () =
  let input = "a,,c\n,b,\n,," in
  match parse_csv input with
  | Ok data ->
      Alcotest.(check int) "number of rows" 3 (List.length data);
      Alcotest.(check (list string))
        "row with empty middle field" [ "a"; ""; "c" ] (List.nth data 0);
      Alcotest.(check (list string))
        "row with empty first and last" [ ""; "b"; "" ] (List.nth data 1);
      Alcotest.(check (list string))
        "row with all empty" [ ""; ""; "" ] (List.nth data 2)
  | Error (ParserError (loc, msg)) ->
      Alcotest.failf "Parse error at position %d: %s" loc msg

let test_escaped_quotes () =
  let input = {|"He said ""Hello""","She said ""Hi"""
"It's ""fine""","OK"|} in
  match parse_csv input with
  | Ok data ->
      Alcotest.(check int) "number of rows" 2 (List.length data);
      Alcotest.(check (list string))
        "first row with escaped quotes"
        [ {|He said "Hello"|}; {|She said "Hi"|} ]
        (List.nth data 0);
      Alcotest.(check (list string))
        "second row" [ {|It's "fine"|}; "OK" ] (List.nth data 1)
  | Error (ParserError (loc, msg)) ->
      Alcotest.failf "Parse error at position %d: %s" loc msg

let test_csv_with_headers () =
  let input = "name,age,city\nAlice,30,Paris\nBob,25,London" in
  match parse_csv_with_headers input with
  | Ok (header, rows) ->
      Alcotest.(check (list string)) "header" [ "name"; "age"; "city" ] header;
      Alcotest.(check int) "number of data rows" 2 (List.length rows);
      Alcotest.(check (list string))
        "first data row" [ "Alice"; "30"; "Paris" ] (List.nth rows 0)
  | Error (ParserError (loc, msg)) ->
      Alcotest.failf "Parse error at position %d: %s" loc msg

let test_empty_csv () =
  let input = "" in
  match parse_csv input with
  | Ok data ->
      Alcotest.(check int) "empty csv should have 0 rows" 0 (List.length data)
  | Error (ParserError (loc, msg)) ->
      Alcotest.failf "Parse error at position %d: %s" loc msg

let test_single_row () =
  let input = "a,b,c\n" in
  match parse_csv input with
  | Ok data ->
      Alcotest.(check int) "one row" 1 (List.length data);
      Alcotest.(check (list string))
        "row content" [ "a"; "b"; "c" ] (List.nth data 0)
  | Error (ParserError (loc, msg)) ->
      Alcotest.failf "Parse error at position %d: %s" loc msg

let test_custom_delimiter () =
  let config = { delimiter = ';'; quote_char = '"'; escape_char = Some '\\' } in
  let input = "a;b;c\n1;2;3" in
  match parse_csv ~config input with
  | Ok data ->
      Alcotest.(check int) "number of rows" 2 (List.length data);
      Alcotest.(check (list string))
        "first row" [ "a"; "b"; "c" ] (List.nth data 0);
      Alcotest.(check (list string))
        "second row" [ "1"; "2"; "3" ] (List.nth data 1)
  | Error (ParserError (loc, msg)) ->
      Alcotest.failf "Parse error at position %d: %s" loc msg

let test_stream_parsing () =
  let input = "a,b,c\n1,2,3\n4,5,6\n" in
  let stream = create_csv_stream input in

  match next_row stream with
  | Ok (stream1, Some row1) -> (
      Alcotest.(check (list string)) "first row" [ "a"; "b"; "c" ] row1;
      match next_row stream1 with
      | Ok (stream2, Some row2) -> (
          Alcotest.(check (list string)) "second row" [ "1"; "2"; "3" ] row2;
          match next_row stream2 with
          | Ok (stream3, Some row3) -> (
              Alcotest.(check (list string)) "third row" [ "4"; "5"; "6" ] row3;
              match next_row stream3 with
              | Ok (_, None) -> ()
              | Ok (_, Some _) -> Alcotest.fail "Expected end of stream"
              | Error (ParserError (loc, msg)) ->
                  Alcotest.failf "Parse error at position %d: %s" loc msg)
          | Ok (_, None) -> Alcotest.fail "Expected third row"
          | Error (ParserError (loc, msg)) ->
              Alcotest.failf "Parse error at position %d: %s" loc msg)
      | Ok (_, None) -> Alcotest.fail "Expected second row"
      | Error (ParserError (loc, msg)) ->
          Alcotest.failf "Parse error at position %d: %s" loc msg)
  | Ok (_, None) -> Alcotest.fail "Expected first row"
  | Error (ParserError (loc, msg)) ->
      Alcotest.failf "Parse error at position %d: %s" loc msg

let test_fold_stream () =
  let input = "1,2\n3,4\n5,6\n" in
  match
    fold_csv_stream input
      (fun acc row ->
        let sum =
          List.fold_left (fun s field -> s + int_of_string field) 0 row
        in
        acc + sum)
      0
  with
  | Ok total ->
      Alcotest.(check int) "sum of all fields" 21 total (* 1+2+3+4+5+6 = 21 *)
  | Error (ParserError (loc, msg)) ->
      Alcotest.failf "Parse error at position %d: %s" loc msg

let test_iter_stream () =
  let input = "a,b\nc,d\ne,f\n" in
  let count = ref 0 in
  match iter_csv_stream input (fun _ -> count := !count + 1) with
  | Ok () -> Alcotest.(check int) "row count" 3 !count
  | Error (ParserError (loc, msg)) ->
      Alcotest.failf "Parse error at position %d: %s" loc msg

let test_complex_csv () =
  let input =
    {|"Product","Price","Quantity","Description"
"Apple",1.50,100,"Fresh red apples"
"Banana",0.80,150,"Organic bananas"
"Orange",2.00,75,"Juicy oranges"|}
  in
  match parse_csv_with_headers input with
  | Ok (header, rows) ->
      Alcotest.(check (list string))
        "header"
        [ "Product"; "Price"; "Quantity"; "Description" ]
        header;
      Alcotest.(check int) "number of products" 3 (List.length rows);
      Alcotest.(check (list string))
        "apple row"
        [ "Apple"; "1.50"; "100"; "Fresh red apples" ]
        (List.nth rows 0)
  | Error (ParserError (loc, msg)) ->
      Alcotest.failf "Parse error at position %d: %s" loc msg

let basic_tests =
  [
    ("parse simple csv", `Quick, test_simple_csv);
    ("parse quoted csv", `Quick, test_quoted_csv);
    ("parse empty fields", `Quick, test_empty_fields);
    ("parse escaped quotes", `Quick, test_escaped_quotes);
    ("parse empty csv", `Quick, test_empty_csv);
    ("parse single row", `Quick, test_single_row);
  ]

let header_tests = [ ("parse csv with headers", `Quick, test_csv_with_headers) ]

let delimiter_tests =
  [ ("parse with custom delimiter", `Quick, test_custom_delimiter) ]

let stream_tests =
  [
    ("stream parsing", `Quick, test_stream_parsing);
    ("fold stream", `Quick, test_fold_stream);
    ("iter stream", `Quick, test_iter_stream);
  ]

let complex_tests = [ ("parse complex csv", `Quick, test_complex_csv) ]

let () =
  Alcotest.run "CSV Parser"
    [
      ("basic parsing", basic_tests);
      ("headers", header_tests);
      ("custom delimiters", delimiter_tests);
      ("streaming", stream_tests);
      ("complex data", complex_tests);
    ]
