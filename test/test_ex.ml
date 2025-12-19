open Lab4.Parser
open Lab4.Json_parser

let test_success name input expected () =
  match parse_json input with
  | Ok value -> Alcotest.(check string) name expected (show_json_value value)
  | Error (ParserError (loc, msg)) ->
      Alcotest.failf "Parse error at position %d: %s" loc msg

let basic_value_tests =
  [
    ("parse null", `Quick, test_success "null" {| null |} "null");
    ("parse true", `Quick, test_success "true" {| true |} "true");
    ("parse false", `Quick, test_success "false" {| false |} "false");
  ]

let number_tests =
  [
    ("parse integer", `Quick, test_success "int" {| 42 |} "42.");
    ("parse negative", `Quick, test_success "negative" {| -123 |} "-123.");
    ("parse float", `Quick, test_success "float" {| 3.14159 |} "3.14159");
    ("parse exp", `Quick, test_success "exp" {| 1.23e-4 |} "0.000123");
  ]

let string_tests =
  [
    ( "parse simple string",
      `Quick,
      test_success "string" {| "hello" |} {|"hello"|} );
  ]

let array_tests =
  [
    ("parse empty array", `Quick, test_success "empty" {| [] |} "[]");
    ( "parse simple array",
      `Quick,
      test_success "simple" {| [1, 2, 3] |} {|[1., 2., 3.]|} );
    ( "parse mixed array",
      `Quick,
      test_success "mixed" {| [null, true, 42, "hello"] |}
        {|[null, true, 42., "hello"]|} );
  ]

let object_tests =
  [
    ("parse empty object", `Quick, test_success "empty" {| {} |} "{}");
    ( "parse simple object",
      `Quick,
      test_success "simple" {| {"key": "value"} |} {|{"key": "value"}|} );
    ( "parse complex object",
      `Quick,
      test_success "complex"
        {|
  {
    "hello": [false, true, null, 42, "foo\n", [1, -2, 3.1415, 4e-6, 5E6, 0.123e+1]],
    "world": null
  }
  |}
        {|{"hello": [false, true, null, 42., "foo\n", [1., -2., 3.1415, 4e-06, 5000000., 1.23]], "world": null}|}
    );
  ]

let () =
  Alcotest.run "JSON Parser"
    [
      ("basic values", basic_value_tests);
      ("numbers", number_tests);
      ("strings", string_tests);
      ("arrays", array_tests);
      ("objects", object_tests);
    ]
