# fp-lab-4

## Лабораторная работа №4

**Студент:** Мазайкина Мария Дмитриевна  
**Группа:** P3309  

## Описание

Реализация библиотеки парсер-комбинаторов с разработкой **JSON парсера** и **потокового CSV парсера**.

## Структура проекта

```
.
├── lib/
│   ├── parser.ml           # Базовые парсер-комбинаторы
│   ├── basic_parsers.ml    # Элементарные парсеры
│   ├── json_parser.ml      # Парсер JSON
│   └── csv_parser.ml       # Потоковый парсер CSV
├── test/
│   ├── test_ex.ml          # Тесты JSON парсера
│   ├── test_csv.ml         # Тесты CSV парсера 
├── bin/
│   └── main.ml             # Демонстрация работы парсеров
└── dune-project
```

## Архитектура

### 1. Базовый парсер (`lib/parser.ml`)

**Типы:**
```ocaml
type input = { loc : int; str : string }
type parser_error = ParserError of int * string
type 'a parser = input -> (input * 'a, parser_error) result
```

**Комбинаторы:**
- `return` — создаёт парсер, всегда возвращающий значение
- `>>=` (bind) — монадическая композиция парсеров
- `<|>` — альтернатива (пробует второй парсер, если первый не сработал)
- `map` / `>|=` — преобразование результата
- `<*>` — аппликативная композиция
- `*>` / `<*` — последовательность с отбрасыванием результата

### 2. Базовые парсеры (`lib/basic_parsers.ml`)

- `char_parser` — распознаёт конкретный символ
- `string_parser` — распознаёт строку
- `span_parser` — собирает последовательность символов по предикату
- `parse_if` — распознаёт символ, удовлетворяющий условию
- `many` — повторяет парсер 0 или более раз
- `sequence` — выполняет список парсеров последовательно
- `sep_by` — парсит элементы, разделённые разделителем

### 3. JSON парсер (`lib/json_parser.ml`)

**Поддерживаемые типы:**
```ocaml
type json_value =
  | JsonNull
  | JsonBool of bool
  | JsonNumber of float
  | JsonString of string
  | JsonArray of json_value list
  | JsonObject of (string * json_value) list
```

**Возможности:**
- Все типы JSON (null, boolean, number, string, array, object)
- Вложенные структуры любой глубины
- Экранирование в строках (`\"`, `\\`, `\n`, `\t`, и т.д.)
- Числа с плавающей точкой и научная нотация (e.g., `1.23e-4`)
- Обработка пробелов и переносов строк

**Пример использования:**
```ocaml
let json = {|{"name": "Alice", "age": 30, "active": true}|}
match parse_json json with
| Ok value -> print_endline (show_json_value value)
| Error (ParserError (loc, msg)) -> Printf.printf "Error at %d: %s\n" loc msg
```

### 4. Потоковый CSV парсер (`lib/csv_parser.ml`)

**Конфигурация:**
```ocaml
type csv_config = {
  delimiter : char;        (* По умолчанию ',' *)
  quote_char : char;       (* По умолчанию '"' *)
  escape_char : char option; (* Опционально *)
}
```

**Возможности:**
- Стандартный формат CSV
- Поля в кавычках с пробелами и запятыми
- Экранирование кавычек (`""` → `"`)
- Пустые поля
- Настраиваемые разделители (`,`, `;`, `|`, и т.д.)

**Потоковый API:**
```ocaml
let stream = create_csv_stream ~config csv_string

match next_row stream with
| Ok (new_stream, Some row) -> (* обработка строки *)
| Ok (_, None) -> (* конец файла *)
| Error e -> (* ошибка *)

fold_csv_stream csv_string (fun acc row -> (* ... *)) init_acc

iter_csv_stream csv_string (fun row -> print_endline (show_csv_row row))
```

**Обычный API:**
```ocaml
match parse_csv csv_string with
| Ok rows -> List.iter print_row rows
| Error e -> handle_error e

match parse_csv_with_headers csv_string with
| Ok (headers, data_rows) -> (* ... *)
| Error e -> handle_error e
```

## Запуск

### Сборка проекта
```bash
dune build
```

### Запуск демонстрации
```bash
dune exec ./bin/main.exe
```

Вывод покажет примеры работы JSON и CSV парсеров.

### Запуск тестов
```bash
dune runtest --force
```

## Выводы

Проект демонстрирует функциональный подход к парсингу через композицию комбинаторов, полное покрытие тестами и расширяемость. Библиотека может быть расширена для парсинга других форматов (XML, YAML, TOML) путём добавления новых комбинаторов.

Топово отпарсила ✅.