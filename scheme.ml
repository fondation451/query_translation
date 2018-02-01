(* Xml to/from database scheme *)

open Xml;;

exception XML_NOT_VALID;;

type scheme = (string * string list) list;; (* (table's name, list of fields *)

let xml_field_to_scheme xml_field =
  match xml_field with
  |Element(field_name, _, []) -> field_name
  |_ -> raise XML_NOT_VALID
;;

let xml_table_to_scheme xml_table =
  match xml_table with
  |Element(table_name, _, field_l) -> table_name, List.fold_left (fun out field -> (xml_field_to_scheme field)::out) [] field_l
  |_ -> raise XML_NOT_VALID
;;

let xml_to_scheme xml : scheme =
  match xml with
  |Element(_, _, xml_table_l) -> List.fold_left (fun out table -> (xml_table_to_scheme table)::out) [] xml_table_l
  |_ -> raise XML_NOT_VALID
;;

let string_to_scheme str = xml_to_scheme (parse_string str);;

let file_to_scheme f = xml_to_scheme (parse_file f);;

let scheme_to_xml scheme =
  Element("?xml",
          ["version", "1.0" ; "encoding", "UTF-8"],
          List.fold_left
            (fun out (table, field_l) ->
              (Element(table,
              [],
              List.fold_left (fun out field -> (Element(field, [], []))::out) [] field_l))::out) [] scheme)
;;

let scheme_to_string scheme = to_string_fmt (scheme_to_xml scheme);;

let scheme_to_file scheme file =
  let f = open_out file in
  output_string f (scheme_to_string scheme);
  close_out f
;;

let field_of table scheme = List.assoc table scheme;;

let table_of scheme = List.map fst scheme;;
