(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

open Camlp4.PreCast
open Syntax
open Camlp4_to_ppx

let replace_field_name loc rtl_name field_name = 
  if rtl_name <> field_name then
    replace loc field_name

let print_after_rtlname loc rtl_name field_name fmt = 
  let open Printf in
  print_after loc @@
    if rtl_name = field_name then sprintf fmt ""
    else sprintf fmt (sprintf "[@rtlname \"%s\"]" rtl_name)

let () = 
    EXTEND Gram
        GLOBAL: module_expr module_type expr;

        hc_uident: [ [ x=UIDENT -> x,_loc ] ];
        hc_lident: [ [ x=LIDENT -> x,_loc ] ];

        hc_ident: [ [ x = hc_uident -> x | x = hc_lident -> x ] ];

        hc_bits: [ [ "["; x = [expr -> _loc]; "]" -> x,_loc ] ];

        hc_vector: [ [ o=["{|" -> _loc]; x = [expr -> _loc]; c=["|}" -> _loc] -> `array,(o,x,c) 
                     | o=["{" -> _loc]; x = [expr -> _loc]; c=["}" -> _loc] -> `list,(o,x,c) ] ];
            
        hc_signal:

        [ [ o=["(" -> _loc]; LIDENT; x=[":" -> _loc]; LIST1 [x = UIDENT -> x] SEP "."; 
            c=[")" -> _loc] -> 
            replace o "";
            replace x ": 'a ";
            replace c ".t;"
          | (rtl_name,i) = hc_ident; v = OPT hc_vector; b = OPT hc_bits -> begin
            let field_name = String.lowercase_ascii rtl_name in
            match v,b with
            | None,None -> (* a *)
              replace_field_name i rtl_name field_name;
              print_after_rtlname i rtl_name field_name " : 'a%s;"
            | None,Some(b,e) -> (* a[1] *)
              replace_field_name i rtl_name field_name;
              print_after i " : 'a";
              print_before b "@bits ";
              print_after_rtlname e rtl_name field_name "%s;"
            | Some(`array,(o,v,c)),None -> (* a{|7|} *)
              replace_field_name i rtl_name field_name;
              print_after i " : 'a array";
              replace o "[@length ";
              replace c "]";
              print_after_rtlname c rtl_name field_name "%s;"
            | Some(`array,(o,v,c)),Some(b,e) -> (* a{|7|}[3] *)
              replace_field_name i rtl_name field_name;
              print_after i " : 'a array";
              replace o "[@length ";
              replace c "]";
              print_before b "@bits ";
              print_after_rtlname e rtl_name field_name "%s;"
            | Some(`list,(o,v,c)),None -> (* a{7} *)
              replace_field_name i rtl_name field_name;
              print_after i " : 'a list";
              replace o "[@length ";
              replace c "]";
              print_after_rtlname c rtl_name field_name "%s;"
            | Some(`list,(o,v,c)),Some(b,e) -> (* a{7}[3] *)
              replace_field_name i rtl_name field_name;
              print_after i " : 'a list";
              replace o "[@length ";
              replace c "]";
              print_before b "@bits ";
              print_after_rtlname e rtl_name field_name "%s;"
          end
        ] ];

        hc_vector_simple: [ [ o=["{|" -> _loc]; c=["|}" -> _loc] -> `array,o,c 
                            | o=["{"  -> _loc]; c=["}"  -> _loc] -> `list,o,c ] ];

        hc_signal_simple:
        [ [ o=["(" -> _loc]; LIDENT; x=[":" -> _loc]; LIST1 [x = UIDENT -> x] SEP "."; 
            c=[")" -> _loc] -> 
            replace o "";
            replace x ": 'a ";
            replace c ".t;"
          | (rtl_name,i) = hc_ident; v = OPT hc_vector_simple -> begin
            let field_name = String.lowercase_ascii rtl_name in
            match v with
            | None -> 
              replace_field_name i rtl_name field_name;
              print_after i " : 'a;"
            | Some(`array,o,c) -> 
              replace_field_name i rtl_name field_name;
              replace o " : 'a array;";
              replace c ""
            | Some(`list,o,c) -> 
              replace_field_name i rtl_name field_name;
              replace o " : 'a list;";
              replace c ""
          end
        ] ];

        module_expr: AFTER "top" 
        [ [ i = ["interface" -> _loc]; if_ports = LIST0 [x = hc_signal -> x]; 
            e = ["end" -> _loc] ->
            replace i "struct\ntype 'a t = {";
            replace e "}[@@deriving hardcaml]\nend";
            <:module_expr< >>
        ] ];

        module_type: AFTER "sig"
        [ [ i=["interface" -> _loc]; LIST0 [x = hc_signal_simple -> x]; e=["end" -> _loc] ->
            replace i "sig\ntype 'a t = {";
            replace e "}[@@deriving hardcaml]\nend";
            <:module_type< >>
        ] ];

        expr: LEVEL "."
        [ [ e0 = SELF; "."; "["; e1 = SELF; colon_loc=[":" -> _loc]; e2 = SELF; "]" ->
            print_before _loc "[%hw ";
            replace colon_loc ",";
            print_after _loc "]";
            <:expr<>>
        ] ];

    END

let linkme = ()

