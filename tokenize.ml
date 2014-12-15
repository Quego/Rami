(* USAGE:

 * in ocaml interpreter
      ocamlc myStream.ml
      ledit ocaml dynlink.cma camlp4o.cma myStream.cmo
      #use "<file>_parser.ml";;

 * Pour interpréter un fichier contenant "parser"
      ledit ocaml dynlink.cma camlp4o.cma 
      #use "<file>_parser.ml";;

 * Pour afficher la traduction en ocaml: 
      camlp4o -impl <file>_parser.ml

 * Pour générer  la traduction dans un fichier .ml:
      camlp4o -impl <File>_parser.ml -o <file>.ml

 * Pour interpréter le fichier ocaml généré: 
      ledit ocaml dynlink.cma camlp4o.cma myStream.cmo
      #use "<file>.ml";;
 *)

(*
#load "dynlink.cma" 
#load "camlp4o.cma"
#load "myStream.cmo";;*)


type filename = string;;

(*  How to parse a string with a stream parser ? *)

let (parse_with: (char Stream.t -> 'result) -> string -> bool * 'result) =
  fun my_parser string ->
	begin
	  print_string (String.concat "" [ "\n parser \"" ; string ; "\" = \n"]) ;
	  MyStream.parse_with my_parser (Stream.of_string string)
	end;;

(* ======= Grammar of identifiers ===================

   Ident  -> Letter Ident_Cont
   Ident_Cont -> "" 
   Ident_Cont -> (Letter | Digit | "_")  Ident_Cont
  =================================================== *)

let rec (ident: char Stream.t -> string) = 
   parser
   | [< ' ('a'..'z' as char) ; string = ident_cont >] -> (String.make 1 char) ^ string

and (ident_cont: char Stream.t -> string) = 
   parser
   | [< ' ('a'..'z' | 'A'..'Z' | '0'..'9' | '_' as char) ; string = ident_cont >] -> (String.make 1 char) ^ string
   | [< >] -> "";;

let rec (identMaj: char Stream.t -> string) = 
   parser
   | [< ' ('A'..'Z' as char) ; string = ident_cont >] -> (String.make 1 char) ^ string

and (identMaj_cont: char Stream.t -> string) = 
   parser
   | [< ' ('a'..'z' | 'A'..'Z' | '0'..'9' | '_' as char) ; string = ident_cont >] -> (String.make 1 char) ^ string
   | [< >] -> "";;


   (* === Grammar of integers ==================
   Integer -1->  At_least_one_digit
   At_least_one_digit -2-> Digit . Some_Digit
   Some_Digit -3-> ""
   Some_Digit -4-> Digit . Some_Digit
  =============================================== *)

let rec (integer: char Stream.t -> int) = fun stream -> at_least_one_digit 0 stream

and (at_least_one_digit: int -> char Stream.t -> int) = fun i ->
      parser
	| [< d = digit ; r = some_digit (10*i+d) >] -> r
		  
and (digit: char Stream.t -> int) = 
  parser
    | [< ' ('0'..'9' as char) >] -> (int_of_char char) - (int_of_char '0')
	      
and (some_digit: int -> char Stream.t -> int) = fun i ->
      parser
	| [< d = digit ; r = some_digit (10*i+d) >] -> r
	| [< >] -> i


(* 5. === symbols ==== *)

let rec (lpar : char Stream.t -> string ) = 
  parser
    | [< ' ('(' as char) >] -> String.make 1 char
;;

let rec (rpar : char Stream.t -> string ) = 
  parser
    | [< ' (')' as char) >] -> String.make 1 char
;;

let rec (symbol: char Stream.t -> string) = 
  parser
    | [< ' ('('|')'|'['|']'|'{'|'}'|','|'!'|'%'|'@'|'*'|'+'|'/' as char) >] -> String.make 1 char
    | [< ' (':'|';'|'='|'<'|'>'|'.'|'|'|'''|'-' as char) ; string = symbol_cont >] -> (String.make 1 char) ^ string

and (symbol_cont: char Stream.t -> string) = 
  parser
    | [< ' (':'|';'|'='|'<'|'>'|'.'|'|'|''' as char) ; string = symbol_cont >] -> (String.make 1 char) ^ string
    | [< >] -> ""

(* 6. How to transform a stream of char into a stream of token (= typed words) with space elimination *)

type keywords = string list
;;

type symbols = string list
;;

type bools = string list
;;

type token = 
  | Kwd of string
  | LPar
  | RPar
  | Other of string
  | Smb of string
  | Ident of string
  | IdentMaj of string
  | Int of int 
  | Bool of string
;;

type token_dico = 
  | Identd of string
;;

let (tokenize_with : keywords -> symbols -> bools -> char Stream.t -> token Stream.t ) = fun keywords  symbols bools stream -> 
  let rec (tokenize : char Stream.t -> token Stream.t) =
  parser
    | [< ' (' '|'\t'|'\n') ; s >] -> tokenize s
    | [< str = ident ; tokens = tokenize >] ->
 		  let token = if List.mem str keywords then Kwd str 
		    else if  List.mem str bools then Bool str 
		    else Ident str
		  in Stream.icons token tokens
    | [< str = identMaj ; tokens = tokenize >] -> Stream.icons (IdentMaj str) tokens
    | [< str = integer ; tokens = tokenize >] -> Stream.icons (Int str) tokens
    | [< str = lpar ; tokens = tokenize >] -> Stream.icons LPar tokens
    | [< str = rpar ; tokens = tokenize >] -> Stream.icons RPar tokens
    | [< str = symbol ; tokens = tokenize >] -> 
		  let token = if List.mem str symbols  then Smb str else Other str
		  in Stream.icons token tokens 
    | [< >] -> Stream.sempty
      in tokenize stream
;;


let rec (tokenize_dico : char Stream.t -> token_dico Stream.t) = 
  parser
    | [< ' ('\n') ; tokens = tokenize_dico >] -> tokens
    | [< str = ident ; tokens = tokenize_dico >] -> Stream.icons (Identd str) tokens
    | [< >] -> Stream.sempty
;;




let  (tokenizer: char Stream.t -> token Stream.t) = fun stream -> tokenize_with ["joueurs";"jeu"; "pioche";"tour"] [ "(" ; ")"; "*"] ["true";"false"]  stream
;;

let  (tokenize_file: string -> bool * 't Stream.t) = fun filename -> MyStream.parse_file_with tokenizer filename
;;

let (tokenize_list_file : string -> bool * token list ) = fun str -> 
  let (bool,token_stream )=  tokenize_file str 
  in (bool, MyStream.to_list token_stream)
;;



(* TEST *)

let (test_tokenize_dico:string -> bool * token_dico list) = fun string ->
      let (bool,token_stream) = parse_with tokenize_dico string 
      in (bool, MyStream.to_list token_stream)
;;


let (test_tokenize_with: keywords -> symbols -> bools -> string -> bool * token list) = fun keywords symbols bools string ->
      let (bool,token_stream) = parse_with (tokenize_with keywords symbols bools) string 
      in (bool, MyStream.to_list token_stream)
;;

let (test_tokenize: string -> bool * token list) = fun s -> test_tokenize_with ["joueurs";"jeu"; "pioche";"tour"] [ "(" ; ")"; "*"] ["true";"false"] s
;;
 (*

test_tokenize "(joueurs
(Pascal 17 true (S P O I N E * ))
(Laurent 42 true (N S A V))
(Marion 0 false (E E I N)))
(jeu 
( F A C I L E)
( C A * B R ))
(pioche
C S N H I )
(tour 3)"
;;



let _ =  tokenize_list_file "test.ml" 
;;


*)
