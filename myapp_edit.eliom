[%%shared
 open Eliom_lib
 open Eliom_content
 open Html.D
 open Html
]

[%%client
 open Html.Manip
]

[%%server
 let nb_per_line = 4

(* Fonction qui splitte la liste de string en une liste de
 * listes de taille nb_per_line (représentant en fait la grille *)
 let split char_list =
   let rec aux_split buf_lists buf size_buf = function
     | [] -> 
       let buf = (List.rev buf) in
       List.rev (buf::buf_lists)
     | elt::tail ->
       if size_buf = nb_per_line then
	 let buf = (List.rev buf) in
	 aux_split (buf::buf_lists) [elt] 1 tail
       else aux_split buf_lists (elt::buf) (size_buf+1) tail
   in
   aux_split [] [] 0 char_list

 let generate_section char =
   section ~a:[a_class ["carre"]] [pcdata char]

 let generate_line l = 
   let sections = List.map generate_section l in
   section ~a:[a_class ["ligne"]] sections
     
 let sections_of_lists lists =
   List.map generate_line lists
     
 (* Fonction qui append une case "+" à la fin de la grille *)
]
 
 let%client rec last = function
   | [] -> failwith "Empty list"
   | [x] -> x
   | _::t -> last t

 let%client add_plus corps plus =
   let l = children corps in
   let last_line_section = last l in
   let children = children last_line_section in
   if List.length children < ~%nb_per_line then 
     appendChild last_line_section plus
   else
     let newline = section ~a:[a_class ["ligne"]] [plus] in
     appendChild corps newline

 let%client add_square corps = 
   fun _ _ ->
     let input = input ~a:[a_input_type `Text;
			   a_maxlength 1;
			   a_class ["nouveauchar"]] () in
     let new_square = 
       section ~a:[a_class ["carre"]] [input]
     in
     add_plus corps new_square;
     Lwt.return ()

[%%server
 let content () =
   let%lwt char_list = Myapp_index.get_all_chars () in
   let cut_list = split char_list in
   (* Liste de section de classe "ligne" *)
   let section_list = sections_of_lists cut_list in
   let clea = section ~a:[a_id "clr"] [pcdata " "] in
   let corps = 
     section ~a:[a_class ["middle"]; a_id "edit_corps"]
	 section_list in
   let lien = 
     Raw.a ~a:[a_href (Xml.uri_of_string "javascript:void(0);");
	       a_class ["lien"]]
       [pcdata "+"]
   in
   let plus = 
     section ~a:[a_class ["carre-plus"]] [lien] in
   (* Ajout du plus à la fin des lignes *)
   let _ = 
     [%client
	 (Eliom_client.onload (fun () ->
	   add_plus ~%corps ~%plus)
	 : unit)]
   in
   (* Bind du plus à un évènement *)
   let _ =
     [%client
	 (let open Lwt_js_events in
	  let plus_but = To_dom.of_section ~%plus in
	  async(fun () -> clicks plus_but (add_square ~%corps))
	   : unit)
     ]
   in
   Lwt.return ([clea;corps])
]
