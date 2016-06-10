[%%shared
 open Eliom_lib
 open Eliom_content
 open Html
 open Html.D
]

[%%server
 module type DB = 
 sig 
   val get_chars : unit -> int list Lwt.t
   val get_char : int -> string Lwt.t
   val add_char : string -> int Lwt.t
 end

 module Db : DB = struct
   let db = Ocsipersist.open_table "chars"
   let lock = Lwt_mutex.create ()
   (** On crée une référence au niveau d'eliom **)
   let last_key =
     Eliom_reference.eref
       ~persistent:"index"
       ~scope:Eliom_common.global_scope (-1)
   (** Récupération d'un caractère **)
   let get_char id = Ocsipersist.find db (string_of_int id)
   let get_chars () =
    let%lwt index = Eliom_reference.get last_key in
    let rec aux n l = if n > index then l else aux (n+1) (n::l) in
    Lwt.return (aux 0 [])
   let add_char v =
     let%lwt () = Lwt_mutex.lock lock in
     let%lwt index = Eliom_reference.get last_key in
     let _ = Printf.printf "%d\n" index in
     let index = index + 1 in
     let%lwt () = Eliom_reference.set last_key index in
     let () = Lwt_mutex.unlock lock in
     let%lwt () = Ocsipersist.add db (string_of_int index) v in
     Lwt.return index
 end

 let basics = ["我";"你";"中";"国";"是";"的";"在";"奥"]
   
 let _ = List.map (Db.add_char) basics
]

let first_char = function
  | [] -> "..."
  | a::_ -> a

let () = Random.self_init ()

let%shared random_elt l = 
  let taille = List.length l in
  let index = Random.int taille in
  List.nth l index  

let%shared random_liste l =
  let rec rand_aux buf = function
    | [] -> buf
    | a::tail -> 
      let avant = (Random.int 2) = 1 in
      if avant then rand_aux (a::buf) tail
      else rand_aux (buf@[a]) tail
  in
  rand_aux [] l

let%server custom_h = Eliom_common.create_scope_hierarchy "custom"

let%server get_all_chars () =
  let%lwt chars = Db.get_chars () in
  let%lwt char2 = 
    Lwt_list.map_s 
      (fun index ->
	Db.get_char index)
      chars
  in Lwt.return (char2)

let%server content () =
  (** On récupère la liste de tous les index des caractères **)
  let%lwt char2 = get_all_chars () in
  let lchar = random_liste char2 in
  let number = List.length lchar in
  let (scope:[< Eliom_common.all_scope]) = `Client_process(custom_h) in
  let index = Eliom_reference.eref ~scope:scope 0 in
  let prem = first_char lchar in
  let clea = [section ~a:[a_id "clr"] [pcdata " "]] in
  let char = section ~a:[a_id "case"] [pcdata prem] in
  let next = button  ~a:[a_id "next"] [pcdata "Suivant"] in
  let prev = button  ~a:[a_id "prev"] [pcdata "Précédent"] in
  let incl = [section ~a:[a_id "corps"; 
			  a_class ["middle"]] 
		 ([char;prev;next])] in
  let _ =
    [%client
     (let open Lwt_js_events in
      let i = ref 0 in
      let case = To_dom.of_section ~%char in
      let nbut = To_dom.of_button ~%next in
      let pbut = To_dom.of_button ~%prev in
      let _ = async (fun () -> clicks nbut 
	(fun _ _ ->
	  let ipu = ((!i) + 1) mod (~%number) in
	  let () = i := ipu in
	  let newchar = List.nth ~%lchar ipu in
	  let js_str = Js.string newchar in
	  let () = case##.textContent := Js.some js_str in
	    Lwt.return () ))
      in
      async (fun () -> clicks pbut
	(fun _ _ -> 
	  let ipu = ((!i) - 1 + (~%number)) mod (~%number) in
	  let () = i := ipu in
	  let newchar = List.nth ~%lchar ipu in
	  let js_str = Js.string newchar in
	  let () = case##.textContent := Js.some js_str in
	  Lwt.return ()
	)
      )
      : unit)
    ]
  in
  Lwt.return (clea@incl)
