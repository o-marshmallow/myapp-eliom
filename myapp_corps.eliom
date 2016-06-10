[%%shared
   open Eliom_lib
   open Eliom_content
   open Html.D
]

[%%server

 let top_menu =
  let nom = h1 ~a:[a_id "nom"] [pcdata "Questions"] in
  let menu =
    [
      ul ~a:[a_class ["uliste"]] [
	li ~a:[a_class ["iliste"]] 
	  [a ~service:Myapp_services.main_service [pcdata "Mode"] ()];
	li ~a:[a_class ["iliste"]] 
	  [a ~service:Myapp_services.edit_service [pcdata "Edition"] ()];
      ]
    ]
  in
  [nav ~a:[a_id "menu"] (nom::menu)]
  

let header = 
  header top_menu

let page titre contenu =
  let%lwt contenu = contenu in
  let content = header::contenu in
  Lwt.return 
    (Eliom_tools.F.html
       ~title:titre
       ~css:[["css";"myapp.css"]]
       Html.F.(body content)
    )
]
