[%%shared
    open Eliom_lib
    open Eliom_content
    open Html.D
]

module Myapp_app =
  Eliom_registration.App (
    struct
      let application_name = "myapp"
      let global_data_path = None
    end)

let () =
  Myapp_app.register
    ~service:Myapp_services.main_service
    (fun () () ->
      let middle = Myapp_index.content ()
      in Myapp_corps.page "Index" middle
    );
  Myapp_app.register
    ~service:Myapp_services.edit_service
    (fun () () ->
      let middle = Myapp_edit.content () in
      Myapp_corps.page "Edition" middle);
  Myapp_app.register
    ~service:Myapp_services.mode_service
    (fun () () -> 
      let middle = Lwt.return ([pcdata "Modes"]) in
      Myapp_corps.page "Modes" middle)
