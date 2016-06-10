let mode_service =
  Eliom_service.create
    ~id:(Eliom_service.Path ["mode"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

let edit_service =
  Eliom_service.create
    ~id:(Eliom_service.Path ["edit"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

let main_service =
  Eliom_service.create
    ~id:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()
