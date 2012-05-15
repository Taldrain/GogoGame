
class event =
object
  val mutable handlers = []
  method add_handler h = handlers <- h:: handlers
  method raise = List.iter (fun f -> f ()) handlers
end

class ['params] param_event params =
object
  inherit event
  val params = params
  method raise = List.iter (fun f -> f params) handlers
end

class var_param_event params =
object
  inherit event
  val mutable params = params
  method set_params p = params <- p
  method raise = List.iter (fun f -> f params) handlers
end