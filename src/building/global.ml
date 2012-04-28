exception Global_not_initialized

class ['a] global name =
object (self)
  val name:string = name
	val mutable value:'a = Obj.magic None
  val mutable empty = true

  method set v = empty <- false; value <- v
  method get = if empty then raise Global_not_initialized else value
  method unset = empty <- true
  method name = name
  method isdef = empty
  method opt = if empty then None else Some value
end