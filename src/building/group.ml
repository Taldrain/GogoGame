(**
Cette classe sert a representer un groupe de pierres sur la board
C'est une classe afin de permettre de l'etendre plus tard avec des
combinaisons et groupes speciaux
**)
open BatPervasives
open Entities.Move
open Entities.Color
open Entities.Vertex

class group color =
object (self)
  val mutable stones = []
  val mutable count = 0
  val c = color
  val id = get_id ()

  method add_stone v =
    (
      stones <- v:: stones;
      count <- count + 1
    )

  method stones = stones
  method contains s = List.mem s stones
end
