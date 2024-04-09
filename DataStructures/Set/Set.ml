open Tree;;

module type SET = sig
  type t

  type set
  val empty : set
  val singleton : t -> set
  val toList : set -> t list
  val fromList : t list -> set
  val fromOrdList : t list -> set
  val size : set -> int
  val member : t -> set -> bool
  val find : t -> set -> t option
  val add : t -> set -> set
  val delete : t -> set -> set
  val union : set -> set -> set
  val intersection : set -> set -> set
  val difference : set -> set -> set

  val reveal: set -> (unit, t) mtree
  val cmp : t -> t -> int
end