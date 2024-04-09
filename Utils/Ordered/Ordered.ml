module type ORDERED = sig
  type t

  val compare : t -> t -> int
end



module OrderedInt: ORDERED = struct
  type t = int

  let compare = Int.compare
end