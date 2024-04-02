module type HEAP = sig
  type heap
  type t

  val empty : heap
  val isEmpty : heap -> bool

  val insert : t -> heap -> heap 
  val merge : heap -> heap -> heap
  
  val findMin : heap -> t
  val deleteMin : heap -> heap
end