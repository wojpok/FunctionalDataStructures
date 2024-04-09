open Set;;
open Tree;;
open Maybe;;

module type FCFSET = sig
  include SET
  val complement : set -> set
end

module FCFSet (FinSet : SET) : FCFSET with type t = FinSet.t = struct
  type set = Fin of FinSet.set  | CoFin of FinSet.set
  type t = FinSet.t

  let unpack = function | Fin a -> a | CoFin a -> a

  let complement = function
  | Fin a -> CoFin a
  | CoFin a -> Fin a

  let cmp = FinSet.cmp
  
  let reveal x = FinSet.reveal @@ unpack x
  let empty =  Fin (FinSet.empty)
  let singleton x = Fin (FinSet.singleton x)

  let toList = function 
  | CoFin _ -> failwith "toList"
  | Fin t -> FinSet.toList t

  let fromOrdList xs = Fin  (FinSet.fromOrdList xs)
  let fromList xs = Fin (FinSet.fromList xs)
  let size = function 
  | CoFin _ -> failwith "a" 
  | Fin t -> FinSet.size t

  let member k = function 
  | Fin t -> FinSet.member k t
  | CoFin t -> not @@ FinSet.member k t

  let find k = function
  | Fin t -> FinSet.find k t
  | CoFin t -> failwith "find"

  let add k = function
  | Fin t -> Fin (FinSet.add k t)
  | CoFin t -> CoFin (FinSet.delete k t)

  let delete k = function
  | Fin t -> Fin (FinSet.delete k t)
  | CoFin t -> CoFin (FinSet.add k t)

  let union t1 t2 = Fin (FinSet.union (unpack t1) (unpack t2))
  let intersection t1 t2 = Fin (FinSet.intersection (unpack t1) (unpack t2))
  let difference t1 t2 = Fin (FinSet.difference (unpack t1) (unpack t2))
end