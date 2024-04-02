open Ordered ;;
open Set ;;
open Maybe ;;

module type MAP =
sig
  type key
  type value
  type map

  val empty : map
  val bind : key -> value -> map -> map
  val lookup : key -> map -> value option
  
  (* NONE gdy klucz nie występuje w słowniku *)
end

module type NONEMPTY = sig 
  type t 
  val soleElem : t 
end

module Map 
  (Set: functor (Elem: ORDERED) -> SET with type t = Elem.t)
  (S : ORDERED)
  (N : NONEMPTY)
: MAP with type key = S.t 
      and type value = N.t 
= struct

  type key = S.t
  type value = N.t

  module M = Set(struct
    type t = S.t * N.t
    let compare (l, _) (r, _) = S.compare l r
  end)

  type map = M.set

  let empty = M.empty
  let bind k v m = M.delete (k, v) m |> M.add (k, v)
  let lookup k m = let* (_, v) = M.find (k, N.soleElem) m in return v
end