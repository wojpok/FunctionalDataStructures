open Set ;;
open Maybe ;;
open Ordered ;;
open Tree ;;

module UnbalancedSet(S : ORDERED): SET with type t = S.t = struct
  type t = S.t
  let cmp = S.compare

  type tree = (unit, t) mtree
  type set = (unit, t) mset

  let reveal (_, t) = t

  let empty = 0, Leaf
  let singleton x = 1, Node((), Leaf, x, Leaf)
  let toList = TreeDefualts.toList
  let fromOrdList = TreeDefualts.fromOrdList ()
  let fromList = TreeDefualts.fromList cmp ()
  let size (s, _) = s
  let member = TreeDefualts.member cmp
  let find = TreeDefualts.find cmp

  let add e (s, tree) =
    let rec iter : tree -> tree option = function
    | Leaf -> return @@ Node ((), Leaf, e, Leaf)
    | Node (_, l, v, r) -> 
      let c = cmp e v in
      if c = 0 then
        None
      else if c = 1 then         
        let* n = iter r in return @@ Node((), l, v, n)
      else 
        let* n = iter l in return @@ Node((), n, v, r)
      in
      match iter tree with
      | None -> s, tree
      | Some t -> (s + 1, t)

  let delete e (s, tree) =
    let rec iter e : tree -> tree option = function
    | Leaf -> None
    | Node (_, l, v, r) -> 
      let c = cmp e v in
      if c = 0 then
        if l = Leaf then
          return r
        else if r = Leaf then
          return l
        else
          let max = TreeDefualts.findMax l in
          let* l = iter max l in
          return @@ Node((), l, max, r)
      else if c = -1 then 
        let* n = iter e l in return @@ Node((), n, v, r)
      else 
        let* n = iter e r in return @@ Node((), l, v, n)
      in
      match iter e tree with
      | None -> s, tree
      | Some t -> (s - 1, t)

  let union t1 t2 = TreeDefualts.union () cmp t1 t2
  let intersection t1 t2 = TreeDefualts.intersection () cmp t1 t2
  let difference t1 t2 = TreeDefualts.difference () cmp t1 t2
end