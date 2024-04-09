open Set ;;
open Maybe ;;
open Ordered ;;
open Tree ;;

module type SEED = sig val seed: int end

module TreepSet(H: SEED)(S : ORDERED): SET with type t = S.t = struct
  let seed = H.seed

  let hash = Rand.hash

  type t = S.t
  let cmp = S.compare
  type tree = (int, t) mtree
  type set = (int * int * tree)

  let drop_seed (s, _, t) = (s, t)

  let reveal (_, _, t) = 
    let rec mapTree = function
    | Leaf -> Leaf
    | Node (_, l, v, r) -> Node((), mapTree l, v, mapTree r)
  in mapTree t

  let empty = 0, seed, Leaf
  let singleton x = 1, seed, Node(seed, Leaf, x, Leaf)
  let toList (_,_,t) = TreeDefualts.toList ((),t)
  let fromOrdList xs = failwith "TODO"
  let fromList xs = failwith "TODO"
  let size (s, _, _) = s
  let member x t = TreeDefualts.member cmp x (drop_seed t)
  let find x t = TreeDefualts.find cmp x (drop_seed t)

  let node_prio = function | Leaf -> 0 | Node(p, _, _, _) -> p

  let balance = function
  | (Node(p1, (Node(p2, a, x, b)), y, (Node(p3, c, z, d)))) as t -> 
    if p3 > p1 then
      TreeDefualts.rotateLeft t
    else if p2 > p1 then
      TreeDefualts.rotateRight t
    else
      t
  | t -> t

  let add x (s, seed, tree) = 
    let nseed = hash seed in
    let rec iter = function
    | Leaf -> Some (Node(nseed, Leaf, x, Leaf))
    | Node(c, l, v, r) ->
      if cmp x v = 1 then       let* n = iter r in return @@ balance @@ Node(c, l, v, n)
      else if cmp x v = -1 then let* n = iter l in return @@ balance @@ Node(c, n, v, r)
      else None
    in
    match iter tree with
    | None -> (s, nseed, tree)
    | Some t -> (s + 1, nseed, balance t)

  let rec findMax = function
    | Leaf -> failwith "findMax"
    | Node (_, _, v, Leaf) -> v
    | Node (_, _, _, r) -> findMax r

  let rec delete x (s, seed, tree) = 
    let rec iter x = function
    | Leaf -> None
    | Node (c, l, v, r) when cmp x v =  1 -> rfmap (iter x r) @@ fun t -> Node(c, l, v, t)
    | Node (c, l, v, r) when cmp x v = -1 -> rfmap (iter x l) @@ fun t -> Node(c, t, v, r)
    | Node (_, Leaf, _, Leaf) -> return Leaf
    | Node (_, Leaf, _, r) -> return r
    | Node (_, l, _, Leaf) -> return l
    | Node (_, l, _, r) as t ->
      if node_prio l > node_prio r then
        iter x @@ TreeDefualts.rotateRight t
      else
        iter x @@ TreeDefualts.rotateLeft t
    in
    match iter x tree with
    | None -> (s, seed, tree)
    | Some t -> (s - 1, seed, balance t)

  let union t1 t2 = let (a, b) = TreeDefualts.union seed cmp (drop_seed t1) (drop_seed t2) in (a, seed, b)
  let intersection t1 t2 = let (a, b) = TreeDefualts.intersection seed cmp (drop_seed t1) (drop_seed t2) in (a, seed, b)
  let difference t1 t2 = let (a, b) = TreeDefualts.difference seed cmp (drop_seed t1) (drop_seed t2) in (a, seed, b)
end