open Set ;;
open Maybe ;;
open Ordered ;;
open Tree ;;

module RedBlackSet(S : ORDERED): SET with type t = S.t = struct
  type color = R | B
  type t = S.t
  let cmp = S.compare
  type tree = (color, t) mtree
  type set = (color, t) mset

  let reveal (_, t) = 
    let rec mapTree = function
    | Leaf -> Leaf
    | Node (_, l, v, r) -> Node((), mapTree l, v, mapTree r)
  in mapTree t

  let empty = 0, Leaf
  let singleton x = 1, Node(B, Leaf, x, Leaf)
  let toList = TreeDefualts.toList
  let fromOrdList xs = 
    let rec findH = function
    | Leaf -> (0, 0)
    | Node (_, l, _, r) -> 
      let (lmax, lmin) = findH l in
      let (rmax, rmin) = findH r in
      (1 + max lmax rmax, 1 + min lmin rmin)
    in
    let rec colorize d = function
    | Leaf -> Leaf
    | Node(_, l, x, r) when d = 0 -> Node(R, l, x, r)
    | Node(_, l, x, r) -> Node(B, colorize (d - 1) l, x, colorize (d - 1) r)
    in
    let (c, t) = TreeDefualts.fromOrdList B xs in
    let (max, min) = findH t in
    if max = min then
      (c, t)
    else
      (c, colorize max t)

  let fromList xs = fromOrdList @@ List.sort cmp xs
  let size (s, _) = s
  let member = TreeDefualts.member cmp
  let find = TreeDefualts.find cmp

  let balance = function
  | Node(B, Node(R, Node(R, a, x, b), y, c), z, d)
  | Node(B, Node(R, a, x, Node(R, b, y, c)), z, d)
  | Node(B, a, x, Node(R, Node(R, b, y, c), z, d))
  | Node(B, a, x, Node(R, b, y, Node(R, c, z, d))) ->
    Node(R, Node(B, a, x, b), y, (Node(B, c, z, d)))
  | t -> t

  let makeRoot = function
  | Leaf -> Leaf
  | Node(_, a, b, c) -> Node(B, a, b, c)

  let add x (s, tree) = 
    let rec iter = function
    | Leaf -> Some (Node(R, Leaf, x, Leaf))
    | Node(c, l, v, r) ->
      if cmp x v = 1 then       let* n = iter r in return @@ balance @@ Node(c, l, v, n)
      else if cmp x v = -1 then let* n = iter l in return @@ balance @@ Node(c, n, v, r)
      else None
    in
    match iter tree with
    | None -> (s, tree)
    | Some t -> (s + 1, makeRoot t)

  let rec findMax = function
    | Leaf -> failwith "findMax"
    | Node (_, _, v, Leaf) -> v
    | Node (_, _, _, r) -> findMax r

  let rec delete x (s, tree) = 
    let rec iter x = function
    | Leaf -> None
    | Node (c, l, v, r) when cmp x v =  1 -> rfmap (iter x r) @@ fun t -> Node(c, l, v, balance t)
    | Node (c, l, v, r) when cmp x v = -1 -> rfmap (iter x l) @@ fun t -> Node(c, balance t, v, r)
    | Node (_, Leaf, _, Leaf) -> return Leaf
    | Node (_, Leaf, _, r) -> return r
    | Node (_, l, _, Leaf) -> return l
    | Node (c, l, _, r) ->
      let max = findMax l in
      let* t = iter max l in
      return @@ Node(c, balance t, max, r)
    in
    match iter x tree with
    | None -> (s, tree)
    | Some t -> (s - 1, t)

  let union t1 t2 = TreeDefualts.union B cmp t1 t2
  let intersection t1 t2 = TreeDefualts.intersection B cmp t1 t2
  let difference t1 t2 = TreeDefualts.difference B cmp t1 t2
end