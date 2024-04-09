open Set ;;
open Maybe ;;
open Ordered ;;
open Tree ;;

module AVL(S : ORDERED): SET with type t = S.t = struct
  type t = S.t
  let cmp = S.compare
  type tree = (int, t) mtree
  type set = (int, t) mset

  let reveal (_, t) = 
    let rec mapTree = function
    | Leaf -> Leaf
    | Node (_, l, v, r) -> Node((), mapTree l, v, mapTree r)
  in mapTree t

  let empty = 0, Leaf
  let singleton x = 1, Node(1, Leaf, x, Leaf)
  let toList = TreeDefualts.toList
  let fromOrdList = TreeDefualts.fromOrdList 0
  let fromList xs = 
    let rec restoreH = function
    | Leaf -> 0, Leaf
    | Node(_, l, v, r) ->
      let lh, lt = restoreH l in
      let rh, rt = restoreH r in
      let nh = 1 + max lh rh in 
      nh, Node (nh, lt, v, rt) 
      in
    let (c, t) = TreeDefualts.fromList cmp 0 xs in
    let (_, t) = restoreH t in
    (c, t)
  let size (s, _) = s
  let member = TreeDefualts.member cmp
  let find = TreeDefualts.find cmp

  let height = function
  | Leaf -> 0
  | Node(h, _, _, _) -> h

  let balance_factor = function
  | Leaf -> 0
  | Node(_, l, _, r) -> height l - height r

  let rotate_left = function
  | Node(_, l, x, Node(_, rl, y, rr)) ->
    let nhl = 1 + max (height l) (height rl) in
    Node(1 + max (nhl) (height rr), Node(nhl, l, x, rl), y, rr)
  | _ -> failwith "rotate_left"

  let rotate_right = function
  | Node(_, Node(_, ll, x, lr), y, r) ->
    let nhr = 1 + max (height r) (height lr) in
    Node(1 + max (nhr) (height ll), ll, x, Node(nhr, lr, y, r))
  | _ -> failwith "rotate_right"

  let balance = function
  | Node (b, l, v, r) as t ->
    let bl = balance_factor t in
    if bl = -2 then (*unbalanced to right*)
      let rbl = balance_factor r in
      let nr = if rbl = 1 then (* unbalanced to left *) rotate_right r  else r in
      rotate_left @@ Node( b, l, v, nr)
    else if bl = 2 then
      let lbl = balance_factor l in
      let nl = if lbl = -1 then rotate_left l else l in
      rotate_right @@ Node(b, nl, v, r)
    else
      t
  | t -> t

  let makeNode l v r = 
    let h = 1 + max (height l) (height r) in
    Node(h, l, v, r)

  let add x (s, tree) = 
    let rec iter = function
    | Leaf -> Some (Node(1, Leaf, x, Leaf))
    | Node(c, l, v, r) ->
      if cmp x v = 1 then       let* n = iter r in return @@ balance @@ makeNode l v n
      else if cmp x v = -1 then let* n = iter l in return @@ balance @@ makeNode n v r
      else None
    in
    match iter tree with
    | None -> (s, tree)
    | Some t -> (s + 1, balance t)

  let rec findMax = function
    | Leaf -> failwith "findMax"
    | Node (_, _, v, Leaf) -> v
    | Node (_, _, _, r) -> findMax r

  let rec delete x (s, tree) = 
    let rec iter x = function
    | Leaf -> None
    | Node (c, l, v, r) when cmp x v =  1 -> rfmap (iter x r) @@ fun t -> makeNode l v (balance t)
    | Node (c, l, v, r) when cmp x v = -1 -> rfmap (iter x l) @@ fun t -> makeNode (balance t) v r
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
    | Some t -> (s - 1, balance t)

  let union t1 t2 = TreeDefualts.union 1 cmp t1 t2
  let intersection t1 t2 = TreeDefualts.intersection 1 cmp t1 t2
  let difference t1 t2 = TreeDefualts.difference 1 cmp t1 t2
end