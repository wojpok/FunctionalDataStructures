open Stream ;;

module RedundandZerolessRandomAccessList : RandomAccessList.RANDOM_ACCESS_LIST = struct
  type 'a tree = Leaf of 'a | Node of int * 'a tree * 'a tree
  type 'a digit = 
    | One   of 'a tree
    | Two   of 'a tree * 'a tree
    | Three of 'a tree * 'a tree * 'a tree
  type 'a rlist = 'a digit Stream.stream

  let empty = Stream.empty

  let isEmpty = Stream.isEmpty

  let head xs = match !xs with
    | Cons (One (Leaf x), _) -> x
    | Cons (Two (Leaf x, _), _) -> x
    | Cons (Three (Leaf x, _, _), _) -> x
    | _ -> failwith "Empty"


  let tsize = function
  | Leaf _ -> 1
  | Node(s, _, _) -> s

  let link t1 t2 = 
    Node (tsize t1 + tsize t2, t1, t2)


  let rec consTree t1 xs = match !xs with
    | Nil -> Stream.singleton (One t1)
    | Cons (One t2, tl) -> 
      Stream.cons (Two (t1, t2)) tl
    | Cons (Two (t2, t3), tl) -> 
      Stream.cons (Three (t1, t2, t3)) tl
    | Cons (Three(t2, t3, t4), tl) ->
      Stream.cons (Two (t1, t2)) 
      @@ consTree (link t3 t4) tl

  let cons x xs = consTree (Leaf x) xs

  let rec unconsTree xs = match !xs with
    | Nil -> failwith "Empty"
    | Cons (Three(t1, t2, t3), tl) ->
      Stream.cons (Two(t2, t3)) tl, t1
    | Cons (Two(t1, t2), tl) ->
      Stream.cons (One t2) tl, t1
    | Cons ((One t1), tl) ->
      match unconsTree tl with
      | _, Leaf _ -> failwith "Impossible"
      | tl', Node (_, t2, t3) ->
        Stream.cons (Two (t2, t3)) tl', t1

  let tail xs = let (xs, _) = unconsTree xs in xs

  let rec lookupTree i = function
  | Leaf x when i = 0 -> x
  | Leaf _ -> failwith "Subscript"
  | Node (s, t1, t2) ->
    if i < s / 2 then
      lookupTree s t1
    else
      lookupTree (i - (s / 2)) t2 
  

  let rec lookup i xs = match !xs with
  | Nil -> failwith "Subscript"
  | Cons(One t, tl) ->
    if tsize t > i then
      lookupTree i t
    else
      lookup (i - tsize t) tl
  | Cons(Two (t1, t2), tl) ->
    let s = tsize t1 in
    if i < s then
      lookupTree i t1
    else if i - s < s then
      lookupTree (i - s) t2
    else
      lookup (i - 2 * s) tl
  | Cons (Three (t1, t2, t3), tl) ->
    let s = tsize t1 in
    if i < s then
      lookupTree i t1
    else if i - s < s then
      lookupTree (i - s) t2
    else if i - (2 * s) < s then
      lookupTree (i - 2*s) t3
    else
      lookup (i - 3 * s) tl

  let rec updateTree i y = function
  | Leaf _ when i = 0 -> Leaf y
  | Leaf _ -> failwith "Subscript"
  | Node (s, t1, t2) ->
    if i < s / 2 then
      Node(s, updateTree s y t1, t2)
    else
      Node(s, t1, updateTree (i - (s / 2)) y t2) 
  

  let rec update i y xs = match !xs with
  | Nil -> failwith "Subscript"
  | Cons(One t, tl) ->
    if tsize t > i then
      Stream.cons (One (updateTree i y t)) tl
    else
      Stream.cons (One t) (update (i - tsize t) y tl)
  | Cons(Two (t1, t2), tl) ->
    let s = tsize t1 in
    if i < s then
      Stream.cons (Two (updateTree i y t1, t2)) tl
    else if i - s < s then
      Stream.cons (Two (t1, updateTree (i - s) y t2)) tl
    else
      Stream.cons (Two (t1, t2)) (update (i - 2 * s) y tl)
  | Cons (Three (t1, t2, t3), tl) ->
    let s = tsize t1 in
    if i < s then
      Stream.cons (Three (updateTree i y t1, t2, t3)) tl
    else if i - s < s then
      Stream.cons (Three (t1, updateTree (i - s) y t2, t3)) tl
    else if i - (2 * s) < s then
      Stream.cons (Three (t1, t2, updateTree (i - 2*s) y t3)) tl
    else
      Stream.cons (Three (t1, t2, t3)) (update (i - 3 * s) y tl)
end






(*: RandomAccessList.RANDOM_ACCESS_LIST*)
module ScheduledRedundandZerolessRandomAccessList : RandomAccessList.RANDOM_ACCESS_LIST  = struct
  type 'a tree = Leaf of 'a | Node of int * 'a tree * 'a tree
  type 'a digit = 
    | One   of 'a tree
    | Two   of 'a tree * 'a tree
    | Three of 'a tree * 'a tree * 'a tree
  type 'a schedule = 'a digit stream list
  type 'a rlist = 'a digit Stream.stream * 'a schedule

  let empty = Stream.empty, []

  let isEmpty (xs, _) = Stream.isEmpty xs

  let head (xs, _) = match !xs with
    | Cons (One (Leaf x), _) -> x
    | Cons (Two (Leaf x, _), _) -> x
    | Cons (Three (Leaf x, _, _), _) -> x
    | _ -> failwith "Empty"


  let tsize = function
  | Leaf _ -> 1
  | Node(s, _, _) -> s

  let link t1 t2 = 
    Node (tsize t1 + tsize t2, t1, t2)

  let exec = function
  | [] -> []
  | x :: xs -> match !x with
    | Cons(Two (_, _), ts) -> ts :: xs
    | _ -> xs

  let exec2 sched = exec (exec sched)

  let rec consTree t1 xs = match !xs with
    | Nil -> Stream.singleton (One t1)
    | Cons (One t2, tl) -> 
      Stream.cons (Two (t1, t2)) tl
    | Cons (Two (t2, t3), tl) -> 
      Stream.cons (Three (t1, t2, t3)) tl
    | Cons (Three(t2, t3, t4), tl) ->
      Stream.cons (Two (t1, t2)) 
      @@ consTree (link t3 t4) tl

  let cons x (xs, sched) =
    let tl = consTree (Leaf x) xs in
    (tl, exec2 (tl :: sched))

  let rec unconsTree xs = match !xs with
    | Nil -> failwith "Empty"
    | Cons (Three(t1, t2, t3), tl) ->
      Stream.cons (Two(t2, t3)) tl, t1
    | Cons (Two(t1, t2), tl) ->
      Stream.cons (One t2) tl, t1
    | Cons ((One t1), tl) ->
      match unconsTree tl with
      | _, Leaf _ -> failwith "Impossible"
      | tl', Node (_, t2, t3) ->
        Stream.cons (Two (t2, t3)) tl', t1

  let tail (xs, sched) = 
    let (xs, _) = unconsTree xs in 
    (xs, exec2 (xs :: sched))

  let rec lookupTree i = function
  | Leaf x when i = 0 -> x
  | Leaf _ -> failwith "Subscript"
  | Node (s, t1, t2) ->
    if i < s / 2 then
      lookupTree s t1
    else
      lookupTree (i - (s / 2)) t2 
  
  let lookup i (xs, _) =
    let rec iter i xs = match !xs with
      | Nil -> failwith "Subscript"
      | Cons(One t, tl) ->
        if tsize t > i then
          lookupTree i t
        else
          iter (i - tsize t) tl
      | Cons(Two (t1, t2), tl) ->
        let s = tsize t1 in
        if i < s then
          lookupTree i t1
        else if i - s < s then
          lookupTree (i - s) t2
        else
          iter (i - 2 * s) tl
      | Cons (Three (t1, t2, t3), tl) ->
        let s = tsize t1 in
        if i < s then
          lookupTree i t1
        else if i - s < s then
          lookupTree (i - s) t2
        else if i - (2 * s) < s then
          lookupTree (i - 2*s) t3
        else
          iter (i - 3 * s) tl
    in iter i xs

  let rec updateTree i y = function
  | Leaf _ when i = 0 -> Leaf y
  | Leaf _ -> failwith "Subscript"
  | Node (s, t1, t2) ->
    if i < s / 2 then
      Node(s, updateTree s y t1, t2)
    else
      Node(s, t1, updateTree (i - (s / 2)) y t2) 
  

  let update i y (xs, sched) = 
    let rec iter i xs sched = 
      let execed = exec2 sched in
      match !xs with
      | Nil -> failwith "Subscript"
      | Cons(One t, tl) ->
        if tsize t > i then
          Stream.cons (One (updateTree i y t)) tl, execed
        else
          let (tl, sched) = (iter (i - tsize t) tl execed) in
          Stream.cons (One t) tl, sched
      | Cons(Two (t1, t2), tl) ->
        let s = tsize t1 in
        if i < s then
          Stream.cons (Two (updateTree i y t1, t2)) tl, execed
        else if i - s < s then
          Stream.cons (Two (t1, updateTree (i - s) y t2)) tl, execed
        else
          let (tl, sched) = (iter (i - 2 * s) tl execed) in
          Stream.cons (Two (t1, t2)) tl, sched
      | Cons (Three (t1, t2, t3), tl) ->
        let s = tsize t1 in
        if i < s then
          Stream.cons (Three (updateTree i y t1, t2, t3)) tl, execed
        else if i - s < s then
          Stream.cons (Three (t1, updateTree (i - s) y t2, t3)) tl, execed
        else if i - (2 * s) < s then
          Stream.cons (Three (t1, t2, updateTree (i - 2*s) y t3)) tl, execed
        else
          let (tl, sched) = (iter (i - 3 * s) tl execed) in
          Stream.cons (Three (t1, t2, t3)) tl, sched
  in iter i xs sched
end
