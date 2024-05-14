open Stream ;;


module RealTimeHeap(S : Ordered.ORDERED) : Heap.HEAP with type t = S.t = struct
  type t = S.t

  type heap     = digit stream * schedule
   and digit    = Zero | One of tree
   and schedule = digit stream list
   and tree     = Node of t * tree list

  let empty = (Stream.empty, [])


  let isEmpty (s, _) = Stream.isEmpty s

  let link (Node (x1, c1) as t1) (Node (x2, c2) as t2) =
    if S.compare x1 x2 = -1 then
      Node(x1, t2 :: c1)
    else
      Node(x2, t1 :: c2)
  
  let rec insTree t xs = match !xs with
  | Nil -> Stream.singleton (One t)
  | Cons (Zero, ds) -> Stream.cons (One t) ds
  | Cons (One t', ds) -> Stream.cons Zero (insTree (link t t') ds)

  let rec mrg xs ys = match !xs, !ys with
  | _, Nil -> xs
  | Nil, _ -> ys
  | Cons (Zero, ds1), Cons(d, ds2) -> Stream.cons d @@ mrg ds1 ds2
  | Cons (d, ds1), Cons(Zero, ds2) -> Stream.cons d @@ mrg ds1 ds2
  | Cons (One t1, ds1), Cons (One t2, ds2) ->
    Stream.cons Zero (insTree (link t1 t2) (mrg ds1 ds2))

  let rec normalize ds = match !ds with
  | Nil -> ds
  | Cons (_, ds') -> ignore @@ normalize ds'; ds

  let exec = function
  | [] -> []
  | job :: sched -> match !job with
    | Cons(Zero, j) -> j :: sched
    | _ -> sched

  let merge (ds1, _) (ds2, _) =
    let ds = normalize (mrg ds1 ds2) in ds, []

  let insert x (ds, sched) = 
    let ds' = insTree (Node (x, [])) ds in
    ds', exec (exec @@ ds' :: sched)

  let rec removeMinTree ds = match !ds with
  | Nil -> failwith "EMPTY"
  | Cons (One t, tl) when Stream.isEmpty tl -> (t, tl)
  | Cons (Zero, ds) ->
    let (t', ds') = removeMinTree ds in
    t', Stream.cons Zero ds'
  | Cons (One (Node (x, _) as t), ds) ->
    match removeMinTree ds with
    | (Node(x', _) as t', ds') ->
      if S.compare x x' = -1 then
        t, Stream.cons Zero ds'
      else
        t', Stream.cons (One t) ds'  

  let findMin (ds, _) =
    let Node(x, _), _ = removeMinTree ds in x

  let deleteMin' (ds, _) = 
    let Node(_, c), ds' = removeMinTree ds in
    let ds'' = mrg (Stream.fromList (List.map (fun x -> One x) (List.rev c))) ds' in
    normalize ds'', []

  let rec mrgWithList xs ys = match xs, !ys with
  | _, Nil -> 
    let rec iter = begin function
    | x :: xs -> Stream.cons (One x) @@ iter xs
    | [] -> Stream.empty
    end in
    iter xs
  | [], _ -> ys
  | d :: ds1, Cons(Zero, ds2) -> Stream.cons (One d) @@ mrgWithList ds1 ds2
  | t1 :: ds1, Cons (One t2, ds2) ->
    Stream.cons Zero (insTree (link t1 t2) (mrgWithList ds1 ds2))

  let deleteMin (ds, _) = 
    let Node(_, c), ds' = removeMinTree ds in
    let ds'' = mrgWithList (List.rev c) ds' in
    normalize ds'', []

  let size _ = failwith "uninmplemented"
end




