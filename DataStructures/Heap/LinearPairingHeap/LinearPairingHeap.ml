module LinearPairingHeap(S : Ordered.ORDERED): Heap.HEAP with type t = S.t = struct
  type t = S.t
  let compare = S.compare

  type heap = E | T of heap * t * heap

  let empty = E
  let isEmpty = function
  | E -> true
  | _ -> false

  let merge t1 t2 = match (t1, t2) with
  | E, h -> h
  | h, E -> h
  | (T(l1, v1, _) as h1), (T(l2, v2, _) as h2) ->
    if compare v1 v2 = -1 then
      T(T(l2, v2, l1), v1, E)
    else
      T(T(l1, v1, l2), v2, E)
  
  let findMin = function
  | E -> failwith "Empty"
  | T(_, x, _) -> x

  let rec mergePairs = function
  | E -> E
  | T(_, _, E) as t -> t
  | T(_, _, (T(_, _, xs) as t2)) as t1 ->
    merge (merge t1 t2) (mergePairs xs)

  let deleteMin = function
  | E -> E
  | T (l, _, _) -> mergePairs l

  let insert x h = merge (T(E, x, E)) h
end