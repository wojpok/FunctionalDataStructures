open Heap ;;
open Ordered ;;


module SplayHeap(S : ORDERED) : HEAP with type t = S.t = struct
type t = S.t
  type heap = E | T of heap * t * heap
let compare = S.compare

let empty = E
let isEmpty = function
| E -> true
| _ -> false

let rec partition = function
| (pivot, E) -> (E, E)
| (pivot, (T (a, x, b) as t)) ->
  if x <= pivot then
    match b with
      | E -> (t, E)
      | T(b1, y, b2) ->
        if y < pivot then
          let (small, big) = partition (pivot, b2) in
          T(T(a, x, b1), y, small), big
        else
          let (small, big) = partition (pivot, b1) in
          T(a, x, small), T(big, y, b2)
  else
    match a with
      | E -> (E, t)
      | T(a1, y, a2) ->
        if y <= pivot then
          let (small, big) = partition (pivot, a2) in
          T(a1, y, small), T(big, x, b)
        else
          let (small, big) = partition (pivot, a1) in
          small, T(big, y, T(a2, x, b))


let insert x t = 
  let (a, b) = partition (x, t) in
  T(a, x, b)

let rec merge' = function
  | (E, t) -> t
  | (T(a, x, b), t) -> 
    let (ta, tb) = partition (x, t) in
    T (merge' (ta, a), x, merge' (tb, b))

let merge t1 t2 = merge' (t1, t2)

let rec findMin = function
| E -> failwith "EMPTY"
| T(E, x, _) -> x
| T(t, _, _) -> findMin t

let rec deleteMin = function
| E -> failwith "EMPTY"
| T(E, _, b) -> b
| T( T(E, x, b), y, c) -> T(b,y,c)
| T( T(a, x, b), y, c) -> T(deleteMin a, x, T(b, y, c))
end

(*
let treeSort xs =
  let rec insertAll t = function
  | [] -> t
  | x :: xs -> insertAll (insert x t) xs
  in
  let rec removeAll t = 
    if isEmpty t then
      []
    else
      let m = findMin t in
      let t' = deleteMin t in
      m :: removeAll t'
  in
  removeAll @@ insertAll empty xs

let sorted = treeSort [1; 6; 3; 8; 9; 0; 3; 2; 7; 1; 5; 8; 9]
*)