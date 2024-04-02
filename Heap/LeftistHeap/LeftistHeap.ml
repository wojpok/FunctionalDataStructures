open Heap ;;
open Ordered ;;

module LeftistHeap(S : ORDERED): HEAP with type t = S.t = struct
  type t = S.t

  let compare = S.compare

  type heap = Empty | Node of int * t * heap * heap

  let getHeap a = a

  let empty = Empty
  let isEmpty = function | Empty -> true | _ -> false

  let findMin = function 
  | Empty -> failwith "Empty"
  | Node (_, e, _, _) -> e

  let rank = function
  | Empty -> 0
  | Node(r, _, _, _) -> r

  let makeNode x a b = 
    if rank a >= rank b then
      Node ((rank b) + 1, x, a, b)
    else
      Node ((rank a) + 1, x, b, a)

  let rec merge t1 t2 = 
    match t1, t2 with
    | Empty, _ -> t2
    | _, Empty -> t1
    | Node(o1, v1, l1, r1), Node(o2, v2, l2, r2) ->
      if compare v1 v2 = -1 then
        makeNode v2 l2 (merge r2 t1)       
      else
        makeNode v1 l1 (merge r1 t2)
  

  let rec implicit_insert x = function
  | Empty -> makeNode x Empty Empty
  | Node(s, v, l, r) as t -> 
    if compare x v = -1 then
      makeNode v l (implicit_insert x r)
    else
      makeNode x t Empty

  
  let insert_t x = merge (Node(1, x, Empty, Empty))
  let insert = implicit_insert 

  let deleteMin = function
  | Empty -> failwith "Empty"
  | Node(_, _, r, l) -> merge r l
end

module H = LeftistHeap(OrderedInt)

let sort xs = 
  let h = List.fold_left 
    (fun a -> fun b -> H.insert b a)  H.empty xs in
  let rec consume_heap heap acc =
    if H.isEmpty heap then
      acc
    else
      let el = H.findMin heap in
      let h' = H.deleteMin heap in
      consume_heap h' (el :: acc)
    in
    consume_heap h []


let fromList xs =
  let rec consume acc = function
  | x :: y :: xs -> consume ((H.merge x y) :: acc) xs
  | [x] -> x :: acc
  | [] -> acc
  in
  let rec iter = function
  | [] -> H.empty
  | [x] -> x
  | xs -> iter @@ consume [] xs
  in
  iter @@ List.map (fun x -> H.insert x H.empty) xs
