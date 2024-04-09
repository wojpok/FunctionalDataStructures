open Heap ;;
open Tree ;;

module PairingHeap(S : Ordered.ORDERED) : sig 
  include HEAP 
  val toBinary : heap -> (unit, t) mtree
end with type t = S.t = struct
  type t = S.t
  type heap = E | T of t * heap list

  let compare = S.compare
  let empty = E

  let isEmpty = function 
  | E -> true
  | _ -> false

  let findMin = function
  | E -> failwith "Empty"
  | T(x, _) -> x

  let merge h1 h2 = match (h1, h2) with
  | (E, h) -> h
  | (h, E) -> h
  | T(x, xs) as tx, (T(y, ys) as ty) ->
    if compare x y = -1 then
      T(x, ty :: xs)
    else
      T(y, tx :: ys)

  let rec mergePairs = function
  | h1 :: h2 :: hs -> merge (merge h1 h2) (mergePairs hs)
  | [h] -> h
  | [] -> E

  let deleteMin = function
  | E -> E
  | T(_, xs) -> mergePairs xs

  let insert x h =  merge (T(x, [])) h

  let rec toBinary = function
  | E -> Leaf
  | T (x, xs) ->
    Node((), fromList xs, x, Leaf)
  and fromList = function
    | [] -> Leaf
    | E :: xs -> fromList xs
    | T(y, ys) :: xs ->
      Node((), fromList ys, y, fromList xs)
end


module PairingHeapBalanced(S : Ordered.ORDERED) : HEAP with type t = S.t = struct
  type t = S.t
  type heap = E | T of t * heap list

  let compare = S.compare
  let empty = E

  let isEmpty = function 
  | E -> true
  | _ -> false

  let findMin = function
  | E -> failwith "Empty"
  | T(x, _) -> x

  let merge h1 h2 = match (h1, h2) with
  | (E, h) -> h
  | (h, E) -> h
  | T(x, xs) as tx, (T(y, ys) as ty) ->
    if compare x y = -1 then
      T(x, ty :: xs)
    else
      T(y, tx :: ys)

  let rec mergePairs hs =
  let rec aux = function
  | h1 :: h2 :: hs -> merge h1 h2 :: aux hs
  | hs -> hs
  in match hs with
  | [h] -> h
  | [] -> E
  | hs -> mergePairs @@ aux hs

  let deleteMin = function
  | E -> E
  | T(_, xs) -> mergePairs xs

  let insert x h =  merge (T(x, [])) h
end

module PairingHeapFoldr(S : Ordered.ORDERED) : HEAP with type t = S.t = struct
  type t = S.t
  type heap = E | T of t * heap list

  let compare = S.compare
  let empty = E

  let isEmpty = function 
  | E -> true
  | _ -> false

  let findMin = function
  | E -> failwith "Empty"
  | T(x, _) -> x

  let merge h1 h2 = match (h1, h2) with
  | (E, h) -> h
  | (h, E) -> h
  | T(x, xs) as tx, (T(y, ys) as ty) ->
    if compare x y = -1 then
      T(x, ty :: xs)
    else
      T(y, tx :: ys)

  let rec mergePairs hs = List.fold_right merge hs E

  let deleteMin = function
  | E -> E
  | T(_, xs) -> mergePairs xs

  let insert x h =  merge (T(x, [])) h
end


let test () = 
  let module H = PairingHeap(Int) in
  let xs = [6; 7; 3; 1; 8; 9; 0; 12; 5; 2; 6; 2; 1; ] in
  let rec insertAll h = function
  | x :: xs -> insertAll (H.insert x h) xs
  | [] -> h
  in print_endline @@ TreeDefualts.pretty @@ H.toBinary @@ H.deleteMin @@ insertAll H.empty xs
