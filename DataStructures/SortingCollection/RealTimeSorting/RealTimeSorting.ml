open Ordered ;;
open SortingCollection ;;
open Stream ;;

module RealTimeSorting (S : ORDERED) : SORTING_COLLECTION with type t = S.t = struct
  type t = S.t
  type sortable = int * (t stream * schedule) list
   and schedule = t stream list

  let rec exec1 = function
  | [] -> []
  | job :: sched ->
    match !job with
    | Nil -> exec1 sched
    | Cons(_, s) -> s :: sched

  let rec exec2 (xs, sched) = (xs, sched |> exec1 |> exec1)

  let rec merge xs ys = match (!xs, !ys) with
  | Nil, _ -> ys
  | _, Nil -> xs
  | Cons(x, xs'), Cons(y, ys') ->
    if S.compare x y = -1 then
      Stream.cons x @@ merge xs' ys
    else
      Stream.cons y @@ merge xs ys'

  let empty = failwith "TODO"
  let add x (size, segs) = 
    let rec addSegs xs segs size rsched = 
      if size mod 2 = 0 then
        (xs, List.rev rsched) :: segs
      else match segs with
      | [] -> failwith "addSegs"
      | (xs', _) :: segs' ->
        let xs'' = merge xs xs' in
        addSegs xs'' segs' (size / 2) (xs' :: rsched)
    in
    let segs' = addSegs (Stream.cons x Stream.empty) segs size [] in
    (size + 1, List.map exec2 segs')


  let sort (_, segs) = 
    let rec mergeAll xs = function
    | [] -> xs
    | (xs', _) :: segs ->
      mergeAll (merge xs xs') segs
    in
    Stream.toList (mergeAll Stream.empty segs)
end