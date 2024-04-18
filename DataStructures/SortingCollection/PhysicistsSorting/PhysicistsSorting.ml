module BottomUpMergeSort(S : Ordered.ORDERED) : Sortingcollection.SortingCollection.SORTING_COLLECTION with type t = S.t =
struct
  type t = S.t
  let compare = S.compare

  type sortable = int * t list list lazy_t

  let empty = (0, lazy [])

  let rec merge = function
  | ([], xs) -> xs
  | (ys, []) -> ys
  | ((x :: xs as xxs), (y :: ys as yys)) ->
    if compare x y = -1 then
      x :: merge (xs, yys)
    else
      y :: merge (xxs, ys) 

  let add x (size, segs) = 
    let rec addSeg seg segs size =
      if size mod 2 = 0 then seg :: segs
      else addSeg (merge (seg, List.hd segs)) (List.tl segs) (size / 2)
    in
    (size + 1, lazy (addSeg [x] (Lazy.force segs) size))


  let sort (_, segs) = 
    let rec mergeAll = function
    | (xs, []) -> xs
    | (xs, seg :: segs) -> mergeAll (merge (xs, seg), segs)
    in
    mergeAll ([], Lazy.force segs)
end

