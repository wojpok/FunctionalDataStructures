let (!) = Stream.(!)

module BankersSorting(S : Ordered.ORDERED) : SortingCollection.SORTING_COLLECTION with type t = S.t =
struct
  type t = S.t
  let compare = S.compare

  type sortable = int * t Stream.stream list

  let empty = (0, [])

  let rec merge (txs, tys) = 
    match (!txs, !tys) with
    | (Stream.Nil, _) -> txs
    | (_, Stream.Nil) -> tys
    | (Cons(x, xs), Cons(y, ys)) ->
      if compare x y = -1 then
        Stream.cons x @@ merge (xs, tys)
      else
        Stream.cons y @@ merge (txs, ys) 

  let add x (size, segs) = 
    let rec addSeg seg segs size =
      if size mod 2 = 0 then seg :: segs
      else addSeg (merge (seg, List.hd segs)) (List.tl segs) (size / 2)
    in
    (size + 1, (addSeg (Stream.singleton x) segs size))


  let sort (_, segs) = 
    let rec mergeAll = function
    | (xs, []) -> Stream.toList xs
    | (xs, seg :: segs) -> mergeAll (merge (xs, seg), segs)
    in
    mergeAll (Stream.empty, segs)
end

(** (a) Hypothesize D(n) is at most 2n, when add and sort repay log(n) + 1 and
  * n debts respectively.
  * When n = 2^k and given i (i < 2^k), total shared cost by add is
  *     2 * [i / 2] + 4 * [i / 4] + ... + 2^(k - 1) * [i / 2^(k - 1)]
  *     <= 2 * i / 2 + 4 * i /4 + ... + 2^(k - 1) * i / 2^(k - 1) = (k - 1)i.
  * If one add repays log(n) + 1 = k + 1 debts, then
  *     D(n + i) = D(n) + (k - 1)i - i * (k + 1) = D(n) - 2i.
  * So, i = 2^k, D(2^(k + 1)) excluding the debt which this add increments is 0.
  * This add adds 2 * 2^(k + 1) - 2 debts, then D(2^(k + 1) = 2n) <= 2 * 2n.
  *
  * The unshared cost of sort at worst case (when n = 2^k - 1) is
  *     1 + (1 + 2) + ... + (1 + 2 + ... + 2^(k - 1)) + n + 1 = 3n - k + 2
  * (mrg created by mrgAll is not unshared).
  * If sort repays 2n debts, because D(n) is 0 sort can force suspension created
  * by add.
  *
  * The amortized cost of add is k + 1 + k + 1 = 2log(n) + 2.
  * The amortized cost of sort is k + 1 + 2n + 2n - k + 1 + n + 1 = 3 + 5n.
  *
  * (b) Unshared cost of take is at most
  *     log(n) + sum_(i=1)^k{log(n) - i + 1) + k
  *     = (k + 1)log(n) - k(k + 1)/2 + 2k
  *     = (k + 1)log(n) - k(k - 3)/2.
  * The cost of taking one element from list is at most O(log(n)).
  * Therefore, if take repays klog(n) debts, take can force suspension.
  * Then the amortized cost of take is (2k + 1)log(n) - k(k - 3)/2.
  *
  * *)

