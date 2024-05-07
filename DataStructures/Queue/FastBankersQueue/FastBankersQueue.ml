open Queue ;;
open Stream ;;

module type CONSTANT = sig
  val c: int
end

module FastBankersQueue(C : CONSTANT) : SIZED_QUEUE = struct
  let c = C.c

  type 'a queue = int * 'a stream * int * 'a list

  let empty = 0, Stream.empty, 0, []
  let isEmpty (lenf, _, _, _) = lenf = 0

  let check ((lenf, f, lenr, r) as q) =
    if lenr <= c * lenf 
      then q
      else (lenf + lenr, f ++ suspListToStream (lazy (List.rev r)), 0, [])

  let snoc x (lenf, f, lenr, r) = 
    check @@ (lenf, f, lenr + 1, x :: r)

  let head = function
  | (0, _, _, _) -> failwith "EMPTY"
  | (_, xs, _, _) -> shd xs

  let tail = function
  | (0, _, _, _) -> failwith "EMPTY"
  | (lenf, f, lenr, r) -> check (lenf - 1, stl f, lenr, r)

  let size (lenf, _, lenr, _) = lenf + lenr
end

module FBQueue  = FastBankersQueue(struct let c = 1 end)

(* 
  Reasoning from Okasaki' Book also applies here.

  We maintain debit invariant that: 
  - D(i) = sum(d(j) for j = 0..i)
  - D(i) <= min (2r, |f| - |r|)

  and we just need to show that debit invariant is maintained when:
  - snoc discharges one debit
  - tail discharges two debits

  For a snoc and tail that does not cause rotation, this is trivial.

  Consider snoc and tail that do create a rotation.

  Just before cheduling a rotation we arrive to a situation where |f| = m and |r| = m + 1.
  To pay off reverse operation we need m + 1 debits and to payoff append we need m debits.

  Since append is incremental and reverse is monolithic, debits are distributed as such:
  - 1     if i < m
  - m + 1 if i = m
  - 0     otherwise

  Note that D(m) = 2m + 1, which equals to a total cost of a rotation.

  The invariant is broken at 0th node, so we just need to discharche one debit to restore it.

  Q.E.D.
*)


module FBQueue2 = FastBankersQueue(struct let c = 2 end)
