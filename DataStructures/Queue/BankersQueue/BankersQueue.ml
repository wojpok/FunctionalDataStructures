open Queue ;;
open Stream ;;

module type CONSTANT = sig
  val c: int
end

module BankersQueue(C : CONSTANT) : SIZED_QUEUE = struct
  let c = C.c

  type 'a queue = int * 'a stream * int * 'a stream

  let empty = 0, lazy Nil, 0, lazy Nil
  let isEmpty (lenf, _, _, _) = lenf = 0

  let check ((lenf, f, lenr, r) as q) =
    if lenr <= c * lenf 
      then q
      else (lenf + lenr, f ++ reverse r, 0, lazy Nil)

  let snoc x (lenf, f, lenr, r) = 
    check @@ (lenf, f, lenr + 1, lazy (Cons(x, r)))

  let head = function
  | (0, _, _, _) -> failwith "head :: EMPTY"
  | (_, xs, _, _) -> shd xs

  let tail = function
  | (0, _, _, _) -> failwith "tail :: EMPTY"
  | (lenf, f, lenr, r) -> check (lenf - 1, stl f, lenr, r)

  let size (lenf, _, lenr, _) = lenf + lenr
end

module BQueue  = BankersQueue(struct let c = 1 end)
module BQueue2 = BankersQueue(struct let c = 2 end)

(* 
  Excerise: Proof that balance factor = 2 also yield constant-time amortized data structure.

  Observation: We just allow |r| to be twice as long, so obviously reverse will be twice as expensive.
  Other than that, this will behave just as normal queue.

  We maintain debit invariant that: 
  - D(i) = sum(d(j) for j = 0..i)
  - D(i) <= min (3i, 2*|f| - |r|)

  and we just need to show that debit invariant is maintained when:
  - snoc discharges one debit
  - tail discharges three debits

  For a snoc and tail that does not cause rotation, this is trivial.

  Adding element to rear stream, |r| rises by 1 and 2*|f| - |r| lowers by 1.
  We just need to discharge one debit.

  Removing element from front stream |f| lowers by 1 and 2*|f| - |r| lowers by 2.
  This lowers 3i by 3, so we just need pay that off.

  Consider snoc and tail that do create a rotation.

  Just before scheduling a rotation we arrive to a situation where |f| = m and |r| = 2 * m + 1 .
  Reverse will cost us 2m + 1 and append will cost us m.

  Since append is incremental and reverse is monolithic, debits are distributed as such:
  - 1      if i < m
  - 2m + 1 if i = m
  - 0      otherwise

  Note that D(m) = 3m + 1, which equals to a total cost of a rotation.

  The invariant is broken at 0th node, so we just need to discharche one debit to restore it.

  Q.E.D.


  Same can be prooven for any c >= 1. One might need to modify invariant to include additional cost of rotation.
*)
