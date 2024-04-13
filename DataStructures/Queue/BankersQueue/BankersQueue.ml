open Queue ;;
open Stream ;;

module type CONSTANT = sig
  val c: int
end

module BankersQueue(C : CONSTANT) : QUEUE = struct
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
