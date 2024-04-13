open Queue ;;
open Stream ;;

module type CONSTANT = sig
  val c: int
end

module FastBankersQueue(C : CONSTANT) : QUEUE = struct
  let c = C.c

  type 'a queue = int * 'a stream * int * 'a list

  let empty = 0, lazy Nil, 0, []
  let isEmpty (lenf, _, _, _) = lenf = 0

  let check ((lenf, f, lenr, r) as q) =
    if lenr <= c * lenf 
      then q
      else (lenf + lenr, f ++ susp_list_to_stream (lazy (List.rev r)), 0, [])

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
module FBQueue2 = FastBankersQueue(struct let c = 2 end)
