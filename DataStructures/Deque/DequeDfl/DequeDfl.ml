open Deque ;;
open Stream ;;

module type CONST = sig val c : int end

module Deque(C : CONST) : DEQUE = struct
  type 'a queue = (int * 'a stream * int * 'a stream)

  let c = C.c

  let empty = (0, Stream.empty, 0, Stream.empty)

  let isEmpty (f, _, r, _) = (f + r) = 0

  let check : 'a queue -> 'a queue = 
    fun ((lf, f, lr, r) as q) ->
    if lf > c * lr + 1 then
      let i = (lf + lr) / 2 in
      let j = (lf + lr) - i in
      let f' = take i f in
      let r' = r ++ reverse (drop i f) in
      (i, f', j, r')
    else if lr > c * lf + 1 then
      let j = (lf + lr) / 2 in
      let i = (lf + lr) - j in
      let r' = take j r in
      let f' = f ++ reverse (drop j r) in
      (i, f', j, r')
    else q

  let cons x (lf, f, lr, r) = check (lf + 1, Stream.cons x f, lr, r)
  let head (_, f, _, r) =
    match !f with
    | Cons(x, _) -> x
    | Nil -> 
      match !r with
      | Nil -> failwith "EMPTY"
      | Cons(x, _) -> x

  let tail (lf, f, lr, r) = 
    match !f with
    | Nil -> empty
    | Cons(_, f') -> check (lf - 1, f', lr, r)

  let snoc (lf, f, lr, r) x = check (lf, f, lr + 1, Stream.cons x  r)

  let last (_, f, _, r) = 
    match !r with
    | Cons(x, _) -> x
    | Nil -> 
      match !f with
      | Nil -> failwith "Empty"
      | Cons(x, _) -> x

  let init (lf, f, lr, r) =
    match !r with
    | Nil -> empty
    | Cons(_, r') -> check (lf, f, lr - 1, r')

  let size (lenf, _, lenr, _) = lenf + lenr
end