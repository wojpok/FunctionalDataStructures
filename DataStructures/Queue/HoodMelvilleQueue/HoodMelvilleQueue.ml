
module HoodMelvilleQueue : Queue.SIZED_QUEUE = struct

  type 'a rotating_state = 
    | Idle
    | Reversing of int * 'a list * 'a list * 'a list * 'a list
    | Appending of int * 'a list * 'a list
    | Done of 'a list

  type 'a queue = int * 'a list * 'a rotating_state * int * 'a list

  let exec = function
  | Reversing (ok, x :: f, f', y :: r, r') ->
    Reversing (ok + 1, f, x :: f', r, y :: r')
  | Reversing (ok, [], f', [y], r') ->
    Appending (ok, f', y :: r')
  | Appending (0, _, r) -> Done r
  | Appending (ok, x :: f, r) -> 
    Appending (ok - 1, f, x :: r)
  | st -> st

  let invalidate = function
  | Reversing (ok, f, f', r, r') ->
    Reversing (ok - 1, f, f', r, r')
  | Appending (0, _, _ :: r') -> Done r'
  | Appending (ok, f, r) -> 
    Appending (ok - 1, f, r)
  | st -> st

  let exec2 (lf, f, state, lr, r) = 
    match exec (exec state) with
    | Done nf -> (lf, nf, Idle, lr, r)
    | ns -> (lf, f, ns, lr, r)
  
  let check ((lf, f, _, lr, r) as q) =
    if lr <= lf then 
      exec2 q
    else
      let ns = Reversing (0, f, [], r, []) in
      exec2 (lf + lr, f, ns, 0, [])

  let empty = (0, [], Idle, 0, [])
  let isEmpty (lf, _, _, _, _) = lf = 0

  let snoc x (lf, f, st, lr, r) = check (lf, f, st, lr, x :: r)

  let head (_, f, _, _, _) = match f with
  | x :: _ -> x
  | _ -> failwith "EMPTY"

  let tail = function
  | (_, [], _, _, _) -> failwith "EMPTY"
  | (lf, _ :: f, st, lr, r) ->
    check (lf - 1, f, invalidate st, lr, r)

  let size (lf, _, _, lr, _) = lf + lr
end