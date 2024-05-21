module type CONSABLE_QUEUE = sig
  include Queue.SIZED_QUEUE

  val cons : 'a -> 'a queue -> 'a queue
end

module ConsableQueue (Q : Queue.QUEUE) : CONSABLE_QUEUE = struct
  type 'a queue = int * 'a list * 'a Q.queue

  let empty = 0, [], Q.empty

  let isEmpty = function
  | (_, [], q) -> Q.isEmpty q
  | _ -> false

  
  let snoc x (s, st, q) = (s + 1, st, Q.snoc x q)
  
  let cons x (s, st, q) = (s + 1, x :: st, q)

  let head = function
  | (_, x :: _, _) -> x
  | (_, [], q) -> Q.head q

  let tail = function
  | (s, _ :: st, q) -> (s - 1, st, q)
  | (s, [], q) -> (s - 1, [], Q.tail q)

  let size (s, _, _) = s
end