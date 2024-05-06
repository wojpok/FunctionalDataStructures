module type QUEUE = sig
  type 'a queue

  val empty : 'a queue
  val isEmpty : 'a queue -> bool
  val snoc : 'a -> 'a queue -> 'a queue
  val head : 'a queue -> 'a
  val tail : 'a queue -> 'a queue
end

module type SIZED_QUEUE = sig
  include QUEUE

  val size : 'a queue -> int
end

module ImplicitSizeQueue(Q : QUEUE) : SIZED_QUEUE = struct
  type 'a queue = int * 'a Q.queue

  let empty = (0, Q.empty)
  let isEmpty (s, _) = s = 0
  let snoc x (s, q) = (s + 1, Q.snoc x q)
  let head (_, q) = (Q.head q)
  let tail (s, q) = (max s 0, Q.tail q)
  let size (s, _) = s
end