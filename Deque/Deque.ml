module type DEQUE = sig
  include Queue.QUEUE

  val snoc : 'a queue -> 'a -> 'a queue
  val last : 'a queue -> 'a
  val init : 'a queue -> 'a queue
end