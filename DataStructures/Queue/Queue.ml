module type QUEUE = sig
  type 'a queue

  val empty : 'a queue
  val isEmpty : 'a queue -> bool
  val snoc : 'a -> 'a queue -> 'a queue
  val head : 'a queue -> 'a
  val tail : 'a queue -> 'a queue

  val size : 'a queue -> int
end