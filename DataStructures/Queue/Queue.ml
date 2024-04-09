module type QUEUE = sig
  type 'a queue

  val empty : 'a queue
  val isEmpty : 'a queue -> bool
  val cons : 'a -> 'a queue -> 'a queue
  val head : 'a queue -> 'a
  val tail : 'a queue -> 'a queue
end