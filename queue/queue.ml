module type QUEUE = sig
  type 'a queue

  val empty : 'a queue
  val isEmpty :  'a queue -> bool
end