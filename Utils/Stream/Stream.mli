type 'a streamCell = Nil | Cons of 'a * 'a stream
 and 'a stream

val (!) : 'a stream -> 'a streamCell
val (++) : 'a stream -> 'a stream -> 'a stream

val empty : 'a stream
val cons : 'a -> 'a stream -> 'a stream
val singleton : 'a -> 'a stream 

val toList : 'a stream -> 'a list
val fromList : 'a list -> 'a stream
val suspListToStream : 'a list lazy_t -> 'a stream

val shd : 'a stream -> 'a
val stl : 'a stream -> 'a stream

val isEmpty : 'a stream -> bool
val length : 'a stream -> int

val reverse : 'a stream -> 'a stream
val map : ('a -> 'b) -> 'a stream -> 'b stream

val take : int -> 'a stream -> 'a stream
val drop : int -> 'a stream -> 'a stream
val foldLeft : ('acc -> 'a -> 'acc) -> 'acc -> 'a stream -> 'acc