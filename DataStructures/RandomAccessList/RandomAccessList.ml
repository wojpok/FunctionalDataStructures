
exception Subscript

module type RANDOM_ACCESS_LIST = sig
  type 'a rlist

  val empty : 'a rlist
  val isEmpty : 'a rlist -> bool

  val cons : 'a -> 'a rlist -> 'a rlist
  val head : 'a rlist -> 'a
  val tail  : 'a rlist -> 'a rlist

  val lookup : int -> 'a rlist -> 'a
  val update : int -> 'a -> 'a rlist -> 'a rlist

end

module type EXTENDED_RANDOM_ACCESS_LIST = sig
  include RANDOM_ACCESS_LIST

  val size : 'a rlist -> int
  val drop : int -> 'a rlist -> 'a rlist
  val create : int -> 'a -> 'a rlist
end



