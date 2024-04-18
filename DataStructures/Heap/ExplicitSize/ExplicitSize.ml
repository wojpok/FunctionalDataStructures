module type EXPLICIT_SIZE_HEAP = sig
  include Heap.HEAP

  val size : heap -> int
end


module ExplicitMinHeap(H : Heap.HEAP) : EXPLICIT_SIZE_HEAP with type t = H.t = 
struct
  type t = H.t
  type heap = int * H.heap
  
  let empty = (0, H.empty)
  let size (c, _) = c
  let isEmpty (c, _) = c = 0
  let insert x (c, h) = (c + 1, H.insert x h)
  let findMin (_, h) = H.findMin h
  let deleteMin (c, h) = (c - 1, H.deleteMin h)
  let merge (c1, h1) (c2, h2) = (c1 + c2, H.merge h1 h2)
end