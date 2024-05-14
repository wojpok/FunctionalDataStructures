module type IntHeap = Heap.HEAP with type t = int 

module TestHeap(H : IntHeap) = struct

  let run () = 
    let rec insertAll h = function
    | x :: xs -> insertAll (H.insert x h) xs
    | [] -> h
    in
    let rec removeAll acc h = 
      if H.isEmpty h then List.rev acc
      else 
        let min = H.findMin h in
        let h' = H.deleteMin h in
        removeAll (min :: acc) h'
    in
    let rec checkSorted = function
    | x :: (y :: _ as ys) ->
      if x <= y then checkSorted ys
      else false
    | _ -> true
    in
    let ins = insertAll H.empty BenchmarkList.xs in
    let acc = removeAll [] ins in
    let res = checkSorted acc in
    if res then
      print_endline "Sorted"
    else
      print_endline "Fail"
end


module Test1 = TestHeap(PairingHeap.PairingHeap(Int))
module Test2 = TestHeap(PairingHeap.PairingHeapFoldr(Int))
module Test3 = TestHeap(PairingHeap.PairingHeapBalanced(Int))
module Test4 = TestHeap(LinearPairingHeap.LinearPairingHeap(Int))