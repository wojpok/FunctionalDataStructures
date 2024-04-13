open Maybe ;;
open Stream ;;

type op = Add | Rem
let randomOps = Stream.map (fun x -> if x mod 2 = 0 then Add else Rem) @@ Rand.randomSizedStream 1000000 0

let time f x =
  let t = Sys.time() in
  let fx = f x in
  Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
  fx

module TestQueue(Q : Queue.QUEUE) = struct
  let test () = 
    let rec iter xs size q = 
      match !xs with
      | Nil -> ()
      | Cons(Add, xs) -> iter xs (size + 1) (Q.snoc () q)
      | Cons(Rem, xs) ->
        if size <= 0 then 
          iter xs size q
        else
          iter xs (size - 1) (Q.tail q)
    in
    time (iter randomOps 0) Q.empty

end

module BenchmarkBanker      = TestQueue(Bankersqueue.BankersQueue.BQueue)
module BenchmarkBanker2     = TestQueue(Bankersqueue.BankersQueue.BQueue2)
module BenchmarkFastBanker  = TestQueue(Fastbankersqueue.FastBankersQueue.FBQueue)
module BenchmarkFastBanker2 = TestQueue(Fastbankersqueue.FastBankersQueue.FBQueue2)