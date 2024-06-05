module type NUMBER = sig
  type nat

  val zero : nat

  val add : nat -> nat -> nat
  val inc : nat -> nat
  val dec : nat -> nat

  val toInt : nat -> int
end


module Zeroless : NUMBER = struct
  type digit = One | Two
  type nat = digit list

  let zero = []

  let rec inc = function
  | [] -> [One]
  | One :: ds -> Two :: ds
  | Two :: ds -> One :: inc ds
  let rec dec = function
  | [] -> []
  | [One] -> []
  | One :: ds -> Two :: dec ds
  | Two :: ds -> One :: ds 

  let rec add x y = match x, y with
  | ds, [] -> ds
  | [], ds -> ds
  | One :: ds1, One :: ds2 -> Two :: add ds1 ds2
  | Two :: ds1, Two :: ds2 -> Two :: add ds1 ds2 |> inc
  | _ :: ds1, _ :: ds2 -> One :: add ds1 ds2 |> inc 

  let rec toInt = function
  | [] -> 0
  | One :: ds -> 1 + 2 * toInt ds
  | Two :: ds -> 2 + 2 * toInt ds 
end

module QuintapleBlockNumbers : NUMBER = struct
  type block = 
    | Ones of int
    | Threes of int
  type digit =
    | Zero
    | Two
    | Four
    | Block of block list
  type nat = digit list

  let zero = []

  let ones i bs ds = match (i, bs, ds) with
  | 0, [], ds -> ds
  | 0 , bs, ds -> Block bs :: ds
  | n, [], Block bs :: ds -> Block (Ones n :: bs) :: ds
  | n, bs, ds -> Block (Ones n :: bs) :: ds

  let threes i bs ds = match (i, bs, ds) with
  | 0, [], ds -> ds
  | 0, bs, ds -> Block bs :: ds
  | n, [], Block bs :: ds -> Block (Threes n :: bs) :: ds
  | n, bs, ds -> Block (Threes n :: bs) :: ds

  let simpleinc = function
  | [] -> [Block [Ones 1]]
  | Block (Ones i :: bs) :: ds -> Two :: ones (i - 1) bs ds
  | Two :: bs -> threes 1 [] bs
  | Block (Threes i :: bs) :: ds -> Four :: threes (i - 1) bs ds
  | _ -> failwith "IMPOSSIBLE"

  let simpledec = function
  | [Block [Ones 1]] -> []
  | Block (Ones i :: bs) :: ds -> Zero :: ones (i - 1) bs ds
  | Two :: ds -> ones 1 [] ds
  | Block (Threes i :: bs) :: ds -> Two :: threes (i - 1) bs ds
  | _ -> failwith "IMPOSSIBLE"

  let fixup = function
  | Four :: ds -> Two :: simpleinc ds
  | Block bs :: Four :: ds -> Block bs :: Two :: simpleinc ds
  | Zero :: ds -> Two :: simpledec ds
  | Block bs :: Zero :: ds -> Block bs :: Two :: simpledec ds
  | ds -> ds

  let inc x = fixup (simpleinc x)
  let dec x = fixup (simpledec x)

  let add _ _ = failwith "TODO"

  let toInt ds = 
    let rec evalBlock w acc = function
    | [] -> (w, acc)
    | Threes 0 :: bs
    | Ones 0 :: bs -> evalBlock w acc bs
    | Ones i :: bs -> evalBlock (w * 2) (acc + w) (Ones (i - 1) :: bs)
    | Threes i :: bs -> evalBlock (w * 2) (acc + 3 * w) (Threes (i - 1) :: bs)
    in
    let rec iter w acc = function
    | [] -> acc
    | Two :: ds -> iter (w * 2) (acc + 2 * w) ds
    | Zero :: ds -> iter (w * 2) acc ds
    | Four :: ds -> iter (w * 2) (acc + 4 * w) ds
    | Block bs :: ds ->
      let (w, acc) = evalBlock w acc bs in
      iter (w * 2) acc ds
    in
    iter 1 0 ds
end



