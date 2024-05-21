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





