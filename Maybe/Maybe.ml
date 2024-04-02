(* Maybe *)

let fmap f = function
  | Some x  -> Some (f x)
  | None    -> None
  let rfmap m f = fmap f m

let bind m f = 
  match m with
  | Some x  -> f x
  | None    -> None

let return x = Some x

let (let* ) = bind
