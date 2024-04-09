open Maybe ;;

type 'a streamCell = Nil | Cons of 'a * 'a stream
 and 'a stream = 'a streamCell lazy_t

type 'a queue = int * 'a stream * int * 'a stream

let (!) = Lazy.force

let drop n xs = 
  let rec iter n xs = 
    if n = 0 then !xs
    else match !xs with
      | Nil -> failwith "DROP"
      | Cons(_, xs) -> (iter (n - 1) xs)
  in
  lazy (iter n xs) 

let take n xs = 
  let rec iter n xs =
    if n = 0 then
      xs
    else match !xs with
    | Nil -> failwith "TAKE"
    | Cons(x, xs) -> lazy (Cons(x, iter (n - 1) xs))
  in
  iter n xs

let (++) xs ys =
  let rec iter xs = 
    match !xs with
    | Cons(x, xs) -> lazy (Cons(x, iter xs))
    | Nil -> ys
  in iter xs

let rec to_list xs =
  match !xs with
  | Nil -> []
  | Cons (x, xs) -> x :: to_list xs

let rec from_list = function
| [] -> lazy(Nil)
| x :: xs -> lazy(Cons(x, from_list xs))
  
let reverse xs = 
  to_list xs |> List.rev |> from_list

let uncons : 'a stream -> ('a * 'a stream) option = fun xs -> 
  match !xs with
  | Nil -> None
  | Cons(x, xs) -> return (x, xs)

