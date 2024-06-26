open Maybe ;;

type 'a streamCell = Nil | Cons of 'a * 'a stream
 and 'a stream = 'a streamCell lazy_t

let empty = lazy Nil

let singleton x = lazy (Cons (x, empty))

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

let rec toList xs =
  match !xs with
  | Nil -> []
  | Cons (x, xs) -> x :: toList xs

let rec fromList = function
| [] -> empty
| x :: xs -> lazy(Cons(x, fromList xs))
  
let reverse xs = 
  toList xs |> List.rev |> fromList

let uncons : 'a stream -> ('a * 'a stream) option = fun xs -> 
  match !xs with
  | Nil -> None
  | Cons(x, xs) -> return (x, xs)

let cons : 'a -> 'a stream -> 'a stream = fun x xs ->
  lazy (Cons(x, xs))

let shd : 'a stream -> 'a = fun xs ->
  match !xs with
  | Cons(x, _) -> x
  | _ -> failwith "EMPTY"

let stl : 'a stream -> 'a stream = fun xs ->
  match !xs with
  | Cons(_, xs) -> xs
  | _ -> failwith "EMPTY"

let suspListToStream : 'a list lazy_t -> 'a stream = fun xs ->
  lazy(match !xs with
  | [] -> Nil
  | x :: xs -> Cons(x, fromList xs))

let rec map: ('a -> 'b) -> 'a stream -> 'b stream = fun f xs -> 
  match !xs with
  | Nil -> empty
  | Cons(x, xs) -> lazy (Cons(f x, map f xs))

let isEmpty : 'a stream -> bool = fun xs ->
  match !xs with
  | Nil -> true
  | Cons(_, _) -> false 

let rec foldLeft : ('acc -> 'a -> 'acc) -> 'acc -> 'a stream -> 'acc = fun f acc xs ->
  match !xs with
  | Nil -> acc
  | Cons(x, xs) -> foldLeft f  (f acc x) xs

let length : 'a stream -> int = fun xs ->
  foldLeft (fun acc _ -> acc + 1) 0 xs