open Queue ;;
open Stream;;

module KMQueue : QUEUE = struct
  type 'a queue = E | Q of 'a * 'a stream * 'a list

  let empty = E
  let isEmpty = function E -> true | _ -> false
  let snoc x = function
  | E -> Q(x, lazy Nil, [])
  | Q(h, f, xs) -> Q(h, f, x :: xs)
  let tail = function
  | E -> failwith "Empty"
  | Q(_, f, xs) ->
    match !f with
    | Nil -> 
      begin match List.rev xs with 
      | x :: xs -> Q(x, from_list xs, [])
      | [] -> empty
      end
    | Cons(x, f) ->
      Q(x, f, xs)
  let head = function
  | E -> failwith "Empty"
  | Q(x, _, _) -> x

  let size _ = failwith "TODO"
end