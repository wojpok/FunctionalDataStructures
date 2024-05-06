open Stream ;;

module RealTimeQueue : Queue.SIZED_QUEUE = struct
  type 'a queue = 'a stream * 'a list * 'a stream

  let empty = (Stream.empty, [], Stream.empty) 
  let isEmpty (f, _, _) = Stream.isEmpty f

  let rec rotate : 'a queue -> 'a stream = fun (f, a, r) -> 
    match (!f, a, r) with
    | (Nil, y :: _, r) -> Stream.cons y r
    | (Cons (x, xs), y :: ys, r) ->
      Stream.cons x @@ rotate (xs, ys, Stream.cons y r)
    | _ -> failwith "rotate - weird pattern"
    
  let exec (f, r, y) = match !y with
  | Cons (_, s) -> (f, r, s)
  | Nil -> 
    let f' = rotate (f, r, Stream.empty) in
    (f', [], f')

  let snoc x (f, r, s) = exec (f, x :: r, s)

  let head (f, _, _) = match !f with
  | Nil -> failwith "Empty"
  | Cons (x, _) -> x

  let tail (f, r, s) = match !f with
  | Nil -> failwith "Empty"
  | Cons (_, f) -> exec (f, r, s)
  

  let size (_, r, s) = 
    2 * List.length r + Stream.length s

end

module SizedRTQueue = Queue.ImplicitSizeQueue(RealTimeQueue)
