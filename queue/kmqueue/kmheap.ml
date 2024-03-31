module KMHeap : Queue.QUEUE = struct
  type 'a queue = E | Q of 'a * 'a list * 'a list

  let empty = E
  let isEmpty = function
  | E -> true
  | _ -> false
end