open Maybe ;;

module BinaryRandomAccessList : RandomAccessList.EXTENDED_RANDOM_ACCESS_LIST = struct
  type 'a tree = Leaf of 'a | Node of int * 'a tree * 'a tree
  type 'a digit = Zero | One of 'a tree
  type 'a rlist = 'a digit list

  let empty = []
  let isEmpty = function
  | [] -> true
  | _ -> false

  let tsize = function
  | Leaf _ -> 1
  | Node(s, _ , _) -> s

  let link  t1 t2 = Node (tsize t1 + tsize t2, t1, t2)

  let rec consTree t = function
  | [] -> [One t]
  | Zero :: ts -> One t :: ts
  | One t' :: ts -> Zero :: consTree (link t t') ts
  
  let rec unconsTree = function
  | [] -> failwith "EMPTY"
  | [One t] -> t, []
  | One t :: ts -> t, Zero :: ts
  | Zero :: ts ->
    match unconsTree ts with
    | (Leaf _, _) -> failwith "IMPOSSIBLE"
    | Node (_, t1, t2), ts' ->
      (t1, One t2 :: ts')
      
  let cons x ts = consTree (Leaf x) ts
  let head ts = match unconsTree ts with
  | Leaf x, _ -> x
  | Node (_, _, _),_ -> failwith "IMPOSSIBLE"

  let tail ts = snd @@ unconsTree ts

  let rec lookupTree n t = match (n, t) with
  | 0, Leaf x -> x
  | _, Leaf _ -> failwith "SUBSCRIPT"
  | i, Node (w, t1, t2) ->
    if i < w / 2 
      then lookupTree i t1
      else lookupTree (i - w / 2) t2

  let rec lookup i = function
  | [] -> failwith "EMPTY"
  | Zero :: ts -> lookup i ts
  | One t :: ts ->
    if i < tsize t then
      lookupTree i t
    else
      lookup (i - tsize t) ts

  let rec updateTree i y = function
  | Leaf _ when i = 0 -> Leaf y
  | Node (w, t1, t2) ->
    if i  < w / 2 then
      Node (w, updateTree i y t1, t2)
    else
      Node (w, t1, updateTree (i - w  / 2) y t2) 
  | _ -> failwith "SUBSCRIPT"

  let rec update i y = function
  | [] -> failwith "SUBSCRIPT"
  | Zero :: ts -> Zero :: update i y ts
  | One t :: ts ->
    if i < tsize t then
      One (updateTree  i y t) :: ts
    else
      One t :: update (i - tsize t) y ts

  let nsize = function
  | One t -> tsize t
  | Zero -> 0

  let size ts = List.fold_left (fun acc el -> nsize el + acc) 0 ts

  let create s x = 
    let rec iter cs ct s =
      if cs > s then 
        []
      else if cs land s > 0 then
        One ct :: iter (cs * 2) (link ct ct) (s - cs)
      else
        Zero :: iter (cs * 2) (link ct ct) s
    in
    iter 1 (Leaf x) s


  let rec drop s ts = 
    let rec dropTree k ts = function
    | Leaf _ -> ts
    | Node (w, t1, t2) -> 
      if k <= (w / 2) then
        dropTree k (One t2 :: ts) t1
      else
        dropTree (k - w / 2) (Zero :: ts) t2
    in
    if s = 0 then ts
    else 
      match ts with
      | [] -> []
      | Zero :: ts -> drop s ts
      | [One t] -> 
        if tsize t >= s then 
          []
        else
          dropTree s [] t
      | One t :: ts ->
        if tsize t < s then
          drop (s - tsize t) ts
        else
          dropTree (s - tsize t) (Zero :: ts) t  
end



module UnsizedBinaryRandomAccessList : RandomAccessList.RANDOM_ACCESS_LIST = struct
  type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree
  type 'a digit = Zero | One of 'a tree
  type 'a rlist = 'a digit list

  let empty = []
  let isEmpty = function
  | [] -> true
  | _ -> false

  let link  t1 t2 = Node (t1, t2)

  let rec consTree t = function
  | [] -> [One t]
  | Zero :: ts -> One t :: ts
  | One t' :: ts -> Zero :: consTree (link t t') ts
  
  let rec unconsTree = function
  | [] -> failwith "EMPTY"
  | [One t] -> t, []
  | One t :: ts -> t, Zero :: ts
  | Zero :: ts ->
    match unconsTree ts with
    | (Leaf _, _) -> failwith "IMPOSSIBLE"
    | Node (t1, t2), ts' ->
      (t1, One t2 :: ts')
      
  let cons x ts = consTree (Leaf x) ts
  let head ts = match unconsTree ts with
  | Leaf x, _ -> x
  | _ -> failwith "SUBSCRIPT"

  let tail ts = snd @@ unconsTree ts

  let rec lookupTree w n t = match (n, t) with
  | 0, Leaf x -> x
  | _, Leaf _ -> failwith "SUBSCRIPT"
  | i, Node (t1, t2) ->
    if i < w / 2 
      then lookupTree (w / 2) i t1
      else lookupTree (w / 2) (i - w / 2) t2

  
  let rec lookup' w i = function
  | [] -> failwith "EMPTY"
  | Zero :: ts -> lookup' (w * 2) i ts
  | One t :: ts ->
    if i < w then
      lookupTree w i t
    else
      lookup' (w * 2) (i - w) ts

  let lookup i ds = lookup' 1 i ds

  let rec updateTree w i y = function
  | Leaf _ when i = 0 -> Leaf y
  | Node (t1, t2) ->
    if i  < w / 2 then
      Node (updateTree (w / 2) i y t1, t2)
    else
      Node (t1, updateTree (w / 2) (i - w  / 2) y t2) 
  | _ -> failwith "SUBSCRIPT"

  let rec update' w i y = function
  | [] -> failwith "SUBSCRIPT"
  | Zero :: ts -> Zero :: update' (w * 2) i y ts
  | One t :: ts ->
    if i < w then
      One (updateTree w i y t) :: ts
    else
      One t :: update' (w * 2) (i - w) y ts

  let update i y ts = update' 1 i y ts
end



