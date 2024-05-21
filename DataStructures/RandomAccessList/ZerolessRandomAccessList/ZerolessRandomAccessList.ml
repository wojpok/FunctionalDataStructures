module ZerolessRandomAccessList : RandomAccessList.RANDOM_ACCESS_LIST = struct
  type 'a tree 
    = Leaf of 'a 
    | Node of int * 'a tree * 'a tree
  type 'a digit 
    = One of 'a tree 
    | Two of 'a tree * 'a tree
  type 'a rlist = 'a digit list

  let empty = []

  let isEmpty = function
  | [] -> true
  | _ -> false

  let tsize = function
  | Leaf _ -> 1
  | Node(s, _, _) -> s

  let link t1 t2 = Node (tsize t1 + tsize t2, t1, t2)

  let rec consTree t = function
  | [] -> [One t]
  | One t1 :: ts -> Two (t, t1) :: ts
  | Two(t1, t2) :: ts -> One t :: consTree (link t1 t2) ts

  let rec unconsTree = function
  | [] -> failwith "EMPTY"
  | [One t] -> t, []
  | Two(t1, t2) :: ts -> t1, One t2 :: ts
  | One t :: ts -> 
    match unconsTree ts with
    | Leaf _, _ -> failwith "IMPOSSIBLE"
    | Node(_, t1, t2), ts ->
      t, Two(t1, t2) :: ts
  
  let cons x ts = consTree (Leaf x) ts

  let head = function
  | One (Leaf x) :: _ -> x
  | Two (Leaf x, _) :: _ -> x
  | _ -> failwith "EMPTY" 

  let tail ts = snd @@ unconsTree ts

  let rec lookupTree n t = match (n, t) with
  | 0, Leaf x -> x
  | _, Leaf _ -> raise RandomAccessList.Subscript
  | i, Node (w, t1, t2) -> 
    if i < w / 2 
      then lookupTree i t1
      else lookupTree (i - w / 2) t2
      
  let rec lookup i = function
  | [] -> raise RandomAccessList.Subscript
  | One t :: ts ->
    if i < tsize t
      then lookupTree i t
      else lookup (i - tsize t) ts
  | Two(t1, t2) :: ts ->
    if i < tsize t1 then
      lookupTree i t1
    else if i < tsize t1 + tsize t2 then
      lookupTree (i - tsize t1) t2
    else
      lookup (i - tsize t1 - tsize t2) ts
      
  let rec updateTree i y = function
  | Leaf _ when i = 0 -> Leaf y
  | Node (w, t1, t2) ->
  if i < w / 2 then
    Node (w, updateTree i y t1, t2)
  else
    Node (w, t1, updateTree (i - w / 2) y t2)
  | _ -> raise RandomAccessList.Subscript   

  let rec update i y = function
  | [] -> raise RandomAccessList.Subscript
  | One t :: ts -> 
    if i < tsize t
      then One (updateTree i y t) :: ts
      else One t :: update (i - tsize t) y ts
  | Two(t1, t2) :: ts ->
    if i < tsize t1 then
      Two (updateTree i y t1, t2) :: ts
    else if i - tsize t1 < tsize t2 then
      Two (t1, updateTree (i - tsize t1) y t2) :: ts
    else 
      Two (t1, t2) :: update (i - tsize t1 - tsize t2) y ts
end