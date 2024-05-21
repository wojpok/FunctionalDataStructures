module SparseBinaryRandomAccessList = struct
  type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree
  type 'a rlist = (int * 'a tree)  list

  let empty = []
  let isEmpty = function
  | [] -> true
  | _ -> false

  let link t1 t2 = Node (t1, t2)

  let rec consTree w t = function
  | [] -> [w, t]
  | (w', t') :: ts     when w = w' -> consTree (w * 2) (link t t') ts
  | (w', _) :: _ as tl when w < w' -> (w, t) :: tl
  | (w', t') :: ts     (* w > w' *)-> (w', t') :: consTree w t ts
  
  let rec unconsTree = function
  | [] -> failwith "EMPTY"
  | [w, t] -> (w, t), []
  | (w, t) :: ts -> (w, t), ts
      
  let cons x ts = consTree 1 (Leaf x) ts
  let head ts = match unconsTree ts with
  | (_, Leaf x), _ -> x
  | _ -> failwith "SUBSCRIPT"

  let tail ts = snd @@ unconsTree ts

  let rec lookupTree w n t = match (n, t) with
  | 0, Leaf x -> x
  | _, Leaf _ -> failwith "SUBSCRIPT"
  | i, Node (t1, t2) ->
    if i < w / 2 
      then lookupTree (w / 2) i t1
      else lookupTree (w / 2) (i - w / 2) t2

  
  let rec lookup i = function
  | [] -> failwith "EMPTY"
  | (w', t') :: ts ->
    if i < w' then
      lookupTree w' i t'
    else
      lookup (i - w') ts

  let rec updateTree w i y = function
  | Leaf _ when i = 0 -> Leaf y
  | Node (t1, t2) ->
    if i  < w / 2 then
      Node (updateTree (w / 2) i y t1, t2)
    else
      Node (t1, updateTree (w / 2) (i - w  / 2) y t2) 
  | _ -> failwith "SUBSCRIPT"

  let rec update' i y = function
  | [] -> failwith "SUBSCRIPT"
  | (w, t) :: ts ->
    if i < w then
      (w, updateTree w i y t) :: ts
    else
      (w, t) :: update' (i - w) y ts
end
