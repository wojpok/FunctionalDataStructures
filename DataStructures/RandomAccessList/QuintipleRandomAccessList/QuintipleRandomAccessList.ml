

module QuintapleRandomAccessListSimplified : RandomAccessList.RANDOM_ACCESS_LIST = struct
  type 'a tree = Leaf of 'a | Node of int * 'a tree * 'a tree
  type 'a bigit = 
    | One of 'a tree 
    | Three of ('a tree * 'a tree * 'a tree)
  type 'a digit =
    | Zero
    | Two of ('a tree * 'a tree)
    | Four of ('a tree * 'a tree * 'a tree * 'a tree)
    | Block of 'a bigit list
  type 'a rlist = 'a digit list

  let empty = []
  let isEmpty = List.is_empty

  let head = function
    | Block (One (Leaf x) :: _) :: _
    | Two (Leaf x, _) :: _
    | Block (Three (Leaf x, _, _) :: _) :: _ -> x
    | _ -> failwith "EMPTY"

  let rec lookupTree i = function
    | Leaf x when i = 0 -> x
    | Leaf _ -> failwith "Subscript"
    | Node (s, t1, t2) ->
      if i < s / 2 then
        lookupTree s t1
      else
        lookupTree (i - (s / 2)) t2

  let rec updateTree i y = function
    | Leaf _ when i = 0 -> Leaf y
    | Leaf _ -> failwith "Subscript"
    | Node (s, t1, t2) ->
      if i < s / 2 then
        Node(s, updateTree s y t1, t2)
      else
        Node(s, t1, updateTree (i - (s / 2)) y t2)

  let addBlock b = function
    | Block [] :: ds -> Block [b] :: ds
    | Block bs :: ds -> Block (b :: bs) :: ds
    | ds -> Block [b] :: ds

  let remBlock bs ds = match bs with
    | [] -> ds
    | bs -> Block bs :: ds

  let tsize = function
    | Node (s, _, _) -> s
    | Leaf _ -> 1
  
  let link t1 t2 = 
    Node (tsize t1 + tsize t2, t1, t2)

  let consTree t1 = function
    | [] -> [Block [One t1]]
    | Block (One t2 :: bs) :: ds             -> Two (t1, t2) :: remBlock bs ds
    | Two (t2, t3) :: ds                     -> addBlock (Three (t1, t2, t3)) ds
    | Block (Three (t2, t3, t4) :: bs) :: ds -> Four (t1, t2, t3, t4) :: remBlock bs ds
    | _ -> failwith "IMPOSSIBLE"

  let unconsTree = function
    | [] -> failwith "EMPTY"
    | [Block [One t1]]                       -> t1, []
    | Block (One t1 :: bs) :: ds             -> t1, Zero :: remBlock bs ds
    | Two (t1, t2) :: ds                     -> t1, addBlock (One t2) ds
    | Block (Three (t1, t2, t3) :: bs) :: ds -> t1, Two (t2, t3) :: remBlock bs ds 
    | _ -> failwith "IMPOSSIBLE"

  let fixup = function
    | Four (t1, t2, t3, t4) :: ds             -> Two (t1, t2) :: consTree (link t3 t4) ds
    | Block bs :: Four (t1, t2, t3, t4) :: ds -> Block bs :: Two (t1, t2) :: consTree (link t3 t4) ds
    | Zero :: ds -> begin match unconsTree ds with
      |  Node(_, t1, t2), ds'                 -> Two (t1, t2) :: ds'
      | _ -> failwith "EMPTY"
      end
    | Block bs :: Zero :: ds -> 
      begin match unconsTree ds with
      | Node (_, t1, t2), ds'                 -> Block bs :: Two (t1, t2) :: ds'
      | _ -> failwith "EMPTY"
      end 
    | ds -> ds

  let cons x xs = fixup (consTree (Leaf x) xs)
  let tail xs = fixup (snd @@ unconsTree xs)

  type ('a, 'b) either = Left of 'a | Right of 'b

  let unpackDigit = function
    | Zero -> []
    | Two (t1, t2) -> [t1; t2]
    | Four (t1, t2, t3, t4) -> [t1; t2; t3; t4]
    | Block _ -> []

  let packDigit = function
    | [] -> Zero
    | [t1; t2] -> Two (t1, t2)
    | [t1; t2; t3; t4] -> Four (t1, t2, t3, t4)
    | _ -> failwith "IMPOSSIBLE"

  let unpackBigit = function
    | One t1 -> [t1]
    | Three (t1, t2, t3) -> [t1; t2; t3]
  
  let packBigit = function
    | [t1] -> One t1
    | [t1; t2; t3] -> Three (t1, t2, t3)
    | _ -> failwith "IMPOSSIBLE"

  let rec lookupList i = function
    | [] -> Right i
    | t :: ts ->
      if i < tsize t then
        Left (lookupTree i t)
      else
        lookupList (i - tsize t) ts
  
  let rec updateList i y = function
    | [] -> Right i
    | t :: ts -> 
      if i < tsize t then
        Left (updateTree i y t :: ts)
      else match updateList i y ts with
        | Left ts -> Left (t :: ts)
        | x -> x

  let rec lookup i = function
    | [] -> failwith "SUBSCRIPT"
    | Block bs :: ds -> begin match lookupBigit i bs with
      | Left x -> x
      | Right i -> lookup i ds
      end
    | d :: ds -> match lookupList i @@ unpackDigit d with
      | Left x -> x
      | Right i -> lookup i ds
  and lookupBigit i = function
    | [] -> Right i
    | d :: ds -> match lookupList i @@ unpackBigit d with
      | Left x -> Left x
      | Right i -> lookupBigit i ds
  

  let rec update i y = function
    | [] -> failwith "SUBSCRIPT"
    | Block bs :: ds -> begin match updateBigit i y bs with
      | Left bs -> Block bs :: ds
      | Right i -> Block bs :: update i y ds
      end
    | d :: ds -> match updateList i y @@ unpackDigit d with
      | Left d -> packDigit d :: ds
      | Right i -> d :: update i y ds
  and updateBigit i y = function
    | [] -> Right i
    | d :: ds -> match updateList i y @@ unpackBigit d with
      | Left d -> Left (packBigit d :: ds)
      | Right i -> match updateBigit i y ds with
        | Left ds -> Left (d :: ds)
        | x -> x
end



module QuintipleRandomAccessList : RandomAccessList.RANDOM_ACCESS_LIST = struct
  type 'a tree 
    = Leaf of 'a 
    | Node of int * 'a tree * 'a tree
  type 'a block 
    = Ones of 'a tree list 
    | Threes of ('a tree * 'a tree * 'a tree) list
  type 'a digits
    = Zero
    | Two of 'a tree * 'a tree
    | Four of 'a tree * 'a tree * 'a tree * 'a tree
    | Block of 'a block list
  type 'a rlist = 'a digits list

  let empty = []

  let isEmpty = function
  | [] -> true
  | _ -> false

  let tsize = function
  | Node (s, _, _) -> s
  | Leaf _ -> 1

  let link t1 t2 = 
    Node (tsize t1 + tsize t2, t1, t2)
  
  let head = function
  | Four (Leaf x, _, _, _) :: _
  | Two (Leaf x, _) :: _
  | Block (Threes ((Leaf x, _, _) :: _) :: _) :: _
  | Block (Ones (Leaf x :: _) :: _) :: _ 
    -> x
  | _ -> failwith "EMPTY"

  let ones os bs ds = match (os, bs, ds) with
  | [], [], ds -> ds
  | [], bs, ds -> Block bs :: ds
  | os, [], Block bs :: ds -> Block (Ones os :: bs) :: ds
  | os, bs, ds -> Block (Ones os :: bs) :: ds

  let consOne t = function
  | Block (Ones os :: bs) :: ds 
  -> Block (Ones (t :: os) :: bs) :: ds
  | Block bs :: ds -> Block (Ones [t] :: bs) :: ds
  | ds -> Block [Ones [t]] :: ds

  let threes os bs ds = match (os, bs, ds) with
  | [], [], ds -> ds
  | [], bs, ds -> Block bs :: ds
  | os, [], Block bs :: ds -> Block (Threes os :: bs) :: ds
  | os, bs, ds -> Block (Threes os :: bs) :: ds

  let consThree t = function
  | Block (Threes ts :: bs) :: ds 
  -> Block (Threes (t :: ts) :: bs) :: ds
  | Block bs :: ds -> Block (Threes [t] :: bs) :: ds
  | ds -> Block [Threes [t]] :: ds

  let consTree t1 = function
  | [] -> [Block [Ones [t1]]]
  | Block (Ones (t2 :: ts) :: bs) :: ds -> Two (t1, t2) :: ones ts bs ds
  | Two (t2, t3) :: ds -> consThree (t1, t2, t3) ds
  | Block (Threes ((t2, t3, t4) :: ts) :: bs) :: ds ->
    Four (t1, t2, t3, t4) :: threes ts bs ds
  | _ -> failwith "IMPOSSIBLE"

  let unconsTree = function
  | [] -> failwith "EMPTY"
  | [Block [Ones [t]]] -> [], t
  | Block (Ones (i :: os) :: bs) :: ds ->
    Zero :: ones os bs ds, i
  | Two (t1, t2) :: ds -> consOne t2 ds, t1
  | Block (Threes ((t1, t2, t3) :: ts) :: bs) :: ds ->
    Two (t2, t3) :: threes ts bs ds, t1
  | _ -> failwith "IMPOSSIBLE"

  let fixup = function
  | Four (t1, t2, t3, t4) :: ds -> Two (t1, t2) :: consTree (link t3 t4) ds
  | Block bs :: Four (t1, t2, t3, t4) :: ds ->
    Block bs :: Two (t1, t2) :: consTree (link t3 t4) ds
  | Zero :: ds ->
    begin match unconsTree ds with
    | ds', Node(_, t1, t2) -> Two (t1, t2) :: ds'
    | _ -> failwith "EMPTY"
    end
  | Block bs :: Zero :: ds -> 
    begin match unconsTree ds with
    | ds', Node (_, t1, t2) -> Block bs :: Two (t1, t2) :: ds'
    | _ -> failwith "EMPTY"
    end 
  | ds -> ds

  let cons x ds = fixup (consTree (Leaf x) ds)
  let tail ds = 
    let (ds, _) = unconsTree ds in
    fixup ds

  let rec lookupTree i = function
  | Leaf x when i = 0 -> x
  | Leaf _ -> failwith "Subscript"
  | Node (s, t1, t2) ->
    if i < s / 2 then
      lookupTree s t1
    else
      lookupTree (i - (s / 2)) t2

  type ('a, 'b) either = Left of 'a | Right of 'b
  
  let rec lookupThrees i ts bs = match ts with
  | (t1, t2, t3) :: ts ->
    let t = tsize t1 in
    if i < t then
      Left (lookupTree i t1)
    else if i < 2 * t then
      Left (lookupTree (i - t) t2)
    else if i < 3 * t then
      Left (lookupTree (i - 2 * t) t3)
    else lookupThrees (i - 3 * t) ts bs
  | [] -> cont i bs
  and lookupOnes i ts bs = match ts with
  | t1 :: ts ->
    let t = tsize t1 in
    if i < t then
      Left (lookupTree i t1)
    else
      lookupOnes (i - t) ts bs
  | [] -> cont i bs
  and cont i = function
    | Threes ts :: bs -> lookupThrees i ts bs
    | Ones os :: bs -> lookupOnes i os bs
    | [] -> Right i

  let rec lookupDigits i = function
  | [] -> failwith "SUBSCRIPT"
  | Zero :: ds -> lookupDigits i ds
  | Two (t1, t2) :: ds ->
    let t = tsize t1 in
    if i < t then
      lookupTree i t1
    else if i < 2 * t then
      lookupTree (i - t) t2
    else
      lookupDigits (i - 2 * t) ds
  | Four (t1, t2, t3, t4) :: ds ->
    let t = tsize t1 in
    if i < t then
      lookupTree i t1
    else if i < 2 * t then
      lookupTree (i - t) t2
    else if i < 3 * t then
      lookupTree (i - 2 * t) t3
    else if i < 4 * t then
      lookupTree (i - 3 * t) t4
    else
      lookupDigits (i - 4 * t) ds
  | Block bs :: ds -> match cont i bs with
    | Left x -> x
    | Right i -> lookupDigits i ds
  
  let lookup = lookupDigits 
  
  let rec updateTree i y = function
  | Leaf _ when i = 0 -> Leaf y
  | Leaf _ -> failwith "Subscript"
  | Node (s, t1, t2) ->
    if i < s / 2 then
      Node(s, updateTree s y t1, t2)
    else
      Node(s, t1, updateTree (i - (s / 2)) y t2)

  type ('a, 'b, 'c) eith3 = O of 'a | T of 'b | R of 'c

  let rec updateThrees i y ts bs = match ts with
  | ((t1, t2, t3) as t') :: ts ->
    let t = tsize t1 in
    if i < t then
      T (((updateTree i y t1, t2, t3) :: ts), bs)
    else if i < 2 * t then
      T (((t1, updateTree (i - t) y t2, t3) :: ts), bs)
    else if i < 3 * t then
      T (((t1, t2, updateTree (i - 2 * t) y t3) :: ts), bs)
    else begin match updateThrees (i - 3 * t) y ts bs with
      | T (ts, bs) -> T (t' :: ts, bs)
      | O (ts, bs) -> T ([t'], Ones ts :: bs)
      | x -> x
    end
  | [] -> contu i y bs
  and updateOnes i y ts bs = match ts with
  | t1 :: ts ->
    let t = tsize t1 in
    if i < t then
      O (updateTree i y t1 :: ts, bs)
    else
      begin match updateOnes (i - t) y ts bs with
      | O (ts, bs) -> O (t1 :: ts, bs)
      | T (ts, bs) -> O ([t1], Threes ts :: bs)
      | x -> x
      end
  | [] -> contu i y bs
  and contu i y = function
    | Threes ts :: bs -> begin match updateThrees i y ts bs with
      | T(ts, bs) -> T([], Threes ts :: bs)
      | x -> x
      end
    | Ones os :: bs -> begin match updateOnes i y os bs with
      | O(ts, bs) -> O([], Ones ts :: bs)
      | x -> x
      end
    | [] -> R i

  let rec updateDigits i y = function
  | [] -> failwith "SUBSCRIPT"
  | Zero :: ds -> Zero :: updateDigits i y ds
  | (Two (t1, t2) as t') :: ds ->
    let t = tsize t1 in
    if i < t then
      Two (updateTree i y t1, t2) :: ds
    else if i < 2 * t then
      Two (t1, updateTree (i - t) y t2) :: ds
    else
      t' :: updateDigits (i - 2 * t) y ds
  | (Four (t1, t2, t3, t4) as t') :: ds ->
    let t = tsize t1 in
    if i < t then
      Four (updateTree i y t1, t2, t3, t4) :: ds
    else if i < 2 * t then
      Four (t1, updateTree (i - t) y t2, t3, t4) :: ds
    else if i < 3 * t then
      Four (t1, t2, updateTree (i - 2 * t) y t3, t4) :: ds
    else if i < 4 * t then
      Four (t1, t2, t3, updateTree (i - 3 * t) y t4) :: ds
    else
      t' :: updateDigits (i - 4 * t) y ds
  | Block bs :: ds -> match contu i y bs with
    | T(ts, bs) -> Block (Threes ts :: bs) :: ds
    | O(os, bs) -> Block (Ones os :: bs) :: ds
    | R i -> Block bs :: updateDigits i y ds

  let update = updateDigits
end


