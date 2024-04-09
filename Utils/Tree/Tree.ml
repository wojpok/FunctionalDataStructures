open Maybe ;;

type ('meta, 'a) mtree = 
  | Leaf 
  | Node of 'meta * ('meta, 'a) mtree * 'a * ('meta, 'a) mtree

type ('meta, 'a) mset = (int * ('meta,'a) mtree)

module TreeDefualts = struct
  let empty = Leaf
  let size (s, _) = s

  let rotateLeft = function
  | Node(m1, a, x, Node(m2, b, y, c)) -> Node(m2, Node(m1, a, x, b), y, c)
  | t -> t

  let rotateRight = function
  | Node(m1, Node(m2, a, x, b), y, c) -> Node(m2, a, x, Node(m1, b, y, c))
  | t -> t

  let fromOrdList dflMeta xs = 
    let rec recur low high input =
      if low = high then Leaf, input
      else
        let mid = low + (high - low) / 2 in
        let left, input = recur low mid input in
        match input with
        | [] -> Leaf, []
        | x::xs ->
          let right, input = recur (mid + 1) high xs in
          Node (dflMeta, left, x, right), input in
    let l = (List.length xs) in
    l, fst (recur 0 l xs)

  let fromList cmp dlfMeta xs = fromOrdList dlfMeta @@ List.sort cmp xs

  let toList (_, tree) =
    let rec iter acc = function
    | Leaf -> acc
    | Node(_, l, v, r) ->
      let r' = v :: iter acc r in
      iter r' l
    in
    iter [] tree

  let member cmp e (_, tree) =
    let rec iter = function
    | Leaf -> false
    | Node(_, l, v, r) when cmp v e = 0 -> true
    | Node(_, l, v, r) when cmp v e = 1 -> iter l
    | Node(_, l, v, r)                  -> iter r
    in
    iter tree

  let find cmp e (_, tree) = 
    let rec iter = function
    | Leaf -> None
    | Node(_, l, v, r) when cmp v e = 0 -> Some v
    | Node(_, l, v, r) when cmp v e = 1 -> iter l
    | Node(_, l, v, r)                  -> iter r
    in
    iter tree

  let rec findMax = function
    | Leaf -> failwith "findMax"
    | Node (_, _, v, Leaf) -> v
    | Node (_, _, _, r) -> findMax r

  let listMerge cmp xs ys decide =
    let rec iter xs ys =
      match xs, ys with
      | [], [] -> []
      | ys, [] -> if decide false then ys else []
      | [], xs -> iter xs []
      | (x :: xs, y :: ys) ->
        let c = cmp x y in
        if c = 0 then
          if decide true then
            x :: (iter xs ys)
          else
            iter xs ys
        else
          if c = 1 then
            if decide false then
              y :: (iter (x :: xs) ys)
            else
              (iter (x :: xs) ys)
          else
            if decide false then
              x :: (iter xs (y :: ys))
            else
              (iter xs (y :: ys))
    in iter xs ys

  let union dlfMeta cmp t1 t2 = 
    fromOrdList dlfMeta @@ listMerge cmp (toList t1) (toList t2) (fun _ -> true)

  let intersection dlfMeta cmp t1 t2 = 
    fromOrdList dlfMeta @@ listMerge cmp (toList t1) (toList t2) (fun t -> t)

  let difference dlfMeta cmp t1 t2 = 
    fromOrdList dlfMeta @@ listMerge cmp (toList t1) (toList t2) (fun t -> not t)

    let pretty h = 
      let rec replicate_str str count = 
        if count = 1 || count = 0 then
          str
        else if (count mod 2) = 1 then
          str ^ (replicate_str (str ^ str) ((count - 1) / 2))
        else
          replicate_str (str ^ str) ((count) / 2)
      in
      let rec align_strings max = function
        | [] -> []
        | str :: xs ->
          let l = String.length str in
          if l < max then
            (str ^ (replicate_str " " (max - l))) :: (align_strings max xs)
          else
            str :: (align_strings max xs)
      in
      let rec align_lengths l = function
      | [] -> if l > 0 then
        "" :: (align_lengths (l - 1) [])
        else []
      | x :: xs -> x :: (align_lengths (l - 1) xs)
      in
      let rec join spaces (ls, lh, lw) (rs, rh, rw)  =
        let nl = max rh lh in
        let rss = align_lengths nl rs in
        let lss = align_lengths nl ls in
        let align_left = align_strings (lw + spaces) lss in
        let front_line = replicate_str "-" (lw + rw + spaces) in
        front_line :: List.map2 (^) align_left rss
      in
      let rec infer = function
      | Leaf -> (["E"], 1, 1)
      | Node(_, l, v, r) ->
        let (rs, rh, rw) as right = infer r in
        let (ls, lh, lw) as left  = infer l in
        let name = string_of_int v in
        let namel = String.length name in
        let subl = rw + lw + 1 in
        let ttl, spaces = 
          if namel > subl then
            namel, namel - subl + 1
          else
            subl + 4, 1 + 4
        in
        let joined = join spaces left right in
        let space_to_left = (ttl - namel) / 2 in
        let header = (replicate_str " " space_to_left) ^ name in
        (header :: joined, 2 + (max rh lh), ttl)
    
      in
      let rec connect = function
      | [] -> "\n"
      | x :: xs -> x ^ "\n" ^ (connect xs)
      in
      let (res, _, _) = infer h in
      connect res
end