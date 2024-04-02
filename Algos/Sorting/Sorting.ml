let rec qsort ord xs =
  let rec iter acc = function
  | [] -> acc
  | [x] -> x :: acc
  | (x :: xs) ->
    let larger = iter acc @@ List.filter (fun a -> ord (x, a)) xs in
    let smaller = iter (x :: larger) @@ List.filter (fun a -> not (ord (x, a))) xs in
    smaller
  in
  iter [] xs

let t1 = [4; 3; 2; 1; 6; 7; 3; 1; 9; 0 ;9 ;9; 5]

let rec merge_sort ord xs =
  let rec split xs accl accr = 
    match xs with
    | [] -> accl, accr
    | [x] -> (x :: accl), accr
    | (x :: y :: ys) -> split ys (x :: accl) (y :: accr)
  in
  let rec merge xs ys = 
    match xs, ys with
    | [], [] -> []
    | _,  [] -> xs
    | [], _  -> ys
    | (x :: _xs, y :: _ys) ->
      if not @@ ord (x, y) then
        x :: (merge _xs ys)
      else
        y :: (merge xs _ys)
  in

  match xs with
  | [] -> []
  | [x] -> [x]
  | _ ->
    let (l, r) = split xs [] [] in
    let sl = merge_sort ord l in
    let sr = merge_sort ord r in
    merge sl sr

let bottomUpMergeSort ord xs =
  let rec merge : 'a list * 'a list -> 'a list = function
  | ((x :: xs), (y :: ys)) ->
    if ord x y then
      x :: (merge (xs, y :: ys))
    else
      y :: (merge (ys, x :: xs))
  | [], ys -> ys
  | xs, [] -> xs
  in
  let initial = List.map (fun x -> [x]) xs in
  let rec consume acc = function
  | x :: y :: ys -> consume ((merge (x, y)) :: acc) ys
  | [x] -> x :: acc
  | [] -> acc 
  in
  let rec iter = function
  | [] -> []
  | [x] -> x
  | xs -> iter @@ consume [] xs
  in
  iter initial
    
let bottomUpMergeSortRev ord xs = 
let revOrd = fun a -> fun b -> not @@ ord a b in
let rec merge ord acc = function
| ((x :: xs) as xxs, ((y :: ys) as yys)) ->
  if ord x y then 
    merge ord (x :: acc) (xs, yys)
  else
    merge ord (y :: acc) (xxs, ys)
| [], y :: ys -> merge ord (y :: acc) ([], ys)
| x :: xs, [] -> merge ord (x :: acc) (xs, [])
| [], [] -> acc
in
let rec merge_pairs ord acc = function
| [] -> acc
| [xs] -> (List.rev xs) :: acc
| xs :: ys :: xss -> merge_pairs ord ((merge ord [] (xs, ys)) :: acc) xss
in
let rec iter ords = function
| [] -> []
| [x] -> x
| xs -> iter (List.tl ords) @@ merge_pairs (List.hd ords) [] xs
in
let rec is_sorted = function
| x :: ((y :: xs) as ys) -> ord x y && is_sorted ys
| _ -> true
in
let initial = List.map (fun x -> [x]) xs in
let rec ords = ord :: revOrd :: ords in
let res = iter ords initial in
if is_sorted res then
  res
else
  List.rev res


let rec mergeSort xs = 
let rec split n acc = function
| [] -> acc, []
| (x :: xs) as xxs -> 
  if n = 0 then
    acc, xxs
  else 
    split (n - 1) (x :: acc) xs
in
let rec merge xxs yys = 
  match xxs, yys with
  | [], [] -> []
  | [], xs -> xs
  | ys, [] -> ys
  | x :: xs, y :: ys ->
    if x > y then
      x :: (merge xs yys)
    else
      y :: (merge xxs ys)
in
let rec iter len xs =
  match len with
  | 0 | 1 -> xs
  | l -> 
    let s = len / 2 in
    let (xs, ys) = split s [] xs in
    merge (iter s xs) (iter (len - s) ys)
in
iter (List.length xs) xs