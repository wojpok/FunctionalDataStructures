let test = Fmt.int

let (++) = Fmt.(++)

let test2 = Fmt.list (Fmt.box (Fmt.int ++ (Fmt.sps 10)))

let runFmt fmt v = Format.printf "%a" fmt v

let test () = runFmt test2 [1; 2; 4; 5; ]


type tree = Node of tree * tree | Leaf


let rec fmtTree : tree Fmt.t = fun fmt t ->
  match t with
  | Leaf -> 
    Format.pp_print_string fmt "Leaf"
  | Node(l, r) ->
    Format.pp_print_string fmt "Node";
    Fmt.vbox(Fmt.pair (Fmt.box ~indent:20 (fmtTree)) (Fmt.box ~indent:20 (fmtTree))) fmt (l, r)


let rec fmtTree2 : tree Fmt.t = fun fmt t ->
  match t with
  | Leaf -> 
    Format.pp_print_string fmt "Leaf"
  | Node(l, r) ->
    Format.pp_print_string fmt "Node";
    Fmt.vbox(Fmt.vbox (Fmt.pair (Fmt.box ~indent:20 (fmtTree)) (Fmt.box ~indent:20 (fmtTree)))) fmt (l, r)
    


  let test3 () = runFmt fmtTree2 (Node(Node(Leaf, Leaf), Node(Leaf, Leaf)))



type 'a ctx
    = Empty
    | Val of 'a
    | Print of string * 'a ctx
    | Cons of 'a * 'a ctx

type ('a) fmt = 'a ctx -> 'a ctx

let append : 'a fmt -> 'a fmt -> 'a fmt = fun f1 f2 -> 
  failwith "TODO"





