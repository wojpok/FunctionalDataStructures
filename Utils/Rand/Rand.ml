let hash seed =
  let t = 16807 * (seed mod 127773) - 2836 * (seed mod 127773) in
  if t > 0 then t else t + 2147483647

let rec randomStream seed =
  let s' = hash seed in
  Stream.cons s' @@ randomStream s'

let rec randomSizedStream size seed = 
  if size <= 0 then 
    Stream.empty
  else
    let s' = hash seed in
    Stream.cons s' @@ randomSizedStream (size - 1) s'