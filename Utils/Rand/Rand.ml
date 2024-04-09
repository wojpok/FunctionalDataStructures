let hash seed =
  let t = 16807 * (seed mod 127773) - 2836 * (seed mod 127773) in
  if t > 0 then t else t + 2147483647

let rec randomStream seed =
  let s' = hash seed in
  lazy (Stream.Cons(s', randomStream s'))