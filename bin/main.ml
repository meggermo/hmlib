module B = Block.Block1D
module S = Hmlib.SuperBlock


let exact x =
  Base.Float.( 0.5 * ( x * log (x * x) - (x - 1.0) * log ((x - 1.0) ** 2.0) -2.0))

let () = 
  let p = 8 in
  let n = Base.Int.shift_left 1 p in
  let b = B.create 1.0 n in
  let s = S.build ~rank:1 ~min_block_size:n b |> S.compute in
  let x = Lacaml.D.Vec.make n 1.0 in
  let y = Lacaml.D.Vec.make n 0.0 in
  let u = S.matvec x ~y s in
  Format.printf "%a\n" (Lacaml.Io.pp_fvec) u
   