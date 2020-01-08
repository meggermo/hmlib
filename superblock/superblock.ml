module SuperBlock = struct
  module B = Block.Block1D
  module F = Fullblock.FullBlock1D
  module R = Rankblock.RankBlock1D

  type t = SB of s | FB of F.t | RB of R.t

  and s = { b : B.t; bs : t list }

  let linspaces = function
    | SB { b; _ } | FB { b; _ } | RB { b; _ } ->
        (B.Row.linspace b, B.Col.linspace b)

  let is_admissible b = B.diam b <= B.dist b

  let is_small_enough min_block_size b =
    min (B.Row.size b) (B.Col.size b) <= min_block_size

  let rec build ~rank ~min_block_size b =
    if is_admissible b then RB (R.create ~rank b)
    else if is_small_enough min_block_size b then FB (F.create b)
    else
      let bs = B.split b |> Base.List.map ~f:(build ~rank ~min_block_size) in
      SB { b; bs }

  let rec fold ~init ~fs ~fr ~ff = function
    | RB b -> fr init b
    | FB b -> ff init b
    | SB b ->
        Base.List.fold ~init:(fs init b)
          ~f:(fun acc x -> fold ~init:acc ~fs ~fr ~ff x)
          b.bs

  let full_blocks =
    fold ~init:[] ~fs:(fun n _ -> n) ~fr:(fun n _ -> n) ~ff:(fun n b -> b :: n)

  let rank_blocks =
    fold ~init:[] ~fs:(fun n _ -> n) ~fr:(fun n b -> b :: n) ~ff:(fun n _ -> n)

  let%expect_test "16 x 16 unit domain" =
    let p = 3 in
    let n = Base.Int.shift_left 1 p in
    let b = B.create 1.0 n in
    let rank, min_block_size = (2, 2) in
    let sb = build ~rank ~min_block_size b in
    let fb = full_blocks sb |> List.length in
    let rb = rank_blocks sb |> List.length in
    Format.printf "R = %i, F = %i " rb fb;
    [%expect {| R = 6, F = 10 |}]

  let compute sb =
    let x, y = linspaces sb in
    fold ~init:sb
      ~fs:(fun b _ -> b)
      ~ff:(fun b fb ->
        F.compute fb x y;
        b)
      ~fr:(fun b rb ->
        R.compute rb x y;
        b)
      sb

  let matvec x ~y sb =
    fold ~init:y
      ~fs:(fun y _ -> y)
      ~ff:(fun y fb -> F.matvec fb x ~y)
      ~fr:(fun y rb -> R.matvec rb x ~y)
      sb
end
