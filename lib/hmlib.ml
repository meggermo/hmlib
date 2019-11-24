
module FullBlock1D = struct

  module F = Matrix.FullMatrix
  module B = Block.Block1D

  type t =
    { b : B.t
    ; m : F.t
    }

  let create b =
    { b
    ; m = F.create (B.Row.size b) (B.Col.size b)
    }

  let matvec { b; m } v ~y =
    let v' = B.Row.subvec b v in
    let y' = B.Col.subvec b y in
    let _ = F.matvec m v' ~y:y' in
    y

  let primitive hk_sqr =
    Base.Float.( 0.25 * hk_sqr * (log hk_sqr - 3.0))

  let loglog h k =
    if k = 0 then
      0.0
    else
      let hk = Base.Float.( h * of_int k) in
      primitive (hk *. hk)

  let generate_exact h n =
    let generate_loglog h n = Base.Array.init n ~f:(loglog h) in
    let lls = generate_loglog h (n + 2) in
    let ll i = Base.Array.get lls i in
    let iis = Base.Array.init n ~f:
        (fun i -> 
           if i = 0 then 
             (2.0 *. ll 1) /. h
           else 
             (ll (i - 1) -. 2.0 *. ll i +. ll (i + 1)) /. h
        ) in
    fun i j -> Base.Array.get iis (abs (i - j))

  let%expect_test "I(i..i+1) I(j..j+1) log|x - y| dx dy" =
    let g_ij = generate_exact 1.0 3 in
    Format.printf "%g %g %g" (g_ij 0 0) (g_ij 0 1) (g_ij 2 0);
    [%expect {| -1.5 -0.113706 0.671167|}]

  let compute { b; m } =
    let rows, cols = B.Row.size b, B.Col.size b in
    let h = B.Row.h b in
    let g_ij = generate_exact h rows in
    for j = 1 to cols do
      for i = j to rows do
        m.{i, j}<- g_ij i j;
        m.{j, i}<- g_ij j i;
      done;
    done;

end

module RankBlock1D = struct

  module R = Matrix.RankMatrix
  module B = Block.Block1D

  type t =
    { b : B.t
    ; m : R.t
    }

  let rank { m; _ } =
    R.rank m

  let create ~rank b =
    { b
    ; m = R.create ~rank (B.Row.size b) (B.Col.size b) 
    }

  let matvec { b; m } v ~y =
    let v' = B.Row.subvec b v in
    let y' = B.Col.subvec b y in
    let _  = R.matvec m v' ~y:y' in
    y

  let compute_a { b; m } x =
    let xc = B.Row.subvec b x |> B.min_xc b in
    for nu = 1 to R.rank m do
      let open Base in
      let nu_p = Float.of_int nu in
      for i = 1 to B.Row.size b do
        m.a.{i, nu}<- (xc.{i + 1} **. nu_p -. xc.{i} **. nu_p) /. nu_p
      done;
    done

  let compute_b { b; m } y =
    let yc = B.Col.subvec b y |> B.min_xc b in
    let open Base in
    for j = 1 to B.Col.size b do
      let ym = Float.abs yc.{j} in
      let yp = Float .abs yc.{j + 1} in
      let lm = Float.log ym in
      let lp = Float.log yp in
      m.b.{j, 1}<- yp *. (lp -. 1.0) -. ym *. (lm -. 1.0);
      m.b.{j, 2}<- lm -. lp;
    done

  let compute rb x y =
    compute_a rb x;
    compute_b rb y;

end

module SuperBlock = struct

  module B = Block.Block1D
  module F = FullBlock1D
  module R = RankBlock1D

  type t =
    | SB of s
    | FB of F.t
    | RB of R.t
  and s =
    { b : B.t
    ; bs : t list
    }

  let linspaces = function
  | SB { b; _ }
  | FB { b; _ }
  | RB { b; _ } -> B.Row.linspace b, B.Col.linspace b     

  let is_admissible b =
    B.diam b <= B.dist b

  let is_small_enough min_block_size b =
    min (B.Row.size b) (B.Col.size b) <= min_block_size

  let rec build ~rank ~min_block_size b =
    if is_admissible b then
      RB (R.create ~rank b)
    else if is_small_enough min_block_size b then
      FB (F.create b)
    else
      let bs = 
        B.split b
        |> Base.List.map ~f:(build ~rank ~min_block_size) in
      SB { b; bs }

  let rec fold ~init ~fs ~fr ~ff = function
    | RB b -> fr init b
    | FB b -> ff init b
    | SB b ->
      Base.List.fold 
        ~init:(fs init b) 
        ~f:(fun acc x -> fold ~init:acc ~fs ~fr ~ff x) b.bs

  let full_blocks =
    fold ~init:[]
      ~fs:(fun n _ -> n)
      ~fr:(fun n _ -> n)
      ~ff:(fun n b -> b :: n)

  let rank_blocks =
    fold ~init:[]
      ~fs:(fun n _ -> n)
      ~fr:(fun n b -> b :: n)
      ~ff:(fun n _ -> n)

  let%expect_test "16 x 16 unit domain" =
    let p = 3 in
    let n = Base.Int.shift_left 1 p in
    let b = B.create 1.0 n in
    let rank, min_block_size = 2, 2 in
    let sb = build ~rank ~min_block_size b in
    let fb = full_blocks sb |> List.length in
    let rb = rank_blocks sb |> List.length in
    Format.printf "R = %i, F = %i " rb fb;
    [%expect {| R = 6, F = 10 |}]

  let compute sb =
    let x, y = linspaces sb in
    fold ~init:sb
      ~fs:(fun b _ -> b)
      ~ff:(fun b fb -> F.compute fb; b)
      ~fr:(fun b rb -> R.compute rb x y; b)
      sb

  let matvec x ~y sb =
    fold ~init:y
      ~fs:(fun y _ -> y)
      ~ff:(fun y fb -> F.matvec fb x ~y:y)
      ~fr:(fun y rb -> R.matvec rb x ~y:y)
      sb

end
