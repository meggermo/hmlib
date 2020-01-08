module FullBlock1D = struct
  module F = Matrix.FullMatrix
  module B = Block.Block1D

  type t = { b : B.t; m : F.t }

  let create b = { b; m = F.create (B.Row.size b) (B.Col.size b) }

  let matvec { b; m } v ~y =
    let v' = B.Row.subvec b v in
    let y' = B.Col.subvec b y in
    let _ = F.matvec m v' ~y:y' in
    y

  let%expect_test "matvec" =
    let ms =
      B.create 1.0 4 |> B.split |> Base.List.map ~f:create |> Base.List.to_array
    in
    let m1 = Base.Array.get ms 0 in
    let m2 = Base.Array.get ms 1 in
    let m3 = Base.Array.get ms 2 in
    let m4 = Base.Array.get ms 3 in
    m1.m.{1, 1} <- 1.0;
    m1.m.{2, 2} <- 1.0;
    m4.m.{1, 1} <- 1.0;
    m4.m.{2, 2} <- 1.0;
    m2.m.{2, 1} <- 1.0;
    let v = Lacaml.D.Vec.make 4 1.0 in
    let y = Lacaml.D.Vec.make 4 0.0 in
    matvec m1 v ~y |> ignore;
    matvec m2 v ~y |> ignore;
    matvec m3 v ~y |> ignore;
    matvec m4 v ~y |> ignore;
    Format.printf "%a" Lacaml.Io.pp_fvec y;
    [%expect {|
      2
      1
      1
      1
    |}]

  let primitive hk_sqr = Base.Float.(0.25 * hk_sqr * (log hk_sqr - 3.0))

  let loglog s = if s = 0.0 then 0.0 else primitive (s *. s)

  let compute { b; m } x y =
    let h_inv = 1.0 /. B.Row.h b in
    let rows, cols = (B.Row.size b, B.Col.size b) in
    let xc = B.Row.subvec_e b x |> B.min_xc b in
    let yc = B.Col.subvec_e b y |> B.min_xc b in
    for j = 1 to cols do
      let ym, yp = (yc.{j}, yc.{j + 1}) in
      for i = 1 to rows do
        let xm, xp = (xc.{i}, xc.{i + 1}) in
        let p_mm = loglog (xm -. ym) in
        let p_mp = loglog (xm -. yp) in
        let p_pm = loglog (xp -. ym) in
        let p_pp = loglog (xp -. yp) in
        m.{i, j} <- h_inv *. (p_mp +. p_pm -. (p_pp +. p_mm))
      done
    done
end
