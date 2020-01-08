module FullMatrix = struct
  module D = Lacaml.D

  type t = D.mat

  let create = D.Mat.make0

  let rows = D.Mat.dim1

  let cols = D.Mat.dim2

  let matvec m v ~y = D.gemv m v ~beta:1.0 ~trans:`T ~y

  let%expect_test "matvec" =
    let n, m = (2, 3) in
    let a = create n m in
    let v = D.Vec.make (rows a) 1.0 in
    let y = D.Vec.make (cols a) 0.0 in
    a.{1, 1} <- 1.0;
    a.{1, 3} <- 1.0;
    a.{2, 2} <- 1.0;
    a.{2, 3} <- 1.0;
    let open Lacaml.Io in
    Format.printf "%a\n" pp_fvec (matvec a v ~y);
    [%expect {|
      1
      1
      2
    |}]
end

module RankMatrix = struct
  module D = Lacaml.D

  type t = { a : D.mat; b : D.mat }

  let create ~rank rows cols =
    { a = D.Mat.make0 rows rank; b = D.Mat.make0 cols rank }

  let rows { a; _ } = D.Mat.dim1 a

  let cols { b; _ } = D.Mat.dim1 b

  let rank { a; _ } = D.Mat.dim2 a

  let matvec m v ~y = v |> D.gemv m.b ~trans:`T |> D.gemv m.a ~beta:1.0 ~y

  let%expect_test "rank rows cols" =
    let m = create ~rank:1 3 4 in
    Format.printf "%i %i %i" (rank m) (rows m) (cols m);
    [%expect {| 1 3 4 |}]

  let%expect_test "matvec" =
    let m = create ~rank:2 2 3 in
    let v = D.Vec.make 3 1.0 in
    let y = D.Vec.make 3 0.0 in

    m.a.{1, 1} <- 1.0;
    m.a.{2, 1} <- 0.0;
    m.a.{1, 2} <- 0.0;
    m.a.{2, 2} <- 1.0;

    m.b.{1, 1} <- 1.0;
    m.b.{2, 1} <- 0.0;
    m.b.{3, 1} <- 0.0;
    m.b.{1, 2} <- 0.0;
    m.b.{2, 2} <- 1.0;
    m.b.{3, 2} <- 0.0;

    Format.printf "%a\n" Lacaml.Io.pp_fvec (matvec m v ~y);
    [%expect {|
      1
      1
      0
    |}]
end
