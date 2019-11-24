module Block1D = struct

  module I = Indexed_domain.Indexed_Domain1D
  module V = Lacaml.D.Vec

  type t =
    { tau : I.t
    ; sigma: I.t
    }

  let create ?(xb=0.0) diam n =
    let i = I.create ~xb diam n in
    { tau = i
    ; sigma = i
    }

  let unit_block =
    create 1.0

  let split { tau; sigma } =
    let t1, t2 = I.split tau in
    let s1, s2 = I.split sigma in
    [ { tau = t1; sigma = s1 }
    ; { tau = t2; sigma = s1 }
    ; { tau = t1; sigma = s2 }
    ; { tau = t2; sigma = s2 }
    ]

  module Row = struct

    let ib { tau; _ } =
      I.ib tau

    let xb { tau; _ } =
      I.xb tau

    let xc { tau; _ } =
      I.xc tau

    let xe { tau; _ } =
      I.xe tau

    let size { tau; _ } =
      I.size tau

    let subvec { tau; _ } v =
      I.subvec tau v

    let h { tau; _} =
      I.h tau

    let linspace b =
      V.linspace (xb b) (xe b) (size b + 1)

  end

  module Col = struct

    let ib { sigma; _ } =
      I.ib sigma

    let size { sigma; _ } =
      I.size sigma

    let h { sigma; _ } =
      I.h sigma

    let subvec { sigma; _ } v =
      I.subvec sigma v

    let xb { sigma; _ } =
      I.xb sigma

    let xc { sigma; _ } =
      I.xc sigma

    let xe { sigma; _ } =
      I.xe sigma

    let linspace b =
      V.linspace (xb b) (xe b) (size b + 1)

  end

  let diam { tau; _ } =
    I.diam tau

  let dist { tau; sigma } =
    I.dist tau sigma

  let minus_xc b v =
    V.add_const (-1.0 *. Row.xc b) v


  let%expect_test "row linspace" =
    let b = unit_block 2 in
    let x = Row.linspace b |> minus_xc b in
    Format.printf "%a" (Lacaml.Io.pp_fvec) x;
    [%expect {|
      -0.5
         0
       0.5
    |}]

  let%expect_test "col linspace" =
    let b = unit_block 4 in
    let y = Col.linspace b |> minus_xc b in
    Format.printf "%a" (Lacaml.Io.pp_fvec) y;
    [%expect {|
       -0.5
      -0.25
          0
       0.25
        0.5
    |}]

end
