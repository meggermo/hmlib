module Domain1D = struct

  type t =
    { xb : float
    ; diam : float
    }

  let create ?(xb=0.0) diam =
    let _ = assert (diam > 0.0) in
    { xb; diam }

  let unit_domain = 
    create 1.0

  let xb { xb; _ } =
    xb

  let diam { diam; _ } =
    diam

  let xe d =
    Base.Float.(xb d + diam d)

  let xc d =
    Base.Float.( 0.5 * (xb d + xe d) )

  let dist d1 d2 =
    if xe d1 <= xb d2 then
      Base.Float.(xb d2 - xe d1)
    else
      Base.Float.(xb d1 - xe d2)

  let split d =
    let dh = Base.Float.( 0.5 * diam d ) in
    create ~xb:(xb d) dh,
    create ~xb:(xc d) dh

  let as_string d =
    Format.sprintf "[ %g; %g ]" (xb d) (xe d)

  let%expect_test "splitting" =
    let d1, d2 = split unit_domain in
    Format.printf "%s %s" (as_string d1) (as_string d2);
    [%expect {| [ 0; 0.5 ] [ 0.5; 1 ] |}]

end
