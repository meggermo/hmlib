module IndexSet1D = struct

  type t =
    { ib : int
    ; size : int
    }

  let create size =
    let _ = assert (size > 0) in
    { ib = 1; size }

  let ib { ib; _ } =
    ib

  let size { size; _ } =
    size

  let half_size { size; _ } =
    size / 2

  let ie i =
    ib i + size i - 1

  let split d =
    let sz1 = half_size d in
    let sz2 = size d - sz1 in
    { size = sz1; ib = ib d },
    { size = sz2; ib = ib d + sz1 }

  let as_string i =
    Format.sprintf "[ %i; %i ]" (ib i) (ie i)

  let%expect_test "splitting" =
    let i1, i2 = split (create 10) in
    Format.printf "%s %s" (as_string i1) (as_string i2);
    [%expect {| [ 1; 5 ] [ 6; 10 ] |}]

  let%expect_test "start and end" =
    let i1, i2 = split (create 11) in
    Format.printf "%i %i %i %i" (ib i1) (ie i1) (ib i2) (ie i2);
    [%expect {| 1 5 6 11 |}]

end

