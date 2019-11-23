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

  let ie i =
    ib i + size i - 1

  let split d =
    let sh = size d / 2 in
    { ib = ib d; size = sh },
    { ib = ib d + sh; size = sh }

  let as_string i =
    Format.sprintf "[ %i; %i ]" (ib i) (ie i)

  let%expect_test "splitting" =
    let i1, i2 = split (create 10) in
    Format.printf "%s %s" (as_string i1) (as_string i2);
    [%expect {| [ 1; 5 ] [ 6; 10 ] |}]

end

