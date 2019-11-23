
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

module Interval1D = struct

  module D = Domain1D
  module I = IndexSet1D

  type t =
    { i : I.t
    ; d : D.t
    }

  let create ?(xb=0.0) diam n =
    { d = D.create ~xb diam
    ; i = I.create n 
    }

  let split { i; d } =
    let i1, i2 = I.split i in
    let d1, d2 = D.split d in
    { d = d1; i = i1 },
    { d = d2; i = i2 }

  let diam { d; _ } =
    D.diam d

  let dist { d = d1; _ } { d = d2; _ } =
    D.dist d1 d2

  let subvec { i; _ }  v =
    Bigarray.Array1.sub v (I.ib i) (I.size i)

  let size { i; _ } =
    I.size i

  let h i =
    diam i /. (Base.Float.of_int (size i - 1))

end

module Block1D = struct

  module I = Interval1D

  type t =
    { tau : I.t
    ; sigma: I.t
    }

  let create ?(xb=0.0) diam n =
    let i = I.create ~xb diam n in
    { tau = i
    ; sigma = i
    }

  let split { tau; sigma } =
    let t1, t2 = I.split tau in
    let s1, s2 = I.split sigma in
    [ { tau = t1; sigma = s1 }
    ; { tau = t2; sigma = s1 }
    ; { tau = t1; sigma = s2 }
    ; { tau = t2; sigma = s2 }
    ]

  module Row = struct

    let size { tau; _ } =
      I.size tau

    let subvec { tau; _ } v =
      I.subvec tau v

    let h { tau; _} =
      I.h tau

  end

  module Col = struct
  
    let size { sigma; _ } =
      I.size sigma

    let h { sigma; _ } =
      I.h sigma

    let subvec { sigma; _ } v =
      I.subvec sigma v

  end
  
  let diam { tau; _ } =
    I.diam tau

  let dist { tau; sigma } =
    I.dist tau sigma

end

module FullMatrix = struct

  module D = Lacaml.D

  type t = D.mat

  let create =
    D.Mat.make0

  let matvec m v ~y =
    D.gemv m v ~beta:1.0 ~y

end

module RankMatrix = struct

  module D = Lacaml.D

  type t =
    { a : D.mat
    ; b : D.mat
    }

  let create ~rank rows cols =
    { a = D.Mat.make0 rows rank
    ; b = D.Mat.make0 cols rank
    } 

  let matvec m v ~y =
    v
    |> D.gemv m.b ~trans:`T
    |> D.gemv m.a ~beta:1.0 ~y

end

module FullBlock1D = struct

  module F = FullMatrix
  module B = Block1D

  type t =
    { b : B.t
    ; m : F.t
    }

  let create b =
    { b
    ; m = F.create (B.Row.size b) (B.Col.size b)
    }

  let compute b f =
    let _ = f b.m in b

  let matvec { b; m } v ~y =
    let v' = B.Row.subvec b v in
    let y' = B.Col.subvec b y in
    let _ = F.matvec m v' ~y:y' in
    y

end

module RankBlock1D = struct

  module R = RankMatrix
  module B = Block1D

  type t =
    { b : B.t
    ; m : R.t
    }

  let create ~rank b =
    { b
    ; m = R.create ~rank (B.Row.size b) (B.Col.size b) 
    }

  let compute b f =
    let _ = f b.m in b

  let matvec { b; m } v ~y =
    let v' = B.Row.subvec b v in
    let y' = B.Col.subvec b y in
    let _  = R.matvec m v' ~y:y' in
    y'

end

module Kernel1D = struct

  module B = Base

  let primitive hk_sqr =
    B.Float.( 0.25 * hk_sqr * (log hk_sqr - 3.0))

  let loglog h k =
    if k = 0 then
      0.0
    else
      let hk = B.Float.( h * of_int k) in
      primitive (hk *. hk)

  let generate_exact h n =
    let open B in
    let generate_loglog h n = B.Array.init n ~f:(loglog h) in
    let lls = generate_loglog h (n + 2) in
    let ll i = B.Array.get lls i in
    let iis = B.Array.init n ~f:
        (fun i -> 
           if i = 0 then 
             2.0 *. ll 1 
           else 
             ll (i - 1) -. 2.0 *. ll i +. ll (i + 1)
        ) in
    fun i j -> B.Array.get iis (abs (i - j))

  let%expect_test "I(i..i+1) I(j..j+1) log|x - y| dx dy = -1.5" =
    let g_ij = generate_exact 1.0 3 in
    Format.printf "%g %g %g" (g_ij 0 0) (g_ij 0 1) (g_ij 2 0);
    [%expect {| -1.5 -0.113706 0.671167|}]

end

module SuperBlock = struct

  module B = Block1D
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

end
