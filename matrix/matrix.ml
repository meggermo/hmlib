
module FullMatrix = struct

  module D = Lacaml.D

  type t = D.mat

  let create =
    D.Mat.make0

  let rows =
    D.Mat.dim1

  let cols =
    D.Mat.dim2

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

  let rows { a; _ }=
    D.Mat.dim1 a

  let cols { b; _ }=
    D.Mat.dim1 b

  let rank { a; _ } =
    D.Mat.dim2 a

  let matvec m v ~y =
    v
    |> D.gemv m.b ~trans:`T
    |> D.gemv m.a ~beta:1.0 ~y

end
