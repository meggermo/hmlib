
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
