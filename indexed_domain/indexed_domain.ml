module Indexed_Domain1D = struct

  module D = Domain.Domain1D
  module I = Indexset.IndexSet1D
  module V = Lacaml.D.Vec

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

  let ib { i; _ } =
    I.ib i

  let size { i; _ } =
    I.size i

  let xb { d; _ } =
    D.xb d

  let xc { d; _ } =
    D.xc d

  let xe { d; _ } =
    D.xe d

  let h i =
    diam i /. (Base.Float.of_int (size i - 1))

  let linspace i =
    V.linspace (xb i) (xe i) (size i + 1)

  let min_xc i v =
    V.add_const ( -1.0 *. xc i) v

end

