module Block1D = struct

  module I = Indexed_domain.Indexed_Domain1D

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
