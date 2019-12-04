
module RankBlock1D = struct

  module R = Matrix.RankMatrix
  module B = Block.Block1D

  type t =
    { b : B.t
    ; m : R.t
    }

  let rank { m; _ } =
    R.rank m

  let create ~rank b =
    { b
    ; m = R.create ~rank (B.Row.size b) (B.Col.size b) 
    }

  let matvec { b; m } v ~y =
    let v' = B.Row.subvec b v in
    let y' = B.Col.subvec b y in
    let _  = R.matvec m v' ~y:y' in
    y

  let compute_a { b; m } x =
    let xc = B.Row.subvec_e b x |> B.min_xc b in
    for j = 1 to R.rank m do
      let open Base in
      let nu = Float.of_int j in
      for i = 1 to B.Row.size b do
        m.a.{i, j}<- (xc.{i + 1} **. nu -. xc.{i} **. nu) /. nu
      done;
    done

  let compute_b { b; m } y =
    let yc = B.Col.subvec_e b y |> B.min_xc b in
    let open Base in
    for j = 1 to B.Col.size b do
      let ym = Float.abs yc.{j    } in
      let yp = Float.abs yc.{j + 1} in
      let lm = Float.log ym in
      let lp = Float.log yp in
      m.b.{j, 1}<- yp *. (lp -. 1.0) -. ym *. (lm -. 1.0);
      m.b.{j, 2}<- lm -. lp;
    done

  let compute rb x y =
    compute_a rb x;
    compute_b rb y;

end
