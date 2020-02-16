module Domain1D : sig
  type t
  (** Representation of a 1D domain *)

  val create : ?xb:float -> float -> t
  (** [create xb diam] creates a domain [xb, xb + diam].
  The diam must be positive*)

  val split : t -> t * t
  (** [split d] splits the domain into [xb d, xc d], [xc d, xe d*)

  val diam : t -> float
  (** [diam d] returns the diameter of d *)

  val dist : t -> t -> float
  (** [dist d1 d2] return the distance between d1 and d2.
  Note that the distance is 0 if the domains overlap. *)

  val xb : t -> float
  (** [xb d] returns the begin of the domain *)

  val xc : t -> float
  (** [xc d] returns the center of the domain *)

  val xe : t -> float
  (** [xe d] returns the end of the domain *)
end = struct
  type t = { xb : float; diam : float }

  let create ?(xb = 0.0) diam =
    let _ = assert (diam > 0.0) in
    { xb; diam }

  let unit_domain = create 1.0

  let xb { xb; _ } = xb

  let diam { diam; _ } = diam

  let xe d = Base.Float.(xb d + diam d)

  let xc d = Base.Float.(0.5 * (xb d + xe d))

  let dist d1 d2 =
    let open Base.Float in
    if xe d1 <= xb d2 then xb d2 - xe d1 else xb d1 - xe d2

  let split d =
    let half_diam = Base.Float.(0.5 * diam d) in
    (create ~xb:(xb d) half_diam, create ~xb:(xc d) half_diam)

  let as_string d = Format.sprintf "[ %g; %g ]" (xb d) (xe d)

  let%expect_test "splitting" =
    let d1, d2 = split unit_domain in
    Format.printf "%s %s" (as_string d1) (as_string d2);
    [%expect {| [ 0; 0.5 ] [ 0.5; 1 ] |}]

  let%expect_test "begin center end" =
    let d = create ~xb:(-1.0) 2.0 in
    Format.printf "%g %g %g" (xb d) (xc d) (xe d);
    [%expect {| -1 0 1 |}]

  let%expect_test "distance" =
    let d1, d2 = split unit_domain in
    let (d11, d12), (d21, d22) = (split d1, split d2) in
    Format.printf "%g %g %g" (dist d12 d21) (dist d12 d22) (dist d11 d22);
    [%expect {| 0 0.25 0.5 |}]
end
