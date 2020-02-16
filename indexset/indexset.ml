(** Module for working with one dimensional index sets *)
module IndexSet1D : sig
  type t
  (** Representation of a 1D index set *)

  val create : int -> t
  (** [create size] creates an index set containing the indices 1,2,...,size  *)

  val ib : t -> int
  (** [ib s] returns the begin index of the set s *)

  val ie : t -> int
  (** [ie s] returns the end index of the set s *)

  val size : t -> int
  (** [size s] returns the size of the set s *)

  val split : t -> t * t
  (** [split s] splits the set s into two sets. If the size of s is even
  then both sets are exactly half the size of s. Otherwise the 1st set is
  one greater than the second one so that the sum of the sizes equals that
  of the original set s.
   *)
end = struct
  type t = { ib : int; size : int }

  let create size =
    let _ = assert (size > 0) in
    { ib = 1; size }

  let ib { ib; _ } = ib

  let size { size; _ } = size

  let ie i = ib i + size i - 1

  let ic i = (ib i + ie i) / 2

  let split i =
    let ib, ie, ic = (ib i, ie i, ic i) in
    ({ size = ic - ib + 1; ib }, { size = ie - ic; ib = ic + 1 })

  let as_string i = Format.sprintf "[ %i; %i ]" (ib i) (ie i)

  let%expect_test "splitting even sized" =
    let i1, i2 = split (create 10) in
    Format.printf "%s %s" (as_string i1) (as_string i2);
    [%expect {| [ 1; 5 ] [ 6; 10 ] |}]

  let%expect_test "splitting odd sized" =
    let i1, i2 = split (create 11) in
    Format.printf "%n %n" (size i1) (size i2);
    [%expect {| 6 5 |}]
end
