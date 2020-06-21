type t =
  | File of string
  | Dir of (string * t list)

val to_seq : t -> string Seq.t

val to_list : t -> string list
