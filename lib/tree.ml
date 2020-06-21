module Seq = struct
  let add : 'a -> 'a Seq.t -> 'a Seq.t = fun x xs ->
    (fun () -> Cons (x, xs))

  let mapi (f: int -> 'a -> 'b) (xs : 'a Seq.t) : 'b Seq.t =
    let rec iter i xs =
      match xs () with
      | Seq.Nil -> Seq.Nil
      | Seq.Cons (x, xs) -> Seq.Cons (f i x, fun () -> iter (i+1) xs)
    in
    (fun () -> iter 0 xs)

  let flat_mapi f xs =
    Seq.flat_map (fun x -> x) @@ mapi f xs

  include Seq
end

type t =
  | File of string
  | Dir of (string * t list)

let to_seq t =
  let rec iter prefix br1 br2 = function
    | File file -> Seq.add (prefix ^ br2 ^ file) Seq.empty
    | Dir (dir, ts) ->
       let length = List.length ts in
       Seq.add (prefix ^ br2 ^ dir) @@
         begin
           List.to_seq ts
           |> Seq.flat_mapi (fun i t ->
                  let prefix, br1, br2 =
                    if i = length - 1 then (prefix ^ br1, "    ", "└── ")
                    else (prefix ^ br1, "│   ", "├── ")
                  in
                  iter prefix br1 br2 t)
         end
  in
  iter "" "" "" t

let to_list t =
  to_seq t
  |> List.of_seq
