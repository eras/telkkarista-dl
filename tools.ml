(* Partitions a list in two parts depending on the criteria returned by predicate.

   Compared to List.partition this also maps each element produced to the lists. *)
let partition_map : ('a -> [< `Left of 'b | `Right of 'c ]) -> 'a list -> 'b list * 'c list  = fun predicate xs ->
  let rec loop xs left right =
    match xs with
    | [] -> (List.rev left, List.rev right)
    | x::xs ->
      match predicate x with
      | `Left x -> loop xs (x::left) (right)
      | `Right x -> loop xs (left) (x::right)
  in
  loop xs [] []

