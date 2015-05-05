let partition_map p xs =
  let rec loop xs left right =
    match xs with
    | [] -> (List.rev left, List.rev right)
    | x::xs ->
      match p x with
      | `Left x -> loop xs (x::left) (right)
      | `Right x -> loop xs (left) (x::right)
  in
  loop xs [] []

