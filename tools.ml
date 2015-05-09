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

let assoc_of_yojson f error (json : Yojson.Safe.json) =
  match json with
  | `Assoc assoc ->
    ( let results = List.map (fun (k, v) -> (k, v, f v)) assoc in
      let ok, errors =
        partition_map (
          function
          | (key, _, `Ok value) -> `Left (key, value)
          | (key, json, `Error error) -> `Right (error ^ "(" ^ Yojson.Safe.to_string json ^ ")"))
          results
      in
      match errors with
      | [] -> `Ok ok
      | errors -> `Error (Printf.sprintf "Error when expecting associative table for %s: %s" error (String.concat " & " errors)) )
  | _ -> `Error (Printf.sprintf "Expected associative table for %s but got %s" error (Yojson.Safe.to_string json))

let assoc_to_yojson f xs =
  `Assoc (
    xs |> List.map @@ fun (k, v) ->
    ((k : string), f v)
  )

type json = Yojson.Safe.json

let pp_json fmt json = Format.fprintf fmt "%s" (Yojson.Safe.to_string json)

let json_of_yojson json = `Ok json

let map_of_yojson f map value =
  match map value with
  | `Ok value -> f value
  | (`Error _) as error -> error
