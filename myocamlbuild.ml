(* Loaned from http://trac.cs.mcgill.ca/beluga/browser/myocamlbuild.ml?rev=c9efedadf7aa85e5c3b7c4b72688f7659dd12743 *)

open Ocamlbuild_plugin
open Unix

let hardcoded_version_file = ".version"
let version_file = "version.ml"
let version_content () =
  let version_from_git () =
    let i = open_process_in "git describe --dirty --always --tags" in
    let l = input_line i in
    if close_process_in i = WEXITED 0 then l
    else raise Not_found in
  let hardcoded_version () =
    let i = open_in hardcoded_version_file in
    let s = input_line i in
    close_in i; s in
  let version =
    try hardcoded_version () with _ ->
      try version_from_git () with _ ->
        failwith "Unable to determine version" in
  "let version = \"" ^ version ^ "\"\n"

let () =
  dispatch begin function
  | After_options ->
     rule "Version file" ~prods:[version_file] (fun env _ -> Echo ([version_content ()], version_file));
  | _ -> ()
  end
