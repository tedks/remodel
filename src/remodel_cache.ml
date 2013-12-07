module L = List
module H = Hashtbl
module P = Printf
let (@@) fn x = fn x


let remodel_path = ".remodel"

let cache_path p = P.sprintf "%s/%s" remodel_path p

let init () = 
  if not @@ Sys.file_exists remodel_path then
    Unix.mkdir remodel_path 0o750
  else 
    if not @@ Sys.is_directory remodel_path then
      failwith "Remodel cache directory occupied by file"
    else ()

let stale path = 
  let cache_ch = open_in @@ cache_path path in 
  let r = Digest.file path <> Digest.input cache_ch in 
  close_in cache_ch; 
  r

let cache path = 
  let ch = open_out @@ cache_path path in 
  Digest.output ch (Digest.file path);
  close_out ch
    
  
