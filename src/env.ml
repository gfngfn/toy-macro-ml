
module VarMap = Map.Make(String)

type ('v0, 'v1) entry =
  | V0   of 'v0
  | V1   of 'v1
  | Both of 'v0

type ('v0, 'v1) t = (('v0, 'v1) entry) VarMap.t


let empty = VarMap.empty


let add = VarMap.add


let find_opt = VarMap.find_opt
