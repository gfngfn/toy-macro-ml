
open Syntax


module VarMap = Map.Make(String)


type bound_to =
  | Normal  of mono_type * stage
  | Late    of mono_type
  | Bindee  of identifier * mono_type * mono_type
  | Macro   of macro_param_type list * mono_type

type t = {
  main : bound_to VarMap.t;
}


let empty =
  {
    main = VarMap.empty;
  }

let add x boundto tyenv =
  {
    main = tyenv.main |> VarMap.add x boundto;
  }

let find_opt x tyenv =
  tyenv.main |> VarMap.find_opt x
