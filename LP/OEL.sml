(*
 OEL : Our Expression Language
*)

datatype expr =
  IConst of int
  (* Catch-all for all binary primitive operations *)
  | Prim2 of string * expr * expr
  | Var of string
  (* an operator to bind a variable to a value in an expression *)
  | Let of string * expr * expr;

exception FreeVar;

fun lookup [] id = raise FreeVar
  | lookup ((k:string, v)::t) id = if k = id then v else lookup t id;

fun eval (e:expr) (st: (string * expr) list) : int =
  case e of
    (IConst i) => i
  | (Var v) => eval (lookup st v) st
  | (Prim2(f, e1, e2)) =>
    let
        val v1 = (eval e1 st);
        val v2 = (eval e2 st) in
    case f of
        ("+") => v1 + v2
      | ("-") => v1 - v2
      | ("*") => v1 * v2
      | ("/") => v1 div v2
      | _ => raise Match
    end
  | (Let(x, e1, e2)) => (* i.e., let val x = e1 in e2 end *)
    let val v = eval e1 st; (* evaluate e1 in the input state *)
        val st' = (x, IConst v) :: st in (* update the state *)
            eval e2 st'               (* evaluate e2 with new state*)
    end
  (* | _ => raise Match; *)


val e1 = Let("z", IConst 17, Prim2("+", Var "z", Var "z"));

val e2 = Let("z", Prim2("-", IConst 5, IConst 4),
           Prim2("*", IConst 100, Var "z"));

(* x = 10
y = 2
3 * x / y *)
val e3 = Let("x", IConst 10, 
  Let("y", IConst 2,
    Prim2("/", 
      Prim2("*",IConst 3,Var "x")
    , Var "y")
  ));

val e6 = Let("x", IConst 5,
             Let("y",
                 Prim2("*", IConst 2, Var "x"),
                 Let("x", IConst 3,
                     Prim2("+", Var "x", Var "y"))));


(*? Computing the set of free variables *)
(*! Define union and diff first *)
(* 
fun freeVars e : string list =
  case e of
    IConst _ => []
  | (Var v) => [v]
  | (Prim2(_, e1, e2)) => union (freeVars e1) (freeVars e2)
  | (Let(v, e1, e2)) =>
    let val v1 = freeVars e1;
        val v2 = freeVars e2
    in
      union v1 (diff v2 [v]) 
      (* let binds x in e2 but not in e1 *)
    end
  | _ => raise Match;
 *)
