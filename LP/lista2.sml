
type Num = int;
type Var = string;

datatype Aexpr =
N of Num
| V of Var
| Plus of Aexpr * Aexpr
| Mult of Aexpr * Aexpr
| Minus of Aexpr * Aexpr;

datatype Bexpr =
  True
| False
| Eq of Aexpr * Aexpr
| Leq of Aexpr * Aexpr
| Not of Bexpr
| And of Bexpr * Bexpr;

datatype Stm =
Assign of Var * Aexpr
| Skip
| Comp of Stm * Stm
| If of Bexpr * Stm * Stm
| While of Bexpr * Stm
| Repeat of Stm * Bexpr
;




fun evalN n : Num = n

exception FreeVar;
fun lookup [] id = raise FreeVar
| lookup (( k : string , v ) :: l ) id = if id = k then v else 
lookup l id;

fun evalA ( N n ) _ = evalN n
| evalA ( V x ) s = lookup s x
| evalA ( Plus ( e1 , e2 ) ) s = ( evalA e1 s ) + ( evalA e2 s )
| evalA ( Mult ( e1 , e2 ) ) s = ( evalA e1 s ) * ( evalA e2 s )
| evalA ( Minus ( e1 , e2 ) ) s = ( evalA e1 s ) - ( evalA e2 s );

fun evalB True _ = true
| evalB False _ = false
| evalB ( Eq ( a1 , a2 ) ) s = ( evalA a1 s ) = ( evalA a2 s )
| evalB ( Leq ( a1 , a2 ) ) s = ( evalA a1 s ) <= ( evalA a2 s )
| evalB ( Not b ) s = not ( evalB b s )
| evalB ( And ( b1 , b2 ) ) s = ( evalB b1 s ) andalso ( evalB b2 s );

fun evalStm ( stm : Stm ) ( s : ( string * int ) list ) : 
  ( string * int ) list =
  case stm of
  ( Assign (x , a ) ) => (x , evalA a s ) :: s
  | Skip => s
  | ( Comp ( stm1 , stm2 ) ) => evalStm stm2 ( evalStm stm1 s )
  | ( If (b , stm1 , stm2 ) ) =>
  if ( evalB b s ) then evalStm stm1 s else evalStm stm2 s
  | While (b, stm ) => 
    let
      val cond = evalB b s;
    in
      if cond 
      then evalStm stm (evalStm stm s)
      else s
    end
  | Repeat (stm,b) => 
    let
      val cond = evalB b s;
      val state = evalStm stm s
    in
      if cond
      then evalStm stm state
      else state
    end
  | _ => raise Match;


fun pow b = 
  let
    fun calculatePow k = k * k
  in 
    calculatePow b
  end;


fun split a = 
  let
    fun aux ([],dir) = ([],[])
    |  aux (x::xs,dir) = 
      let
        val (e,d) = aux(xs,~dir)
      in
        if dir>0 then (e,x::d)
        else (x::e,d)
      end
  in
    aux (a,~1)
  end;\


   
fun expr () =
  let
    val x = 1
  in
    let val x = 2 in x + 1 end
    +
    let val y = x + 2 in y + 1 end
  end;

> val it = 7: int;