open Ast


exception Notimplemented
exception CannotStep
exception Nottyped

let parse (s:string) : expr =
let lexbuf = Lexing.from_string s in
let ast = Parser.prog Lexer.read lexbuf in 
ast

(*env is of type (string*typ) list *)
let empty = []

let insert env x t = (x,t) :: env

let rec lookup env x t = match env with 
[] -> false
| hd::tl -> if (hd = (x,t)) then true else lookup tl x t

let rec lookuptyp env x = match env with
[] -> None
| (y,t)::tl -> if (x=y) then Some t else lookuptyp tl x 

let rec typeof env e = match e with 
| Var x -> (match (lookuptyp env x) with None -> raise Nottyped | Some t -> t)
| Integer _ -> Tint
| Boolean _ -> Tbool
| Binop (bop, e1, e2) -> typeofbop env bop e1 e2
| If (e1, e2, e3) -> if (typeof env e1 = Tbool) then (let t2= (typeof env e2) in let t3= (typeof env e3) in if (t2=t3) then t2 else raise Nottyped) else raise Nottyped
| Let (x, t1, e1, e2) -> if (typeof env e1 = t1) then (typeof (insert env x t1) e2) else raise Nottyped 
| Fun (x, t1, e2) -> let t2 = typeof (insert env x t1) e2 in Tfun(t1,t2)
| App (e1, e2) -> let t2 = (typeof env e2) in  let t1=(typeof env e1) in (match t1 with Tfun(x, y) -> (if t2=x then y else raise Nottyped)  | _ -> raise Nottyped)
| Nil t -> t
| Cons (e1, e2) -> let t1 = typeof env e1 in let t2 = typeof env e2 in if t2 = Tlist t1 then Tlist t1 else raise Nottyped
| Match(e1, t1, e2, x1, x2, e3) ->
  match t1 with
    | Tlist(t2) ->
      if typeof env e1 = t1 then
        let t4 = typeof env e2 in let t3 = typeof (insert (insert env x1 t2) x2 t1) e3 in
        if t4 = t3 then t4 else raise Nottyped
      else raise Nottyped
    | _ -> raise Nottyped








and typeofbop env bop e1 e2 = match bop with 
| Plus ->  if (typeof env e1 = Tint && typeof env e2 = Tint) then Tint else raise Nottyped 
| Minus ->  if (typeof env e1 = Tint && typeof env e2 = Tint) then Tint else raise Nottyped 
| Times -> if (typeof env e1 = Tint && typeof env e2 =Tint) then Tint else raise Nottyped
| Div -> if (typeof env e1 = Tint && typeof env e2 =Tint) then Tint else raise Nottyped
| Leq -> if (typeof env e1 = Tint && typeof env e2=Tint) then Tbool else raise Nottyped
| Geq -> if (typeof env e1 = Tint && typeof env e2=Tint) then Tbool else raise Nottyped
| Le -> if (typeof env e1 = Tint && typeof env e2=Tint) then Tbool else raise Nottyped
| Ge -> if (typeof env e1 = Tint && typeof env e2=Tint) then Tbool else raise Nottyped
| Eq -> if (typeof env e1 = Tint && typeof env e2=Tint) then Tbool else raise Nottyped
| Or -> if (typeof env e1 = Tbool && typeof env e2 = Tbool) then Tbool else raise Nottyped
| And-> if (typeof env e1 = Tbool && typeof env e2 = Tbool) then Tbool else raise Nottyped

let typecheck e = let _ = (typeof empty  e) in e 


let rec isval (e:expr) : bool = match e with 
| Integer _ -> true
| Boolean _ -> true
| Fun _ -> true
| Nil _ -> true
| Cons(e1,e2) -> isval(e1) && isval(e2)
| _ -> false

let calc bop e1 e2 = match bop, e1, e2 with
| Plus, Integer(v1), Integer(v2) ->  Integer(v1+v2)
| Minus, Integer(v1), Integer(v2) ->  Integer(v1-v2)
| Minus, Integer(v1), Integer(v2) ->  Integer(v1-v2)
| Times, Integer(v1), Integer(v2) -> Integer(v1*v2)
| Div, Integer(v1), Integer(v2) ->  Integer(v1/v2)
| Leq, Integer(v1), Integer(v2) -> Boolean (v1<=v2)
| Le, Integer(v1), Integer(v2) -> Boolean (v1<v2)
| Geq, Integer(v1), Integer(v2) -> Boolean (v1>=v2)
| Ge, Integer(v1), Integer(v2) -> Boolean (v1>v2)
| Eq, Integer(v1), Integer(v2) -> Boolean (v1=v2)
| And, Boolean(v1), Boolean(v2) -> Boolean (v1 && v2)
| Or, Boolean(v1), Boolean(v2) -> Boolean (v1 || v2)
| _ -> raise CannotStep

let rec subst x (e1:expr) (e2:expr) = match e2 with 
| Var(z) -> if z=x then e1 else Var(z)
| Integer(w) -> Integer(w)
| Boolean(w) -> Boolean(w)
| Binop(bop, e3, e4) -> Binop (bop, (subst x e1 e3),(subst x e1 e4)) 
| Let(y, t, e3, e4) -> if y=x then Let(y, t, (subst x e1 e3), e4) else Let(y, t, (subst x e1 e3), (subst x e1 e4))
| If(e3, e4, e5) ->  If((subst x e1 e3), (subst x e1 e4), (subst x e1 e5))
| Fun(y, t, e3) -> if y=x then Fun(y, t, e3) else Fun(y, t, (subst x e1 e3))
| App(e3, e4) ->  App((subst x e1 e3), (subst x e1 e4))
| Nil(t) -> Nil(t)
| Cons(e3, e4) -> Cons((subst x e1 e3), (subst x e1 e4))
| Match( e3, t, e4, y, z, e5) -> if (y=x || z=x) then Match( (subst x e1 e3), t, (subst x e1 e4), y, z, e5)  else Match( (subst x e1 e3), t, (subst x e1 e4), y, z, (subst x e1 e5)) 

let rec step (w:expr) : expr = match w with
| Var _ -> raise CannotStep
| Integer _ -> raise CannotStep
| Boolean _ -> raise CannotStep
| Nil _ -> raise CannotStep
| Fun _  -> raise CannotStep
| Binop (bop, e1, e2) -> if(isval e1) then (if (isval e2) then (calc bop e1 e2) else Binop(bop, e1, step e2)) else Binop(bop, step e1, e2)
| Let(x, t, e1, e2) -> if(isval e1) then (subst x e1 e2) else Let(x, t ,step e1, e2)
| If(e, e1, e2) -> if(e=Boolean(true)) then e1 else (if(e=Boolean(false)) then e2 else If(step e, e1, e2))
| App(e1,e2) -> if (isval e1) then (match e1 with Fun(x,_,e3) -> (subst x e2 e3)| _ -> raise CannotStep) else App((step e1), e2)
| Cons(e1,e2) -> if (isval e1) then (if (isval e2) then raise CannotStep else Cons(e1, step e2)) else Cons((step e1), e2)
|Match(e1, t, e2, y, z, e3) -> (if (isval e1) then (match e1 with 
| Nil _ -> e2 
| Cons(e4,e5) -> (subst  z e5 (subst y e4 e3))
| _ -> raise CannotStep)
 else Match((step e1), t, e2, y, z, e3))


let rec stepstar (e:expr) : expr = 
if (isval e) then e else (stepstar (step e))

let interp_small (s:string) : expr = 
step (parse s)

let interp_small_eval (s:string) : expr = 
let e= parse s in stepstar e

let parse_typ (s:string) : expr =
let e= parse s in typecheck e

let typ_eval (s:string) : expr =
let e= parse s in (let e1 = typecheck e in (stepstar e1))