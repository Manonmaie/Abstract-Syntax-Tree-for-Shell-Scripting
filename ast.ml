exception ParseError of string

type op =
    Add
  | Sub
  | Mult
  | Div
  | Assign

type non_essential =
    Colon
  | Dollar
  | LParen
  | RParen

type cond_op =
    Eq
  | Lt
  | Gt

type token =
    Op of op
  | Num_tok of int
  | Var_tok of string
  | EOF

type expr =
    Num of int
  | Var of string
  | BinOp of expr * op * expr

type echo =
    Echo_lex

type my_if =
    If_lex

type my_while =
    While_lex

type cond =
    expr * cond_op * expr

type generic_obj =
    Echo_generic of echo
  | If_generic of my_if
  | While_generic of my_while
  | Tokens_generic of token list
  | Expr_generic of expr
  | Cond of cond
  | Others of non_essential

type return_vals =
    Generic_echo of echo * generic_obj
  | Generic_if of cond * generic_obj * generic_obj
  | Generic_only_if of cond * generic_obj
  | Generic_while of cond * generic_obj
  | Generic_expr of expr

(* Infix to prefix for mathematical expressions *)
let rec prefix lst =
  match lst with
     [] -> raise (ParseError "empty string")
    |h::t->
      match t with
        [] -> h::[]
        |h'::t' ->
        match h' with
            Op(o) -> h'::h::prefix t'
           |Num_tok(o) -> raise (ParseError "premature end string")
           |Var_tok(o) -> raise (ParseError "premature end string")
           |EOF -> raise (ParseError "premature end string")

(* Parser for mathematical expressions *)
let rec parser_expr lst =
  match lst with
   [] -> raise (ParseError "premature end of string")
  |h::t ->
    match h with
      Op(o) ->
       let (p',t') = parser_expr t in
       let (p'',t'') = parser_expr t' in
       (BinOp(p',o,p''),t'')
       |Num_tok(n) -> (Num(n),t)
       |Var_tok(n) -> (Var(n),t)
       |EOF -> raise (ParseError "premature end string")

let wrapper_parser_expr lst =
  let prefix_lst = prefix lst in
  let (binop,list) = parser_expr prefix_lst in
  Expr_generic(binop)

let parser_echo h t =
    match h with
      Echo_generic(o) ->
        begin
        match t with
          Tokens_generic(o') ->
            let binop = wrapper_parser_expr o' in
              Generic_echo(o,binop)
          |Expr_generic(_) ->
            Generic_echo(o,t)
          |_ -> raise(ParseError "Invalid")
          end
      |_ -> raise(ParseError "Invalid")

let parser_if c t f =
  match c with
    Cond(o) ->
      begin
      match t with
        Tokens_generic(o') ->
          begin
          match f with
            Tokens_generic(o'') ->
              let binop_t = wrapper_parser_expr o' in
              let binop_f = wrapper_parser_expr o'' in
              Generic_if(o,binop_t,binop_f)
            |Expr_generic(o'') ->
              let binop_t = wrapper_parser_expr o' in
              Generic_if(o,binop_t,f)
            |_ -> raise(ParseError "Invalid")
            end
        |Expr_generic(o') ->
          begin
          match f with
            Tokens_generic(o'') ->
              let binop_f = wrapper_parser_expr o'' in
              Generic_if(o,t,binop_f)
            |Expr_generic(o'') ->
              Generic_if(o,t,f)
            |_ -> raise(ParseError "Invalid")
            end
        |_ -> raise(ParseError "Invalid")
        end
    |_ -> raise(ParseError "Invalid")

let parser_only_if c t =
  match c with
    Cond(o) ->
      begin
      match t with
        Tokens_generic(o') ->
              let binop_t = wrapper_parser_expr o' in
              Generic_only_if(o,binop_t)
        |Expr_generic(o') ->
              Generic_only_if(o,t)
        |_ -> raise(ParseError "Invalid")
      end
    |_ -> raise (ParseError "Invalid")

let parser_while c s =
  match c with
    Cond(o) ->
      begin
      match s with
        Tokens_generic(o') ->
              let binop = wrapper_parser_expr o' in
              Generic_while(o,binop)
        |Expr_generic(o') ->
              Generic_while(o,s)
        |_ -> raise(ParseError "Invalid")
      end
    |_ -> raise (ParseError "Invalid")

let rec parser lst =
  match lst with
     [] -> []
    |h::t ->
      begin
      match h with
          If_generic(_) ->
            begin
            match t with
                []->raise(ParseError "Incomplete")
              | h'::t' ->
                  begin
                  match h' with
                    Cond(_) ->
                      begin
                      match t' with
                          [] -> raise(ParseError "Incomplete")
                        | h''::t'' ->
                            begin
                            match h'' with
                                Tokens_generic(_) | Expr_generic(_) ->
                                  begin
                                  match t'' with
                                     [] -> parser_only_if h' h''::parser t''
                                    |h'''::t'''->
                                      begin
                                      match h''' with
                                          Expr_generic(_) | Tokens_generic(_) ->
                                            parser_if h' h'' h''' :: parser t'''
                                        | While_generic(_) | Echo_generic(_) | If_generic(_) ->
                                            parser_only_if h' h''::parser t''
                                        |_ -> raise(ParseError "invalid")
                                      end
                                  end
                              | _ ->raise(ParseError "Invalid")
                            end
                      end
                    |_ -> raise(ParseError "Invalid")
                  end
            end
        | While_generic(_) ->
            begin
            match t with
                [] -> raise(ParseError "Incomplete")
              | h'::t' ->
                  begin
                  match h' with
                      Cond(_) ->
                        begin
                        match t' with
                            [] -> raise(ParseError "Incomplete")
                          | h''::t''->
                              begin
                              match h'' with
                                  Expr_generic(_) | Tokens_generic(_) ->
                                    parser_while h' h''::parser t''
                                | _ ->raise(ParseError "Invalid")
                              end
                        end
                    | _ -> raise(ParseError "Invalid")
                  end
            end
        | Echo_generic(_) ->
            begin
            match t with
               [] -> raise(ParseError "Odd length")
              |h'::t' ->
                begin
                match h' with
                Expr_generic(_) | Tokens_generic(_) ->
                 (parser_echo h h')::parser t'
                |_ -> raise(ParseError "Invalid")
                end
            end
        | Tokens_generic(o) ->
            let binop = wrapper_parser_expr o in
            begin
            match binop with
                Expr_generic(o') -> Generic_expr(o')::parser t
              | _ -> raise(ParseError "Invalid")
            end
        | Others(_) -> parser t
        | _ -> raise(ParseError "Invalid")
      end

let t1() =
  let e = [Echo_generic(Echo_lex);Tokens_generic([Num_tok(2);Op(Mult);Num_tok(3);Op(Add);Num_tok(4)]);Echo_generic(Echo_lex);Tokens_generic([Num_tok(2);Op(Mult);Num_tok(3)])] in
  parser e;;

let t2() =
let e = [Others(LParen);Tokens_generic([Num_tok(2);Op(Div);Num_tok(3);Op(Sub);Num_tok(4)]);Others(RParen)] in
  parser e;;

let t3() =
let e = [If_generic(If_lex);Cond(Var("x"),Eq,Num(3));Tokens_generic([Var_tok("x");Op(Div);Num_tok(3);Op(Sub);Var_tok("x1")]);Tokens_generic([Var_tok("3");Op(Div);Num_tok(3)]);
If_generic(If_lex);Cond(Var("y"),Lt,Num(3));Tokens_generic([Var_tok("y");Op(Div);Num_tok(1)]);Tokens_generic([Var_tok("3");Op(Div);Num_tok(3)])] in
  parser e;;

let t4() =
let e = [If_generic(If_lex);Cond(Var("x"),Eq,Num(3));Tokens_generic([Var_tok("x");Op(Div);Num_tok(3);Op(Sub);Var_tok("x1")]);
If_generic(If_lex);Cond(Var("y"),Lt,Num(3));Tokens_generic([Var_tok("y");Op(Div);Num_tok(1)]);Tokens_generic([Var_tok("3");Op(Div);Num_tok(3)])] in
  parser e;;

let t5() =
let e = [While_generic(While_lex);Cond(Var("x"),Lt,Num(3));Tokens_generic([Var_tok("x");Op(Assign);Num_tok(3);Op(Add);Var_tok("y")]);
         While_generic(While_lex);Cond(Var("y"),Gt,Num(5));Tokens_generic([Var_tok("y");Op(Assign);Num_tok(3)])] in
  parser e;;

let t6() =
let e = [While_generic(While_lex);Cond(Var("y"),Gt,Num(5));
         Tokens_generic([Var_tok("y");Op(Assign);Num_tok(3)]);
         If_generic(If_lex);Cond(Var("x"),Eq,Num(3));
	       Tokens_generic([Var_tok("x");Op(Div);Num_tok(3);Op(Sub);Var_tok("x1")]);
	       If_generic(If_lex);Cond(Var("x"),Eq,Num(3));
	       Tokens_generic([Var_tok("x");Op(Div);Num_tok(3);Op(Sub);Var_tok("x1")]);
	       Tokens_generic([Var_tok("3");Op(Div);Num_tok(3)]);
         Echo_generic(Echo_lex);
	       Tokens_generic([Num_tok(2);Op(Mult);Num_tok(3);Op(Add);Num_tok(4)]);
         Tokens_generic([Num_tok(2);Op(Mult);Num_tok(3);Op(Add);Num_tok(4)])] in
    parser e;;

(*echo x+3*)
let t7() =
let e = [Echo_generic(Echo_lex);Tokens_generic([Var_tok("x");Op(Add);Num_tok(3)])] in
parser e;;

(*5/8+x*10*)
let t8() =
let e = [Tokens_generic([Num_tok(5);Op(Div);Num_tok(8);Op(Add);Var_tok("x");Op(Mult);Num_tok(10)])] in
parser e;;

(*
  while(y>5):
    y = y+1
  if(x=4):
    x+1
  else:
    x=4
*)
let t9()=
let e = [
  While_generic(While_lex);Cond(Var("y"),Gt,Num(5));Tokens_generic([Var_tok("y");Op(Add);Num_tok(1)]);
  If_generic(If_lex);Cond(Var("x"),Eq,Num(4));Tokens_generic([Var_tok("x");Op(Add);Num_tok(1)]);Tokens_generic([Var_tok("x");Op(Assign);Num_tok(4)])
] in
parser e;;
