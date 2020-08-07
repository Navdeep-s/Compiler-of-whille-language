
Control.Print.printDepth:= 100;

exception lex_error

signature Navdeep = 
sig
	datatype token = ID of string| NUM of int| PROGRAM | VAR | INT | BOOL | READ | WRITE | IF | THEN | ELSE | ENDIF | WHILE | DO | ENDWH | DOUBLE_COLON | COLON | COMMA | ASSIGN | SEMICOLON | C_LPAREN | C_RPAREN | NEG | LPAREN | RPAREN | OR | AND | TRUE | FALSE | NEGATION | LT | LET | EQUAL | GT | GET | LGT | PLUS | MINUS | MUL | DIV | MOD | EOF


	

datatype program = block of (string*declarations*Commands)
	and declarations = none| decs of (declaration*declarations) 
	and types = intType|boolType
	and declaration = nodes of (types*variables)
	and variables = no_variable| vari of (string*variables)
	and Commands = some of (Command*Commands)|noCommand
	and Command = read of string| write of intExpression| assingment of (string*Expression)
			|ast_if of (boolExpression*Commands*Commands)
			|ast_while of (boolExpression*Commands)
	and Expression = itexp of intExpression|blexp of boolExpression
	and boolExpression = ast_or of (boolExpression*boolExpression)
			| ast_and of (boolExpression*boolExpression)
			| ast_true 
			| ast_false
			| ast_bool_variable of string
			| ast_not of boolExpression
			| ast_LT of (intExpression*intExpression)
			| ast_GT of (intExpression*intExpression)
			| ast_LET of (intExpression*intExpression)
			| ast_GET of (intExpression*intExpression)
			| ast_LGT of (intExpression*intExpression)
			| ast_EQUAL of (intExpression*intExpression)
	and intExpression = ast_int_variable of string
			|ast_num of int
			|ast_plus of (intExpression*intExpression)
			|ast_minus of (intExpression*intExpression)
			|ast_mul of (intExpression*intExpression)
			|ast_mod of (intExpression*intExpression)
			|ast_div of (intExpression*intExpression)
			|ast_negative of intExpression

val give_tokenf : string -> token list


val parser_it : string -> program * token list



val give_ir :string -> string

val evaluate : string -> unit

	
end
	




structure compiler:Navdeep =
struct

datatype token = ID of string| NUM of int| PROGRAM | VAR | INT | BOOL | READ | WRITE | IF | THEN | ELSE | ENDIF | WHILE | DO | ENDWH | DOUBLE_COLON | COLON | COMMA | ASSIGN | SEMICOLON | C_LPAREN | C_RPAREN | NEG | LPAREN | RPAREN | OR | AND | TRUE | FALSE | NEGATION | LT | LET | EQUAL | GT | GET | LGT | PLUS | MINUS | MUL | DIV | MOD | EOF


open TextIO

local 

(*this funciton will give us the word starting with letter in the input stream upto the next space*)
fun get_word letter stream=	
	case lookahead stream of 
		NONE => letter
		|SOME x => if Char.isAlpha x orelse Char.isDigit x then 
			let
			val fre= input1 stream			
			in
			(get_word (letter ^ (str x)) stream )
			end
			else letter
			


(*this funciton will gives use number starting from num*)
fun getnum num stream=
			case lookahead stream of
			    NONE   => num
			  | SOME d =>
			      if Char.isDigit d then
				(input1 stream; getnum (10*num + ord d - ord #"0") stream)
			      else
				num


fun next_token stream = 
	case input1 stream of 
	NONE => EOF
	|SOME x => 
		if Char.isSpace x then 
			next_token stream
		else if Char.isAlpha x then 
			let 
				val wordf = (get_word (str x) stream )
			in
			case wordf of
			"tt"=>TRUE
			|"ff"=>FALSE
			|"program"=>PROGRAM
			|"var" =>VAR
			|"int" =>INT
			|"bool" =>BOOL
			|"read" =>READ
			|"write"=>WRITE
			|"if" =>IF
			|"then" =>THEN
			|"else"=>ELSE
			|"endif"=>ENDIF
			|"while" =>WHILE
			|"do" =>DO
			|"endwh"=>ENDWH
			|_ => ID wordf
			end
		else if Char.isDigit x then 	 
			      NUM (getnum (ord x - ord #"0") stream)

		else if Char.isGraph x then 
			
			case x of 
			#":"  =>( case lookahead stream of 
				SOME #":" => (input1 stream;DOUBLE_COLON)
				|SOME #"=" =>  (input1 stream;ASSIGN)				
				|_ => COLON)
			| #","  =>  COMMA
			| #";"  =>  SEMICOLON
			| #"{"  =>  C_LPAREN
			| #"}"  =>  C_RPAREN
			| #"~"  =>  NEG
			| #"("  =>  LPAREN
			| #")"  =>  RPAREN
			| #"!"  =>  NEGATION
			| #"<"  => ( case lookahead stream of 
				SOME #"=" =>  (input1 stream;LET)
				|SOME #">" => (input1 stream; LGT)				
				|_ => LT)
			| #"="  =>  EQUAL
			| #">"  => ( case lookahead stream of 
				SOME #"=" =>(input1 stream;GET)
				 |_ =>  GT)
			| #"+"  =>  PLUS
			| #"-"  =>  MINUS
			| #"*"  =>  MUL
			| #"/"  =>  DIV
			| #"%"  =>  MOD
			| #"|"  =>  ( case lookahead stream of 
				 SOME #"|" => (input1 stream;OR) 
				|_ => (print ("\nillegal character " ^ str x ^ "\n");raise lex_error))
			| #"&"  =>  ( case lookahead stream of 
				SOME #"&" =>(input1 stream;AND) 
				|_ => (print ("\nillegal character " ^ str x ^ "\n");raise lex_error))
			
			|_    =>  (print ("\nillegal character " ^ str x ^ "\n");raise lex_error)




		
		else  (print ("\nillegal character " ^ str x ^ "\n");raise lex_error)
			 

fun all_tokens r stream = 
	case next_token stream of 
		EOF => rev (EOF::r)
		|k  => all_tokens (k::r) stream
in		
fun give_tokens code = all_tokens [] (openString code)


fun give_tokenf file = all_tokens [] (openIn file)

	

end




(*.......................................................................................................starting of parser.......................................................*)



(*val k = give_tokenf("test.txt")*)

exception error

exception multiple_declaration


datatype program = block of (string*declarations*Commands)
	and declarations = none| decs of (declaration*declarations) 
	and types = intType|boolType
	and declaration = nodes of (types*variables)
	and variables = no_variable| vari of (string*variables)
	and Commands = some of (Command*Commands)|noCommand
	and Command = read of string| write of intExpression| assingment of (string*Expression)
			|ast_if of (boolExpression*Commands*Commands)
			|ast_while of (boolExpression*Commands)
	and Expression = itexp of intExpression|blexp of boolExpression
	and boolExpression = ast_or of (boolExpression*boolExpression)
			| ast_and of (boolExpression*boolExpression)
			| ast_true 
			| ast_false
			| ast_bool_variable of string
			| ast_not of boolExpression
			| ast_LT of (intExpression*intExpression)
			| ast_GT of (intExpression*intExpression)
			| ast_LET of (intExpression*intExpression)
			| ast_GET of (intExpression*intExpression)
			| ast_LGT of (intExpression*intExpression)
			| ast_EQUAL of (intExpression*intExpression)
	and intExpression = ast_int_variable of string
			|ast_num of int
			|ast_plus of (intExpression*intExpression)
			|ast_minus of (intExpression*intExpression)
			|ast_mul of (intExpression*intExpression)
			|ast_mod of (intExpression*intExpression)
			|ast_div of (intExpression*intExpression)
			|ast_negative of intExpression




val ints = ref nil : string list ref
val bools = ref nil : string list ref
val assigned_values = ref [] : Command list ref


fun not_in x [] = true
	|not_in x (h::t) = if x=h then false	
					else (not_in x t) 

fun not_in2 x [] = true
	|not_in2 x ((assingment(h,_))::t) = if x=h then false	
					else (not_in2 x t) 

fun are_assigned [] lis = true
	|are_assigned (h::t) lis = if not(not_in2 h lis) then (are_assigned t lis)
								else (print("variable "^h^ "'s value is not assigned ");raise error)






fun get_variable_from_intExpression lis (ast_int_variable(x)) = (x::lis)
	|get_variable_from_intExpression lis (ast_plus(a,b)) = (get_variable_from_intExpression [] a)@(get_variable_from_intExpression [] b)@lis
	|get_variable_from_intExpression lis (ast_minus(a,b)) = (get_variable_from_intExpression [] a)@(get_variable_from_intExpression [] b)@lis
	|get_variable_from_intExpression lis (ast_mul(a,b)) = (get_variable_from_intExpression [] a)@(get_variable_from_intExpression [] b)@lis
	|get_variable_from_intExpression lis (ast_mod(a,b)) = (get_variable_from_intExpression [] a)@(get_variable_from_intExpression [] b)@lis
	|get_variable_from_intExpression lis (ast_div(a,b)) = (get_variable_from_intExpression [] a)@(get_variable_from_intExpression [] b)@lis
	|get_variable_from_intExpression lis (ast_negative(a)) = (get_variable_from_intExpression [] a)@lis
	|get_variable_from_intExpression lis _  = lis

fun get_variable_from_boolExpression lis (ast_bool_variable(x)) = (x::lis)
	|get_variable_from_boolExpression lis (ast_or(a,b)) = (get_variable_from_boolExpression [] a)@(get_variable_from_boolExpression [] b)@lis
	|get_variable_from_boolExpression lis (ast_and(a,b)) = (get_variable_from_boolExpression [] a)@(get_variable_from_boolExpression [] b)@lis
	|get_variable_from_boolExpression lis (ast_LT(a,b)) = (get_variable_from_intExpression [] a)@(get_variable_from_intExpression [] b)@lis
	|get_variable_from_boolExpression lis (ast_LET(a,b)) = (get_variable_from_intExpression [] a)@(get_variable_from_intExpression [] b)@lis
	|get_variable_from_boolExpression lis (ast_GT(a,b)) = (get_variable_from_intExpression [] a)@(get_variable_from_intExpression [] b)@lis
	|get_variable_from_boolExpression lis (ast_GET(a,b)) = (get_variable_from_intExpression [] a)@(get_variable_from_intExpression [] b)@lis
	|get_variable_from_boolExpression lis (ast_LGT(a,b)) = (get_variable_from_intExpression [] a)@(get_variable_from_intExpression [] b)@lis
	|get_variable_from_boolExpression lis (ast_EQUAL(a,b)) = (get_variable_from_intExpression [] a)@(get_variable_from_intExpression [] b)@lis
	|get_variable_from_boolExpression lis (ast_not(a)) = (get_variable_from_boolExpression [] a)@lis
	|get_variable_from_boolExpression lis _  = lis
	



local 



(*geting variables from variables data type*)
fun get_variables lis (no_variable) = lis
	|get_variables lis (vari(x,y)) = if  (not_in x lis) then get_variables (x::lis) y 	
					else (print("multiple declarations of "^x);raise error)



fun is_int (nodes(intType,_)) = true
	|is_int (nodes(boolType,_)) = false
(*	|is_int _ = (print("not a declaration");raise error) *)

fun extend_lis [] (b,c) = b
	|extend_lis (h::t) (b,c) = if (not_in h b) andalso (not_in h c) then (extend_lis t ((h::b),c))
							else (print("mutiple declartion of "^ h);raise error)

(*add values of int and bool from a declaration to previous_int and previous_bool*)
fun collect_variables previous_int previous_bool (nodes(intType,k)) = 	let val varia = (get_variables [] k) 
																		in ((extend_lis varia (previous_int,previous_bool)),previous_bool) end
	|collect_variables previous_int previous_bool (nodes(boolType,k)) = let val varia = (get_variables []  k) 
																		in (previous_int,(extend_lis varia (previous_bool,previous_int))) end

(*will get values of in and bool from the declarations and add them to pi and pb*)
fun get_values pi pb none = (pi,pb)
	|get_values pi pb (decs(a,b)) = let val (ni,nb) = (collect_variables pi pb a)
									in (get_values ni nb b) 
									end
(*	|get_values pi pb r = (print("not declarations");raise error)*)
in

fun give_me_int_bool tree = get_values [] [] tree
end




fun match_Program lis = 
		let val rest = if hd lis = PROGRAM then tl lis
				else (print("expecting program at the start of the program");raise error)

		val (id,second_rest) = case hd rest of 
					ID(x) => (x,tl rest)
					|_ => (print("expecting identifier after the keyword program");raise error)
		val final_rest = if hd second_rest = DOUBLE_COLON then tl second_rest
				else (print("expecting :: after program identifier");raise error)
		val (ast_dec,final2_rest) = match_declarations none final_rest
		val (i,b) = give_me_int_bool ast_dec 

		val (ast_commands,final3_rest) = (ints:=i; bools:=b;match_commandSeq final2_rest)
				
		in
		((block(id,ast_dec,ast_commands)),final3_rest)
	
		end


(* matches a single declaration given a declaraction exists should be called with no_vaiable 
also it is explected that var token is already taken out by the calling funcctoin*)
and  match_declaration varitree lis = let 	
				val (tree_id,rest) = case (hd lis) of 
					ID(x) => (match_variable varitree lis)
					|_ => (print("expecting a variable for declaration");raise error)
			in
			case hd rest of 
			COMMA => (match_declaration tree_id (tl rest))
			|COLON => let 
					val rest2 = (tl rest)
					in
					case hd rest2 of	
						INT => if hd (tl rest2) = SEMICOLON then (nodes(intType,tree_id),(tl (tl rest2)))
							else (print("expecting : after declaration of int");raise error)
						|BOOL => if hd (tl rest2) = SEMICOLON then (nodes(boolType,tree_id),(tl (tl rest2)))
							else (print("expecting : after declaration of bool");raise error)
						|_ => (print("expecting int or bool for type ");raise error)
					
					end
			|_ =>(print("expecting , or : before declaring the type of variables");raise error)
			
			end

(* return ast of variable combining varitree with it *)
and match_variable varitree lis = 
			case hd lis of 
				ID(x)=> ((vari(x,varitree)),(tl lis))
				|_ => ((no_variable),(tl lis))




and match_declarations dec_tree lis = let 
				
				val (tree_dec , rest) = case (hd lis) of 
						VAR =>let 
							val (i,j)= (match_declaration no_variable (tl lis))
							in
							(decs(i,dec_tree),j)
							end
						|_=>(dec_tree,lis)
				in

				case (hd rest) of 
					VAR => (match_declarations tree_dec rest) (*for adding addition decs i.e more than one*)
					|_ => ( tree_dec,rest)
								

				end

and match_command lis =
		case hd lis of
			READ => let val rest = tl lis
				in
				case hd rest of 
					ID(x) =>  ((assigned_values:=assingment(x,blexp(ast_true))::(!assigned_values));(read(x),tl rest))
							
					|_ => ( print("expecting variable");raise error)
				end
			|WRITE => let val (int_exp,rest) = match_intExpression(tl lis)
				in
				(write(int_exp),rest)
				end

(*to check wheter each variable in bool expression is declared or not*)
			|IF => let val (bool_exp, rest) = match_boolExpression (tl lis)
						 val int_var = (get_variable_from_boolExpression [] bool_exp)
							val k =if (are_assigned int_var (!assigned_values)) then true
									else (raise error)

				in
				if hd rest = THEN then let 
				val (commands1,rest1) = match_commandSeq (tl rest)
				in 
				if hd rest1 = ELSE then let
				val (commands2, rest2) = match_commandSeq(tl rest1)
				in 
				if hd rest2 = ENDIF then (ast_if(bool_exp,commands1,commands2),(tl rest2))
				else (print("expecting endif");raise error)
				end
				else (print("expecting else");raise error)
				end
				else (print("expecting then");raise error)
				end
			|WHILE => let val (bool_exp, rest) = match_boolExpression (tl lis)
						 val int_var = (get_variable_from_boolExpression [] bool_exp)
							val k =if (are_assigned int_var (!assigned_values)) then true
									else (raise error)
				in
				if hd rest = DO then let 
				val (commands1,rest1) = match_commandSeq (tl rest)
				in 
				if hd rest1 = ENDWH then (ast_while(bool_exp,commands1),(tl rest1))
				else (print("expecting endwh");raise error)
				end
				else (print("expecting do");raise error)
				end

			|ID(x) => let val rest = tl lis
				in
				if hd rest = ASSIGN then let val(a,b) = match_expression (x,(tl rest))
				in
				case a of
					itexp(expi) => let val int_var = (get_variable_from_intExpression [] expi)
									in if (are_assigned int_var (!assigned_values))	then ((assigned_values:=assingment(x,a)::(!assigned_values));(assingment(x,a),b))
									else (raise error) end
					|blexp(expi) => let val bool_var = (get_variable_from_boolExpression [] expi)
									in if (are_assigned bool_var (!assigned_values))	then ((assigned_values:=assingment(x,a)::(!assigned_values));(assingment(x,a),b))
									else (raise error) end

				end
				else (print("expecting assignment operator");raise error)
				end
				
			|_ => (read("yo"), lis)

and match_commands command lis =

			let val (com, rest) = 
				case hd lis of
				ID(x) => 
				let val (a,b) = match_command (lis)
					in
					if hd b = SEMICOLON then 
					(some(a,command),(tl b))
					else (print("semicolon required");raise error)
					end
				|_=>
				(if (hd lis) = READ orelse (hd lis) = WRITE orelse (hd lis) = IF orelse
				(hd lis) = WHILE then
					let val (a,b) = match_command lis
					in
					if hd b = SEMICOLON then 
					(some(a,command),(tl b))
					else (print("semicolon required");raise error)
					end
				else (command, lis))

			in
			case hd rest of
			ID(x) => (match_commands com rest)
			|_=>
			if (hd rest) = READ orelse (hd rest) = WRITE orelse (hd rest) = IF orelse
				(hd rest) = WHILE then (match_commands com rest)
			else (com, rest)

			end

and match_commandSeq lis = if hd lis = C_LPAREN then 
					let val (a,b) = match_commands noCommand (tl lis)
					in
					if hd b = C_RPAREN then (a ,(tl b))
					else (print("expecting } at the end of command seq");raise error)
					end
			else (print("expecting { at start of command seq");raise error)
				
					


and match_intFactor lis =
		case hd lis of 
		NUM(x) => (ast_num(x),(tl lis))
		|ID(x) =>(if not(not_in x (!ints)) then (ast_int_variable(x), (tl lis))
					else (print(x^" is not a int type variable");raise error)
					)
		|LPAREN => let val (r,j) = match_intExpression (tl lis)
				in
				if (hd j) = RPAREN then (r,(tl j))
				else (print("expecting )");raise error)
				end
		|NEG => let val (r,j) = match_intFactor (tl lis)
				in
				(ast_negative(r), j)
				end
		|PLUS => let val (r,j) = match_intFactor (tl lis)
				in
				(r,j)
				end
		|_=> (print("expecting integer expression");raise error )
and match_intTerm  lis = 
		let val (_,rest) = match_intFactor lis
		
		fun make_rest ast lis_som = 
				if (hd lis_som = MUL) orelse (hd lis_som = MOD) orelse (hd lis_som = DIV)  then
				let val (right_tree,rl) = match_intFactor (tl lis_som)
						in
					case hd rl of 
						MUL => make_rest (ast_mul(ast,right_tree)) rl
						|DIV => make_rest (ast_div(ast,right_tree)) rl
						|MOD => make_rest (ast_mod(ast,right_tree)) rl
						|_ => case hd lis_som of
							MUL => (ast_mul(ast,right_tree),rl)
							|DIV => (ast_div(ast,right_tree),rl)
							|MOD => (ast_mod(ast,right_tree),rl)
					end
				else (ast,lis_som)						

	  	
				in
		case hd rest of
(*first match a MulOP b and then reamain can be mactched by funtion make_mul \5+6 kind of things*)
			MUL =>
				let val (a,b) = match_intFactor (tl rest)
				val (c,d) = match_intFactor (lis) in 
				make_rest (ast_mul(c,a)) b end
			|DIV => 
				let val (a,b) = match_intFactor (tl rest)
				val (c,d) = match_intFactor (lis) in
				make_rest (ast_div(c,a)) b end
			|MOD => let val (a,b) = match_intFactor (tl rest)
				val (c,d) = match_intFactor (lis) in
				make_rest (ast_mod(c,a)) b end
			|_ => match_intFactor lis
			end					
			


and match_intExpression  lis = 
		let val (_,rest) = match_intTerm lis
		
		fun make_rest ast lis_som = 
				if (hd lis_som = PLUS) orelse (hd lis_som = MINUS)   then
				let val (right_tree,rl) = match_intTerm (tl lis_som)
						in
					case hd rl of 
						PLUS => make_rest (ast_plus(ast,right_tree)) rl
						|MINUS => make_rest (ast_minus(ast,right_tree)) rl
						|_ => case hd lis_som of
							PLUS => (ast_plus(ast,right_tree),rl)
							|MINUS => (ast_minus(ast,right_tree),rl)
					end
				else (ast,lis_som)						

	  	
				in
		case hd rest of
(*first match a MulOP b and then reamain can be mactched by funtion make_mul \5+6 kind of things*)
			PLUS =>
				let val (a,b) = match_intTerm (tl rest)
				val (c,d) = match_intTerm (lis) in 
				make_rest (ast_plus(c,a)) b end
			|MINUS => 
				let val (a,b) = match_intTerm (tl rest)
				val (c,d) = match_intTerm (lis) in
				make_rest (ast_minus(c,a)) b end
			
			|_ => match_intTerm lis
			end					

and match_boolFactor lis =



		case hd lis of
		TRUE => (ast_true, (tl lis))
		|FALSE => (ast_false, (tl lis))
		|LPAREN => let 
				fun brack temp = case (hd temp) of
							RPAREN => (tl temp)
							|_ => (brack (tl temp))

				val u = (brack lis)
		val (r,j)=(
				if (hd u) = PLUS orelse (hd u) = MINUS orelse (hd u) = MUL orelse (hd u) = DIV  (*for the case like (a+b)-4<8*)
				orelse (hd u) = MOD orelse (hd u) = LT orelse (hd u) = LET orelse (hd u) = EQUAL 
				orelse (hd u) = GT orelse (hd u) = GET orelse (hd u) = LGT orelse (hd u) = LPAREN 
				orelse (hd u) = NEGATION then
				
				let
							val (cmp1,l1) = (match_intExpression lis)
							val (cmp2, l2) = (match_intExpression (tl l1))
							in 
							case hd l1 of
							LT => (ast_LT(cmp1,cmp2),l2)
							|LET =>(ast_LET(cmp1,cmp2),l2)
							|EQUAL =>(ast_EQUAL(cmp1,cmp2),l2)
							|GT =>(ast_GT(cmp1,cmp2),l2)
							|GET =>(ast_GET(cmp1,cmp2),l2)
							|LGT =>(ast_LGT(cmp1,cmp2),l2)				
							|_ => (print("expecting a relational operator ");raise error)

							end

				
				else
					let
					val (r,j) = match_boolExpression (tl lis)
					in
					if (hd j) = RPAREN then (r,(tl j))
					else (print("expecting )");raise error)
					end)
				in
				(r,j)

				end
		|NEGATION => let val (i,j) = match_boolFactor (tl lis)
				in 
				(ast_not(i),j)
				end
		|ID(x) => let
			 val rest = (tl lis)
			in 
			if (hd rest) = PLUS orelse (hd rest) = MINUS orelse (hd rest) = MUL orelse (hd rest) = DIV
				orelse (hd rest) = MOD orelse (hd rest) = LT orelse (hd rest) = LET orelse (hd rest) = EQUAL 
				orelse (hd rest) = GT orelse (hd rest) = GET orelse (hd rest) = LGT orelse (hd rest) = LPAREN 
				orelse (hd rest) = NEGATION then 
							let
							val (cmp1,l1) = (match_intExpression lis)
							val (cmp2, l2) = (match_intExpression (tl l1))
							in 
							case hd l1 of
							LT => (ast_LT(cmp1,cmp2),l2)
							|LET =>(ast_LET(cmp1,cmp2),l2)
							|EQUAL =>(ast_EQUAL(cmp1,cmp2),l2)
							|GT =>(ast_GT(cmp1,cmp2),l2)
							|GET =>(ast_GET(cmp1,cmp2),l2)
							|LGT =>(ast_LGT(cmp1,cmp2),l2)				
							|_ => (print("expecting a relational operator ");raise error)

							end

				

			else  (if not(not_in x (!bools)) then (ast_bool_variable(x),(tl lis))
					else (print(x^" is not a bool type variable");raise error)
					)

			


			end
		|NUM(x) =>let
							val (cmp1,l1) = (match_intExpression lis)
							val (cmp2, l2) = (match_intExpression (tl l1))
							in 
							case hd l1 of
							LT => (ast_LT(cmp1,cmp2),l2)
							|LET =>(ast_LET(cmp1,cmp2),l2)
							|EQUAL =>(ast_EQUAL(cmp1,cmp2),l2)
							|GT =>(ast_GT(cmp1,cmp2),l2)
							|GET =>(ast_GET(cmp1,cmp2),l2)
							|LGT =>(ast_LGT(cmp1,cmp2),l2)				
							|_ => (print("expecting a relational operator ");raise error)

							end

		|_ => (print("expecting a bolean operator ");raise error)

				
and match_boolTerm  lis = 
		let val (_,rest) = match_boolFactor(lis)
		
		fun make_rest ast lis_som = 
				if (hd lis_som = AND) then
				let val (right_tree,rl) = match_boolFactor (tl lis_som)
						in
					case hd rl of 
						AND => make_rest (ast_and(ast,right_tree)) rl
						|_ => case hd lis_som of
							AND => (ast_and(ast,right_tree),rl)
					end
				else (ast,lis_som)						

	  	
				in
		case hd rest of
			AND =>
				let val (a,b) = match_boolFactor (tl rest)
				val (c,d) = match_boolFactor (lis) in 
				make_rest (ast_and(c,a)) b end
			|_ => match_boolFactor lis
			end					
			


and match_boolExpression  lis = 
		let val (_,rest) = match_boolTerm(lis)
		
		fun make_rest ast lis_som = 
				if (hd lis_som = OR) then
				let val (right_tree,rl) = match_boolTerm (tl lis_som)
						in
					case hd rl of 
						OR => make_rest (ast_or(ast,right_tree)) rl
						|_ => case hd lis_som of
							OR => (ast_or(ast,right_tree),rl)
					end
				else (ast,lis_som)						

	  	
				in
		case hd rest of
			OR =>
				let val (a,b) = match_boolTerm (tl rest)
				val (c,d) = match_boolTerm (lis) in 
				make_rest (ast_or(c,a)) b end
			|_ => match_boolTerm lis
			end					
			
and match_expression (x,lis) = 
				if not(not_in x (!ints)) then
							 let val (a,b)= match_intExpression lis
								in	(itexp(a),b) end
								else if not(not_in x (!bools)) then let val (a,b)=match_boolExpression lis
									in (blexp(a),b) end
								else (print("variable "^x^" not declared");raise error)
	





fun parser_it file = (match_Program( give_tokenf(file)))


(*.......................................................................................................starting of ir_generator.......................................................*)

exception error_ir

(*val k = match_Program(k)*)

val current_program = ref []: string list ref

val values_table = ref []: (string * string) list ref

val counter = ref 1
val counter1 = ref 1

fun emit_value counter = ((counter:=(!counter)+1);"@t"^(Int.toString(!counter)))

fun emit_label counter = ((counter:=(!counter)+1);"@L"^(Int.toString(!counter)))

fun in_table value [] = false
	|in_table value ((a,b)::c) = if value = a then true 
								else (in_table value c)

fun get_val_from_table value [] = "novalue"
	|get_val_from_table value ((a,b)::c) = if value = a then b 
								else (get_val_from_table value c)
(*
fun emit_int_assign(assingment(a,itexp(   ast_num(y)  ))) = (values_table:=((a,Int.toString(y))::(!values_table));print(a^" := "^Int.toString(y))^"\n") (* assgining value y to a and putting in the symbol tabel*)

	emit_int_assign(assingment(a,itexp(   ast_int_variable(y)  ))) = if (in_table y (!values_table)) then (values_table:=((a,y)::(!values_table));print(a^" := "^y^"\n"))
																	else (print("value of "^y^" was not declared earlier");raise error_ir)
																									
	emit_int_assign(assingment(a,ast_plus(   e1,e2  ))) = if (in_table y (!values_table)) then (values_table:=((a,y)::(!values_table));print(a^" := "^y))
	*)



fun emit_variables (ast_int_variable(x)) = if (in_table x (!values_table)) then x
																	else (print("value of "^x^" was not declared earlier");raise error_ir)

	|emit_variables (ast_num(x)) = Int.toString(x)



fun emit_intExpression  x (ast_plus(a,b)) = (emit_plus x (ast_plus(a,b)))
	|emit_intExpression  x (ast_mul(a,b)) = (emit_mul x (ast_mul(a,b)))
	|emit_intExpression  x (ast_div(a,b)) = (emit_div x (ast_div(a,b)))
	|emit_intExpression  x (ast_mod(a,b)) = (emit_mod x (ast_mod(a,b)))
	|emit_intExpression  x (ast_minus(a,b)) = (emit_minus x (ast_minus(a,b)))
	|emit_intExpression  x (ast_negative(y)) = emit_negative x (ast_negative(y))
	|emit_intExpression  x (ast_int_variable(r)) =(x^" := "^r^"\n")
	|emit_intExpression  x (ast_num(r))= (x^" := "^Int.toString(r)^"\n")

															

and emit_negative x (ast_negative(a)) =	let val temp1  = (emit_value(counter))
										in
										(emit_intExpression temp1 a)^(x^" := ~ "^temp1^"\n")
										end

and emit_plus x (ast_plus(a,b)) = let val temp1  = (emit_value(counter))
															val temp2 = (emit_value(counter))
										in
										(emit_intExpression temp1 a)^(emit_intExpression temp2 b)^(x^" := "^temp1^" + "^ temp2 ^"\n")
										end




and emit_mul x (ast_mul(a,b)) = let val temp1  = (emit_value(counter))
															val temp2 = (emit_value(counter))
										in
										(emit_intExpression temp1 a)^(emit_intExpression temp2 b)^(x^" := "^temp1^" * "^ temp2 ^"\n")
										end


and emit_div x (ast_div(a,b)) = let val temp1  = (emit_value(counter))
															val temp2 = (emit_value(counter))
										in
										(emit_intExpression temp1 a)^(emit_intExpression temp2 b)^(x^" := "^temp1^" / "^ temp2 ^"\n")
										end



and emit_minus x (ast_minus(a,b)) = let val temp1  = (emit_value(counter))
															val temp2 = (emit_value(counter))
										in
										(emit_intExpression temp1 a)^(emit_intExpression temp2 b)^(x^" := "^temp1^" - "^ temp2 ^"\n")
										end



and emit_mod x (ast_mod(a,b)) = let val temp1  = (emit_value(counter))
															val temp2 = (emit_value(counter))
										in
										(emit_intExpression temp1 a)^(emit_intExpression temp2 b)^(x^" := "^temp1^" % "^ temp2 ^"\n")
										end






and emit_boolExpression  x (ast_or(a,b)) = (emit_or x (ast_or(a,b)))
	|emit_boolExpression  x (ast_and(a,b)) = (emit_and x (ast_and(a,b)))
	|emit_boolExpression  x (ast_LT(a,b)) = (emit_LT x (ast_LT(a,b)))
	|emit_boolExpression  x (ast_GT(a,b)) = (emit_GT x (ast_GT(a,b)))
	|emit_boolExpression  x (ast_LET(a,b)) = (emit_LET x (ast_LET(a,b)))
	|emit_boolExpression  x (ast_GET(a,b)) = (emit_GET x (ast_GET(a,b)))
	|emit_boolExpression  x (ast_LGT(a,b)) = (emit_LGT x (ast_LGT(a,b)))
	|emit_boolExpression  x (ast_EQUAL(a,b)) = (emit_EQUAL x (ast_EQUAL(a,b)))

	|emit_boolExpression  x (ast_not(y)) = emit_not x (ast_not(y))
	|emit_boolExpression  x (ast_bool_variable(r)) = (x^" := "^r^"\n")
	|emit_boolExpression  x (ast_true)= (x^" := tt\n")
	|emit_boolExpression  x (ast_false)= (x^" := ff\n")

															

and emit_not x (ast_not(a)) =	let val temp1  = (emit_value(counter))
										in
										(emit_boolExpression temp1 a)^(x^" := ! "^temp1^"\n")
										end

and emit_or x (ast_or(a,b)) = let val temp1  = (emit_value(counter))
															val temp2 = (emit_value(counter))
										in
										(emit_boolExpression temp1 a)^(emit_boolExpression temp2 b)^(x^" := "^temp1^" || "^ temp2 ^"\n")
										end




and emit_and x (ast_and(a,b)) = let val temp1  = (emit_value(counter))
															val temp2 = (emit_value(counter))
										in
										(emit_boolExpression temp1 a)^(emit_boolExpression temp2 b)^(x^" := "^temp1^" && "^ temp2 ^"\n")
										end


and emit_LT x (ast_LT(a,b)) = let val temp1  = (emit_value(counter))
															val temp2 = (emit_value(counter))
										in
										(emit_intExpression temp1 a)^(emit_intExpression temp2 b)^(x^" := "^temp1^" < "^ temp2 ^"\n")
										end



and emit_LET x (ast_LET(a,b)) = let val temp1  = (emit_value(counter))
															val temp2 = (emit_value(counter))
										in
										(emit_intExpression temp1 a)^(emit_intExpression temp2 b)^(x^" := "^temp1^" <= "^ temp2 ^"\n")
										end



and emit_GT x (ast_GT(a,b)) = let val temp1  = (emit_value(counter))
															val temp2 = (emit_value(counter))
										in
										(emit_intExpression temp1 a)^(emit_intExpression temp2 b)^(x^" := "^temp1^" > "^ temp2 ^"\n")
										end






and emit_GET x (ast_GET(a,b)) = let val temp1  = (emit_value(counter))
															val temp2 = (emit_value(counter))
										in
										(emit_intExpression temp1 a)^(emit_intExpression temp2 b)^(x^" := "^temp1^" >= "^ temp2 ^"\n")
										end



and emit_LGT x (ast_LGT(a,b)) = let val temp1  = (emit_value(counter))
															val temp2 = (emit_value(counter))
										in
										(emit_intExpression temp1 a)^(emit_intExpression temp2 b)^(x^" := "^temp1^" <> "^ temp2 ^"\n")
										end



and emit_EQUAL x (ast_EQUAL(a,b)) = let val temp1  = (emit_value(counter))
															val temp2 = (emit_value(counter))
										in
										(emit_intExpression temp1 a)^(emit_intExpression temp2 b)^(x^" := "^temp1^" = "^ temp2 ^"\n")
										end


and emit_expressions x (itexp(a)) = emit_intExpression x a
	|emit_expressions x (blexp(a)) = emit_boolExpression x a

and emit_assignment (assingment(x,b)) = emit_expressions x b

and emit_if (ast_if(a,b,c)) = let val temp1  = (emit_value(counter))
								val label1 = (emit_label(counter1))
								val label2 = (emit_label(counter1))
								val label3 = (emit_label(counter1))
										in
						(emit_boolExpression temp1 a)^("if "^temp1^" goto "^label1^" \ngoto "^label2^"\n")^(label1^"\n")^(emit_commands b)^"goto "^label3^"\n"^(label2^"\n")^(emit_commands c)^label3^"\n"
						end
						
and emit_while (ast_while(a,b)) = let val temp1  = (emit_value(counter))
								val label1 = (emit_label(counter1))
								val label2 = (emit_label(counter1))
								val label3 = (emit_label(counter1))
				 						in
						(label2^"\n")^(emit_boolExpression temp1 a)^("if "^temp1^" goto "^label1^" \ngoto "^label3^"\n")^(label1^"\n")^(emit_commands b)^("goto "^label2^"\n")^(label3^"\n")
						 end

and emit_command (read(x)) = ("read "^x^"\n")
	|emit_command (write(b)) = let  val temp1  = (emit_value(counter)) 
					in (emit_intExpression temp1 b)^("write "^temp1^"\n") end
	|emit_command (assingment(a,b)) = (emit_assignment (assingment(a,b)))
	|emit_command (ast_if(a,b,c)) =(emit_if (ast_if(a,b,c)) )
	|emit_command (ast_while(a,b)) =(emit_while (ast_while(a,b)))
								
and emit_commands (noCommand) = ""
	|emit_commands (some(a,b)) = ((emit_commands(b))^(emit_command a))

fun emit_program (block (a,b,c) ) = let val k =(emit_commands c) 
					val u =	(print(".......................start of ir code ........................\n");
						current_program:=(a::(!current_program));print(k);
						print("..........................end of ir code ........................\n\n\n"))
					in k end


fun give_ir file = let val (i,j) = match_Program( give_tokenf(file))
			in
			if (hd j) = EOF then  emit_program (i) 
			else (print("Expecting EOF at the end of the progrram");(raise error ))
			end



(*.......................................................................................................starting of interpreter.......................................................*)

(*function and variables used from the other codes inst, bools , not_in*)

exception interpreter_error

fun is_newline k = if k = #"\n" then true else false
fun is_space k = if k = #" " then true else false

fun presetup file = let 
		val k = (give_ir file)
		val u = String.tokens is_newline k
		in 
		u end

fun not_num k = not(Char.isDigit k) 

fun if_number k = let val u= (String.fields not_num k)
		in if (List.length u)=1 then true else false
		end





(*this will get a label in string format and a list of all commands and returns commands list after that variable*)
fun get_label k (h::t) = if (String.isPrefix k h) then t
			else (get_label k t)

	|get_label k _ = (print("no label declared yet");(raise error))


fun is_variable st = (String.isPrefix "@t" st)

fun is_label st = (String.isPrefix "@L" st)

fun extract_tokens str = (String.tokens is_space str)

fun evaluate file =

let

val lis = presetup file


val current_lis = ref lis


val lis_org = ref lis

val values_lis = ref [] : (string*string) list ref

fun is_there_a k [] = false
	|is_there_a k ((a,b)::c) = if k = a then true 
							else (is_there_a k c)


fun 	get_value_from k [] = 	if (Int.fromString(k))=NONE then 
				if k ="tt" orelse k="ff" then k else (print(k^" is not define may be due to branch not taken\n");print(".................exiting Navdeep's interpreter...................\n");(raise interpreter_error))
				else k

	|get_value_from k ((a,b)::c) = case Int.fromString(k) of
						NONE=>	if k = a then (get_value_from b ((a,b)::c))
							else if k="tt" then k
							else if k="ff" then k
							else (get_value_from k c)
						|SOME(x)=> k
	

fun add_value a b lis = (lis:=((a,b)::(!lis)))


fun goto lis  = (current_lis:=lis)

fun goto_next a = (current_lis:= (tl (!current_lis)))

fun getNumber(op1) = (
print ("->  enter int "^ op1 ^ ": ");
let
    val str = valOf (inputLine stdIn)
	val str1 = hd (String.tokens is_newline str)
	val ui = if (String.isPrefix "~" str1) then String.extract (str1,1,NONE) else str1
    val i = (case  (if_number ui) of 
		true => str1
		|false => (print("[!] only integers should be entered\n");getNumber(op1)))
in
    i
end
)





fun getBool(op1) = (
print ("->  enter bool "^ op1 ^ ": ");
let
    val str = valOf (inputLine stdIn)
	val str1 = hd (String.tokens is_newline str)
	val i = if str1 ="tt" orelse str1="ff" then str1 
		else (print("[!] only tt of ff are values for bolean\n");(getBool(op1)))
in
    i
end
)





fun solve_fun str = let val lis = extract_tokens str
			val len = (List.length lis)
				in

				if len=1 then (goto_next "next")

				else if  len=2 then case hd lis of  
					"write" => let val a = hd (tl lis) 
												val b = (get_value_from a (!values_lis) )
											in (print("->  "^b^"\n"));(goto_next "next") end
										
					(*this part is not done yet*)
					|"read" => let val op1 = hd (tl lis) in
						if not(not_in op1 (!ints )) then
						let
						val value = getNumber(op1)
						in (add_value op1 value values_lis);(goto_next "next") end
						else if not(not_in op1 (!bools)) then 
						let
						val value = getBool(op1)
						in (add_value op1 value values_lis);(goto_next "next") end
						else print("this is not going to happen")
						end

					|"goto" => let val a=(get_label (hd (tl lis)) (!lis_org)) 
							in  	(goto a)	end
					|_ => print("can never happen")
								
				else if len= 3 then let val op1 = hd lis 
						 val value = (get_value_from (hd (tl (tl lis))) (!values_lis))
						in
					(add_value op1 value values_lis);goto_next "next" end


				else if len= 4 then		let val op1 = hd lis
						
						in
						case op1 of 
						"if" => let val condi = hd (tl lis)
							 val labe = hd(tl(tl (tl lis)))
							val condi_val = (get_value_from condi (!values_lis))
							in  if condi_val ="tt" then let val a=(get_label labe (!lis_org)) 
											in  	(goto a)	end
								else (goto_next "next")
						end



						|_ => let val symb = (hd(tl (tl lis)))
							val value = (get_value_from (hd(tl(tl(tl lis)))) (!values_lis) )
							in 
							if symb = "~" then let val SOME k = Int.fromString value in let val out = ~(k)
														val outf = Int.toString out	in (add_value op1 outf values_lis);goto_next "next" end end
							else if symb = "!" then let val outf = if value = "tt" then "ff" else "tt" in (add_value op1 outf values_lis);goto_next "next" end 
							else (print("there may be a case "))
							
							end
						end



				else let 
						val addr = hd lis
						val op1 = (get_value_from (hd (tl (tl lis))) (!values_lis) )
						val operator = (hd (tl (tl (tl lis))))
						val op2 = (get_value_from (hd (tl (tl (tl (tl lis))))) (!values_lis) )
						in
						case operator of 
						"*"=> let val SOME x = Int.fromString op1
									val SOME y = Int.fromString op2
									val out = x*y
									val outf = Int.toString out
									in
								(add_value addr outf values_lis);goto_next "next"
								end
						|"+"=> let val SOME x = Int.fromString op1
									val SOME y = Int.fromString op2
									val out = x+y
									val outf = Int.toString out
									in
								(add_value addr outf values_lis);goto_next "next"
								end
						|"/"=> let val SOME x = Int.fromString op1
									val SOME y = Int.fromString op2
									val out = x div y
									val outf = Int.toString out
									in
								(add_value addr outf values_lis);goto_next "next"
								end
						|"-"=> let val SOME x = Int.fromString op1
									val SOME y = Int.fromString op2
									val out = x-y
									val outf = Int.toString out
									in
								(add_value addr outf values_lis);goto_next "next"
								end
						|"<"=> let val SOME x = Int.fromString op1
									val SOME y = Int.fromString op2
									val outf = if x<y then "tt" else "ff"
									in
								(add_value addr outf values_lis);goto_next "next"
								end
						|">"=> let val SOME x = Int.fromString op1
									val SOME y = Int.fromString op2
									val outf = if x>y then "tt" else "ff"
									in
								(add_value addr outf values_lis);goto_next "next"
								end
						|"<="=> let val SOME x = Int.fromString op1
									val SOME y = Int.fromString op2
									val outf = if x<=y then "tt" else "ff"
									in
								(add_value addr outf values_lis);goto_next "next"
								end
						|">="=> let val SOME x = Int.fromString op1
									val SOME y = Int.fromString op2
									val outf = if x>=y then "tt" else "ff"
									in
								(add_value addr outf values_lis);goto_next "next"
								end
						|"="=> let val SOME x = Int.fromString op1
									val SOME y = Int.fromString op2
									val outf = if x=y then "tt" else "ff"
									in
								(add_value addr outf values_lis);goto_next "next"
								end
						

						|"<>"=> let val SOME x = Int.fromString op1
									val SOME y = Int.fromString op2
									val outf = if x<>y then "tt" else "ff"
									in
								(add_value addr outf values_lis);goto_next "next"
								end
						

						|"%"=> let val SOME x = Int.fromString op1
									val SOME y = Int.fromString op2
									val out = x mod y
									val outf = Int.toString out
									in
								(add_value addr outf values_lis);goto_next "next"
								end
						|"||"=> let val outf = if op1 ="ff" andalso op2 ="ff" then "ff" else "tt"
									in
								(add_value addr outf values_lis);goto_next "next"
								end
						|"&&"=> let val outf = if op1 ="tt" andalso op2 ="tt" then "tt" else "ff"
									in
								(add_value addr outf values_lis);goto_next "next"
								end
						|_ => print("can never happen")
						end
				
						
				end
			
fun solve_commands st =  if List.length(!current_lis) = 0 then print("end")
			else (solve_fun (hd (!current_lis)))

val f = solve_commands
val r =""

fun final lis = if List.length(!current_lis) = 0 then print("")
			else ((solve_commands "");final [])



fun interpret () = (print(".................starting Navdeep's interpreter..................\n");
		print((hd (!current_program))^"\n");
	
	(final []);print(".................exiting Navdeep's interpreter...................\n"))

in 

interpret()

end






end
