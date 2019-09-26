%{
#include <stdbool.h>
#include <stdarg.h>
#include <stdio.h>	
#include "cgen.h"
#include "ptuclib.h"

extern int yylex(void);
extern int line_num;
%}

%union
{
	char* crepr;
	int integer;
	int boolean;	
	char character;	
	double real;
}

%token <crepr> IDENT
%token <integer> POSINT 
%token <real> REAL 
%token <crepr> STRING

%token <crepr> KW_AND
%token <crepr> KW_ARR
%token <crepr> KW_BOOL
%token <crepr> KW_CHAR	
%token <crepr> KW_BEGIN
%token <crepr> KW_DIV
%token <crepr> KW_DO
%token <crepr> KW_ELSE
%token <crepr> KW_FOR
%token <crepr> KW_END
%token <crepr> KW_FUNC
%token <crepr> KW_GOTO
%token <crepr> KW_IF
%token <crepr> KW_INT
%token <crepr> KW_VAR
%token <crepr> KW_MOD
%token <crepr> KW_NOT
%token <crepr> KW_OF
%token <crepr> KW_OR
%token <crepr> KW_WHILE
%token <crepr> KW_PROC
%token <crepr> KW_PROGRAM
%token <crepr> KW_REAL
%token <crepr> KW_REP
%token <crepr> KW_TO
%token <crepr> KW_RES
%token <crepr> KW_RET
%token <crepr> KW_THEN
%token <crepr> KW_UNT
%token <crepr> KW_DOWTO
%token <crepr> KW_TYPE

%token <boolean> BOOL_TRUE_CONST
%token <boolean> BOOL_FALSE_CONST

%token <crepr> CONST_STR

%token <crepr> '+'
%token <crepr> '-'
%token <crepr> '*'
%token <crepr> '/'

%token <crepr> '='
%token <crepr> neq
%token <crepr> '<'
%token <crepr> leq
%token <crepr> '>'
%token <crepr> meq

%token <crepr> assign

%token <crepr> ';'
%token <crepr> '('
%token <crepr> ')'
%token <crepr> ','
%token <crepr> '['
%token <crepr> ']'
%token <crepr> ':'
%token <crepr> '.'

%right KW_NOT
%left '*' '/' KW_DIV KW_MOD 
%left '+' '-'
%left '=' neq leq meq '<' '>'
%left KW_AND KW_OR 

%right KW_THEN KW_ELSE

%token <crepr> READ_STR
%token <crepr> READ_INT
%token <crepr> READ_REAL
%token <crepr> WRITE_STR
%token <crepr> WRITE_INT
%token <crepr> WRITE_REAL

%start program

%type <crepr> program_decl main_body statements statement_list arguements arglist 
%type <crepr> statement proc function procedure func_body proc_body
%type <crepr> global variable variables types array bracket expr
%type <crepr> func_head ParamList casting func name namecol defines def
%type <crepr> exp_arg log_not op_func op_term compare log_and log_or expression
%type <crepr> assignment signed return mbody_stat pbody_stat fbody_stat body if_then if_then_else result
%type <crepr> for_loop for while_loop repeat_loop func_proc_call array_call
%%
/*-----------------------------------------------HOW_IT_SIMPLY_WORKS-----------------------------------------------------------*/
program : program_decl global main_body  '.'   		{
								/* We have a successful parse! 
									Check for any errors and generate output. 
								*/
								if(yyerror_count==0) {
								printf("/* program %s */\n", $1);
								puts(c_prologue);			
								printf("%s\n", $2);		
								printf("int main()%s", $3);
								}
							};

program_decl : KW_PROGRAM IDENT ';'  			{ $$ = $2; };

global : %empty						{ $$ = "";}	
       | global variables				{ $$ = template("%s%s", $1, $2); }
       | global function				{ $$ = template("%s%s", $1, $2); }
       | global procedure				{ $$ = template("%s%s", $1, $2); }
       | global defines					{ $$ = template("%s%s", $1, $2); };

main_body : KW_BEGIN mbody_stat KW_END 			{ $$ = template("{\n%sreturn 0;\n}\n", $2); };
/*-----------------------------------------------------------------------------------------------------------------------------*/

/*-----------------------------------------------ARRAY-------------------------------------------------------------------------*/
array_call : IDENT bracket				{ $$ = template("%s%s", $1, $2); };

array : KW_ARR KW_OF					{ $$ = template("*", $2); }
      | KW_ARR bracket KW_OF				{ $$ = $2; };

bracket : '[' expression ']'				{ $$ = template("[%s]", $2); }
	| bracket '[' expression ']'			{ $$ = template("%s[%s]", $1, $3); };
/*-----------------------------------------------------------------------------------------------------------------------------*/

/*-----------------------------------------------ASSIGNMENT--------------------------------------------------------------------*/
assignment : IDENT assign expression			{ $$ = template("%s = %s", $1, $3); };

result : KW_RES assign expression			{ $$ = template("result = %s;", $3); };

return : KW_RET						{ $$ = ""; };
       | KW_RET assign expression			{ $$ = template("return %s;", $3); };
/*-----------------------------------------------------------------------------------------------------------------------------*/

/*-----------------------------------------------EXPRESSIONS-------------------------------------------------------------------*/
exp_arg : POSINT					{ $$ = template("%s", $<crepr>1); }
	| REAL						{ $$ = template("%s", $<crepr>1); }
	| IDENT						{ $$ = $1; }
	| STRING					{ $$ = string_ptuc2c($1); }
	| BOOL_TRUE_CONST				{ $$ = template("1"); }
	| BOOL_FALSE_CONST				{ $$ = template("0"); }
	| func_proc_call				{ $$ = $1; }
	| array_call					{ $$ = $1; };

log_not : KW_NOT expr					{ $$ = template("!%s", $2); }; 

casting : '(' types ')'					{ $$ = template("(%s) ", $2); };

op_func : expr '*' expr					{ $$ = template("%s * %s", $1, $3); }					
	| expr '/' expr					{ $$ = template("%s / %s", $1, $3); }
	| expr KW_DIV expr				{ $$ = template("%s / %s", $1, $3); }
	| expr KW_MOD expr				{ $$ = template("%s %% %s", $1, $3); }
	| '(' op_func ')'				{ $$ = template("(%s)", $2); };
	
op_term : expr '+' expr					{ $$ = template("%s + %s", $1, $3); }
	| expr '-' expr					{ $$ = template("%s - %s", $1, $3); } 
	| '(' op_term ')'				{ $$ = template("(%s)", $2); };

signed : '+' expr 					{ $$ = template("(+ %s)", $2); }
       | '-' expr 					{ $$ = template("(- %s)", $2); }
       | '(' signed ')'					{ $$ = template("(%s)", $2); };

compare : expr '=' expr					{ $$ = template("%s == %s", $1, $3); }
	| expr neq expr					{ $$ = template("%s != %s", $1, $3); }
	| expr '<' expr					{ $$ = template("%s < %s", $1, $3); }
	| expr '>' expr					{ $$ = template("%s > %s", $1, $3); }
	| expr leq expr					{ $$ = template("%s <= %s", $1, $3); }
	| expr meq expr					{ $$ = template("%s >= %s", $1, $3); }
	| '(' compare ')'				{ $$ = template("(%s)", $2); };

log_and : expr KW_AND expr				{ $$ = template("%s && %s", $1, $3); }
	| '(' log_and ')'				{ $$ = template("(%s)", $2); };

log_or : expr KW_OR expr				{ $$ = template("%s || %s", $1, $3); }
       | '(' log_or ')'					{ $$ = template("(%s)", $2); };

arguements : %empty					{ $$ = ""; }
	  | arglist	 				{ $$ = $1; };

arglist: expression					{ $$ = $1; }
       | arglist ',' expression 			{ $$ = template("%s,%s", $1, $3);  };

expr : exp_arg 						{ $$ = $1; }
     | op_func						{ $$ = $1; }
     | op_term						{ $$ = $1; }
     | log_not						{ $$ = $1; }
     | compare						{ $$ = $1; }
     | log_and	 					{ $$ = $1; }
     | log_or 						{ $$ = $1; }
     | signed						{ $$ = $1; };

expression : expr					{ $$ = template("%s", $1); }
	   | casting expr				{ $$ = template("%s%s", $1, $2); };		
/*-----------------------------------------------------------------------------------------------------------------------------*/

/*-----------------------------------------------FUNCTION_PROCEDURE------------------------------------------------------------*/
func_proc_call : IDENT '(' arguements ')'		{ $$ = template("%s(%s)", $1, $3); }; 

func_body : global KW_BEGIN fbody_stat KW_END ';'	{ $$ = template("{\n%s%s\n}", $1, $3); };

proc_body : global KW_BEGIN pbody_stat KW_END ';'	{ $$ = template("{\n%s%s\n}", $1, $3); };

func_head : KW_FUNC IDENT '(' ParamList ')' ':' types ';'{ $$ = template("\n%s %s(%s)", $7, $2, $4); }; 
	  | KW_FUNC IDENT '(' ParamList ')' ':' array types ';'{ $$ = template("\n%s%s %s(%s)", $8, $7, $2, $4); }; 

func : KW_FUNC '(' ParamList ')' ':' 			{ $$ = template("(%s)", $3); }; 

function : func_head func_body				{ $$ = template("%s%s\n", $1, $2); }; 
		
proc : KW_PROC IDENT '(' ParamList ')'			{ $$ = template("\nvoid %s(%s)", $2, $4); };

procedure : proc ';' proc_body				{ $$ = template("%s%s\n", $1, $3); }; 

ParamList : %empty					{ $$ = "";}
	  | IDENT ':' types				{ $$ = template("%s %s", $3, $2); }
	  | IDENT ':' array types			{ $$ = template("%s %s%s", $4, $1, $3); }
	  | ParamList ';' IDENT ':' types		{ $$ = template("%s, %s %s", $1, $5, $3); }	
	  | ParamList ';' IDENT ':' array types		{ $$ = template("%s, %s %s%s", $1, $6, $3, $5); };
	  | name ',' IDENT ':' types			{ $$ = template("%s %s, %s %s", $5, $1, $5, $3); };
	  | ParamList ';' name ',' IDENT ':' types	{ $$ = template("%s, %s %s, %s %s", $1, $7, $3, $7, $5); }
	  | name ',' IDENT ':' array types		{ $$ = template("%s %s%s, %s %s%s", $6, $1, $5, $6, $3, $5); }
	  | ParamList ';' name ',' IDENT ':' array types{ $$ = template("%s, %s %s%s, %s %s%s", $1, $8, $3, $7, $8, $5, $7); };
/*-----------------------------------------------------------------------------------------------------------------------------*/

/*-----------------------------------------------IF_THEN_ELSE------------------------------------------------------------------*/
body : KW_BEGIN mbody_stat KW_END			{ $$ = template("{\n%s\n}\n", $2); }
     | statement					{ $$ = template("%s", $1); };

if_then : KW_IF expression KW_THEN body			{ $$ = template("if (%s)%s", $2, $4); };

if_then_else : if_then %prec KW_ELSE			{ $$ = template("%s",$1); }
	     | if_then KW_ELSE body			{ $$ = template("%s\n\telse%s",$1, $3); };
/*-----------------------------------------------------------------------------------------------------------------------------*/

/*-----------------------------------------------LOOPS-------------------------------------------------------------------------*/
for : KW_FOR IDENT assign expression KW_TO expression	{ $$ = template("for (%s = %s; %s <= %s; %s++)", $2, $4, $2, $6, $2 ); }
    | KW_FOR IDENT assign expression KW_DOWTO expression{ $$ = template("for (%s = %s; %s >= %s; %s--)", $2, $4, $2, $6, $2 ); };

for_loop: for KW_DO body 				{ $$ = template("%s%s", $1, $3); };

while_loop : KW_WHILE expression KW_DO body		{ $$ = template("while (%s)%s", $2, $4); };	

repeat_loop : KW_REP body KW_UNT expression		{ $$ = template("do %s while(%s);", $2, $4); };		
/*-----------------------------------------------------------------------------------------------------------------------------*/

/*-----------------------------------------------STATEMENTS--------------------------------------------------------------------*/
mbody_stat : %empty					{ $$ = ""; } 
	   | statements 				{ $$ = $1; };

fbody_stat : %empty					{ $$ = ""; }
	   | statements					{ $$ = template("\tint result;\n%s\treturn result;\n", $1); };

pbody_stat : %empty					{ $$ = ""; }
	   | statements					{ $$ = $1; };

statements : statement_list		   		{ $$ = $1; };

statement_list : statement                 		{ $$ = $1; };    
	       | statement_list ';' statement  		{ $$ = template("%s%s", $1, $3); }; 

statement : func_proc_call				{ $$ = template("\t%s;\n", $1); }
	  | assignment					{ $$ = template("\t%s;\n", $1); }
	  | if_then_else				{ $$ = template("\t%s\n", $1); }
	  | while_loop					{ $$ = template("\t%s\n", $1); }
	  | repeat_loop					{ $$ = template("\t%s\n", $1); }
	  | for_loop					{ $$ = template("\t%s\n", $1); }
	  | return					{ $$ = template("\t%s\n", $1); }
      | result					{ $$ = template("\t%s\n", $1); };		
/*-----------------------------------------------------------------------------------------------------------------------------*/

/*-----------------------------------------------TYPES-------------------------------------------------------------------------*/
types : KW_INT						{ $$ = template("int"); }
      | KW_REAL						{ $$ = template("real"); }
      | KW_CHAR						{ $$ = template("char"); }
      | KW_BOOL						{ $$ = template("_Bool"); }
      | IDENT						{ $$ = $1; };
/*-----------------------------------------------------------------------------------------------------------------------------*/

/*-----------------------------------------------TYPEDEF-----------------------------------------------------------------------*/
defines : KW_TYPE def 					{ $$ = template("\n%s", $2); };

def : IDENT '=' types ';'				{ $$ = template("typedef %s %s;\n", $3, $1); }
    | IDENT '=' array types ';'				{ $$ = template("typedef %s%s %s;\n", $4, $3, $1); }
    | IDENT '=' func types ';'				{ $$ = template("typedef %s (*%s) %s;\n", $4, $1, $3); }
    | def IDENT '=' types ';'				{ $$ = template("%stypedef typeof(%s(*)%s) %s;\n", $1, $4, $2); }
    | def IDENT '=' array types ';'			{ $$ = template("%stypedef %s%s %s;\n", $1, $5, $4, $2); }
    | def IDENT '=' func types ';'			{ $$ = template("%stypedef %s%s %s;\n", $1, $5, $4, $2); };
/*-----------------------------------------------------------------------------------------------------------------------------*/

/*-----------------------------------------------VARIABLES---------------------------------------------------------------------*/
variables : KW_VAR variable				{ $$ = template("\n%s", $2); };

variable : namecol types ';'				{ $$ = template("%s %s;\n", $2, $1); }
	 | namecol array types ';'			{ $$ = template("%s %s%s;\n", $3, $1, $2); }
	 | namecol func types ';'			{ $$ = template("%s %s%s;\n", $3, $1, $2); }
	 | variable namecol types ';'			{ $$ = template("%s%s %s;\n", $1, $3, $2); }
       	 | variable namecol array types ';'		{ $$ = template("%s%s %s%s;\n", $1, $4, $2, $3); }
	 | variable namecol func types ';'		{ $$ = template("%s%s %s%s;\n", $1, $4, $2, $3); };

name : IDENT 						{ $$ = $1; } 
      | name ',' IDENT					{ $$ = template("%s,%s", $1, $3); };

namecol : name ':'					{ $$ = $1; };
/*-----------------------------------------------------------------------------------------------------------------------------*/
%%
