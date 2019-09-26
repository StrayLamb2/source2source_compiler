%{
#include <assert.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include "ptucc_parser.tab.h"
#include "cgen.h"

int line_num = 1;

/* 
	A primitive macro facility:
	just one macro is allowed to be defined! 
*/
#define MAXMACRO 32
char* mactable[MAXMACRO][2];
int mactable_size = 0;	

/* Return 1 on success, 0 on failure (macro table full) */
int set_macro(char* name, char* def);

/* Return def for macro, or NULL if no such macro is defined. */
char* get_macro(char* name);

%}
ID		[a-zA-Z_][0-9a-zA-Z_]*
DIGIT		[0-9]
NUMBER  	{DIGIT}{DIGIT}*
DECIMAL 	("."{NUMBER})|({NUMBER}("."{NUMBER}?)?)
REAL    	{DECIMAL}([eE][+-]?{NUMBER})?
STRING  	(\'.*\')|(\".*\")
CONST_STR	("\\"+[\nNtTrR'"])

%x comment
%x line_comment
%x macro

and		return TK_AND;
array	 	return TK_ARRAY;


%%
@defmacro[ \r\t]+       BEGIN(macro);
<macro>{ID}             {
							/* Store macro name */
							char* name = strdup(yytext);
							char* def = NULL;
							size_t deflen = 0;
							char c;

							FILE* deff = open_memstream(&def, &deflen);

							while((c=input())!='\n')
							   fputc(c, deff);
							fclose(deff);

							if(!set_macro(name, def))
								yyerror("lexer error: failed to define macro '%s'\n", name);
							BEGIN(INITIAL);
						};
						
(?i:and)|"&&"					return KW_AND;
(?i:array)					return KW_ARR;
(?i:boolean)					return KW_BOOL;	
(?i:char)					return KW_CHAR;
(?i:begin)					return KW_BEGIN;
(?i:div)					return KW_DIV;
(?i:do)						return KW_DO;
(?i:else)					return KW_ELSE;
(?i:for)					return KW_FOR;
(?i:end)					return KW_END;
(?i:function)					return KW_FUNC;
(?i:goto)					return KW_GOTO;
(?i:if)						return KW_IF;
(?i:integer)					return KW_INT;
(?i:var)					return KW_VAR;
(?i:mod)					return KW_MOD;
(?i:not)|"!"					return KW_NOT;
(?i:of)						return KW_OF;
(?i:or)|"||"					return KW_OR;
(?i:while)					return KW_WHILE;
(?i:procedure)					return KW_PROC;
(?i:program)					return KW_PROGRAM;
(?i:real)					return KW_REAL;
(?i:repeat)					return KW_REP;
(?i:to)						return KW_TO;
(?i:result)					return KW_RES;
(?i:return)					return KW_RET;
(?i:then)					return KW_THEN;
(?i:until)					return KW_UNT;
(?i:downto)					return KW_DOWTO;
(?i:type)					return KW_TYPE;

(?i:true)					return BOOL_TRUE_CONST;
(?i:false)					return BOOL_FALSE_CONST;

{CONST_STR}					return CONST_STR;

{ID}  					{
						   char* def = get_macro(yytext);
						   if(def==NULL) {
						   		yylval.crepr = strdup(yytext);
						   		return IDENT;
						   	}
						   for(int i=strlen(def); i>0; i--) {
						   	  unput(def[i-1]);
						   }
						} 						
{NUMBER}				{ yylval.crepr = strdup(yytext); return POSINT; }
{REAL}					{ yylval.crepr = strdup(yytext); return REAL; }
{STRING}				{ yylval.crepr = strdup(yytext); return STRING; }

"+"						return '+';
"-"						return '-';
"*"						return '*';
"/"						return '/';

"="						return '=';
"<>"						return neq;
"<"						return '<';
"<="						return leq;
">"						return '>';
">="						return meq;

":="						return assign;

";"						return ';';
"("						return '(';
")"						return ')';
","						return ',';
"["						return '[';
"]"						return ']';
":"						return ':';
"."						return '.';

"(*"					BEGIN(comment);
<comment>[^*\n]*        /* eat anything that's not a '*' */
<comment>"*"+[^*)\n]*   /* eat up '*'s not followed by ')'s */
<comment>\n             ++line_num;
<comment>"*"+")"        BEGIN(INITIAL);

"//"					BEGIN(line_comment);
<line_comment>[^\n]*
<line_comment>\n             {++line_num; BEGIN(INITIAL);}


[ \r\t]					/* skip whitespace */
\n 						++line_num;
<<EOF>> 				return EOF;

.        { yyerror("lexical error: unrecognized literal '%s'\n", yytext); }


%%

int set_macro(char* name, char* def)
{
	/* Check to see if macro already defined, and redefine it. */
	int i;
	for(i=0; i<mactable_size; i++) {
		if(strcmp(mactable[i][0], name)==0) {
			/* found ! */
			free(name);
			free(mactable[i][1]);
			mactable[i][1] = def;
			break;
		}
	}
	if(i<mactable_size)
		return 1;
	else if(mactable_size < MAXMACRO) {
		/* new entry */
		assert(i==mactable_size);
		mactable[i][0] = name;
		mactable[i][1] = def;
		mactable_size++;
		return 1;
	}
	else
		return 0;
}

char* get_macro(char* name)
{
	for(int i=0;i<mactable_size; i++) {
		if(strcmp(mactable[i][0], name)==0)
			return mactable[i][1];
	}
	return NULL;
}


