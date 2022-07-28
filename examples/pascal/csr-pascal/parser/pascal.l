%{
#include <stdio.h>
#include <stdlib.h>

#include "pascal.tab.h"

int comment_depth = 0;
int line = 1;
%}

%x COMMENT

STRINGCONST   \'([^']|\'\')*\'
REALCONST     [0-9]+((\.[0-9]+)([Ee][+-][0-9]+)?|(\.[0-9]+)?([Ee][+-][0-9]+))
INTEGERCONST  [0-9]+

%%

<INITIAL,COMMENT>[ \t]+ ;
<INITIAL,COMMENT>[\n]   { line++; }

"true"|"false"       { printf ("BOOLEANCONST:%d\n",line); return _BOOLEANCONST; }
{STRINGCONST}        { printf ("STRINGCONST:%d\n",line); return _STRINGCONST; }
{REALCONST}          { printf ("REALCONST:%d\n",line); return _REALCONST; }
{INTEGERCONST}       { printf ("INTEGERCONST:%d\n",line); return _INTEGERCONST; }

"+"                  { printf ("ADD:%d\n",line); return _ADD; }
":="                 { printf ("ASSIGN:%d\n",line); return _ASSIGN; }
":"                  { printf ("COLON:%d\n",line); return _COLON; }
","                  { printf ("COMMA:%d\n",line); return _COMMA; }
"/"                  { printf ("DIV:%d\n",line); return _DIV; }
"."                  { printf ("DOT:%d\n",line); return _DOT; }
"="                  { printf ("EQU:%d\n",line); return _EQU; }
">="                 { printf ("GEQ:%d\n",line); return _GEQ; }
">"                  { printf ("GTH:%d\n",line); return _GTH; }
".."                 { printf ("INTERVAL:%d\n",line); return _INTERVAL; }
"["                  { printf ("LBRACKET:%d\n",line); return _LBRACKET; }
"("                  { printf ("LPARENT:%d\n",line); return _LPARENT; }
"<="                 { printf ("LEQ:%d\n",line); return _LEQ; }
"<"                  { printf ("LTH:%d\n",line); return _LTH; }
"*"                  { printf ("MUL:%d\n",line); return _MUL; }
"<>"                 { printf ("NEQ:%d\n",line); return _NEQ; }
"^"                  { printf ("PTR:%d\n",line); return _PTR; }
"]"                  { printf ("RBRACKET:%d\n",line); return _RBRACKET; }
")"                  { printf ("RPARENT:%d\n",line); return _RPARENT; }
";"                  { printf ("SEMIC:%d\n",line); return _SEMIC; }
"-"                  { printf ("SUB:%d\n",line); return _SUB; }

"boolean"            { printf ("BOOLEAN:%d\n",line); return _BOOLEAN; }
"char"               { printf ("CHAR:%d\n",line); return _CHAR; }
"real"               { printf ("REAL:%d\n",line); return _REAL; }
"integer"            { printf ("INTEGER:%d\n",line); return _INTEGER; }

"and"                { printf ("AND:%d\n",line); return _AND; }
"array"              { printf ("ARRAY:%d\n",line); return _ARRAY; }
"begin"              { printf ("BEGIN:%d\n",line); return _BEGIN; }
"case"               { printf ("CASE:%d\n",line); return _CASE; }
"const"              { printf ("CONST:%d\n",line); return _CONST; }
"do"                 { printf ("DO:%d\n",line); return _DO; }
"downto"             { printf ("DOWNTO:%d\n",line); return _DOWNTO; }
"else"               { printf ("ELSE:%d\n",line); return _ELSE; }
"end"                { printf ("END:%d\n",line); return _END; }
"file"               { printf ("FILE:%d\n",line); return _FILE; }
"for"                { printf ("FOR:%d\n",line); return _FOR; }
"forward"            { printf ("FORWARD:%d\n",line); return _FORWARD; }
"function"           { printf ("FUNCTION:%d\n",line); return _FUNCTION; }
"goto"               { printf ("GOTO:%d\n",line); return _GOTO; }
"div"                { printf ("IDIV:%d\n",line); return _IDIV; }
"if"                 { printf ("IF:%d\n",line); return _IF; }
"mod"                { printf ("IMOD:%d\n",line); return _IMOD; }
"in"                 { printf ("IN:%d\n",line); return _IN; }
"label"              { printf ("LABEL:%d\n",line); return _LABEL; }
"nil"                { printf ("NIL:%d\n",line); return _NIL; }
"not"                { printf ("NOT:%d\n",line); return _NOT; }
"of"                 { printf ("OF:%d\n",line); return _OF; }
"or"                 { printf ("OR:%d\n",line); return _OR; }
"packed"             { printf ("PACKED:%d\n",line); return _PACKED; }
"procedure"          { printf ("PROCEDURE:%d\n",line); return _PROCEDURE; }
"program"            { printf ("PROGRAM:%d\n",line); return _PROGRAM; }
"record"             { printf ("RECORD:%d\n",line); return _RECORD; }
"repeat"             { printf ("REPEAT:%d\n",line); return _REPEAT; }
"set"                { printf ("SET:%d\n",line); return _SET; }
"step"               { printf ("STEP:%d\n",line); return _STEP; }
"then"               { printf ("THEN:%d\n",line); return _THEN; }
"to"                 { printf ("TO:%d\n",line); return _TO; }
"type"               { printf ("TYPE:%d\n",line); return _TYPE; }
"until"              { printf ("UNTIL:%d\n",line); return _UNTIL; }
"var"                { printf ("VAR:%d\n",line); return _VAR; }
"while"              { printf ("WHILE:%d\n",line); return _WHILE; }
"with"               { printf ("WITH:%d\n",line); return _WITH; }

[A-Za-z][A-Za-z0-9]* { printf ("IDENTIFIER:%d\n",line); return _IDENTIFIER; }

<INITIAL>\{ { comment_depth++; BEGIN(COMMENT); }
<COMMENT>\{ { comment_depth++; }
<COMMENT>\} { comment_depth--; if (comment_depth == 0) BEGIN(INITIAL); }
<COMMENT>.  ;

. {
    printf ("LEX: Unexpected character '%c' [%d].\n", yytext[0], yytext[0]);
    exit (1);
  }

%%

#ifdef LEXER
int main ()
{
	while (yylex () != 0) {
	}
	return 0;
}
#endif
