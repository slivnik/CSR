%{
#include <stdio.h>
#include <stdlib.h>

  int yylex ();
  int yyerror (char *msg);
  
%}

%token _ADD _AND _ARRAY _ASSIGN _BEGIN _BOOLEAN _BOOLEANCONST _CASE _CHAR _COLON _COMMA _CONST _DIV _DO _DOT _DOWNTO _ELSE _END _EQU _EXTERNAL _FOR _FORWARD _FUNCTION _GEQ _GOTO _GTH _FILE _IDENTIFIER _IDIV _IF _IMOD _IN _INTEGER _INTEGERCONST _INTERVAL _NIL _LABEL _LBRACKET _LPARENT _LEQ _LTH _MUL _NEQ _NOT _OF _OR _PACKED _PROCEDURE _PROGRAM _PTR _RBRACKET _RECORD _REPEAT _REAL _REALCONST _RPARENT _SEMIC _SET _STEP _STRINGCONST _SUB _THEN _TO _TYPE _UNTIL _VAR _WHILE _WITH 

%%
