all: 	parser lexer

parser:	pascal.y pascal.l
	flex pascal.l
	bison -d pascal.y
	gcc -c lex.yy.c
	gcc -o parser pascal.tab.c lex.yy.o -lfl

lexer: pascal.l
	cat pascal.l | sed 's/:\%d//' > lex.l
	flex lex.l
	gcc -DLEXER -c lex.yy.c
	gcc -o lexer lex.yy.o -lfl

pascal.y: rules pascal.y.prolog pascal.y.epilog
	cat pascal.y.prolog rules pascal.y.epilog > pascal.y

rules:	grammar.lr
	cat grammar.lr | sed 's/->/:/' | sed 's/\.//' | awk '/#.*/{print "";next;}/:/{print $$0"\n\t {printf(\"%s\\n\",\""$$0"\");} ;";}' | awk '/^[a-z]/{for(I=1;I<=NF;I++){if($$I ~ /[A-Z]/)$$I="_"$$I;}};{print;}' > rules

clean: distclean
distclean:
	'rm' -fr lex.l *~ lex.yy.* pascal.tab.* pascal.y rules lexer parser
