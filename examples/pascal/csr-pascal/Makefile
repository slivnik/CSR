all:
	$(MAKE) -C parser all
	$(MAKE) -C ../../../src all

PARSER="dfa"
MODE="no"

%:
	$(MAKE) -C . all
	cat tests/$@.p | parser/lexer | ../../../src/CSRparser pascal.csr $(PARSER) $(MODE)

clean distclean:
	$(MAKE) -C parser distclean
