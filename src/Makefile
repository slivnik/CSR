GHC = ghc
ALEX = alex
HAPPY = happy
RM = 'rm' -fr

all: CSRparser

CSRparser: Main.hs Common.hs InpAST.hs InpLexer.hs InpParser.hs CSR.hs TrieParser.hs DFAParser.hs
	$(GHC) -O2 -o CSRparser $<

InpLexer.hs: InpLexer.x
	$(ALEX) $<

InpParser.hs: InpParser.y InpLexer.x
	$(HAPPY) --info $<

.PHONY: clean distclean

clean:
	$(RM) InpLexer.hs InpParser.hs *.info
	$(RM) *.hi *.o

distclean: clean
	$(RM) CSRparser
