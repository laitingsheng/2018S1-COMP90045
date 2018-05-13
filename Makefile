Paz: Paz.hs PazLexer.hs PazParser.hs Compiler.hs
	ghc -dynamic Paz.hs

clean:
	rm -f *.o *.hi Paz
