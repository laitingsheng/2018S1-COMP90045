Paz: Paz.hs PazLexer.hs PazParser.hs Compiler.hs
	ghc -O3 -dynamic Paz.hs

clean:
	rm -f *.o *.hi Paz
