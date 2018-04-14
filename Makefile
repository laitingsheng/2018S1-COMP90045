CC       = ghc
COptions = -O2 -Wall -XPackageImports
EXE      = Paz
OBJ      = Paz.hs

$(EXE): $(OBJ) PazLexer.hs PazParser.hs
	$(CC) $(COptions) -o $(EXE) $(OBJ)

clean:
	rm -f *.o *.hi Paz
