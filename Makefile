CC       = ghc
COptions = -O2 -XPackageImports
EXE      = Paz
OBJ      = Paz.hs

$(EXE): $(OBJ) PazLexer.hs PazParser.hs PazPrettify.hs
	$(CC) $(COptions) -o $(EXE) $(OBJ)

clean:
	rm -f *.o *.hi Paz
