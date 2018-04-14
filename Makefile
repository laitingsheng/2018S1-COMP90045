CC       = ghc
COptions = -O2 -Wall -XPackageImports
EXE      = Paz
OBJ      = Paz.hs

$(EXE): $(OBJ)
	$(CC) $(COptions) -o $(EXE) $(OBJ)

clean:
	rm -f *.o *.hi Paz
