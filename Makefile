################################################################################
# Author: Tingsheng Lai
# Student Number: 781319
# Email: tingshengl@student.unimelb.edu.au
# Makefile for the Paz compiler
################################################################################

CC       = ghc
COptions = -O2 -XPackageImports
EXE      = Paz
OBJ      = Paz.hs

$(EXE): $(OBJ) PazLexer.hs PazParser.hs PazPrettify.hs
	$(CC) $(COptions) -o $(EXE) $(OBJ)

clean:
	rm -f *.o *.hi Paz
