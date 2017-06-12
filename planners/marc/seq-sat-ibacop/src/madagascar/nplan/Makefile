# 2010 (C) Jussi Rintanen, Jussi.Rintanen@nicta.com.au

PARSER=y
NAME = nplan

CC = gcc

CFLAGS = -O3 -Wall -funroll-loops -ftree-loop-im -ftracer -maccumulate-outgoing-args -momit-leaf-frame-pointer -DGLOBALUS -DIPC -DVSIDS 

LINKFLAGS = -static

OBJ = $(PARSER).tab.o main.o asyntax.o lex.yy.o tables.o operators.o staticpredicates.o instantiation.o NEWinvariants.o intsets.o cleanup.o translate2sat.o NEWNEWscc.o ordintsets.o dimacs.o ../nsat/clausesets.o ../nsat/printplan.o ../nsat/clausedb.o ../nsat/dimacsinput.o heuristics2.o ../nsat/rset.o
HDR = main.h asyntax.h tables.h operators.h staticpredicates.h instantiation.h invariants.h intsets.h cleanup.h NEWNEWscc.h intsets.h translate2sat.h ordintsets.h dimacs.h heuristics2.h

all: nplan

nplan: nsat date lex.yy.c Makefile $(PARSER).tab.c $(OBJ) $(HDR)
	$(CC) $(LINKFLAGS) $(CFLAGS) -o $(NAME) $(OBJ) -lm

nsat:
	(cd ../nsat ; make)

date:
	./makedate

%.o: %.c Makefile $(HDR)
	$(CC) $(CFLAGS) -c $<

main.o: zPRINTDATE

y.tab.c: parser.y
	yacc -d parser.y

y.tab.h: parser.y
	yacc -d parser.y

lex.yy.c: lexer.lex $(PARSER).tab.h y.tab.h
	flex lexer.lex

clean:
	rm -f $(OBJ) y.tab.c y.tab.h
