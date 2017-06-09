# 2010 (C) Jussi Rintanen, Jussi.Rintanen@nicta.com.au

CC = gcc

CFLAGS = -O3 -Wall -funroll-loops -ftree-loop-im -ftracer -maccumulate-outgoing-args -momit-leaf-frame-pointer -DGLOBALUS -DIPC -DVSIDS

OBJ = clausesets.o printplan.o clausedb.o dimacsinput.o rset.o
HDR = interface.h clausesets.h printplan.h clausedb.h dimacsinput.h rset.h

all: nsat

nsat: Makefile $(OBJ) $(HDR)

%.o: %.c Makefile $(HDR)
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f $(OBJ)

tar:
