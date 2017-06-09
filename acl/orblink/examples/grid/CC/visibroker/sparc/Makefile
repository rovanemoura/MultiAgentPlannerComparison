include stdmk

EXE = 	grid_client

all: $(EXE)

clean:
	-rm -f core *.o *.hh *.cc $(EXE) 
	-rm -rf Templates.DB

#
# "Account" specific make rules
#


grid_c.cc: grid.idl
	$(ORBCC) grid.idl

grid_client: grid_c.o grid_clnt.o 
	$(CC) -o grid_client grid_clnt.o \
	grid_c.o $(LIBPATH) $(LIBORB) $(STDCC_LIBS)

