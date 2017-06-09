SHELL = sh

ior-file = ../../cl/grid.ior

all: idl classes

fum: 
	echo classp is: ${classp}

idl:
	idl2java ../../idl/grid.idl

classes:
	vbjc IorIo.java gridImpl.java Server.java Client.java


clean:
	rm -f *.class
	rm -f -r example

run-client:
	vbj Client ${ior-file}

run-server:
	 vbj Server ${ior-file}


