ior-file = ./chat.ior

all: idl classes

idl:
	idl2java ./chat.idl

classes:
	vbjc ChatClient.java IorIo.java 

clean:
	rm -f *.class
	rm -f -r chat

run-client:
	vbj ChatClient ${ior-file}

