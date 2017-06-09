ior-file = ../../cl/grid.ior
orbhome = $(ORBIXWEB_HOME)
orbclasses=$(orbhome)/classes
thisclasspath= $(orbclasses):$(CLASSPATH)

idl: 
	idl -jO. ../../idl/grid.idl

classes:
	javac -classpath $(thisclasspath) *.java

run-server:
	owjava  -classpath $(thisclasspath) Server $(ior-file)

run-client:
	owjava -classpath $(thisclasspath) Client $(ior-file)

clean:
	rm -rf *.class example
