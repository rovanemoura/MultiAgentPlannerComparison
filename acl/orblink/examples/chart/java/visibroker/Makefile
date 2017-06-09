SHELL = sh

ior-file = ../../dataset.ior

SRC= \
 ExampleFrame.java \
 mydatasource.java \
 ChartDrawer.java \
 IorIo.java

JAVAC=vbjc

CLASSES = $(SRC:.java=.class)

classes:  $(CLASSES)

%.class: %.java
	$(JAVAC) $<

all: idl $(CLASSES)

idl:
	idl2java ../../chart.idl

clean:
	rm -f *.class
	rm -f -r chart

run:
	vbj ChartDrawer ${ior-file}


