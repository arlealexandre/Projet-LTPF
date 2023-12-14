CC=ocamlc

.PHONY: all clean

all: area.byte

anacomb.cmo: anacomb.ml
	$(CC) -c $<

variableAnalyseur.cmo: variableAnalyseur.ml anacomb.cmo anacomb.cmi
	$(CC) -c $<

aexpAnalyseur.cmo: aexpAnalyseur.ml anacomb.cmo anacomb.cmi variableAnalyseur.cmo variableAnalyseur.cmi
	$(CC) -c $<

bexpAnalyseur.cmo: bexpAnalyseur.ml anacomb.cmo anacomb.cmi variableAnalyseur.cmo variableAnalyseur.cmi aexpAnalyseur.cmo aexpAnalyseur.cmi
	$(CC) -c $<

whileAnalyseur.cmo: whileAnalyseur.ml bexpAnalyseur.cmo bexpAnalyseur.cmi aexpAnalyseur.cmo aexpAnalyseur.cmi anacomb.cmo anacomb.cmi variableAnalyseur.cmo variableAnalyseur.cmi
	$(CC) -c $<

state.cmo: state.ml whileAnalyseur.cmo whileAnalyseur.cmi anacomb.cmo anacomb.cmi aexpAnalyseur.cmo aexpAnalyseur.cmi bexpAnalyseur.cmo bexpAnalyseur.cmi variableAnalyseur.cmo variableAnalyseur.cmi
	$(CC) -c $<

area.cmo: area.ml state.cmo state.cmi whileAnalyseur.cmo whileAnalyseur.cmi anacomb.cmo anacomb.cmi aexpAnalyseur.cmo aexpAnalyseur.cmi bexpAnalyseur.cmo bexpAnalyseur.cmi variableAnalyseur.cmo variableAnalyseur.cmi
	$(CC) -c $<

area.byte: anacomb.cmo variableAnalyseur.cmo aexpAnalyseur.cmo bexpAnalyseur.cmo whileAnalyseur.cmo state.cmo area.cmo
	$(CC) -o $@ $^

clean:
	rm -f *.byte *.cmi *.cmo
