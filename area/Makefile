CC=ocamlc

.PHONY: all clean

all: area.byte

anacomb.cmo: anacomb.ml
	$(CC) -c $<

bexp.cmo: bexp.ml anacomb.cmo anacomb.cmi
	$(CC) -c $<

whileB.cmo: whileB.ml bexp.cmo bexp.cmi
	$(CC) -c $<

state.cmo: state.ml whileB.cmo whileB.cmi
	$(CC) -c $<

area.cmo: area.ml state.cmo state.cmi
	$(CC) -c $<

area.byte: anacomb.cmo bexp.cmo whileB.cmo state.cmo area.cmo
	$(CC) -o $@ $^

clean:
	rm -f *.byte *.cmi *.cmo
