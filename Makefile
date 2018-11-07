S=cmx
A=cmxa
OBJS=common.$S debug.$S options.$S parray.$S union_find.$S solver.$S sat.$S parser.$S main.$S
FLAGS=-annot -g
OCAML=ocamlopt
all: esmt

esmt: $(OBJS)
	$(OCAML) $(FLAGS) -o $@ nums.$A $(OBJS)

.SUFFIXES: .mli .ml .cmi .cmo .cmx

.mli.cmi:
	$(OCAML) $(FLAGS) -c  $<

.ml.cmo:
	$(OCAML) $(FLAGS) -c $<

.ml.cmx:
	$(OCAML) $(FLAGS) -c $<


clean:
	rm -f *.cm[iox] *.o *.annot *~ esmt
	rm -f .depend
	rm -f *.log *.aux *.synctex.gz

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend
