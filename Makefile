OBJS = parser.cmo scanner.cmo interpret.cmo sol.cmo

TARFILES = Makefile testall.sh scanner.mll parser.mly \
	ast.mli interpret.ml sol.ml \
	$(TESTS:%=tests/test-%.mc) \
	$(TESTS:%=tests/test-%.out)

sol : $(OBJS)
	ocamlc -o sol $(OBJS)

.PHONY : test
test : sol testall.sh
	./testall.sh

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

sol.tar.gz : $(TARFILES)
	cd .. && tar czf sol/sol.tar.gz $(TARFILES:%=sol/%)

.PHONY : clean
clean :
	rm -f sol parser.ml parser.mli scanner.ml testall.log *.cmo *.cmi

# Generated by ocamldep *.ml *.mli
interpret.cmo: ast.cmi 
interpret.cmx: ast.cmi 
sol.cmo: scanner.cmo parser.cmi interpret.cmo 
sol.cmx: scanner.cmx parser.cmx interpret.cmx 
parser.cmo: ast.cmi parser.cmi 
parser.cmx: ast.cmi parser.cmi 
scanner.cmo: parser.cmi 
scanner.cmx: parser.cmx 
parser.cmi: ast.cmi 