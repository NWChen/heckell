TARFILES = Makefile scanner.mll parser.mly ast.ml toplevel.ml
OBJS = parser.cmo scanner.cmo ast.cmo pprinting.cmo toplevel.cmo

YOPTS =

toplevel : $(OBJS)
	ocamlc -o toplevel $(OBJS)

# codegen : $(OBJS)
# 	ocamlc -o codegen $(OBJS)

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc $(YOPTS)  parser.mly
	
%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

# codegen.tar.gz : $(TARFILES)
# 	cd .. && tar zcf codegen/codegen.tar.gz $(TARFILES:%=codegen/%)

.PHONY : clean
clean :
	rm -f toplevel parser.ml parser.mli scanner.ml *.cmo *.cmi

# Generated by ocamldep *.ml *.mli
# codegen.cmo: scanner.cmo parser.cmi ast.cmo 
# codegen.cmx: scanner.cmx parser.cmx ast.cmo 

toplevel.cmo: scanner.cmo parser.cmi ast.cmo pprinting.cmo
toplevel.cmx: scanner.cmx parser.cmx ast.cmo pprinting.cmo
parser.cmo: ast.cmo parser.cmi 
parser.cmx: ast.cmo parser.cmi 
scanner.cmo: parser.cmi 
scanner.cmx: parser.cmx 
parser.cmi: ast.cmo 


