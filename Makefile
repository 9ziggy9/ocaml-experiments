all: main.ml
	ocamlfind opt -package core -thread -linkpkg main.ml
	./a.out

clean:
	rm -rf *.cmi *.o *.out *.cmx
