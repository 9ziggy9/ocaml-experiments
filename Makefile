all: hello.ml
	ocamlfind opt -package core -thread -linkpkg hello.ml
	./a.out

clean:
	rm -rf *.cmi *.o *.out *.cmx
