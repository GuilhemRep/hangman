all: 
	ocamlc -c Hangman.ml
	ocamlc -o hangman Hangman.cmo

clean:
	rm -rf hangman *.cmi *.cmx *.o *.cmo