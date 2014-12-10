
main:
		ocamlbuild -pp "camlp4o.opt -unsafe" -libs graphics main.native	       
clean:
		ocamlbuild -clean
		rm -f *~
 
