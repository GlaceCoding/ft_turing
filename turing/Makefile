OCAML = ocamlfind ocamlc
OCAMLFLAGS = -package yojson,str -linkpkg 

NAME = ft_turing
SOURCES = ft_turing.ml

all: $(NAME)

re: clean
	make all

$(NAME): $(SOURCES)
	$(OCAML) $(OCAMLFLAGS) -o $(NAME) $(SOURCES)

clean:
	rm -f *.cmi *.cmo *.byte

fclean: clean
	rm -f $(NAME)

.PHONY: all clean re fclean