FROM ocaml/opam:debian-10-ocaml-4.12

RUN opam init -y && \
    opam switch create 4.12.0 && \
    eval $(opam env) && \
    opam install -y yojson dune

# COPY . .

VOLUME .:/home/opam/src

WORKDIR /home/opam/src

RUN echo "eval $(opam env)" >> ~/.bashrc