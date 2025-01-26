FROM ocaml/opam:debian-10-ocaml-4.12

RUN opam init -y && \
    opam switch create 4.12.0 && \
    eval $(opam env) && \
    opam install -y yojson

RUN opam install -y ocamlfind

COPY turing /home/opam/src

#IDK WHY: ne fonctionne pas sur ma machine (j'utilise donc COPY)
#VOLUME turing /home/opam/src

WORKDIR /home/opam/src
USER root
RUN chown -R opam:opam /home/opam/src
RUN chmod 755 /home/opam/src
USER opam

RUN echo "PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@ENV_TURING\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '" >> ~/.bashrc

RUN echo "eval $(opam env)" >> ~/.bashrc

RUN echo "########################\n###### ENV_TURING ######\n########################\n" >> ~/.bashrc