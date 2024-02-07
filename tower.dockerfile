# Build with `docker build -t oopsla22-artifact -f tower.dockerfile .`
# Run with `docker run -it --rm oopsla22-artifact bash`

FROM ocaml/opam:debian-11-ocaml-4.13
USER opam

RUN opam update -q \
    && opam install -y dune core menhir ppx_deriving \
    && echo 'eval $(opam env)' >> /home/opam/.bashrc

WORKDIR /home/opam/
RUN curl -L -O https://github.com/psg-mit/tower-oopsla22/archive/refs/heads/main.zip \
    && unzip main.zip \
    && mv tower-oopsla22-main oopsla22-artifact

WORKDIR /home/opam/oopsla22-artifact/
RUN eval $(opam env) && make
