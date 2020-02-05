FROM ocaml/opam2:4.09
RUN sudo apt-get install -y m4 git net-tools iproute2 libev-dev qemu-kvm
RUN opam update -y
RUN opam install -y conf-libev mirage
RUN mkdir /home/opam/pasteur
RUN git clone https://github.com/dinosaure/pasteur.git /home/opam/pasteur
WORKDIR /home/opam/pasteur
RUN git checkout tuyau+aes
RUN opam config exec -- mirage configure -t hvt
RUN make depends
RUN opam config exec -- mirage build
COPY tap.sh /home/opam/pasteur/tap.sh
EXPOSE 80
ENV VIRTUAL_HOST paste.x25519.net
ENV LETSENCRYPT_HOST paste.x25519.net
COPY run.sh /home/opam/pasteur/run.sh
