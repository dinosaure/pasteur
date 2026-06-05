vendors:
	test ! -d $@
	mkdir vendors
	@./source.sh

pasteur.hvt.target: | vendors
	@echo " BUILD main.exe"
	@dune build --root . --profile=release ./main.exe
	@echo " DESCR main.exe"
	@$(shell dune describe location \
		--context solo5 --no-print-directory --root . --display=quiet \
		./main.exe 1> $@ 2>&1)

pasteur.hvt: pasteur.hvt.target
	@echo " COPY pasteur.hvt"
	@cp $(file < pasteur.hvt.target) $@
	@chmod +w $@
	@echo " STRIP pasteur.hvt"
	@strip $@

pasteur.install: pasteur.hvt
	@echo " GEN pasteur.install"
	@ocaml install.ml > $@

all: pasteur.install | vendors

.PHONY: clean
clean:
	if [ -d vendors ] ; then rm -fr vendors ; fi
	rm -f pasteur.hvt.target
	rm -f pasteur.hvt
	rm -f pasteur.install

install: pasteur.intall
	@echo " INSTALL pasteur"
	opam-installer pasteur.install
