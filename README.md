# Pasteur

Pasteur is an unikernel (MirageOS) which provide an mini-website to paste
snippets and save them into a Git repository. The goal of this project is
to be an example of: how to write an unikernel today?

This example uses several stacks like the HTTP stack (with [http/af][http-af]),
the Git stack (with [ocaml-git][ocaml-git]) with [Irmin][irmin] with underlaying
layers - available in MirageOS organization.

Design comes from [paste.isomorphis.me][paste].

## How to use it?

MirageOS let user to choose which backend he wants. So we have 3 generals backends:
- KVM (with Solo5)
- Unix
- Xen

Currently, the project is tested with the Unix backend - the most easy to deploy - and Solo5.

### Unix backend

First, you need to have the MirageOS tool:

```sh
$ opam install mirage checkseum.0.0.9 digestif.0.7.4
```

Then, under the repository:

```sh
$ mirage configure -t unix
$ make depends
$ mirage build
```

And you builded the unikernel! However, this unikernel needs to communicate with a remote Git
repository. In an other shell, we need to make a new Git repository with, at least, one commit
in `master`:

```sh
$ mkdir pasteur
$ cd pasteur
$ git init --bare
$ git read-tree --empty
$ FIRST_COMMIT=`git commit-tree $(git write-tree) -m .`
$ git update-ref "refs/heads/master" $FIRST_COMMIT`
```

Then, we can launch the Git server:

```sh
$ cd ..
$ git daemon --base-path=. --export-all --reuseaddr --informative-errors --verbose --enable=receive-pack 
```

You can clone it by this way:

```sh
$ git clone git://127.0.0.1/pasteur
```

So we can start to launch the unikernel:

```sh
$ ./main.native
```

It listens into 4343, so you can open `http://127.0.0.1:4343` and see your Git repository feeded.

### Solo5 backend

A Solo5 backend of `pasteur` is possible with `mirage configure -t hvt`. However, you need to precise:
- IPv4 of your unikernel
- Gateway
- Git remote repository according your network topology

Imagine a bridge interface `br1` mounted to `10.0.0.1`, a possible configuration is to make
a TAP interface, to bridge it with `br1` and configure `pasteur` with:

```sh
$ mirage configure -t hvt --ipv4=10.0.0.3/24 --ipv4-gateway=10.0.0.1 -r git://10.0.0.1/pasteur
```

About the Git daemon, it should listen 10.0.0.1 with `--listen 10.0.0.1` and you can run the
unikernel with (Solo5 0.6.0):

```sh
$ solo5-hvt --net:service=tap100 pasteur.hvt
```

### Dunification

`duniverse` branch is a special branch which provides an unikernel and let us to compile it with `dune`
instead `ocamlbuild`. The way to get the unikernel is slightly complexe:

```sh
$ opam pin add dune-private-libs https://github.com/ocaml/dune
$ opam pin add dune-configurator https://github.com/ocaml/dune
$ opam pin add dune --dev
$ opam pin add duniverse https://github.com/avsm/duniverse
$ opam remove add mirage https://github.com/dinosaure/opam-overlays.git#mirage-from-dinosaure
$ opam install mirage
```

We currently need last version of `dune`, then we can compile `pasteur` with:

```sh
$ mirage configure -t hvt --ipv4=...
$ duniverse init --opam-remote=https://github.com/dinosaure/opam-overlays.git#mirage-from-dinosaure
$ duniverse opam-install
$ duniverse pull
$ mirage build
```

[http-af]: https://github.com/inhabitedtype/httpaf
[ocaml-git]: https://github.com/mirage/ocaml-git
[irmin]: https://github.com/mirage/irmin
[paste]: https://paste.isomorphis.me
