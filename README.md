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

Currently, the project is tested with the Unix backend - the most easy to deploy.

### Unix backend

First, you need to have the MirageOS tool:

```sh
$ opam install mirage
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

[http-af]: https://github.com/inhabitedtype/httpaf
[ocaml-git]: https://github.com/mirage/ocaml-git
[irmin]: https://github.com/mirage/irmin
[paste]: https://paste.isomorphis.me
