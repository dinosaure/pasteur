# Pasteur

Pasteur is an unikernel (MirageOS) which provides an mini-website to paste
snippets and save them into a Git repository. The goal of this project is to be
an example of: how to write a website as an unikernel today?

This example uses several stacks:
- HTTP with [http/af][http-af]
- Git with [ocaml-git][ocaml-git]
- Irmin with [irmin][irmin]
- SSH with [awa][awa]
- TLS with [ocaml-tls][ocaml-tls]
- TCP/IP of course with [mirage-tcpip][mirage-tcpip]
- JavaScript with [`js_of_ocaml`][js-of-ocaml] and [brr][brr]

Design comes from [paste.isomorphis.me][paste].

## How to use it?

MirageOS let user to choose which backend he wants. So we have 3 generals
backends:
- KVM (with Solo5)
- Unix
- Xen

Currently, the project is tested with the Unix backend - the most easy to
deploy - and Solo5.

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

And you builded the unikernel! However, this unikernel needs to communicate with
a remote Git repository. In an other shell, we need to make a new Git repository
with, at least, one commit in `master`:

```sh
$ mkdir pasteur
$ cd pasteur
$ git init --bare
$ git read-tree --empty
$ FIRST_COMMIT=`git commit-tree $(git write-tree) -m .`
$ git update-ref "refs/heads/master" $FIRST_COMMIT
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

It listens into 4343, so you can open `http://127.0.0.1:4343` and see your Git
repository be fed (and updated) by the server.

### Solo5 backend

A Solo5 backend of `pasteur` is possible with `mirage configure -t hvt`.
However, you need to precise:
- IPv4 of your unikernel
- Gateway
- Git remote repository according your network topology

Imagine a bridge interface `br1` mounted to `10.0.0.1`, a possible configuration
is to make a TAP interface, to bridge it with `br1` and configure `pasteur`
with:

```sh
$ mirage configure -t hvt --ipv4=10.0.0.3/24 --ipv4-gateway=10.0.0.1 -r git://10.0.0.1/pasteur
```

About the Git daemon, it should listen 10.0.0.1 with `--listen 10.0.0.1` and you
can run the unikernel with (Solo5 0.6.0):

```sh
$ solo5-hvt --net:service=tap100 pasteur.hvt
```

### Dockerfile & scripts

The current project (master) is available here: https://paste.x25519.net/

#### HTTP + TLS

Even if MirageOS can handle TLS natively with `ocaml-tls`, the process to deploy
a website with Let's Encrypt still is difficult. By this fact, the current
website uses:
- `jwilder/nginx-proxy`
- `jrcs/letsencrypt-nginx-proxy-companion`

To be able to deploy the website with a proper certificate.

#### Deployement

The repository provides a Dockerfile (hint: I'm not an expert about `docker`)
which does anything to be able to __compile__ the unikernel and be usable by
Solo5. Currently, the process to build & run is:

```sh
$ cd pasteur
$ ls
Dockerfile tap.sh run.sh ...
$ docker build -t pasteur .
$ docker run -it --privileged \
  --cap-add=NET_ADMIN \
  --device /usr/dev/tun:/usr/dev/tun \
  --device /dev/kvm:/dev/kvm \
  pasteur
# sudo chown opam:opam /dev/kvm
# ./tap.sh
# ./run.sh
```

`tap.sh` creates a TAP interface (you should be able to do that inside the
docker with `--device /usr/dev/tun:/usr/dev/tun`). It creates a bridge a plug
the initial `eth0` and the new `tap100` to the bridge. Then, it configures
correctly the bridge to communicate with the rest of the world.

At this stage, you still be able to keep your Git repository outside the docker
and give the right IP address - my current state is to ask to Git to listen on
10.0.0.1 where my `br1` is mounted.

`run.sh` collects then informations (like IP, gateway, etc.) and run the
compiled unikernel with Solo5 and good arguments. The Dockerfile still is
specific to my topology network and you should trick on it (mostly on `run.sh` I
think).

[http-af]: https://github.com/inhabitedtype/httpaf
[ocaml-git]: https://github.com/mirage/ocaml-git
[irmin]: https://github.com/mirage/irmin
[paste]: https://paste.isomorphis.me
