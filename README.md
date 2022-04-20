# Pasteur

Pasteur is an unikernel (MirageOS) which provides an mini-website to paste
snippets and save them into a Git repository. The goal of this project is to be
an example of: how to write a website as an unikernel today?

This example uses several stacks:
- HTTP with [http/af][http-af] and [paf][paf]
- Git with [ocaml-git][ocaml-git]
- Irmin with [irmin][irmin]
- SSH with [awa][awa]
- TLS with [ocaml-tls][ocaml-tls]
- TCP/IP of course with [mirage-tcpip][mirage-tcpip]
- JavaScript with [`js_of_ocaml`][js-of-ocaml] and [brr][brr]

Design comes from [paste.isomorphis.me][paste].

## How to use it?

MirageOS lets user to choose which backend he wants. So we have 2 general
backends:
- Unix target (a simple executable as usual)
- KVM or Xen with [Solo5][solo5]

### The unix backend

The unix backend is the easiest way to deploy pasteur. It does not require
exotic processes and it requires, at least if you want to use `*:443` or `*:80`,
the `sudo` access. An extra step is needed: we need to compile the client-side
of `pasteur` with `js_of_ocaml` (so, yes, the entire _unikernel_ is in OCaml!):

```sh
$ sudo apt install opam
$ opam init
$ opam install mirage brr
$ git clone https://github.com/dinosaure/pasteur
$ cd pasteur
$ cd js
$ dune build ./pasteur.js
$ cp pasteur.js ../public/pasteur.js
$ cd ..
$ mirage configure
$ make depends
$ mirage build
```

Pasteur has several arguments:
- *req* `--remote` is the Git remote repository (we are able to pull/push on it)
- *req* `-h`/`--hostname` is the _hostname_ of the unikernel
- *opt* `-p`/`--port` is the port of the webserver
- *opt* `--https` if you want to start an HTTP server with TLS
  in that case, the unikernel will do the let's encrypt challenge according
  to some others arguments
- *opt* `--dns-key`/`--dns-port`/`--dns-addr`: only if you want to do a DNS
  challenge. In that context, you should take a look on
  [Deploying an authoritative OCaml DNS server with MirageOS][mirage-dns]
- *opt* `--email`/`--cert-seed`/`--account-seed`/`--production`: only if
  you want to do a HTTP challenge. In that context, the unikernel must run under
  the given `hostname`. It will launch an HTTP server (on `*:80*`) and do the
  challenge then.
- *opt* `--ssh-seed` is the seed to re-generate the private RSA key used to
  pull/push to the given Git repository.
- *opt* `--ssh-auth` is the fingerprint of the SSH server which allows the Git
  synchronization
  
#### Synchronization with Git via SSH

Pasteur gives your several mechanisms to be synchronized to a Git repository.
You can use a GitHub repository or a local one with few extra parameters. Let's
start with a GitHub respository.

The most easy way to synchronize pasteur with GitHub is SSH. About that, you
must create a new private RSA key:
```sh
$ awa_gen_key
seed is XXX
ssh-rsa ,,, awa@awa.local
```

The first line is the seed to be able to reproduce the private RSA key with the
fortuna random generator. Keep this seed somewhere. The second line is the public
RSA key which should be paste to your GitHub account (on authorized SSH keys).

Then, we can accept only an SSH connection to GitHub, we will try to get the
_fingerprint_ of GitHub:
```sh
$ ssh-keyscan git-server > /tmp/ssh.pk
$ ssh-keygen -l -E sha256 -f /tmp/ssh.pk
2048 SHA256:YYY git-server (RSA)
```

The fingerprint is `SHA256:...`. Now, we are able to communicate with GitHub:
```sh
$ ./pasteur --ssh-seed XXX --ssh-auth=SHA256:YYY --remote git@github.com:username/repo.git
```

#### Synchronization with Git via TCP/IP

We can decide to use our own Git server. It needs few tweak to be able to push on it:
```sh
$ mkdir git-pasteur
$ cd git-pasteur
$ git init --bare --shared
$ git config receive.denyCurrentBranch ignore
$ git read-tree --empty
$ FIRST_COMMIT=`git commit-tree $(git write-tree) -m .`
$ git update-ref "refs/heads/main" $FIRST_COMMIT
$ cd ..
```

Then, we are able to launch a Git server
```sh
$ git daemon --base-path=. --export-all --reuseaddr --informative-errors --verbose --enable=receive-pack
```

On the other side, pasteur can be launched with:
```sh
$ ./pasteur -r git://localhost/git-pasteur
```

*NOTE*: such way is not really compatible with Solo5! Indeed, the `git daemon`
must be launched on a specific IP which can communicate with the _unikernel_.
Depending on your network topology, `--listen` is needed.

#### Synchronization with Git via HTTP/HTTPS

Currently this way does not really work when we don't have a way to pass a
_token_ via HTTP/HTTPS. It should not be difficult to implement it!

#### Let's encrypt challenge

If you launch pasteur with `--https`, the _unikernel_ will try to do the let's
encrypt challenge. Depending on which argument you give to us, we are able to do
2 kinds of challenge:
- over DNS
- over HTTP

The first one is may be more complex. Indeed, it requires a primary DNS server
(which serves your `--hostname`) and a way to communicate with it. However, it
fits well with another unikernel: [a primary DNS server with
MirageOS][mirage-dns].

So if you want to have an other unikernel, you should follow the tutorial. At
the end, you will have enough informations such as:
- the DNS IP address and its port (usually `*:53`)
- your DNS key

*NOTE*: `--production` has no meaning if you use the DNS challenge. Indeed, only
the `letsencrypt` _unikernel_ has this information.

##### Let's encrypt challenge via HTTP

Otherwise, pasteur can do the challenge via HTTP. However, the unikernel must
run behind your `--hostname`. In other words, if you want:
```sh
$ ./pasteur --hostname zero.bin --https
```

The _unikernel_ must be accessible on `*:80` (so, with privileges) via
`http://zero.bin/`. If it's not the case, let's encrypt will be not able to
create the TLS certificate. You can do some tries with `--production=false`
before.

You can pass some others informations via HTTP such as:
- `--cert-seed`
- `--account-seed`
- `--email`

### The solo5 backend

Of course, the main purpose of MirageOS is the ability to create a full operating system. About that,
you can use the Solo5 backend. You just need to reconfigure the _unikernel_ with:
```sh
$ mirage configure -t hvt
$ make depends
$ mirage build
```


 
For most of them, they depends on your context. For example, if your Git
repository is accessible via SSH, you should define some arguments such as:

- `--ssh-seed` available with `awa` (used by pasteur) and `awa_gen_key`

- `--ssh-auth` available with (on Linux):

```sh
$ ssh-keyscan git-server > /tmp/ssh.pk
$ ssh-keygen -l -E sha256 -f /tmp/ssh.pk
2048 SHA256:... git-server (RSA)
```

We are interested only by `SHA256:...` as the authenticator of your server
`git-server`. Of course, the private key (generated by `awa_gen_key`) must be
allowed by your git server. `awa_gen_key` gives you the public-key which should
be added to your git server.

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

And you builded the unikernel! However, this unikernel needs to communicate with
a remote Git repository. In an other shell, we need to make a new Git repository
with, at least, one commit in `main`:

```sh
$ mkdir pasteur
$ cd pasteur
$ git init --bare
$ git read-tree --empty
$ FIRST_COMMIT=`git commit-tree $(git write-tree) -m .`
$ git update-ref "refs/heads/main" $FIRST_COMMIT
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
