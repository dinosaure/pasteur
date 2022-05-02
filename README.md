# Pasteur

Pasteur is an unikernel ([MirageOS][mirage]) which provides an mini-website to
paste snippets and save them into a Git repository. The goal of this project is
to be a full example of: how to write a website as an unikernel today?

This example uses several stacks:
- HTTP with [http/af][http-af] and [paf][paf]
- Git with [ocaml-git][ocaml-git]
- Irmin with [irmin][irmin]
- SSH with [awa][awa]
- TLS with [ocaml-tls][ocaml-tls]
- TCP/IP of course with [mirage-tcpip][mirage-tcpip]
- JavaScript with [`js_of_ocaml`][js-of-ocaml] and [brr][brr]
- a read-only filesystem with [docteur][docteur]

The project needs MirageOS 4 and it is available here: https://paste.osau.re/

## How to use it?

MirageOS lets user to choose which backend he/she wants. So we have 2 general
backends:
- Unix target (a simple executable as usual)
- KVM or Xen with [Solo5][solo5]

We will explain the Solo5 target which provides a full operating system which
can be executed into [KVM][kvm]. It needs few steps on your server to be able
to run it. We will use [albatross][albatross] which helps us to deploy an
unikernel.

### Solo5 target - a full Operating System

To be able to deploy an unikernel, you should follow this little tutorial about
`albatross` which describes how to install the albatross service and how to use
it: [Robur Reproducible Builds][robur-tutorial]

Then, we will play a bit with `iptables` to allow our unikernel to communicate
with Internet and let clients to communicate with our unikernel. When we want
to virtualize an operating system, we need to configure a _bridge_ on the host
system such as:
```
                               . [ vm00 ]
  [ eht0 ] <- NAT -> [ br0 ] -|
                               ` [ vm01 ]
```

The Robur's tutorial explain, on Linux, you can modify the file
`/etc/network/interface` and add a new bridge with a specific MAC address:
```
auto service
iface service inet manual
    up ip link add service-master address 02:00:00:00:00:01 type dummy
    up ip link set dev service-master up
    up ip link add service type bridge
    up ip link set dev service-master master service
    up ip addr add 10.0.42.1/24 dev service
    up ip link set dev service up
    down ip link del service
    down ip link del service-master
```

You can restart your system or restart your Network service. You can check the
creation of your bridge _via_ `sudo ip a` and you will your new `service`
bridge.

More concretely, when you want to launch a unikernel with `albatross`, the
Albatross daemon will create a [TAP][tap] interface and it will connect this
one to your bridge. Then, you just need to configure your NAT to allow
incoming and outcoming connections.

#### `iptables` and NAT configuration

In this situation, we will try to configure your NAT to allow incoming and
outcoming communications. But we need to allow IPv4 forwarding first:
```sh
user$ sudo su
root$ echo "1" > /proc/sys/net/ipv4/ip_forward
```

Finally, we can 1) let our host system to keep a table between our private IP
address and Internet (such as our unikernel is able to maintain a connection
with outside):
```sh
$ sudo iptables -t nat -A POSTROUTING -i eth0 -j MASQUERADE
```

Then, we will redirect incoming connection from our `<public-ip>:{80,443}`[^1]
to our private IP address of our unikernel (let's say `10.0.42.2`):
```sh
$ sudo iptables -t nat POSTROUTING -p tcp -m tcp --dport 80 -j DNAT \
  --to-destination 10.0.42.2:80
$ sudo iptables -t nat POSTROUTING -p tcp -m tcp --dport 443 -j DNAT \
  --to-destination 10.0.42.2:443
```

[^1]: We must redirect `*:80` and `*:443` because `pasteur` will do the HTTP
let's encrypt challenge. So Let's Encrypt should be able to communicate with
our unikernel via `*:80`.

**Warning**: depending on your `iptables`'s rules but sometimes, the `FORWARD`
chain is setted to `DROP`. Even if you follow this tutorial, any packets sent
by your unikernel will be lost - so you must update the `FORWARD` chain with
`ACCEPT`:
```sh
$ sudo iptables -A FORWARD -o service -m conntrack --ctstate \
  RELATED,ESTABLISHED -j ACCEPT
$ sudo iptables -A FORWARD -i service ! -o service -j ACCEPT
$ sudo iptables -A FORWARD -i service -o service -j ACCEPT
```

#### Git repository

As we said in introduction, we use a Git repository to store _pastes_. So we
need to configure a bit the host server to setting up a Git server. You can
follow this tutorial: [Git SCM - Setting up the server][git-scm-tutorial]

**Warning**: at some points, if you want to clone a Git repository, be sure
that you use the good branch in both sides. Indeed, for Debian, the _main_
branch still is `master` and your _main_ branch into your computer is
probably `main`.

Finally, on the MirageOS side, the easy way to communicate with a Git
repository is to generate a _in-the-fly_ RSA key, put it (as explained into
the tutorial) into the `.ssh/authorized_keys` and keep the _seed_ which will
be used by our unikernel:
```sh
$ opam install awa
$ awa_gen_key > awa.gen.key
$ tail -n1 awa.gen.key | ssh git@localhost 'cat >> ~/.ssh/authorized_keys'
$ head -n1 awa.gen.key
seed is svIdyO8boxNQP03NrUgfwDxk4iPaURukAyKl1cx8
```

#### Build & launch the unikernel

Finally, we can build and launch our unikernel:
```sh
$ git clone https://github.com/dinosaure/pasteur
$ cd pasteur
$ opam install mirage
$ mirage configure -t hvt
$ make depends
$ mirage build
```

The distribution builds 2 artifacts:
- the unikernel `dist/pasteur.hvt`
- an external image as a read-only filesystem `dist/disk`

We need to add a new block device from our `dist/disk` and launch our
unikernel:
```sh
$ albatross-client-local create_block --data dist/disk pasteur 1
$ albatross-client-local create \
  --mem=512 --net=service:service --block=disk:pasteur dist/pasteur.hvt \
  --arg='--remote git@10.0.42.1:pasteur.git' \
  --arg='--ssh-key=rsa:svIdyO8boxNQP03NrUgfwDxk4iPaURukAyKl1cx8' \
  --arg='--hostname <HOSTNAME>' \
  --arg='--https=true' \
  --arg='--ipv4=10.0.42.2/24' \
  --arg='--ipv4-gateway=10.0.42.1'
```

As you can see, we set the `--ssh-key` with our seed given by `awa_gen_key`
and we set a `<HOSTNAME>` which will be our hostname used by our unikernel - be
sure that your DNS is well configured to your public IP address.

At the boot, `pasteur` will do the Let's encrypt challenge and if it can get
the let's encrypt certificate, it will launch an HTTPS server.

**Warning**: the JavaScript script which encrypt/decrypt contents on the client
side requires an HTTPS connection! That mostly means that if you _access_ to
the Pasteur website without TLS, the script does not work!

### Another way to deploy `pasteur`

Pasteur is fully configurable about:
- the Git repository: the user can use a local or a remote Git repository. The
  underlying protocol used to clone/pull/push can be SSH (as we did) or TCP/IP
  _via_ the `git://` URL or _via_ HTTP(S)
- The unikernel can launch a simple HTTP server without a TLS certificate _via_
  `--https=false` - and let something else such as `nginx` or
  [`contruno`][contruno] to handle certificates
- To test your deployement, you can ask a **non-productive** certificate - and
  be able to launch your unikernel several times without the Let's encrypt
  limit _via_ the `--production=false` option
- You are able to set the CSS if you want, you just need to update
  `public/pastisserie.css` and regenerate the image _via_ `docteur`:
  `docteur.make file://$(pwd)/public dist/disk`

[robur-tutorial]: https://robur.coop/Projects/Reproducible_builds
[git-scm-tutorial]: https://git-scm.com/book/en/v2/Git-on-the-Server-Setting-Up-the-Server
[contruno]: https://github.com/dinosaure/contruno
[mirage]: https://mirage.io/
[http-af]: https://github.com/inhabitedtype/httpaf
[paf]: https://github.com/dinosaure/paf-le-chien
[ocaml-git]: https://github.com/mirage/ocaml-git
[irmin]: https://github.com/mirage/irmin
[awa]: https://github.com/mirage/awa-ssh
[ocaml-tls]: https://github.com/mirleft/ocaml-tls
[mirage-tcpip]: https://github.com/mirage/mirage-tcpip
[js-of-ocaml]: https://github.com/ocsigen/js_of_ocaml
[brr]: https://github.com/dbuenzli/brr
[docteur]: https://github.com/dinosaure/docteur
[solo5]: https://github.com/Solo5/solo5
[kvm]: https://www.linux-kvm.org/page/Main_Page
[albatross]: https://github.com/roburio/albatross
