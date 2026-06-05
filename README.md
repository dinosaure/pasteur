# Pasteur, a zero-knowledge pastebin as an unikernel in OCaml

Pasteur is an unikernel which provides a mini-website to paste snippets and save
them onto a block device. The goal of this project is to be a full example of:
how to write a website as a [Solo5][solo5] unikernel today?

This version is built directly on the [robur.coop][robur] effect-based stack
(no MirageOS tooling anymore - a plain `dune build` is enough):
- the [miou][miou] scheduler and the [mkernel][mkernel] runtime for Solo5
- the [mnet][mnet] TCP/IP stack (with [utcp][utcp])
- the [vif][vif] web framework (`vifu`) and [mhttp][mhttp]/[h1][h1] for HTTP
- a FAT32 filesystem on a block device with [mfat][mfat]
- client-side encryption with [`js_of_ocaml`][js-of-ocaml] and [brr][brr]
  (the Web Crypto API)

The project needs OCaml `>= 5.3.0`. A live instance is available here:
https://paste.osau.re/

## How it works

Pasteur is a single Solo5 `hvt` unikernel that declares two devices (see
`manifest.json`):
- a network interface named **`service`** (`NET_BASIC`)
- a block device named **`pasteur`** (`BLOCK_BASIC`), formatted as FAT32, where
  the snippets are stored

The unikernel serves **plain HTTP** (default port `80`). It does *not*
terminate TLS itself. The snippets are encrypted/decrypted **in the browser**
through the Web Crypto API (`crypto.subtle`). We must point out that the Web
Crypto API is only available in a *secure context* (`localhost` or via a TLS
connection). However, Pasteur **does not** provide a server with TLS. Users can
add the unikernel’s IP address as a trusted source in their web browser or (and
this is likely what will be required in production) deploy the unikernel behind
a TLS-terminating reverse proxy (such as [contruno][contruno], `nginx` with
`certbot` or [Caddy][caddy]).


## Building the unikernel

The whole build is driven by `dune`. The `dune-workspace` declares a `solo5`
cross-compilation context, so a plain build produces the `hvt` binary:

```sh
$ git clone https://github.com/dinosaure/pasteur
$ cd pasteur
# fetch the (pinned) sources of the robur-coop stack into ./vendors
$ ./source.sh
$ dune build --profile=release
```

The convenience `Makefile` wraps this and gives you a ready-to-deploy artifact
named `pasteur.hvt`:

```sh
$ make            # produces ./pasteur.hvt (stripped)
$ file pasteur.hvt
pasteur.hvt: ELF 64-bit LSB executable, x86-64, ... interpreter /nonexistent/solo5/ ...
```

Alternatively, you can let OPAM build & install it for you (it runs `make all`
under the hood and installs `pasteur.hvt` into `$(opam var bin)`):

```sh
$ opam install .
$ file $(opam var bin)/pasteur.hvt
```

## Preparing the block device

The snippets are stored on the `pasteur` block device, which must be a **FAT32**
image. The simplest way is to create an empty file and format it with the
standard `mkfs.fat` tool (from `dosfstools`, available everywhere):

```sh
# a 50 MiB image
$ truncate -s 50M disk.img            # or: dd if=/dev/zero of=disk.img bs=1M count=50
$ mkfs.fat -F 32 disk.img
```

If you prefer to stay within the OCaml toolchain, `mfat` can format it too
(`-s` is the number of 512-byte sectors, so `100000` ≈ 50 MiB):

```sh
$ opam install mfat
$ mfat make disk.img -s 100000
Formatted disk.img: 100000 sectors (51200000 bytes), FAT32
```

`disk.img` is the file you will hand over to the unikernel as its `pasteur`
block device in every deployment method below. Its content (the saved pastes)
persists across reboots.

## Runtime arguments

The unikernel binary takes its configuration on the command line:

| Argument                        | Default       | Description                                          |
| ------------------------------- | ------------- | ---------------------------------------------------- |
| `--ipv4=<CIDR>`                 | *(required)*  | IPv4 address of the unikernel, e.g. `10.0.0.2/24`.   |
| `--ipv4-gateway=<IPv4>`         | *(none)*      | IPv4 gateway, e.g. `10.0.0.1`.                       |
| `--ipv6=<IPv6\|eui64\|random>`  | `eui64`       | IPv6 configuration.                                  |
| `-p`, `--port=<PORT>`           | `80`          | HTTP port the unikernel listens on.                  |
| `-m`, `--max=<SIZE>`            | `65535`       | Maximum snippet size (accepts `64KiB`, `1MiB`, ...). |
| `--length=<LENGTH>`             | `4`           | Length of the generated snippet IDs.                 |
| `--title=<STRING>`              | `Pasteur`     | Title shown on the front page.                       |
| `--subtitle=<STRING>`           | `past-isserie`| Subtitle shown on the front page.                    |
| `--auth=<USER:PASSWORD>`        | *(none)*      | Protect the whole service with HTTP basic auth.      |
| `--color=<always\|never\|auto>` | `auto`        | Colorise the logs.                                   |
| `-l <REGEXP>`, `-v`             | -             | Log filtering / verbosity (see `--help`).            |

The two device *names*, `service` (net) and `pasteur` (block), are fixed by
`manifest.json` and must match whatever you wire up on the host side.

---

# Deployment

Three ways are described below, from the most "production-ready" to the most
"hands-on":

1. with [albatross][albatross] - the recommended way to run Solo5 unikernels;
2. with [aussi][aussi] - an OCI runtime, to drive unikernels from Docker;
3. directly with `solo5-hvt` (and `tmux`) - handy for local testing.

The unikernel binary is the same in all cases; what differs is how the host
provides its `service` network interface and `pasteur` block device.

## Host networking (albatross & solo5-hvt)

albatross and the bare `solo5-hvt` tender attach the unikernel to a host
*bridge*; you therefore need that bridge, plus IPv4 forwarding + NAT so the
guest can reach (and be reached from) the network. (The Docker method below
does *not* need this — aussi/Docker set the networking up for you.)

```
                                   . [ pasteur ]
  [ eth0 ] <- NAT -> [ service ] -|
                                   ` [ ... ]
```

```sh
$ sudo sysctl -w net.ipv4.ip_forward=1
$ sudo ip link add name service type bridge
$ sudo ip addr add 10.0.0.1/24 dev service
$ sudo ip link set service up
# NAT outgoing traffic and allow forwarding (replace eth0 with your uplink)
$ sudo iptables -t nat -A POSTROUTING -s 10.0.0.0/24 -o eth0 -j MASQUERADE
$ sudo iptables -A FORWARD -i service -o eth0 -j ACCEPT
$ sudo iptables -A FORWARD -i eth0 -o service -m state --state RELATED,ESTABLISHED -j ACCEPT
```

If you want the website reachable from the outside on `:80`, also DNAT the
incoming traffic to the unikernel's private address (here `10.0.0.2`):

```sh
$ sudo iptables -t nat -A PREROUTING -i eth0 -p tcp --dport 80 -j DNAT \
  --to-destination 10.0.0.2:80
```

> **Note**: remember to terminate TLS in front of the unikernel (the in-browser
> encryption requires a secure context). A common setup is to publish `:443`
> with a reverse-proxy that forwards cleartext HTTP to `10.0.0.2:80`.

## 1. With albatross

[albatross][albatross] is a daemon dedicated to deploying Solo5 unikernels. It
takes care of creating the TAP interface and attaching it to your bridge. Follow
the [Robur tutorial][robur-tutorial] to install the `albatrossd` service first.

Once the daemon is running and the `service` bridge exists, register the block
device from the FAT image we prepared and launch the unikernel:

```sh
# import disk.img as an albatross block named "pasteur" (size in MB)
$ albatross-client create-block --data disk.img pasteur 50

# launch the unikernel
$ albatross-client create \
  --mem=64 \
  --net=service:service \
  --block=pasteur:pasteur \
  pasteur pasteur.hvt \
  --arg='--ipv4=10.0.0.2/24' \
  --arg='--ipv4-gateway=10.0.0.1'
```

- `--net=service:service` maps the unikernel's `service` interface (left) to the
  host `service` bridge (right).
- `--block=pasteur:pasteur` maps the unikernel's `pasteur` block (left) to the
  albatross block we just created (right).

Inspect and follow it with:

```sh
$ albatross-client info
$ albatross-client console pasteur
```

## 2. With aussi (through Docker)

[aussi][aussi] is an OCI-compatible runtime for Solo5 `hvt` unikernels. Once
registered with Docker, it lets you build and run the unikernel as a regular
container image: aussi allocates the TAP interface and wires up the networking
for you.

```sh
$ opam pin add https://github.com/robur-coop/aussi
```

Register `aussi` as a Docker runtime:

```sh
$ cat >/etc/docker/daemon.json<<EOF
{
  "runtimes": {
    "solo5": { "path": "$(opam var bin)/aussi" }
  }
}
EOF
$ systemctl restart docker
```

Build an image that ships the unikernel plus a `solo5.json` using the `docker`
network type (aussi wires the container's `eth0` to the guest), and bakes the
FAT image in as the `pasteur` block:

**Dockerfile**:
```Dockerfile
FROM ocaml/opam:debian-12-ocaml-5.4 AS builder
USER root
RUN apt-get update && apt-get install -y --no-install-recommends \
  pkg-config m4 build-essential libgmp-dev libseccomp-dev \
  && rm -rf /var/lib/apt/lists/*
USER opam
RUN opam update && opam install -y solo5 ocaml-solo5 mfat
RUN git clone https://github.com/dinosaure/pasteur pasteur
WORKDIR /home/opam/pasteur
RUN opam pin -yn .
RUN opam install --deps-only pasteur
RUN opam exec -- make all
RUN opam exec -- mfat make disk.img -s 100000

FROM scratch
COPY --from=builder /home/opam/pasteur/pasteur.hvt /pasteur.hvt
COPY --from=builder /home/opam/pasteur/disk.img /disk.img
COPY solo5.json /solo5.json
ENTRYPOINT ["/pasteur.hvt"]
```

**solo5.json**:
```json
{
  "version": 1,
  "type": "solo5.config",
  "mem": 64,
  "nets":   { "service": { "type": "docker", "iface": "eth0" } },
  "blocks": { "pasteur": { "path": "/disk.img" } },
  "argv": [
    "--ipv4=%{solo5.net.service.ip}",
    "--ipv4-gateway=%{solo5.net.service.gw}",
    "--color=always"
  ]
}
```

And you can run these commands where above files are:
```sh
$ docker build -t pasteur .
$ docker run -d --runtime=solo5 -p 8080:80 pasteur
$ curl http://localhost:8080/
```

## 3. Directly with solo5-hvt (and tmux)

For local testing you can skip any orchestrator and run the `hvt` tender
yourself. Create a TAP interface, attach it to the `service` bridge, then start
the unikernel. The `--net:` and `--block:` flags use the *device names* from
`manifest.json`:

```sh
$ sudo ip tuntap add tap0 mode tap
$ sudo ip link set tap0 master service
$ sudo ip link set tap0 up

$ sudo solo5-hvt \
  --net:service=tap0 \
  --block:pasteur=disk.img \
  pasteur.hvt \
  --ipv4=10.0.0.2/24 \
  --ipv4-gateway=10.0.0.1
```

`solo5-hvt` runs in the foreground, so use **`tmux`** to keep it alive after you
disconnect:

```sh
$ tmux new -s pasteur        # start a session
# ... run the solo5-hvt command above ...
# detach with: Ctrl-b d
$ tmux attach -t pasteur     # come back later to see the logs
```

You can now reach the unikernel at `http://10.0.0.2/`. To stop it, re-attach to
the tmux session and `Ctrl-c` the tender (or `tmux kill-session -t pasteur`).

## Customising the look

The CSS and front-page assets are embedded in the binary at build time (see the
`mcrunch` rules in `dune`). To change the theme, edit `pastisserie.css` /
`highlight.css` and rebuild the unikernel.

[solo5]: https://github.com/Solo5/solo5
[robur]: https://robur.coop/
[albatross]: https://github.com/robur-coop/albatross
[aussi]: https://github.com/robur-coop/aussi
[robur-tutorial]: https://robur.coop/Projects/Reproducible_builds
[contruno]: https://git.robur.coop/robur/contruno
[miou]: https://github.com/robur-coop/miou
[mkernel]: https://github.com/robur-coop/mkernel
[mnet]: https://github.com/robur-coop/mnet
[utcp]: https://github.com/robur-coop/utcp
[mfat]: https://github.com/robur-coop/mfat
[mhttp]: https://github.com/robur-coop/mhttp
[h1]: https://github.com/robur-coop/ocaml-h1
[vif]: https://github.com/robur-coop/vif
[js-of-ocaml]: https://github.com/ocsigen/js_of_ocaml
[brr]: https://github.com/dbuenzli/brr
[caddy]: https://caddyserver.com/
[certbot]: https://certbot.eff.org/
