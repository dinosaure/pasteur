#!/bin/sh

INET_STR=`ip addr show dev eth0 | grep "inet\ " | awk '{print $2}'`
echo $INET_STR
CIDR=`echo $INET_STR | awk -F'/' '{print $2}'`
echo $CIDR
IP_ADDR=`echo $INET_STR | awk -F'/' '{print $1}'`
echo $IP_ADDR
GW=`ip route | grep ^default | awk '{print $3}'`
echo $GW

sudo ip tuntap add tap100 mode tap
sudo ip link set dev tap100 up

sudo ip addr del $INET_STR dev eth0
sudo ip link add name br0 type bridge
sudo ip link set eth0 master br0
sudo ip link set tap100 master br0
sudo ip link set br0 up
sudo ip addr add $INET_STR brd + dev br0
sudo route add default gw $GW dev br0
