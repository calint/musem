#!/bin/sh
# 2023-01-29:
#   built using NASM version 2.15.05
#   tested on QEMU emulator version 7.0.0 (Debian 1:7.0+dfsg-7ubuntu2.1)

nasm osmo.s -o osmo.img
qemu-system-i386 -m 1 osmo.img
# sudo dd if=osmo.img of=/dev/sda && sync
