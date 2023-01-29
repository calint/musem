#!/bin/sh
nasm osmo.s -o osmo.img
qemu-system-i386 osmo.img
