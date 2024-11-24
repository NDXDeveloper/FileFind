#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Linking /home/vero/Documents/code/Lazarus/FileFind/bin/release/x86_64-linux/filefind
OFS=$IFS
IFS="
"
/usr/bin/ld -b elf64-x86-64 -m elf_x86_64  --dynamic-linker=/lib64/ld-linux-x86-64.so.2  --gc-sections -s  -L. -o /home/vero/Documents/code/Lazarus/FileFind/bin/release/x86_64-linux/filefind -T /home/vero/Documents/code/Lazarus/FileFind/bin/release/x86_64-linux/link5633.res -e _start
if [ $? != 0 ]; then DoExitLink /home/vero/Documents/code/Lazarus/FileFind/bin/release/x86_64-linux/filefind; fi
IFS=$OFS
