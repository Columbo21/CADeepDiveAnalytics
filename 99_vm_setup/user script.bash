#!/bin/bash
USERBLANK="ca-user"

max=10
for i in `seq 1 $max`
do
        sudo adduser --gecos "" --disabled-password $USERBLANK$i
done

for i in `seq 1 $max`
do
    echo "$USERBLANK$i:iloveca2019" | sudo chpasswd
done
