#!/usr/bin/env bash

# pipe the output of a process into an emacs buffer
# choose the name of the buffer

BUFFER_NAME=$1
TMP="$(mktemp /tmp/stdin-XXX)";
cat >$TMP;
emacsclient --eval "(open-from-terminal \"$TMP\" \"$BUFFER_NAME\")"
rm $TMP;
