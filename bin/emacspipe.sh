#!/usr/bin/env bash

# pipe the output into a running emacs instance

set -o errexit
set -o pipefail

if [ -t 0 ]; then    #is standard in a tty?
    emacsclient "$@"
else # we're piping
    TMP="$(mktemp /tmp/stdin-XXX)"; # make temp file
    cat > $TMP;                     # pipe output into temp file
    emacsclient "$@" $TMP           # launch emacs with args
    rm $TMP;                        # delete temp file
fi
