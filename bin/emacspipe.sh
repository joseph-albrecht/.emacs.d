#!/usr/bin/env bash

# use emacs as a filter in a pipeline

if [ -t 0 ] && [ -t 1 ]; then #not piping
    emacsclient "$@"
else # we're piping
    TMP="$(mktemp /tmp/stdin-XXX)"
    cat > $TMP
    emacsclient --quiet "$@" $TMP
    cat $TMP
    rm $TMP
fi
