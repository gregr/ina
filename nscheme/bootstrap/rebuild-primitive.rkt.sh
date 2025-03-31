#!/bin/bash
set -eufo pipefail
here="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

racket "$here/run-file.rkt" "$here/rebuild-primitive.rkt.scm"
