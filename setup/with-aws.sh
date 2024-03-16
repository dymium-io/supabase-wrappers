#!/usr/bin/env bash

set -e

case "$1" in
    '--profile' | '-profile')
	prf="$2"
	shift 2
	;;
    *)
	prf='dymium-dev'
	;;
esac

eval $(sed -n -e '/^\['"$prf"'\]/,/\[/!d' -e '/^\[/d; /^ *$/d; s/^\([^ ]*\) *= *\(.*\)$/\1='"'"'\2'"'"';/;p' $HOME/.aws/credentials)
export AWS_ACCESS_KEY_ID="${aws_access_key_id}"
export AWS_SECRET_ACCESS_KEY="${aws_secret_access_key}"
export AWS_SESSION_TOKEN="${aws_session_token}"
"$@"
