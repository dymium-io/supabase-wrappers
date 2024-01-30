#!/bin/bash

export DATABASE_HOST=${DATABASE_HOST:-localhost}
export DATABASE_PORT=${DATABASE_PORT:-5432}
export DATABASE_USER=${DATABASE_USER:-dymium}
export DATABASE_ADMIN_USER=postgres
export DATABASE_TLS=${DATABASE_TLS:-disable}
export MESH_PORT_RANGE=30000-30001
export AUTH0_ADMIN_DOMAIN='https://dymium-dev-admin.us.auth0.com/'
export AUTH0_ADMIN_CLIENT_ID='XiRxsWQLAvSSwLWEQ72hTvvhHoLaEBIE'
export AUTH0_ADMIN_CLIENT_SECRET='lNV1m9JARql55ade-5mjUESzM2kiLgZB5xnmsRcH2uw8urpE3ROCjeOINtsF2Te7'
export AUTH0_ADMIN_REDIRECT_URL='https://admin.dymium.local:3000/auth/redirect'
export AUTH0_ADMIN_RETURN_URL='https://admin.dymium.local:3000/app/logout'
export AUTH0_ADMIN_AUDIENCE='https://admin.dymium.local/api/handler'
export AUTH0_ADMIN_ORGANIZATION="org_Qqns7tW4acoBx9tc"


export AUTH0_PORTAL_DOMAIN='https://dymium-dev.us.auth0.com/'
export AUTH0_PORTAL_CLIENT_ID='cTUANDkvlhLCQmxJb8NSEWdJIwXa82Wc'
export AUTH0_PORTAL_CLIENT_SECRET='It6h0TJrhPgF7TksPmS79DwsXnA5MTUoOtWjt_34VWJzB_XoiYEeuShFVK0b2klF'
export AUTH0_PORTAL_REDIRECT_URL='https://portal.dymium.local:3001/auth/redirect'
export AUTH0_PORTAL_RETURN_URL='https://portal.dymium.local:3001/app/logout'
export AUTH0_PORTAL_AUDIENCE='https://portal.dymium.local/api/handler'


export SESSION_SECRET="b<Qu9K)q}Ksh+R)JzlJJ'X1sHMp$UI@t&OCjDYTEmVe6WZe,rdJ}!I=4N[|yoTq"
export CUSTOMER_HOST='portal.dymium.local'
export ADMIN_HOST='admin.dymium.local'
export FILESYSTEM_ROOT='../../assets/'
export AWS_LAMBDAS='{ "DbAnalyzer": "localhost:9080",
                      "DbSync": "localhost:9081"
                    }'
export MESH_PORT_RANGE='30000-30064'
export CA_PASSPHRASE='TestKeyB00'
export CA_AUTHORITY='{
"key":"-----BEGIN RSA PRIVATE KEY-----\nProc-Type: 4,ENCRYPTED\nDEK-Info: DES-EDE3-CBC,D6CC3696D497E328\n\nRCEZ1dsgqJiPBEl2NwoiwnR/Go2Xi42FVb141kKDuxYDBEse5Uc5Zgtz1PHF3cSh\nz5Heeyj+w/54jZw0b+Ht1ZcKTIYMWq87uHRvNnMz9/RRvGtBn9GGHSxezzvfdGfq\nY4E0LYccTpnlBjQuj4+F1vpsXk8tqASdqf9o9zTisb80sfo9OnXRElDWeJnhAaXM\nKMdnZsm50IJ7dtJH4XuPec+GmZjXrJDGjbPUfdAiY8c9Kp4I6eh+zaR8qCX2CnJW\n6vw9zfQX2eseKiPOtwQ33IB0fNiCl76y9oKyzDZjRY+v6HTjqo6ssqVhHxeCRzbQ\nNdFudionkcKyAQp3iP91aLRqeY6/Rsm5PWxcPccq1n0Ag+8ccKzMNQ0ioTdC3JMh\n+dU8yvq+rdSHRU2MDbnDxcKgr4/pmvl1CWQagO04U/3gmVuQ24Jzs7aDiH2sNt9C\n+KOe3pYgUD4/4dBcdn9sYgqkthOAqusLxGS7VHC4sIcI0gZS7JiKLQqWw4MFYgHV\nRvVcnM8EdcB9DIXHw9asbGl4e8zrgzu/MOFJmBtfucX3XNkEg0hXW5A4o3FHFNA4\nJok9chcN1utZTx7aMSilpiCU0PP2pPYm74rPPBvQLArEhXa00sETe/1YtnNajSnl\nIwh0NdRMFq6uonFa9WfQDVkeedgDAakTQmENrEGTcp21+mCMIX9EHqE3EYLQhW/m\nLYYT19cGLNDXiY4FN4XnKne2ncISIT/ESBlxE20XcQRSBdwKdznuA3K4Lolr7AMI\n7/ORSOlQlSNVrLyqxWA3duPLhOQ9KMKBMoR2yGW7AibTBbMxODPC1xBuY3vsLsj0\nqjtvoljvKTQjqIybCy1ZEVxLErcIjQ5n9yhteaa+Ah21VvA6Lee/QIu4ofwRjv+C\nQuT2VhbwuTEgPctArcv4EEwHHjk25c1vDXHAkop/Lxxw3xBN2UcDJ3oI2QBCM9LA\nUzfbsxkuL/tBbvWFH5BylwDAuGKrF/CzRJffX+q/DDuLjWqHYy/mlPM32vbzGMDi\nty1zQIaCE64k4UQXSZENffbpx5oTaQXtkowbh6w0SyInydYFr3LX5NdFWqkA4U4+\nh+y+sdpMiYNnW10T8KL/0YhRXoNABxdp4dO7VfN0qFOSW+0NSokdcIbT77nCrrYa\nUhumsVjfzGRIyvi/nlPwF4ezN+cFp+R+H1unNW+NUfo6lEvYn21qsJL4PJfU9XUx\n5LK7IEzrrqs5dqkBmT79nlX7qy+n1BFfZJhW8cZX0ZzjW12nAgT1pbrOXJnpJRRM\ntDCKe9IOjMmQJfGiv92pW3clYCD0r5MTyKueWJYh0321EOvUACd+Plmy+MPjHfUv\nRvWOyPKXOH3pXMSHIy4VSDqHEmr1mxSYx+4wAmSXM+WuHxxeIRrPk9MLsEOr75tr\ny6LCQaI6OTuui7lljvjfM7838WdYhBnRoQBt0me4k7fsAegHAaoUzHnpLx5Lf2QT\nuxwm2tyLtEaN/YtEcEgcWaJxPCqf6yB5x9RfHLdZHcUWCkKHTTwdSuxBfwGeavMQ\nZsFQXVgFnPPWR5+dGvd+TFLuE4H5E6ucPoAFV7O9hyYNU1UQVebsg3gdvNRD2UF3\n-----END RSA PRIVATE KEY-----", 
"certificate":"-----BEGIN CERTIFICATE-----\nMIID+TCCAuGgAwIBAgIUNyqJ0ErGc8YpUhbvKCDTiy8oYzUwDQYJKoZIhvcNAQEL\nBQAwgYsxCzAJBgNVBAYTAlVTMQswCQYDVQQIDAJDQTESMBAGA1UEBwwJU3Vubnl2\nYWxlMRIwEAYDVQQKDAlEeW1pdW0gSW8xFDASBgNVBAsMC0VuZ2luZWVyaW5nMRIw\nEAYDVQQDDAlEeW1pdW0gQ0ExHTAbBgkqhkiG9w0BCQEWDmlnb3JAZHltaXVtLmlv\nMB4XDTIyMDQyNzAwMjQxMloXDTI3MDQyNjAwMjQxMlowgYsxCzAJBgNVBAYTAlVT\nMQswCQYDVQQIDAJDQTESMBAGA1UEBwwJU3Vubnl2YWxlMRIwEAYDVQQKDAlEeW1p\ndW0gSW8xFDASBgNVBAsMC0VuZ2luZWVyaW5nMRIwEAYDVQQDDAlEeW1pdW0gQ0Ex\nHTAbBgkqhkiG9w0BCQEWDmlnb3JAZHltaXVtLmlvMIIBIjANBgkqhkiG9w0BAQEF\nAAOCAQ8AMIIBCgKCAQEA9LPaIV9FelNaWcjQ8oKj7tTWeQLjSaYsNMmIDVrGWCWp\nCDL29/9eRqs5Y/lI1bJiACFAhy+0o8Um0ZoNwvBW6zZ9IrdPpLmDJsbf9hPhAQIH\njKFMzWtxB/RLRMZ3YJTKNvV+J6lNz8bFe/xmPwlSp5Nct0h9D99W3Bvc+/vkqupH\nBqXzbGUiuhOvRi8xDp1z6OQ9ghmugDlf0AdTm69g3LSaX9l6P4g3ErH58xBLKZ5g\n8oHPvJ6IHW2gt3pzS/dziSIaVY0q0bRO3PRAab6qZ0MGUJ5XYW7EdjMllro+YPgr\ngDGLAIRhSITZkpMX2kZ7vWEakiglsm4aPm2zg058dwIDAQABo1MwUTAdBgNVHQ4E\nFgQU/bpVioiPle511QtpGmQcs3P4YxcwHwYDVR0jBBgwFoAU/bpVioiPle511Qtp\nGmQcs3P4YxcwDwYDVR0TAQH/BAUwAwEB/zANBgkqhkiG9w0BAQsFAAOCAQEAJdTh\nTgSmZ47QmgpnNv9L4rPD87SMwUzcx/6NCq4qwQ6UQsz4Ma95heOVS+gVa1q9qoFk\nibZVCqnXwYcUg89xwOcYR3gm5Ikaj2T6YIvIT1lOKri3rguJGCqQvIQ+K7/4q/Mi\nuyr4nWZz/rx08eRCcxD9eXlDKZYJYbajVeEkQk4Y0c7bC9LqxDnjSgwl7KMLRa/x\nB/GrSFysm2YYXZm508Zl3jtRtHLxxsEVoqgu2AJ4D2fHpy4aEcUZwjM7RdNsDQiI\nM73Zgp9IbaaKpEfxu5OzWu7bp6sfHmR2gwKNkdE6tjuR8V0co6UmrAZGeRzK4YqS\nWQfKT+rRTGPDAfdl/w==\n-----END CERTIFICATE-----" 
}'
export LOCAL_ENVIRONMENT=true
export SPOOFCORP_KEY="6874AB957AA1F505EC6ACC84162B131FA5513558BB64ACEF294388AE6ECDA9C9"

function getPassword () {
    host="$1"
    port="$2"
    user="$3"
    password=$( grep "^$host:\\($port\\|[*]\\):[^:]*:$user:" $HOME/.pgpass | cut -f 5 -d : )
    [ -z "$password" ] && {
	echo "DATABASE_PASSWORD for $user in $host:$port not defined"
	exit
    }
    echo "$password"
}

[ -z "$DATABASE_PASSWORD" ] && {
    DATABASE_PASSWORD=$(getPassword "$DATABASE_HOST" "$DATABASE_PORT" "$DATABASE_USER")
}

[ -z "$DATABASE_ADMIN_PASSWORD" ] && {
    DATABASE_ADMIN_PASSWORD=$(getPassword "$DATABASE_HOST" "$DATABASE_PORT" "$DATABASE_ADMIN_USER")
}
export DATABASE_PASSWORD DATABASE_ADMIN_PASSWORD

go test -v ./...


