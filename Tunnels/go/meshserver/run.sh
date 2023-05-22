#!/bin/bash


#export PASSPHRASE='F00B00'
#export CERTIFICATE='{
#"key":"-----BEGIN RSA PRIVATE KEY-----\nProc-Type: 4,ENCRYPTED\nDEK-Info: DES-CBC,D8C4BF9B3E8B21F1\n\nBEaa+9iglZx771eV4yFOs2DGwZaQ4cWDyKV1J6m+EzaI0aheICwM7v/REkjmpFbz\nagsI0su/S/TiqQBBPEuQZxTn/xFTyGTCbcvMGveNxlpTvomVIFjjwTYNymWoF5u9\n1batq/3HtKuvevervF13V00g3pJPiqkO8lUIr6v8AOhGlsDcjKSGKYvflefRpLUn\n6NDNMjVcHLxbPQ14BCTmgJjZNSc1zp1jGqRNm6JiQ4P8fx1bQ+Np0p3jDzwyAsK7\nloA4NTSOWcF0e21sUwuXxAEbxkpvwchHowhWqaN5cE4mH0ixnjBUqysb/8W8n9LC\n28YJjibtJ/MlYxAYu25+YTTz4sT7az9KIYtIUgTuM53llWdUdEWBj6n9kPcvDi3E\n+JLb/2oiEyhhcA9+7GBQ9bGsr3YCOts2ZdS6btnMMjB4Cl5h6C/ZUPguQ7zbqoci\nKLorthSAmExA31PQoisGBNctyIH/esf1q9tftixopmTfX/cmYERW2Dl+a2dGtFwo\nuF1kR52jPRMtLx33WbBJiapbMw7l7YIoz4UA18EqAHuJBVbdBHzrscaK5uWYhZ5u\nktwakgmqpvZ+9CvXTccMB7rxHn92BASYYD9yXjD3qrdwoCmZEx9C71hjcyzqNFcK\nsf3myia15zPfOLOIQXG5Kqp+ObnZn1aDqCWJuHZuINeko9FwNR3JA1AuAgRyK8v6\nIiajRUQNMNqMoah7gc3AwU4LJ5qU4rJno1m4ZqjDDSAVZd0s++zAZByP0BobGzfq\nnAGhXomjL+WdX45tRGzSE921mxHRtd7zksuG1nRkiKopFkML9qqzkWhE7ULC3Wto\nOPoihKAa7Q/0b1RAHdgfihlMSxnqphdHrPI4klj/2Ka2uQ5vN8lphFr+HrmQg6W+\nFmHAOyKXCimamAk1GoREStBP8hdzkELht72qfQ+OSbgRqFBfZob4f+AQK56RnmW2\nksD6dPbJRnvczyMU7srruO/TrMrRaheQbEo23whJHy2kBqXdR5VbHRVCN+F9kqhN\ny62lVp0WEqJIhTT0Js5MnQtRTmJMdfzvaa6mxMe7Xar1/z1px9BapdewdzPepNVp\nmJlGkkmp+7UcaoQUuR7McuiHZi6jB5xyl2qgirZ4UOSL1P4ag3+qqvzfSM3/9IgA\nBMyBHtudi8kyoGpuTOy2IXVA3oynuPH90i1YPH4DsUzMyNmviig2lzReZTWR+8D/\nmEVN23Jhzd3jaLYK34B9hTY6UDrdq7ZH+ls0QPUs3NZMxYDM2xC7Yewetnn5g7LW\n9isKbkY4e+5dwaOyJORNTnGCMafx7Mebn0ZC7NG5p6k49xsoYcr+KWp7Uy5UdHpq\nxFbpinAgFhPNx+Kb08DwsP8QhlciU4VcHsGQQtqhroa8nYRRUdxLL9DT0BGT2NW/\n6kk+TIMQe35t1p9fT0ENW6rG3FV9nNbEiAysWfnIcTpIDusDyB8eJOhjKGIZTMYQ\nw7i59P1lpfsOMdEI9eIPBwrrvYpXJWuht1AP+aNxTVc5CLgpfBamwOIwBgyKv9W3\nxjWiMLwwcWPAcpw8eLqb51R1iZBMvM2LvhVZPB+oKwEAuNrIte/ANuw2k74vXsN2\n-----END RSA PRIVATE KEY-----", 
#"certificate":"-----BEGIN CERTIFICATE-----\nMIIEKzCCAxOgAwIBAgIUFjr31LsOmvcpPV48nk4XR8DWZ3QwDQYJKoZIhvcNAQEL\nBQAwgYsxCzAJBgNVBAYTAlVTMQswCQYDVQQIDAJDQTESMBAGA1UEBwwJU3Vubnl2\nYWxlMRIwEAYDVQQKDAlEeW1pdW0gSW8xFDASBgNVBAsMC0VuZ2luZWVyaW5nMRIw\nEAYDVQQDDAlEeW1pdW0gQ0ExHTAbBgkqhkiG9w0BCQEWDmlnb3JAZHltaXVtLmlv\nMB4XDTIyMDUwNDAxMjg1MloXDTI0MDgwNjAxMjg1MlowgYIxCzAJBgNVBAYTAlVT\nMQswCQYDVQQIDAJDQTESMBAGA1UEBwwJU3Vubnl2YWxlMQ8wDQYDVQQKDAZEeW1p\ndW0xDDAKBgNVBAsMA0VuZzEUMBIGA1UEAwwLKi5keW1pdW0udXMxHTAbBgkqhkiG\n9w0BCQEWDmlnb3JAZHltaXVtLmlvMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIB\nCgKCAQEAsq/0JV9xunOyJym08spVUgRgHaRb1KrIVlj+2Ryhp2WMquWsNL3DgDVE\nAVb6XImdMFy3Gw2mtKK81Z9Diib7M1Arb0vPvytkSxHFiDn/TwJ+KFdBeyBvBuRC\n55U2tWtLMRavtJlrO642SRZYrRpI1XvYeW3GUaUHU8XoD0rywU2gwZlu81DbMaDm\n1mUWQUfP9Ditr0nMhmGkHW4mqDhATaW0nX5f97nqfyvVQF8WpJmAc7DD65pbmQq4\n9N+XWdzQixBG7yjxsR2ObCgEIwQPySHgXTcf4B8b2bdKdLseY2dkWaWTpGAeVxef\nyjZeIbDFx9kqAkQucNuueCwvI8dXFwIDAQABo4GNMIGKMB8GA1UdIwQYMBaAFP26\nVYqIj5XuddULaRpkHLNz+GMXMAkGA1UdEwQCMAAwCwYDVR0PBAQDAgTwME8GA1Ud\nEQRIMEaCCyouZHltaXVtLnVzggsqLmR5bWl1bS5pb4IMKi5keW1pdW0ubmV0ggwq\nLmR5bWl1bS5jb22CDiouZHltaXVtLmxvY2FsMA0GCSqGSIb3DQEBCwUAA4IBAQCS\nyB6Qo6z+9ACCELmLqbDBV/XKNmCCY/lF76Yz3naBk4n4DNKUoH2rsmpzwxnrY/KO\nLTqzYniBcjEuZ/XmP3jrNATTmAqjo31LpleuHWRcOnvJUoNEPCH6EA+0iNQlYTKo\n0rG/d2zGbSOLjNlTJ6yfoaiUyJvpBc7qP+43Nk75xB8tACgWhSQOkHT04MKgDJEx\nmQCSVwRTyYcGMhd6anYyqshrvtdguDFWSiVcJWdQ6ZLwVbgbxyzrLNgYIp3OIAxb\nPuc9/hn1UefSW/6mV2wPPObQyCF+9an7dbIXJxtKYnnaPFMY1CU28fVUTG9x0Jk7\nT0fUkWCdTl7UXDWQniNc\n-----END CERTIFICATE-----\n" 
#}'

export PASSPHRASE=''
export CERTIFICATE='{
"key":"-----BEGIN RSA PRIVATE KEY-----\nMIIEpAIBAAKCAQEAsq/0JV9xunOyJym08spVUgRgHaRb1KrIVlj+2Ryhp2WMquWs\nNL3DgDVEAVb6XImdMFy3Gw2mtKK81Z9Diib7M1Arb0vPvytkSxHFiDn/TwJ+KFdB\neyBvBuRC55U2tWtLMRavtJlrO642SRZYrRpI1XvYeW3GUaUHU8XoD0rywU2gwZlu\n81DbMaDm1mUWQUfP9Ditr0nMhmGkHW4mqDhATaW0nX5f97nqfyvVQF8WpJmAc7DD\n65pbmQq49N+XWdzQixBG7yjxsR2ObCgEIwQPySHgXTcf4B8b2bdKdLseY2dkWaWT\npGAeVxefyjZeIbDFx9kqAkQucNuueCwvI8dXFwIDAQABAoIBAAvhIMUfTEtOB8kM\nv8foZbW3LsCpvaUs2XbMId1DRwf++1QdQs6a0xWB9Qx1wN1IFNgzQcUdqjiWHSHC\nndXgc5DcwpZ6nswh0WAgRVaiLHQPQV3jNyo9ZMQ7hqsow2NaR7xyuqmyDWwBUACZ\n6LlRZwgyXJjuEGI7K3qJTL0/iVYKe7NPdRe6skBFXJB84Yd0wgZ9NTIMv53ehQ1v\ntkD1e84o3tvn9CSBqUC7b4/DvS7hVyTu6wJHPynk0z1miC6S7ABOmo/AWkfDY/De\nTSfVYBZ83yrX9auWQzuhcEkpatUYc+V98VqbQMzATfs1hAtf10GjdDx52yy0o5lX\nlFL6bCkCgYEA3Bb3xExo2mlahNIs3/ppONyKkKbV7Rs1+TggJrVf+3TmJuD6+4V2\nZfvo8IbLO1BaT+J/J+GW1g41Hj9bwB54WMgi8dn1dlaE9jyw3oZEPKJFLKFsBjBs\n/fhfO9HZ9l/WW0+jDEuG9SVEZFLtgSjNMg5bvp498Y3RnStp5iMzGDsCgYEAz9eh\nHTcbHzhJY5ybRg/5eOCcrnBlNAy9225HYOpXMh/csg+L6LzlOnUAe10T8q13avRe\ny9rgXA1kFHAyQM9zl9QqckOBgguWrEPH4FS98wFv2PFfE5ox6/GjgTvlzGIWlpXq\n2+xoYl0uUeFtdApYpAfUJpduMqFcbPXZKS63qtUCgYBpzhjao6kcpt0/URMFsgp3\nrX5nv+zECfrOjq7WLQexiSdDtZCy7hxXNt1rFEILh7uC/+4FLCwpR7c9jVP8bimI\n9N3nytJ4S2usPM0CjSWsyI3cwNV/vDQJfCe8SEwyZyxTxVsrA/wjSgExAbTwDL9o\nuwJcBxLcV0NX/ZDxdCaSFQKBgQCQTpf3z+4N5CfYeMC6iZbeHZ3L5A/wbrRXqZ73\npF18WPED1JWAt760ss3t2XdjNJvIb/ltsv+6CjYllqzdoOwSS+ZYxZQi9ebcnocl\ns9Da63F8AELSOmpU9sZekXcBTE2AqoCRx/fF7AqgIVO8kP5wjVr4nIWPs8eXg68O\nz/gILQKBgQCRvTdkbhru+KXFJiuVgbWGna8wP9qmNvSY7XDbXHdJwewIvoE16+FQ\nYGcJqNFLYMiYApnOulc1SVLDxSNLxOjqWP6OCqWZ2CctDQ0lkYKxp2eDdtYJspJD\nDzX9xjjAfU0WpPAiKEm68JwYiMliN+YshAA/FBSak4Y4dVP2b8gjRg==\n-----END RSA PRIVATE KEY-----", 
"certificate":"-----BEGIN CERTIFICATE-----\nMIIEKzCCAxOgAwIBAgIUFjr31LsOmvcpPV48nk4XR8DWZ3QwDQYJKoZIhvcNAQEL\nBQAwgYsxCzAJBgNVBAYTAlVTMQswCQYDVQQIDAJDQTESMBAGA1UEBwwJU3Vubnl2\nYWxlMRIwEAYDVQQKDAlEeW1pdW0gSW8xFDASBgNVBAsMC0VuZ2luZWVyaW5nMRIw\nEAYDVQQDDAlEeW1pdW0gQ0ExHTAbBgkqhkiG9w0BCQEWDmlnb3JAZHltaXVtLmlv\nMB4XDTIyMDUwNDAxMjg1MloXDTI0MDgwNjAxMjg1MlowgYIxCzAJBgNVBAYTAlVT\nMQswCQYDVQQIDAJDQTESMBAGA1UEBwwJU3Vubnl2YWxlMQ8wDQYDVQQKDAZEeW1p\ndW0xDDAKBgNVBAsMA0VuZzEUMBIGA1UEAwwLKi5keW1pdW0udXMxHTAbBgkqhkiG\n9w0BCQEWDmlnb3JAZHltaXVtLmlvMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIB\nCgKCAQEAsq/0JV9xunOyJym08spVUgRgHaRb1KrIVlj+2Ryhp2WMquWsNL3DgDVE\nAVb6XImdMFy3Gw2mtKK81Z9Diib7M1Arb0vPvytkSxHFiDn/TwJ+KFdBeyBvBuRC\n55U2tWtLMRavtJlrO642SRZYrRpI1XvYeW3GUaUHU8XoD0rywU2gwZlu81DbMaDm\n1mUWQUfP9Ditr0nMhmGkHW4mqDhATaW0nX5f97nqfyvVQF8WpJmAc7DD65pbmQq4\n9N+XWdzQixBG7yjxsR2ObCgEIwQPySHgXTcf4B8b2bdKdLseY2dkWaWTpGAeVxef\nyjZeIbDFx9kqAkQucNuueCwvI8dXFwIDAQABo4GNMIGKMB8GA1UdIwQYMBaAFP26\nVYqIj5XuddULaRpkHLNz+GMXMAkGA1UdEwQCMAAwCwYDVR0PBAQDAgTwME8GA1Ud\nEQRIMEaCCyouZHltaXVtLnVzggsqLmR5bWl1bS5pb4IMKi5keW1pdW0ubmV0ggwq\nLmR5bWl1bS5jb22CDiouZHltaXVtLmxvY2FsMA0GCSqGSIb3DQEBCwUAA4IBAQCS\nyB6Qo6z+9ACCELmLqbDBV/XKNmCCY/lF76Yz3naBk4n4DNKUoH2rsmpzwxnrY/KO\nLTqzYniBcjEuZ/XmP3jrNATTmAqjo31LpleuHWRcOnvJUoNEPCH6EA+0iNQlYTKo\n0rG/d2zGbSOLjNlTJ6yfoaiUyJvpBc7qP+43Nk75xB8tACgWhSQOkHT04MKgDJEx\nmQCSVwRTyYcGMhd6anYyqshrvtdguDFWSiVcJWdQ6ZLwVbgbxyzrLNgYIp3OIAxb\nPuc9/hn1UefSW/6mV2wPPObQyCF+9an7dbIXJxtKYnnaPFMY1CU28fVUTG9x0Jk7\nT0fUkWCdTl7UXDWQniNc\n-----END CERTIFICATE-----\n" 
}'

export CA_CERTIFICATE='{
"certificate":"-----BEGIN CERTIFICATE-----\nMIID+TCCAuGgAwIBAgIUNyqJ0ErGc8YpUhbvKCDTiy8oYzUwDQYJKoZIhvcNAQEL\nBQAwgYsxCzAJBgNVBAYTAlVTMQswCQYDVQQIDAJDQTESMBAGA1UEBwwJU3Vubnl2\nYWxlMRIwEAYDVQQKDAlEeW1pdW0gSW8xFDASBgNVBAsMC0VuZ2luZWVyaW5nMRIw\nEAYDVQQDDAlEeW1pdW0gQ0ExHTAbBgkqhkiG9w0BCQEWDmlnb3JAZHltaXVtLmlv\nMB4XDTIyMDQyNzAwMjQxMloXDTI3MDQyNjAwMjQxMlowgYsxCzAJBgNVBAYTAlVT\nMQswCQYDVQQIDAJDQTESMBAGA1UEBwwJU3Vubnl2YWxlMRIwEAYDVQQKDAlEeW1p\ndW0gSW8xFDASBgNVBAsMC0VuZ2luZWVyaW5nMRIwEAYDVQQDDAlEeW1pdW0gQ0Ex\nHTAbBgkqhkiG9w0BCQEWDmlnb3JAZHltaXVtLmlvMIIBIjANBgkqhkiG9w0BAQEF\nAAOCAQ8AMIIBCgKCAQEA9LPaIV9FelNaWcjQ8oKj7tTWeQLjSaYsNMmIDVrGWCWp\nCDL29/9eRqs5Y/lI1bJiACFAhy+0o8Um0ZoNwvBW6zZ9IrdPpLmDJsbf9hPhAQIH\njKFMzWtxB/RLRMZ3YJTKNvV+J6lNz8bFe/xmPwlSp5Nct0h9D99W3Bvc+/vkqupH\nBqXzbGUiuhOvRi8xDp1z6OQ9ghmugDlf0AdTm69g3LSaX9l6P4g3ErH58xBLKZ5g\n8oHPvJ6IHW2gt3pzS/dziSIaVY0q0bRO3PRAab6qZ0MGUJ5XYW7EdjMllro+YPgr\ngDGLAIRhSITZkpMX2kZ7vWEakiglsm4aPm2zg058dwIDAQABo1MwUTAdBgNVHQ4E\nFgQU/bpVioiPle511QtpGmQcs3P4YxcwHwYDVR0jBBgwFoAU/bpVioiPle511Qtp\nGmQcs3P4YxcwDwYDVR0TAQH/BAUwAwEB/zANBgkqhkiG9w0BAQsFAAOCAQEAJdTh\nTgSmZ47QmgpnNv9L4rPD87SMwUzcx/6NCq4qwQ6UQsz4Ma95heOVS+gVa1q9qoFk\nibZVCqnXwYcUg89xwOcYR3gm5Ikaj2T6YIvIT1lOKri3rguJGCqQvIQ+K7/4q/Mi\nuyr4nWZz/rx08eRCcxD9eXlDKZYJYbajVeEkQk4Y0c7bC9LqxDnjSgwl7KMLRa/x\nB/GrSFysm2YYXZm508Zl3jtRtHLxxsEVoqgu2AJ4D2fHpy4aEcUZwjM7RdNsDQiI\nM73Zgp9IbaaKpEfxu5OzWu7bp6sfHmR2gwKNkdE6tjuR8V0co6UmrAZGeRzK4YqS\nWQfKT+rRTGPDAfdl/w==\n-----END CERTIFICATE-----" 
}'
export CUSTOMER=spoofcorp
export DATABASE_HOST=${DATABASE_HOST:-localhost}
export DATABASE_PORT=${DATABASE_PORT:-5432}
export DATABASE_NAME=${DATABASE_NAME:-dymium}
export DATABASE_USER=${DATABASE_USER:-dymium}
export DATABASE_TLS=${DATABASE_TLS:-disable}
export ADDRESS=localhost

[ -z "$DATABASE_PASSWORD" ] && {
    DATABASE_PASSWORD=$( grep "^$DATABASE_HOST:\\($DATABASE_PORT\\|[*]\\):[^:]*:$DATABASE_USER:" $HOME/.pgpass | cut -f 5 -d : )
}
export DATABASE_PASSWORD=$DATABASE_PASSWORD
export LOCAL_ENVIRONMENT=true

export LOG_LEVEL=Debug

export PORT=3009
export CUSTOMER_PORTAL=portal.dymium.local
export LOCAL_ENVIRONMENT=true
export LOCAL_SEARCH=${LOCAL_SEARCH:-}
export LOCAL_SEARCH_USER=${LOCAL_SEARCH_USER:-elastic}
export LOCAL_SEARCH_PASSWD=${LOCAL_SEARCH_PASSWD:-admin123}
export SEARCH_IN_PIPELINE=${SEARCH_IN_PIPELINE:-jsonmessage}

./meshserver 


