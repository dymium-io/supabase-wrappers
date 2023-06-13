#!/bin/bash

set -e

[ -z "$(hash yq 2>&1)" ] || {
    echo "Please install 'yq' utility:"
    echo "  brew install yq"
    exit 255
}

export DATABASE_HOST=docker.for.mac.host.internal
export DATABASE_USER='dymium'
export DATABASE_PASSWORD='dymiumP@ss'

export AUTH0_DOMAIN='https://dymium.us.auth0.com/'
export AUTH0_CLIENT_ID='oXX423SHkmtfR6qxrcJddAnneYffsdki'
export AUTH0_CLIENT_SECRET='oGLFxbQ5Jrl9luxzQvIpoNrd9TPTnDlqXTUq3LPEPHmmE06kRT4JLu2698fYi1C2'
export AUTH0_CALLBACK_URL='https://admin.dymium.local:3000/auth/callback'
export AUTH0_REDIRECT_URL='https://admin.dymium.local:3000/auth/redirect'
export AUTH0_RETURN_URL='https://admin.dymium.local:3000/logout'

export SESSION_SECRET="b<Qu9K#q}Ksh+R#JzlJJ@X1sHMp#UI@t&OCjDYTEmVe6WZe,rdJ}EI=4N[|yoTq"
export CUSTOMER_HOST='app.dymium.local'
export ADMIN_HOST='admin.dymium.local'

echo docker run --rm -m="0.5Gb" --cpus="0.5" --name dymium -p 90:80 \
    -e DATABASE_HOST=$DATABASE_HOST \
    -e DATABASE_USER=$DATABASE_USER \
    -e DATABASE_PASSWORD=$DATABASE_PASSWORD \
    -e AUTH0_DOMAIN=$AUTH0_DOMAIN \
    -e AUTH0_CLIENT_ID=$AUTH0_CLIENT_ID \
    -e AUTH0_CLIENT_SECRET=$AUTH0_CLIENT_SECRET \
    -e AUTH0_REDIRECT_URL=$AUTH0_REDIRECT_URL \
    -e AUTH0_RETURN_URL=$AUTH0_RETURN_URL \
    -e SESSION_SECRET=$SESSION_SECRET \
    -e CUSTOMER_HOST=$CUSTOMER_HOST \
    -e ADMIN_HOST=$ADMIN_HOST \
    dymium

docker run --rm -m="0.5Gb" --cpus="0.5" --name dymium -p 90:80 \
    -e DATABASE_HOST=$DATABASE_HOST \
    -e DATABASE_USER=$DATABASE_USER \
    -e DATABASE_PASSWORD=$DATABASE_PASSWORD \
    -e AUTH0_DOMAIN=$AUTH0_DOMAIN \
    -e AUTH0_CLIENT_ID=$AUTH0_CLIENT_ID \
    -e AUTH0_CLIENT_SECRET=$AUTH0_CLIENT_SECRET \
    -e AUTH0_REDIRECT_URL=$AUTH0_REDIRECT_URL \
    -e AUTH0_RETURN_URL=$AUTH0_RETURN_URL \
    -e SESSION_SECRET=$SESSION_SECRET \
    -e CUSTOMER_HOST=$CUSTOMER_HOST \
    -e ADMIN_HOST=$ADMIN_HOST \
    -e LOG_LEVEL=Debug \
    dymium
