#!/bin/bash


[ -z "$(hash yq 2>&1)" ] || {
    echo "Please install 'yq' utility:"
    echo "  brew install yq"
    exit 255
}

SESSION_SECRET=150f273766a7649d2b35a1bdfadc76b7
DATABASE_HOST=docker.for.mac.host.internal
DATABASE_PASSWORD='U$erZ#itr0'



SESSION_SECRET=150f273766a7649d2b35a1bdfadc76b7

DATABASE_PASSWORD='U$erZ#itr0'
PRODUCT_SERVER='localhost:8032'

BORROWER_HOST='app.zeitro.ai'
ZEITRO_HOST='lo.zeitro.ai'
ADMIN_HOST='admin.zeitro.ai'
MOBILE_HOST='mobile.zeitro.ai'
LANDING_HOST='zeitro.ai'

FORM1003_SERVER='localhost:8910'
MISMO_SERVER='localhost:82'
ZBRAIN_SERVER='localhost:82'
STREAMLINE_SERVER='localhost:83'



docker run --rm -m="0.5Gb" --cpus="0.5" --name zeitro -p 90:80 \
       -e SESSION_SECRET=$SESSION_SECRET \
       -e DATABASE_HOST=$DATABASE_HOST \
       -e DATABASE_PASSWORD=$DATABASE_PASSWORD \
       -e BORROWER_HOST=$BORROWER_HOST \
       -e ZEITRO_HOST=$ZEITRO_HOST \
       -e ADMIN_HOST=$ADMIN_HOST \
       -e MOBILE_HOST=$MOBILE_HOST \
       -e LANDING_HOST=$LANDING_HOST \
       -e FORM1003_SERVER=$FORM1003_SERVER \
       -e MISMO_SERVER=$MISMO_SERVER \
       -e ZBRAIN_SERVER=$ZBRAIN_SERVER \
       -e STREAMLINE_SERVER=$STREAMLINE_SERVER \
       zeitro
