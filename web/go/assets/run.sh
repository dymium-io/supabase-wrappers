#!/bin/bash

export DATABASE_HOST=localhost
export DATABASE_USER='dymium'
export DATABASE_PASSWORD='dymiumP@ss'


export AUTH0_ADMIN_DOMAIN='https://dymium-dev-admin.us.auth0.com/'
export AUTH0_ADMIN_CLIENT_ID='XiRxsWQLAvSSwLWEQ72hTvvhHoLaEBIE'
export AUTH0_ADMIN_CLIENT_SECRET='lNV1m9JARql55ade-5mjUESzM2kiLgZB5xnmsRcH2uw8urpE3ROCjeOINtsF2Te7'
export AUTH0_ADMIN_REDIRECT_URL='https://admin.dymium.local:3000/auth/redirect'
export AUTH0_ADMIN_RETURN_URL='https://admin.dymium.local:3000/app/logout'
export AUTH0_ADMIN_ORGANIZATION="org_Qqns7tW4acoBx9tc"



export SESSION_SECRET="b<Qu9K)q}Ksh+R)JzlJJ'X1sHMp$UI@t&OCjDYTEmVe6WZe,rdJ}!I=4N[|yoTq"
export CUSTOMER_HOST='app.dymium.local'
export ADMIN_HOST='admin.dymium.local'


./server

