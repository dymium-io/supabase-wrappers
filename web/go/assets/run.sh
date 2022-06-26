#!/bin/bash

export DATABASE_HOST=${DATABASE_HOST:-localhost}
export DATABASE_PORT=${DATABASE_PORT:-5432}
export DATABASE_NAME=${DATABASE_NAME:-test}
export DATABASE_USER=${DATABASE_USER:-dymium}
export DATABASE_TLS=${DATABASE_TLS:-disable}

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

export AWS_LAMBDAS='{ "DbAnalyzer": "localhost:9080",
                      "DbSync": "localhost:9081"
                    }'

./server


