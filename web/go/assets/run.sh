#!/bin/bash

export DATABASE_HOST=localhost
export DATABASE_USER='dymium'
export DATABASE_PASSWORD='dymiumP@ss'


export AUTH0_DOMAIN='https://dymium.us.auth0.com/'
export AUTH0_CLIENT_ID='oXX423SHkmtfR6qxrcJddAnneYffsdki'
export AUTH0_CLIENT_SECRET='oGLFxbQ5Jrl9luxzQvIpoNrd9TPTnDlqXTUq3LPEPHmmE06kRT4JLu2698fYi1C2'
export AUTH0_CALLBACK_URL='https://admin.dymium.local:3000/auth/callback'
export AUTH0_REDIRECT_URL='https://admin.dymium.local:3000/auth/redirect'
export AUTH0_RETURN_URL='https://admin.dymium.local:3000/logout'

export SESSION_SECRET="b<Qu9K)q}Ksh+R)JzlJJ'X1sHMp$UI@t&OCjDYTEmVe6WZe,rdJ}!I=4N[|yoTq"
export CUSTOMER_HOST='app.dymium.local'
export ADMIN_HOST='admin.dymium.local'


./server

