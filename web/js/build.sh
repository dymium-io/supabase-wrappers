#!/bin/bash
yarn install
cd packages/contrib/react-bootstrap-table-next
yarn install
yarn build
cd ../react-bootstrap-table2-paginator
yarn install
yarn build
cd ../react-bootstrap-table2-toolkit
yarn install
yarn build
cd ../../portal
yarn install
yarn build
cd ../admin
yarn install
yarn build