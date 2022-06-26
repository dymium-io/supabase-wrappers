module dymium.com/dymium

require (
	aws v0.0.0-00010101000000-000000000000
	github.com/Jeffail/gabs v1.4.0
	github.com/NYTimes/gziphandler v1.1.1
	github.com/andybalholm/brotli v1.0.0
	github.com/aws/aws-sdk-go v1.44.27
	github.com/dgrijalva/jwt-go v3.2.0+incompatible
	github.com/disintegration/imaging v1.6.2
	github.com/evanphx/json-patch/v5 v5.0.0
	github.com/gavv/httpexpect v2.0.0+incompatible
	github.com/go-http-utils/etag v0.0.0-20161124023236-513ea8f21eb1
	github.com/go-session/session v3.1.2+incompatible
	github.com/gorilla/handlers v1.4.2
	github.com/gorilla/mux v1.7.3
	github.com/gorilla/pat v1.0.1
	github.com/gorilla/sessions v1.1.1
	github.com/itchio/go-brotli v0.0.0-20190702114328-3f28d645a45c
	github.com/lib/pq v1.2.0
	github.com/markbates/goth v1.59.0
	github.com/nsf/jsondiff v0.0.0-20210926074059-1e845ec5d249
	github.com/signintech/gopdf v0.9.8
	github.com/smartystreets/goconvey v1.6.4
	github.com/stretchr/testify v1.6.1
	github.com/tidwall/buntdb v1.1.0
	github.com/victorspringer/http-cache v0.0.0-20190721184638-fe78e97af707
	golang.org/x/crypto v0.0.0-20200622213623-75b288015ac9
	golang.org/x/net v0.0.0-20220127200216-cd36cc0744dd
	golang.org/x/oauth2 v0.0.0-20210220000619-9bb904979d93
	google.golang.org/api v0.40.0
)

require (
	cloud.google.com/go v0.78.0 // indirect
	github.com/ajg/form v1.5.1 // indirect
	github.com/coreos/go-oidc/v3 v3.1.0 // indirect
	github.com/davecgh/go-spew v1.1.1 // indirect
	github.com/evanphx/json-patch v0.5.2 // indirect
	github.com/fasthttp-contrib/websocket v0.0.0-20160511215533-1f3b11f56072 // indirect
	github.com/fatih/structs v1.1.0 // indirect
	github.com/go-http-utils/fresh v0.0.0-20161124030543-7231e26a4b27 // indirect
	github.com/go-http-utils/headers v0.0.0-20181008091004-fed159eddc2a // indirect
	github.com/go-redis/redis v6.15.6+incompatible // indirect
	github.com/google/go-querystring v1.0.0 // indirect
	github.com/gorilla/websocket v1.4.1 // indirect
	github.com/imkira/go-interpol v1.1.0 // indirect
	github.com/jmespath/go-jmespath v0.4.0 // indirect
	github.com/k0kubun/colorstring v0.0.0-20150214042306-9440f1994b88 // indirect
	github.com/mattn/go-colorable v0.1.4 // indirect
	github.com/moul/http2curl v1.0.0 // indirect
	github.com/pmezard/go-difflib v1.0.0 // indirect
	github.com/sergi/go-diff v1.0.0 // indirect
	github.com/snorwin/jsonpatch v1.4.0 // indirect
	github.com/tidwall/btree v0.0.0-20170113224114-9876f1454cf0 // indirect
	github.com/tidwall/gjson v1.3.2 // indirect
	github.com/tidwall/grect v0.0.0-20161006141115-ba9a043346eb // indirect
	github.com/tidwall/rtree v0.0.0-20180113144539-6cd427091e0e // indirect
	github.com/tidwall/tinyqueue v0.0.0-20180302190814-1e39f5511563 // indirect
	github.com/valyala/fasthttp v1.6.0 // indirect
	github.com/xeipuuv/gojsonschema v1.2.0 // indirect
	github.com/yalp/jsonpath v0.0.0-20180802001716-5cc68e5049a0 // indirect
	github.com/yudai/gojsondiff v1.0.0 // indirect
	github.com/yudai/golcs v0.0.0-20170316035057-ecda9a501e82 // indirect
	github.com/yudai/pp v2.0.1+incompatible // indirect
	go.opencensus.io v0.23.0 // indirect
	google.golang.org/genproto v0.0.0-20210225212918-ad91960f0274 // indirect
	google.golang.org/grpc v1.36.0 // indirect
	gopkg.in/antage/eventsource.v1 v1.0.0-20150318155416-803f4c5af225 // indirect
	gopkg.in/yaml.v3 v3.0.0-20200313102051-9f266ea9e77c // indirect
)

replace aws => ../../../libs/go/aws

go 1.18
