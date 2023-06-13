package log

import (
	"crypto/tls"
	"github.com/apex/log/handlers/es"
	"github.com/apex/log/handlers/text"
	"net/http"
	"os"
	"time"

	"github.com/apex/log"
	"github.com/apex/log/handlers/json"
	"github.com/apex/log/handlers/kinesis"
	"github.com/apex/log/handlers/multi"
)

var gComponent string
var Debugf func(string, ...interface{})
var Infof func(string, ...interface{})
var Warnf func(string, ...interface{})
var Errorf func(string, ...interface{})
var Fatalf func(string, ...interface{})

var Debug func(string)
var Info func(string)
var Warn func(string)
var Error func(string)
var Fatal func(string)
var Panic func(error)

/*
var Debugf func(string, ...interface{})
var Infof func(string, ...interface{})
var Warnf func(string, ...interface{})
var Errorf func(string, ...interface{})
var Fatalf func(string, ...interface{})

var Debug func(string)
var Info func(string)
var Warn func(string)
var Error func(string)
var Fatal func(string)
var Panic func(error)
*/

func DebugTenantf(tenant, format string, data ...interface{}) {
	extra := log.Fields{
		"component": gComponent,
		"tenant":    tenant,
	}
	Log := log.WithFields(extra)
	if len(data) == 0 {
		Log.Debugf(format)
	} else {
		Log.Debugf(format, data...)
	}
}
func InfoTenantf(tenant, format string, data ...interface{}) {
	extra := log.Fields{
		"component": gComponent,
		"tenant":    tenant,
	}
	Log := log.WithFields(extra)
	if len(data) == 0 {
		Log.Infof(format)
	} else {
		Log.Infof(format, data...)
	}
}
func WarnTenantf(tenant, format string, data ...interface{}) {
	extra := log.Fields{
		"component": gComponent,
		"tenant":    tenant,
	}
	Log := log.WithFields(extra)
	if len(data) == 0 {
		Log.Warnf(format)
	} else {
		Log.Warnf(format, data...)
	}
}
func ErrorTenantf(tenant, format string, data ...interface{}) {
	extra := log.Fields{
		"component": gComponent,
		"tenant":    tenant,
	}
	Log := log.WithFields(extra)
	if len(data) == 0 {
		Log.Errorf(format)
	} else {
		Log.Errorf(format, data...)
	}
}
func FatalTenantf(tenant, format string, data ...interface{}) {
	extra := log.Fields{
		"component": gComponent,
		"tenant":    tenant,
	}
	Log := log.WithFields(extra)
	if len(data) == 0 {
		Log.Fatalf(format)
	} else {
		Log.Fatalf(format, data...)
	}
}

func DebugTenantArrayf(tenant, format string, arr []string, data ...interface{}) {
	extra := log.Fields{
		"component":     gComponent,
		"tenant":        tenant,
		"messageattach": arr,
	}
	Log := log.WithFields(extra)
	if len(data) == 0 {
		Log.Fatalf(format)
	} else {
		Log.Fatalf(format, data...)
	}
}
func InfoTenantArrayf(tenant, format string, arr []string, data ...interface{}) {
	extra := log.Fields{
		"component":     gComponent,
		"tenant":        tenant,
		"messageattach": arr,
	}
	Log := log.WithFields(extra)
	if len(data) == 0 {
		Log.Infof(format)
	} else {
		Log.Infof(format, data...)
	}
}
func ErrorTenantArrayf(tenant, format string, arr []string, data ...interface{}) {
	extra := log.Fields{
		"component":     gComponent,
		"tenant":        tenant,
		"messageattach": arr,
	}
	Log := log.WithFields(extra)
	if len(data) == 0 {
		Log.Errorf(format)
	} else {
		Log.Errorf(format, data...)
	}
}

func DebugUserf(tenant, session, user string, groups, roles []string, format string, data ...interface{}) {
	extra := log.Fields{
		"component": gComponent,
		"tenant":    tenant,
		"useremail": user,
		"groups":    groups,
		"roles":     roles,
		"session":   session,
	}

	Log := log.WithFields(extra)
	if len(data) == 0 {
		Log.Debugf(format)
	} else {
		Log.Debugf(format, data...)
	}
}
func InfoUserf(tenant, session, user string, groups, roles []string, format string, data ...interface{}) {
	extra := log.Fields{
		"component": gComponent,
		"tenant":    tenant,
		"useremail": user,
		"groups":    groups,
		"roles":     roles,
		"session":   session,
	}

	Log := log.WithFields(extra)
	if len(data) == 0 {
		Log.Infof(format)
	} else {
		Log.Infof(format, data...)
	}
}
func WarnUserf(tenant, session, user string, groups, roles []string, format string, data ...interface{}) {
	extra := log.Fields{
		"component": gComponent,
		"tenant":    tenant,
		"useremail": user,
		"groups":    groups,
		"roles":     roles,
		"session":   session,
	}

	Log := log.WithFields(extra)
	if len(data) == 0 {
		Log.Warnf(format)
	} else {
		Log.Warnf(format, data...)
	}
}
func ErrorUserf(tenant, session, user string, groups, roles []string, format string, data ...interface{}) {
	extra := log.Fields{
		"component": gComponent,
		"tenant":    tenant,
		"useremail": user,
		"groups":    groups,
		"roles":     roles,
		"session":   session,
	}

	Log := log.WithFields(extra)
	if len(data) == 0 {
		Log.Errorf(format)
	} else {
		Log.Errorf(format, data...)
	}
}
func FatalUserf(tenant, session, user string, groups, roles []string, format string, data ...interface{}) {
	extra := log.Fields{
		"component": gComponent,
		"tenant":    tenant,
		"useremail": user,
		"groups":    groups,
		"roles":     roles,
		"session":   session,
	}

	Log := log.WithFields(extra)
	if len(data) == 0 {
		Log.Fatalf(format)
	} else {
		Log.Fatalf(format, data...)
	}
}

func DebugUserArrayf(tenant, session, user string, groups, roles []string, format string, arr []string, data ...interface{}) {
	extra := log.Fields{
		"component":     gComponent,
		"tenant":        tenant,
		"useremail":     user,
		"groups":        groups,
		"roles":         roles,
		"session":       session,
		"messageattach": arr,
	}

	Log := log.WithFields(extra)
	if len(data) == 0 {
		Log.Debugf(format)
	} else {
		Log.Debugf(format, data...)
	}
}
func InfoUserArrayf(tenant, session, user string, groups, roles []string, format string, arr []string, data ...interface{}) {
	extra := log.Fields{
		"component":     gComponent,
		"tenant":        tenant,
		"useremail":     user,
		"groups":        groups,
		"roles":         roles,
		"session":       session,
		"messageattach": arr,
	}

	Log := log.WithFields(extra)
	if len(data) == 0 {
		Log.Infof(format)
	} else {
		Log.Infof(format, data...)
	}
}
func WarnUserArrayf(tenant, session, user string, groups, roles []string, format string, arr []string, data ...interface{}) {
	extra := log.Fields{
		"component":     gComponent,
		"tenant":        tenant,
		"useremail":     user,
		"groups":        groups,
		"roles":         roles,
		"session":       session,
		"messageattach": arr,
	}

	Log := log.WithFields(extra)
	if len(data) == 0 {
		Log.Warnf(format)
	} else {
		Log.Warnf(format, data...)
	}
}
func ErrorUserArrayf(tenant, session, user string, groups, roles []string, format string, arr []string, data ...interface{}) {
	extra := log.Fields{
		"component":     gComponent,
		"tenant":        tenant,
		"useremail":     user,
		"groups":        groups,
		"roles":         roles,
		"session":       session,
		"messageattach": arr,
	}

	Log := log.WithFields(extra)
	if len(data) == 0 {
		Log.Errorf(format)
	} else {
		Log.Errorf(format, data...)
	}
}
func FatalUserArrayf(tenant, session, user string, groups, roles []string, format string, arr []string, data ...interface{}) {
	extra := log.Fields{
		"component":     gComponent,
		"tenant":        tenant,
		"useremail":     user,
		"groups":        groups,
		"roles":         roles,
		"session":       session,
		"messageattach": arr,
	}

	Log := log.WithFields(extra)
	if len(data) == 0 {
		Log.Fatalf(format)
	} else {
		Log.Fatalf(format, data...)
	}
}

func InfoArrayf(format string, arr []string, data ...interface{}) {
	extra := log.Fields{
		"component":     gComponent,
		"messageattach": arr,
	}

	Log := log.WithFields(extra)
	if len(data) == 0 {
		Log.Infof(format)
	} else {
		Log.Infof(format, data...)
	}
}

// For logsupervisor added few pass-through
func addMetadata(tenant, session, user string, extra log.Fields) log.Fields {
	extra["tenant"] = tenant
	extra["session"] = session
	extra["user"] = user
	return extra
}

func InfofCollector(tenant, session, user string, extra log.Fields, msg string) {
	extra = addMetadata(tenant, session, user, extra)
	Log := log.WithFields(extra)
	Log.Infof(msg)
}
func DebugfCollector(tenant, session, user string, extra log.Fields, msg string) {
	extra = addMetadata(tenant, session, user, extra)
	Log := log.WithFields(extra)
	Log.Debugf(msg)
}
func WarnfCollector(tenant, session, user string, extra log.Fields, msg string) {
	extra = addMetadata(tenant, session, user, extra)
	Log := log.WithFields(extra)
	Log.Warnf(msg)
}
func ErrorfCollector(tenant, session, user string, extra log.Fields, msg string) {
	extra = addMetadata(tenant, session, user, extra)
	Log := log.WithFields(extra)
	Log.Errorf(msg)
}
func FatalfCollector(tenant, session, user string, extra log.Fields, msg string) {
	extra = addMetadata(tenant, session, user, extra)
	Log := log.WithFields(extra)
	Log.Fatalf(msg)
}
func PanicCollector(tenant, session, user string, extra log.Fields, msg string) {
	extra = addMetadata(tenant, session, user, extra)
	Log := log.WithFields(extra)
	Log.Fatalf(msg)
}

func Init(component string) {
	//log.SetHandler(json.New(os.Stderr))
	_, ok := os.LookupEnv("LOCAL_ENVIRONMENT")
	if ok || component == "connector" { // this is a hack
		searchUrl, ok := os.LookupEnv("LOCAL_SEARCH")
		if ok && len(searchUrl) > 0 {
			// TODO - we should decide how we are going to connect/auth users for local search
			user, _ := os.LookupEnv("LOCAL_SEARCH_USER")
			passwd, _ := os.LookupEnv("LOCAL_SEARCH_PASSWD")
			pipeline, _ := os.LookupEnv("SEARCH_IN_PIPELINE")
			esClient := es.NewClient(searchUrl)
			esClient.SetAuthCredentials(user, passwd)
			defaultTransport := http.DefaultTransport.(*http.Transport)

			// Create new Transport that ignores self-signed SSL
			customTransport := &http.Transport{
				Proxy:                 defaultTransport.Proxy,
				DialContext:           defaultTransport.DialContext,
				MaxIdleConns:          defaultTransport.MaxIdleConns,
				IdleConnTimeout:       defaultTransport.IdleConnTimeout,
				ExpectContinueTimeout: defaultTransport.ExpectContinueTimeout,
				TLSHandshakeTimeout:   defaultTransport.TLSHandshakeTimeout,
				TLSClientConfig:       &tls.Config{InsecureSkipVerify: true},
			}
			esClient.HTTPClient = &http.Client{
				Timeout:   5 * time.Second,
				Transport: customTransport,
			}

			esh := es.New(&es.Config{
				Client:     esClient,
				BufferSize: 1,
				Format:     "devlogs-06-01-02",
				Pipeline:   pipeline,
			})
			log.SetHandler(esh)
		} else {
			log.SetHandler(text.New(os.Stderr))
		}
	} else {
		log.SetHandler(multi.New(
			json.New(os.Stderr),
			kinesis.New("dymium-data-stream"),
		))

	}
	loglevel, ok := os.LookupEnv("LOG_LEVEL")
	if ok {
		log.SetLevelFromString(loglevel)
	}
	//log.SetHandler(logfmt.New(os.Stderr))
	gComponent = component

	extra := log.Fields{
		"component": component,
	}
	Log := log.WithFields(extra)
	Debugf = Log.Debugf
	Infof = Log.Infof
	Warnf = Log.Warnf
	Errorf = Log.Errorf
	Fatalf = Log.Fatalf
	Debug = Log.Debug
	Info = Log.Info
	Warn = Log.Warn
	Error = Log.Error
	Fatal = Log.Fatal
	Panic = func(err error) {
		if err != nil {
			Fatal(err.Error())
		}
	}
}
