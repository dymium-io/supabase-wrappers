package log
import (
	"os"
	"github.com/apex/log"
	"github.com/apex/log/handlers/text"
	"github.com/apex/log/handlers/json"
	"github.com/apex/log/handlers/kinesis"
	"github.com/apex/log/handlers/multi"
)
var Log *log.Entry

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
var extra log.Fields


func clearTenant() {
	delete(extra, "tenant")
}
func clearAll() {
	delete(extra, "useremail")
	delete(extra, "groups")
	delete(extra, "roles")
	delete(extra, "tenant")
}
func setAll(user string, groups, roles []string, tenant, session string) {
	extra["useremail"] =  user
	extra["groups"] =  groups
	extra["roles"] =  roles
	extra["tenant"] = tenant
	extra["session"] = session
}
func Userf(f func(string, ...interface{}), tenant, session, user string, groups, roles []string, 
	format string, data ...interface{}) {
		setAll(user, groups, roles, tenant, session)
		if(len(data) == 0) {
			f(format)
		} else {
			f(format, data...)
		}
		clearAll()			
}

func UserArrayf(f func(string, ...interface{}), tenant, session, user string, groups, roles []string, 
	format string, arr []string, data ...interface{}) {
		setAll(user, groups, roles, tenant, session)
		extra["messageattach"] = arr
		if(len(data) == 0) {
			f(format)
		} else {
			f(format, data...)
		}
		clearAll()			
		delete(extra, "messageattach")
}

func Tenantf(f func(string, ...interface{}), tenant string, format string,  data ...interface{}) {
	extra["tenant"] = tenant
	f(format, data...)
	clearTenant()
}
func DebugTenantf(tenant, format string, data ...interface{}){
	Tenantf(Debugf, tenant, format, data...)
}
func InfoTenantf(tenant, format string, data ...interface{}){
	Tenantf(Infof, tenant, format, data...)
}
func WarnTenantf(tenant, format string, data ...interface{}){
	Tenantf(Warnf, tenant, format, data...)
}
func ErrorTenantf(tenant, format string, data ...interface{}){
	Tenantf(Errorf, tenant, format, data...)
}
func FatalTenantf(tenant, format string, data ...interface{}){
	Tenantf(Fatalf, tenant, format, data...)
}

func DebugUserf(tenant, session, user string, groups, roles []string, format string, data ...interface{}) {
	Userf(Debugf, tenant, session, user, groups, roles, format, data...)
}
func InfoUserf(tenant, session, user string, groups, roles []string, format string, data ...interface{}) {
	Userf(Infof, tenant, session, user, groups, roles, format, data...)
}
func WarnUserf(tenant, session, user string, groups, roles []string, format string, data ...interface{}) {
	Userf(Warnf, tenant, session, user, groups, roles, format, data...)
}
func ErrorUserf(tenant, session, user string, groups, roles []string, format string, data ...interface{}) {
	Userf(Errorf, tenant, session, user, groups, roles, format, data...)
}
func FatalUserf(tenant, session, user string, groups, roles []string, format string, data ...interface{}) {
	Userf(Fatalf, tenant, session, user, groups, roles, format, data...)
}

func DebugUserArrayf(tenant, session, user string, groups, roles []string, format string, arr []string, data ...interface{}) {
	UserArrayf(Debugf, tenant, session, user, groups, roles, format, arr, data...)
}
func InfoUserArrayf(tenant, session, user string, groups, roles []string, format string, arr []string, data ...interface{}) {
	UserArrayf(Infof, tenant, session, user, groups, roles, format, arr, data...)
}
func WarnUserArrayf(tenant, session, user string, groups, roles []string, format string, arr []string, data ...interface{}) {
	UserArrayf(Warnf, tenant, session, user, groups, roles, format, arr, data...)
}
func ErrorUserArrayf(tenant, session, user string, groups, roles []string, format string, arr []string, data ...interface{}) {
	UserArrayf(Errorf, tenant, session, user, groups, roles, format, arr, data...)
}
func FatalUserArrayf(tenant, session, user string, groups, roles []string, format string, arr []string, data ...interface{}) {
	UserArrayf(Fatalf, tenant, session, user, groups, roles, format, arr, data...)
}

func InfoArrayf(format string, arr []string, data ...interface{}) {
	extra["messageattach"] =  arr
	Infof(format, data...)
	delete(extra, "messageattach")
}

func Init(component string) {
	//log.SetHandler(json.New(os.Stderr))
	_, ok := os.LookupEnv("AWS_LAMBDAS")
	if(ok) {
		log.SetHandler( text.New(os.Stderr) )
	} else {
		log.SetHandler(multi.New(
			json.New(os.Stderr),
			kinesis.New("dymium-data-stream"),
		))
	
	}
	//log.SetHandler(logfmt.New(os.Stderr))

	extra = log.Fields{
		"component": component,
	}

	Log = log.WithFields(extra)

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
		if(err != nil) {
			Fatal(err.Error())
		}
	}
}