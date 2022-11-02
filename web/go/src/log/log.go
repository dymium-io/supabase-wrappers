package log
import (
	"os"
	"github.com/apex/log"
	"github.com/apex/log/handlers/text"
	_ "github.com/apex/log/handlers/logfmt"
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
func setAll(user string, groups, roles []string, tenant string) {
	extra["useremail"] =  user
	extra["groups"] =  groups
	extra["roles"] =  roles
	extra["tenant"] = tenant
}
func Userf(f func(string, ...interface{}), tenant, user string, groups, roles []string, 
	format string, data ...interface{}) {
		setAll(user, groups, roles, tenant)
		if(len(data) == 0) {
			f(format)
		} else {
			f(format, data...)
		}
		clearAll()			
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

func DebugUserf(tenant, user string, groups, roles []string, format string, data ...interface{}) {
	Userf(Debugf, tenant, user, groups, roles, format, data...)
}
func InfoUserf(tenant, user string, groups, roles []string, format string, data ...interface{}) {
	Userf(Infof, tenant, user, groups, roles, format, data...)
}
func WarnUserf(tenant, user string, groups, roles []string, format string, data ...interface{}) {
	Userf(Warnf, tenant, user, groups, roles, format, data...)
}
func ErrorUserf(tenant, user string, groups, roles []string, format string, data ...interface{}) {
	Userf(Errorf, tenant, user, groups, roles, format, data...)
}
func FatalUserf(tenant, user string, groups, roles []string, format string, data ...interface{}) {
	Userf(Fatalf, tenant, user, groups, roles, format, data...)
}

func Init() {
	log.SetHandler(text.New(os.Stderr))
	//log.SetHandler(logfmt.New(os.Stderr))

	extra = log.Fields{
		"component": "webserver",
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