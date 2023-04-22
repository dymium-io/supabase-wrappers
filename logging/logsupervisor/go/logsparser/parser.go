package logsparser

import (
	"context"
	"encoding/csv"
	"encoding/json"
	"errors"
	"github.com/looplab/fsm"
	"reflect"
	"regexp"
	"strconv"
	"strings"
	"time"
	"unsafe"
)

// ParserFSM - process log messages. Behaviour is defined by FSM
// Should be created with an appropriate definition of FSM for expected input
// Ex.: for stdout - p:=ParserFSM{initParserStdout()}
type ParserFSM struct {
	FSM *fsm.FSM
}

func ParserInterface(parserFSM *fsm.FSM) *ParserFSM {
	return &ParserFSM{FSM: parserFSM}
}

func InitParserFSM(fsm *fsm.FSM) *ParserFSM {
	return &ParserFSM{FSM: fsm}
}

// EventType - event types for FSM
type EventType int

const (
	newEvent = iota
	continueEvent
	flushEvent
	unknownEvent
	flush
)

func (e EventType) eventName() string {

	switch e {
	case newEvent:
		return "newEvent"
	case continueEvent:
		return "continueEvent"
	case flushEvent:
		return "flushEvent"
	case unknownEvent:
		return "unknownEvent"
	}
	return "unknownEvent"
}

var timeLayouts = []string{
	"2006-01-02 15:04:05.000 MST",
	"01/02 03:04:05PM '06 -0700",          // Layout
	"Mon Jan _2 15:04:05 2006",            // ANSIC
	"Mon Jan _2 15:04:05 MST 2006",        // UnixDate
	"Mon Jan 02 15:04:05 -0700 2006",      // RubyDate
	"02 Jan 06 15:04 MST",                 //
	"02 Jan 06 15:04 -0700",               // RFC822 with numeric zone. RFC822Z
	"Monday, 02-Jan-06 15:04:05 MST",      // RFC850
	"Mon, 02 Jan 2006 15:04:05 MST",       // RFC1123
	"Mon, 02 Jan 2006 15:04:05 -0700",     // RFC1123 with numeric zone. RFC1123Z
	"2006-01-02T15:04:05Z07:00",           //
	"2006-01-02T15:04:05.999999999Z07:00", // RFC3339Nano
}

func SplitNonPGLogStr(str string) []string {
	// Some Log messages in PG doesn't follow csv format
	// Ex.: 2023-04-04 03:20:24.193 UTC [1] LOG:  redirecting log output to logging collector process
	// Lets try to search for timestamp with regex
	//  (.+) \[.*\] ([[:upper:]]+)\:(.*)
	reLogNonPg := regexp.MustCompile(`^(.+)\s+\[.*\] (.+)\:(.*)`)
	groups := reLogNonPg.FindStringSubmatch(str)
	return groups
}

func (parser *ParserFSM) StrStartsWithTimestamp(str string) bool {
	before, _, ok := strings.Cut(str, ",")
	if ok {
		for _, layout := range timeLayouts {
			_, err := time.Parse(layout, before)
			if err == nil {
				return true
			}
		}
	}

	// Let's try non CSV PG log format
	groups := SplitNonPGLogStr(str)
	if len(groups) > 3 {
		// String might has datastamp at pos=1
		for _, layout := range timeLayouts {
			_, err := time.Parse(layout, groups[1])
			if err == nil {
				return true
			}
		}
	}
	return false
}

func (parser *ParserFSM) getLogEventType(msg string) string {
	var et EventType
	if parser.StrStartsWithTimestamp(msg) {
		et = newEvent
	} else {
		et = continueEvent
	}
	return EventType.eventName(et)
}

type pgMsgCallback func(msg string, wellformed bool)

type fsmFlags struct {
	flushBuffer bool
}
type Option func(flags *fsmFlags)

// FlushBufferFlag Optional parameter. Used to flush accumulator buffer at the end of process
func FlushBufferFlag(f bool) Option {
	return func(flags *fsmFlags) {
		flags.flushBuffer = f
	}
}

// ProcessPgLogMessage called on log message events.
// It detects event type, collect all event to assemble complete message
// and sens it to an appropriate logger
func (parser *ParserFSM) ProcessPgLogMessage(line string, opts ...Option) (*PostgresLogMessage, error) {
	var pgMsg *PostgresLogMessage = nil
	event := parser.getLogEventType(line)

	msgProcessed := false
	wellFormedMsg := false
	var fullMsg string

	var endMessage = pgMsgCallback(func(msg string, wellformed bool) {
		// This callback signal end of processing
		msgProcessed = true
		wellFormedMsg = wellformed
		fullMsg = msg
	})

	flags := fsmFlags{flushBuffer: false}
	for _, opt := range opts {
		opt(&flags)
	}

	ctx := context.WithValue(context.Background(), string("engProcessingCallback"), endMessage)
	ctx = context.WithValue(ctx, string("msgLine"), line)
	if flags.flushBuffer {
		// replace event to flush accumulator buffer
		event = EventType(flushEvent).eventName()
	}
	err := parser.FSM.Event(ctx, event)

	if err == nil {
		if msgProcessed {
			if wellFormedMsg {
				pgMsg, err = parser.ParseStdoutMsg(fullMsg)
			}
		}
	}

	return pgMsg, err
}

type LogAccumulator struct {
	sbuffer *strings.Builder
}

func newLogAccumulator() *LogAccumulator {
	return &LogAccumulator{sbuffer: new(strings.Builder)}
}

// InitPgParserStdout Create parser FSM for STDOUT log stream.
// First event in log message is expected to start with timestamp.
// Continuation massages, that not started with time stamp collected and
// combined and parsed into JSON structured log message.
// CSV format is based on postgreSQL documentation:
// https://www.postgresql.org/docs/14/runtime-config-logging.html#RUNTIME-CONFIG-LOGGING-CSVLOG
func InitPgParserStdout(logAccumulator *LogAccumulator) *fsm.FSM {
	var parser = fsm.NewFSM(
		"ready",
		fsm.Events{
			{Name: "newEvent", Src: []string{"ready", "nonPgMsg", "messageEnded"}, Dst: "processing"},
			{Name: "continueEvent", Src: []string{"ready"}, Dst: "nonPgMsg"},
			{Name: "continueEvent", Src: []string{"nonPgMsg"}, Dst: "ready"},

			{Name: "continueEvent", Src: []string{"processing"}, Dst: "collectMsg"},
			{Name: "continueEvent", Src: []string{"collectMsg"}, Dst: "processing"},

			{Name: "newEvent", Src: []string{"collectMsg"}, Dst: "messageEnded"},
			{Name: "newEvent", Src: []string{"processing"}, Dst: "messageEnded"},

			{Name: "newEvent", Src: []string{"messageEnded"}, Dst: "processing"},
			{Name: "continueEvent", Src: []string{"messageEnded"}, Dst: "processing"},

			{Name: "flushEvent", Src: []string{"processing", "collectMsg", "nonPgMsg"}, Dst: "ready"},
		},
		fsm.Callbacks{
			"flushEvent": func(ctx context.Context, event *fsm.Event) {
				line := ctx.Value("msgLine").(string)
				logAccumulator.sbuffer.WriteByte(' ')
				logAccumulator.sbuffer.WriteString(line)

				endProcCallback := ctx.Value("engProcessingCallback").(pgMsgCallback)
				endProcCallback(logAccumulator.sbuffer.String(), true)
				logAccumulator.sbuffer.Reset()
			},
			"messageEnded": func(ctx context.Context, event *fsm.Event) {
				if logAccumulator.sbuffer.Len() > 0 {
					// end of collected message, start collecting new one
					// Finished processing. Return accumulated message
					endProcCallback := ctx.Value("engProcessingCallback").(pgMsgCallback)
					endProcCallback(logAccumulator.sbuffer.String(), true)
					logAccumulator.sbuffer.Reset()
				}
				line := ctx.Value("msgLine").(string)
				logAccumulator.sbuffer.WriteByte(' ')
				logAccumulator.sbuffer.WriteString(line)
			},
			"processing": func(ctx context.Context, event *fsm.Event) {
				if event.Src == "messageEnded" && event.Event == "newEvent" {
					// Single line message
					if logAccumulator.sbuffer.Len() > 0 {
						// end of collected message, start collecting new one
						// Finished processing. Return accumulated message
						endProcCallback := ctx.Value("engProcessingCallback").(pgMsgCallback)
						endProcCallback(logAccumulator.sbuffer.String(), true)
						logAccumulator.sbuffer.Reset()
					}
				}
				line := ctx.Value("msgLine").(string)
				logAccumulator.sbuffer.WriteByte(' ')
				logAccumulator.sbuffer.WriteString(line)
			},
			"nonPgMsg": func(ctx context.Context, event *fsm.Event) {
				// This is not wellformed postgres message. Pass it to log collector.
				endProcCallback := ctx.Value("engProcessingCallback").(pgMsgCallback)
				endProcCallback(logAccumulator.sbuffer.String(), false)
				logAccumulator.sbuffer.Reset()
			},
			"collectMsg": func(ctx context.Context, event *fsm.Event) {
				line := ctx.Value("msgLine").(string)
				logAccumulator.sbuffer.WriteByte(' ')
				logAccumulator.sbuffer.WriteString(line)
			},
			"ready": func(ctx context.Context, event *fsm.Event) {
				line := ctx.Value("msgLine").(string)
				if event.Event == "newEvent" {
					logAccumulator.sbuffer.WriteString(line)
				}

			},
		},
	)
	return parser
}

// PostgresLogMessage Log message structure based on PostrgreSQL documentation
// See: https://www.postgresql.org/docs/14/runtime-config-logging.html#RUNTIME-CONFIG-LOGGING-CSVLOG
// PRIMARY KEY (session_id, session_line_num)
type PostgresLogMessage struct {
	Log_time               string //timestamp(3) with time zone
	User_name              string // text
	Database_name          string // text
	Process_id             int    // integer
	Connection_from        string // text
	Session_id             string // text
	Session_line_num       uint64 // bigint
	Command_tag            string // text
	Session_start_time     string // timestamp with time zone
	Virtual_transaction_id string // text
	Transaction_id         uint64 // bigint
	Error_severity         string // text
	Sql_state_code         string // text,
	Message                string // text,
	Detail                 string // text,
	Hint                   string // text,
	Internal_query         string // text,
	Internal_query_pos     int    // integer,
	Context                string // text,
	Query                  string // text,
	Query_pos              int    // integer,
	Location               string // text,
	Application_name       string // text,
	Backend_type           string // text,
	Leader_pid             int    // integer,
	Query_id               uint64 // bigint,
}

func (parser *ParserFSM) ParseStdoutMsg(logMessage string) (*PostgresLogMessage, error) {
	// check string content and generate event for FSM
	r := csv.NewReader(strings.NewReader(logMessage))
	record, err := r.Read()
	if len(record) != reflect.TypeOf(PostgresLogMessage{}).NumField() {
		return nil, errors.New("Message doesn't match PG csv log message structure.")
	}

	// create struct - PG log messages have only string, int and bigint(uint64) data
	if err == nil {
		pgLogMessage := new(PostgresLogMessage)
		element := reflect.ValueOf(pgLogMessage).Elem()

		for i := 0; i < element.NumField(); i++ {
			rf := element.Field(i)
			switch rf.Kind() {
			case reflect.Int, reflect.Int8, reflect.Int16,
				reflect.Int32, reflect.Uint:
				v, err := strconv.Atoi(record[i])
				if err == nil {
					ri := reflect.ValueOf(&v).Elem()
					rf = reflect.NewAt(rf.Type(), unsafe.Pointer(rf.UnsafeAddr())).Elem()
					rf.Set(ri)
				}

			case reflect.Uint64:
				v, err := strconv.ParseUint(record[i], 10, 64)
				if err != nil {
					//  missing value
					v = 0
				}
				if err == nil {
					ri := reflect.ValueOf(&v).Elem()
					rf = reflect.NewAt(rf.Type(), unsafe.Pointer(rf.UnsafeAddr())).Elem()
					rf.Set(ri)
				}

			case reflect.Int64:
				v, err := strconv.ParseInt(record[i], 10, 64)
				if err != nil {
					//  missing value
					v = 0
				}

				if err == nil {
					ri := reflect.ValueOf(&v).Elem()
					rf = reflect.NewAt(rf.Type(), unsafe.Pointer(rf.UnsafeAddr())).Elem()
					rf.Set(ri)
				}

			case reflect.String:
				v := record[i]
				rf = reflect.NewAt(rf.Type(), unsafe.Pointer(rf.UnsafeAddr())).Elem()
				ri := reflect.ValueOf(&v).Elem()
				rf.Set(ri)

			default:
				// Nothing to do?

			}
		}
		return pgLogMessage, err
	} else {
		return nil, err

	}
}

func (message *PostgresLogMessage) JsonString() (string, error) {
	jsonData, err := json.Marshal(message)
	if err != nil {
		return "", err
	}
	msg := string(jsonData)
	return msg, nil
}
