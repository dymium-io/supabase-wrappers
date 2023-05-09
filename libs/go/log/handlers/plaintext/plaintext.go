// Package text implements a development-friendly textual handler.
package plaintext

import (
	"fmt"
	"io"
	"os"
	"sync"
	"time"

	"github.com/apex/log"
)

// Default handler outputting to stderr.
var Default = New(os.Stderr)

// start time.
var start = time.Now()


// Handler implementation.
type Handler struct {
	mu     sync.Mutex
	Writer io.Writer
}

// New handler.
func New(w io.Writer) *Handler {
	return &Handler{
		Writer: w,
	}
}

// HandleLog implements log.Handler.
func (h *Handler) HandleLog(e *log.Entry) error {


	h.mu.Lock()
	defer h.mu.Unlock()

	fmt.Fprintf(h.Writer, "%s\n", e.Message)

	return nil
}
