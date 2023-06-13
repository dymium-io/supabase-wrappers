package plaintext_test

import (
	"bytes"
	"testing"
	"time"

	"github.com/apex/log"
	"github.com/apex/log/handlers/plaintext"
	"github.com/stretchr/testify/assert"
)

func init() {
	log.Now = func() time.Time {
		return time.Unix(0, 0)
	}
}

func Test(t *testing.T) {
	var buf bytes.Buffer

	log.SetHandler(plaintext.New(&buf))
	log.WithField("user", "tj").WithField("id", "123").Info("hello")
	log.WithField("user", "tj").Info("world")
	log.WithField("user", "tj").Error("boom")

	expected := "hello\nworld\nboom\n"

	assert.Equal(t, expected, buf.String())
}
