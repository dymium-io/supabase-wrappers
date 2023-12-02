// Copyright 2013 The Gorilla Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// hacked by igor@dymium.io to make it discretionary
package main

import (
	"compress/flate"
	"compress/gzip"
	"github.com/andybalholm/brotli"
	"io"
	"net/http"
	"strings"
)

type compressResponseWriter struct {
	io.Writer
	http.ResponseWriter
	http.Hijacker
	http.Flusher
	http.CloseNotifier
}

func (w *compressResponseWriter) WriteHeader(c int) {
	w.ResponseWriter.Header().Del("Content-Length")
	w.ResponseWriter.WriteHeader(c)
}

func (w *compressResponseWriter) Header() http.Header {
	h := w.ResponseWriter.Header()
	return h
}

func (w *compressResponseWriter) Write(b []byte) (int, error) {

	h := w.ResponseWriter.Header()

	ct := h.Get("Content-Type")
	if ct == "" {
		ct = http.DetectContentType(b)
	}

	h.Set("Content-Type", ct)
	h.Del("Content-Length")

	return w.Writer.Write(b)
}

type flusher interface {
	Flush() error
}

func (w *compressResponseWriter) Flush() {
	// Flush compressed data if compressor supports it.
	if f, ok := w.Writer.(flusher); ok {
		f.Flush()
	}
	// Flush HTTP response.
	if w.Flusher != nil {
		w.Flusher.Flush()
	}
}

// CompressHandler gzip compresses HTTP responses for clients that support it
// via the 'Accept-Encoding' header.
//
// Compressing TLS traffic may leak the page contents to an attacker if the
// page contains user input: http://security.stackexchange.com/a/102015/12208
func CompressHandler(h http.Handler) http.Handler {
	return CompressHandlerLevel(h, gzip.DefaultCompression)
}

// CompressHandlerLevel gzip compresses HTTP responses with specified compression level
// for clients that support it via the 'Accept-Encoding' header.
//
// The compression level should be gzip.DefaultCompression, gzip.NoCompression,
// or any integer value between gzip.BestSpeed and gzip.BestCompression inclusive.
// gzip.DefaultCompression is used in case of invalid compression level.
func CompressHandlerLevel(h http.Handler, level int) http.Handler {
	if level < gzip.DefaultCompression || level > gzip.BestCompression {
		level = gzip.DefaultCompression
	}

	const (
		gzipEncoding   = "gzip"
		flateEncoding  = "deflate"
		brotliEncoding = "br"
	)

	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// detect what encoding to use
		var encoding string
		encodings := strings.Split(r.Header.Get("Accept-Encoding"), ",")

		checkEncoding := func(enc string) bool {
			for _, curEnc := range encodings {
				curEnc = strings.TrimSpace(curEnc)
				if curEnc == enc {
					return true
				}
			}
			return false
		}
		if checkEncoding(brotliEncoding) {
			encoding = brotliEncoding
		} else if checkEncoding(gzipEncoding) {
			encoding = gzipEncoding
		} else if checkEncoding(flateEncoding) {
			encoding = flateEncoding
		}

		// if we weren't able to identify an encoding we're familiar with, pass on the
		// request to the handler and return
		if encoding == "" || strings.HasSuffix(r.URL.Path, ".png") || strings.HasSuffix(r.URL.Path, ".gif") ||
			strings.HasSuffix(r.URL.Path, ".woff2") || strings.HasPrefix(r.URL.Path, "/bin/") {
			h.ServeHTTP(w, r)
			return
		}

		// wrap the ResponseWriter with the writer for the chosen encoding
		var encWriter io.WriteCloser
		if encoding == gzipEncoding {
			encWriter, _ = gzip.NewWriterLevel(w, level)
		} else if encoding == flateEncoding {
			encWriter, _ = flate.NewWriter(w, level)
		} else if encoding == brotliEncoding {
			encWriter = brotli.NewWriterOptions(w, brotli.WriterOptions{Quality: 4, LGWin: 0})
		}

		defer encWriter.Close()

		w.Header().Set("Content-Encoding", encoding)
		r.Header.Del("Accept-Encoding")
		w.Header().Add("Vary", "Accept-Encoding")
		if strings.HasSuffix(r.URL.Path, ".svg") {
			w.Header().Set("Content-Type", "image/svg+xml; charset=utf-8")
		}
		if strings.HasSuffix(r.URL.Path, ".js") {
			w.Header().Set("Content-Type", "application/javascript; charset=utf-8")
		}

		hijacker, ok := w.(http.Hijacker)
		if !ok { /* w is not Hijacker... oh well... */
			hijacker = nil
		}

		flusher, ok := w.(http.Flusher)
		if !ok {
			flusher = nil
		}

		closeNotifier, ok := w.(http.CloseNotifier)
		if !ok {
			closeNotifier = nil
		}

		w = &compressResponseWriter{
			Writer:         encWriter,
			ResponseWriter: w,
			Hijacker:       hijacker,
			Flusher:        flusher,
			CloseNotifier:  closeNotifier,
		}

		h.ServeHTTP(w, r)
	})
}
