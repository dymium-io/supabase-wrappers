package DbSetup

import (
	"crypto/sha256"
	"fmt"
)

var setupObfuscator func(exec func(string, ...interface{}) error, localUser string) error

func init() {
	on := func(n string) string {
		return fmt.Sprintf(`_%x_`, sha256.Sum224([]byte(n)))
	}
	type oT struct {
		n string
		k int
	}
	lst := []oT{
		{
			n: "obfuscate_text",
			k: 0x226193b1,
		},
		{
			n: "obfuscate_text_array",
			k: 0xf8bfced0,
		},
		{
			n: "obfuscate_uuid",
			k: 0x4b0a02b,
		},
		{
			n: "obfuscate_uuid_array",
			k: 0xced64e86,
		},
	}
	type obfT struct {
		n string
		k string
	}
	obfS := make(map[string]obfT, len(lst))
	for _, o := range lst {
		obfS[o.n] = obfT{
			n: on(o.n),
			k: fmt.Sprintf("x'%x'::int", o.k),
		}
	}
	obf = func(name string) (string, string) {
		o, ok := obfS[name]
		if !ok {
			panic(fmt.Sprintf("function [obf] is called with wrong argument [%s]", name))
		}
		return o.n, o.k
	}
	obf_funcs := func() []string {
		r := make([]string, 0, len(lst))
		for _, o := range lst {
			r = append(r, obfS[o.n].n)
		}
		return r
	}

	setupObfuscator = func(exec func(string, ...interface{}) error, localUser string) error {
		var err error
		if err = exec("CREATE EXTENSION IF NOT EXISTS obfuscator WITH SCHEMA _dymium"); err != nil {
			return err
		}
		for _, f := range obf_funcs() {
			if err := exec("GRANT EXECUTE ON FUNCTION _dymium." + f + " TO " + localUser); err != nil {
				return err
			}
		}
		return nil
	}
}
