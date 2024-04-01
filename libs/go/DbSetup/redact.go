package DbSetup

import (
	"regexp"
)

func init() {
	redactDefs := []string{`^interval                                 => '0 days'`,
		`^(char|var|bpchar).*\((1|2)\)                            => ''`,
		`^(char|var|text|bpchar)                                  => 'xxx'`,
		`^(bigint|int|smallint|double|float|real|decimal|numeric) => 0`,
		//
		`^bool                                                    => false`,
		`^xml                                                     => ''`,
		`bytea                                                    => E'\x'`,
		//
		`^jsonb?                                                  => '{}'`,
		`^uuid                                                    => '00000000-0000-0000-0000-000000000000'`,
		//
		`^timestamp.*with +time *zone                             => '2000-01-01 00:00:00 UTC'`,
		`^timestamp                                               => '0001-01-01 00:00:00'`,
		//
		`^time.*with +time *zone                                  => '00:00:00 UTC'`,
		`^time                                                    => '00:00:00'`,
		//
		`^date                                                    => '0001-01-01'`,
		`^money                                                   => 0`,
		//
		`^point                                                   => '(0,0)'`,
		`^line                                                    => '(0,0,0)'`,
		//
		`^(lseg|box|path)                                         => '((0,0), (1,1))'`,
		`^polygon                                                 => '((0,0), (0,1), (1,0))'`,
		//
		`^circle                                                  => '<(0,0), 1>'`,
		`^(inet|cidr)                                             => '0.0.0.0/0'`,
		//
		`^macaddr8                                                => '00:00:00:00:00:00:00:00'`,
		`^macaddr                                                 => '00:00:00:00:00:00'`,
		//
		`^bit                                                     => B'0'`,
	}
	spl := regexp.MustCompile(` *=> *`)
	type regexpMap struct {
		r *regexp.Regexp
		t string
	}
	redacts := make([]regexpMap, 0, len(redactDefs))
	for _, r := range redactDefs {
		ri := spl.Split(r, 2)
		redacts = append(redacts,
			regexpMap{
				r: regexp.MustCompile(`(?i:` + ri[0] + `)`),
				t: string(ri[1]),
			})
	}
	redact_value = func(t string) string {
		for _, ri := range redacts {
			if ri.r.MatchString(t) {
				return ri.t
			}
		}
		return `''`
	}
}
