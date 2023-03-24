package common

import (
	"DbAnalyzer/types"
	"fmt"
	"regexp"
)

type Detectors struct {
	columnNameDetectors []detector
	contentDetectors    []detector
	invokeComprehend    bool
}

type detector struct {
	name string
	re   regexp.Regexp
}

func Compile(piis []types.PIIDetector) (*Detectors, error) {
	d := Detectors{
		columnNameDetectors: []detector{},
		contentDetectors:    []detector{},
		invokeComprehend:    false,
	}
	for _, p := range piis {
		switch p.Method {
		case types.PIIDT_Comprehend:
			d.invokeComprehend = true
		case types.PIIDT_Columnregexp:
			if r, err := regexp.Compile(p.Data); err != nil {
				return nil, fmt.Errorf("Erroneous regexp %s: %v", p.Data, err)
			} else {
				d.columnNameDetectors = append(d.columnNameDetectors,
					detector{
						name: p.Name,
						re:   *r,
					})
			}
		case types.PIIDT_Contentregexp:
			if r, err := regexp.Compile(p.Data); err != nil {
				return nil, fmt.Errorf("Erroneous regexp %s: %v", p.Data, err)
			} else {
				d.contentDetectors = append(d.contentDetectors,
					detector{
						name: p.Name,
						re:   *r,
					})
			}
		}
	}
	return &d, nil
}

func MatchColumnName(d *Detectors, s string) *string {
	for k := range d.columnNameDetectors {
		p := &d.columnNameDetectors[k]
		if p.re.MatchString(s) {
			return &p.name
		}
	}
	return nil
}

func MatchContent(d *Detectors, s []string) *string {
	pred := func(p *detector) func(string) bool {
		return func(ss string) bool {
			return p.re.MatchString(ss)
		}
	}
	for k := range d.contentDetectors {
		p := &d.contentDetectors[k]
		if All(s, pred(p)) {
			return &p.name
		}
	}
	return nil

}

func All[T any](ts []T, pred func(T) bool) bool {
	if len(ts) == 0 {
		return false
	}
	for _, t := range ts {
		if !pred(t) {
			return false
		}
	}
	return true
}

func Any[T any](ts []T, pred func(T) bool) bool {
	for _, t := range ts {
		if pred(t) {
			return true
		}
	}
	return false
}
