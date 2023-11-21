package utils

import "golang.org/x/exp/constraints"


var Unsupported *string

func init() {
	s := "UNSUPPORTED"
	Unsupported = &s
}

func Min[T constraints.Ordered](x, y T) T {
    if x < y {
        return x
    }
    return y
}
