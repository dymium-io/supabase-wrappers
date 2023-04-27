package utils

func CopyPointers[T any](s []T, p []*T, isNullable []bool) ([]*T) {
	r := make([]*T, len(s))
	for k, _ := range s {
		if isNullable[k] {
			if p[k] != nil {
				v := *p[k]
				r[k] = &v
			}
		} else {
			v := s[k]
			r[k] = &v
		}
	}
	return r
}
