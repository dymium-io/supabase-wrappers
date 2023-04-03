package utils

func CopyPointers[T any](s []*T) ([]*T) {
	r := make([]*T, len(s))
	for k, p := range s {
		if p != nil {
			v := *p
			r[k] = &v
		}
	}
	return r
}
