package utils

type element interface {
	IsTheSame(any) bool
}

type Cache[T element] struct {
	capacity int
	nCached  int
	cached   []T
}

func MakeCache[T element](cap int) *Cache[T] {
	return &Cache[T]{
		capacity: cap,
		nCached:  0,
		cached:   make([]T, cap),
	}
}

func (c *Cache[T]) Find(el any) T {
	var ret T
	for k := 0; k != c.nCached; k++ {
		if c.cached[k].IsTheSame(el) {
			ret = c.squeeze(k)
			c.cached[c.nCached] = ret
			c.nCached++
			break
		}
	}
	return ret
}

func (c *Cache[T]) Pop() T {
	return c.squeeze(c.nCached - 1)
}

func (c *Cache[T]) Push(el T) T {
	var ret T
	if c.nCached == c.capacity {
		ret = c.squeeze(0)
	}
	c.cached[c.nCached] = el
	c.nCached++
	return ret
}

func (c *Cache[T]) squeeze(k int) T {
	var ret T
	if k < 0 || c.nCached == 0 || k >= c.nCached {
		return ret
	}
	ret = c.cached[k]
	for kk := k + 1; kk != c.nCached; kk++ {
		c.cached[kk-1] = c.cached[kk]
	}
	c.nCached--
	return ret

}
