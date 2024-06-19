package tokenizer

import (
	"errors"
	"strings"
)

var ParseError = errors.New("parse error")

type Tokenizer struct {
	source   string
	position int
}

func New(sourceCode string) *Tokenizer {
	return &Tokenizer{
		source:   sourceCode,
		position: 0,
	}
}

// func (t *Tokenizer) Advance() string {
// }

// func (t *Tokenizer) Peek()

func (t *Tokenizer) parseWhitespace() {
	t.source = strings.TrimLeft(t.source, " \t\n")
}

func (t *Tokenizer) parseLineComment() error {
	if !strings.HasPrefix(t.source, "//") {
		return ParseError
	}
	_, after, _ := strings.Cut(t.source, "\n")
	t.source = after
	return nil
}

func (t *Tokenizer) parseBlockComment() error {
	if !strings.HasPrefix(t.source, "/*") {
		return ParseError
	}
	_, after, _ := strings.Cut(t.source, "*/")
	t.source = after
	return nil
}
