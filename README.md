# Golang Specification in PEG (Pigeon) format

The [golang_peg.go](golang_peg.go) file on the repository, written in PEG (Pigeon) format, is based on the following:

* Official: [Golang specification](https://golang.org/ref/spec)
* [golang_scraped.ebnf](golang_scraped.ebnf): Scraped from the spec above via [EBNF scraper](https://gist.github.com/chewxy/d9642a9552973dfc0731) at 2020/02/16
* [nma/pigeon](https://github.com/mna/pigeon) -- a PEG-parser generator in Go

## PEG

PEG is a strict analytical formal grammar that has an ability to describe formal languages, and has no ambiguity in its syntax (while NBNF and regexp still have ambiguities). The characteristics is good to describe specifications for programming languages, rather than describe the ones for natural languages.

* [Parsing Expression Grammar \- Wikipedia](https://en.wikipedia.org/wiki/Parsing_Expression_Grammar)
* [Packrat Parsing and Parsing Expression Grammars](https://bford.info/packrat/)
* [Links, Research and Papers about Parser Expression Grammar \(PEG\)](https://github.com/mna/pigeon/wiki) -- Pigeon

Note that PEG notation has many dialects in implementation of PEG-parser generators. See also Pigeon's Wiki above.

* [https://github.com/pointlander/peg](pointlander/peg) for Go
* [pegjs/pegjs](https://github.com/pegjs/pegjs) for JavaScript
* [erikrose/parsimonious](https://github.com/erikrose/parsimonious) for Python
* [kschiess/parslet](https://github.com/kschiess/parslet) for Ruby

## The reason I chose Pigeon

* Pros
  * Pigeon is written in Go
  * Expanded notations:
    * UNICODE character categories such as `[\pL]` or `[\pNd]`
    * You can use the following PEG definition operator:`=` `<-` `←` `⟵`
    * Ability to Embed Go codes
  * BSD-based license
* Cons
  * Too Go-specific notation (I have to set the PEG file extension to `.go` to make syntax highlighint work...)
  * Pretty big library


