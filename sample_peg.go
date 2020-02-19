{
  // main() is just to build and test
  func main() {
    if len(os.Args) != 2 {
      log.Fatal("Usage: go_parser 'EXPR'")
      return
    }
    got, err := ParseReader("", strings.NewReader(os.Args[1]))
    if err != nil {
      log.Fatal(err)
    }
      fmt.Println("=", got)
  }
}

SourceFile        "source file organization"    = PackageClause

PackageClause     "'package' clause"            = "package" PackageName
PackageName       "package name"                = identifier


// # Characters =============================

newline           "line feed"                   = LF
unicode_char      "unicode char"                = &[\pL] !newline
unicode_letter    "unicode letter"              = [\pL]
unicode_digit     "unicode digit"               = [\pNd]

// # Letters    =============================

letter            "letter"                      = unicode_letter / UBAR
decimal_digit     "decimal digit"               = [0-9]
binary_digit      "binary digit"                = "0" / "1"
octal_digit       "octal digit"                 = [0-7]
hex_digit         "hexadecimal digit"           = [0-9] / [A-F]i / [a-f]i

// # Identifiers ============================

identifier        "identifier"                  = !Keyword BD letter ( letter / unicode_digit )* BD

// # primitive signs -----------------------

SEMICOLON         "semicolon"                   = ';'
COLON             "colon"                       = ':'
DOT               "dot"                         = '.'

LPAREN            "left parenthesis"            = '('
LBRACK            "left bracket"                = '['
LBRACE            "left brace"                  = '{'
COMMA             "comma"                       = ','
PERIOD            "period"                      = '.'

RPAREN            "right parenthesis"           = ')'
RBRACK            "right bracket"               = ']'
RBRACE            "right brace"                 = '}'

PLUS              "plus"                        = '+'
MINUS             "minus"                       = '-'

SLASH             "slash"                       = '/'
BSLASH            "backslash"                   = [U+005C]        // \

EQUAL             "equal"                       = '='
LSS               "less than"                   = '<'
GTR               "greater than"                = '>'

AMPER             "ampersand"                   = '&'
BANG              "bang"                        = '!'
// QUESTION          "question"                    = '?'
HAT               "hat"                         = '^'
PERCENT           "percent"                     = '%'
BAR               "bar"                         = '|'
UBAR              "underbar"                    = '_'

DQUO              "double quote"                = [U+0022]        // "
SQUO              "single quote"                = [U+0027]        // '
BQUO              "back quote/grave accent"     = [U+0060]        // `

LF                "line feed"                   = [U+000A]
// CR                "carriage return"             = [U+000D]
// TAB               "horizontal tab"              = [U+0009]
// SPACE             "white space"                 = [U+0020]

// AT                "atmark"                      = '@'
// NUM               "number"                      = '#'
// TILDA             "tilda"                       = '~'

// # operators/delimiters ------------------

NOT               "not"                         = BANG            // '!'

ADD               "add"                         = PLUS            // '+'
SUB               "subtract"                    = MINUS           // '-'
MUL               "multiply"                    = ASTERISK        // '*'
QUO               "quotient"                    = SLASH           // '/'
REM               "remainder"                   = PERCENT         // '%

AND               "bit: AND"                    = AMPER           // '&'
OR                "bit: OR"                     = BAR             // '|'
XOR               "bit: XOR"                    = HAT             // '^'
AND_NOT           "bit: AND NOT"                = AMPER HAT       // '&^'

NEG               "unary bit negation"          = HAT             // '^'

SHL               "bit: shift left"             = LSS LSS         // '<<'
SHR               "bit: shift right"            = GTR GTR         // '>>'

LAND              "logical AND"                 = AMPER AMPER     // '&&'
LOR               "logical OR"                  = BAR BAR         // '||'
ARROW             "arrow"                       = LSS MINUS       // '<-'
INC               "increment"                   = PLUS PLUS       // '++'
DEC               "decrement"                   = MINUS MINUS     // '--'

EQL               "equal"                       = EQUAL EQUAL     // '=='
NEQ               "not equal"                   = BANG EQUAL      // '!='
LEQ               "less or equal"               = LSS EQUAL       // '<='
GEQ               "greater or equal"            = GTR EQUAL       // '>='
DEFINE            "define"                      = COLON EQUAL     // ':='
ELLIPSIS          "ellipsis"                    = DOT DOT DOT     // '...'

ASSIGN            "assign"                      = EQUAL           // '='
ADD_ASSIGN        "assign: add"                 = PLUS EQUAL      // '+='
SUB_ASSIGN        "assign: subtract"            = MINUS EQUAL     // '-='
MUL_ASSIGN        "assign: multiply"            = ASTERISK EQUAL  // '*='
QUO_ASSIGN        "assign: quotient"            = SLASH EQUAL     // '/='
REM_ASSIGN        "assign: remainder"           = PERCENT EQUAL   // '%='

AND_ASSIGN        "assign: logical AND"         = AMPER EQUAL     // '&='
OR_ASSIGN         "assign: logical OR"          = BAR EQUAL       // '|='
XOR_ASSIGN        "assign: logical XOR"         = HAT EQUAL       // '^='
AND_NOT_ASSIGN    "assign: logical AND NOT"     = AMPER HAT EQUAL // '&^='

SHL_ASSIGN        "assign: shift left"          = LSS LSS EQUAL   // '<<='
SHR_ASSIGN        "assign: shift right"         = GTR GTR EQUAL   // '>>='

ADDR              "address"                     = AMPER           // '&'
REFR              "resolve reference"           = ASTERISK        // '*'

COMMENT_MULTI     "multiline-comment"           = "/*" ( !"*/" . )* "*/"
COMMENT_LINE      "end-of-line comment"         = "//" ( ![\r\n] . )* [\r\n]

SPC               "space character"             = ([ ])+
WHITESPACE        "white space"                 = [\n \t\r]+
WSP               "spaces to be ignored"        = ( WHITESPACE / COMMENT_MULTI / COMMENT_LINE )*

BD                "boundary"                    = WSP
                                                / SEMICOLON

EOF               "end-of-file"                 = !.

// # Keywords ===============================
// via https://github.com/golang/go/src/go/token/token.go

Keyword           "keyword"                     = "break"
                                                / "case"
                                                / "chan"
                                                / "const"
                                                / "continue"

                                                / "default"
                                                / "defer"
                                                / "else"
                                                / "fallthrough"
                                                / "for"

                                                / "func"
                                                / "go"
                                                / "goto"
                                                / "if"
                                                / "import"

                                                / "interface"
                                                / "map"
                                                / "package"
                                                / "range"
                                                / "return"

                                                / "select"
                                                / "struct"
                                                / "switch"
                                                / "type"
                                                / "var"
