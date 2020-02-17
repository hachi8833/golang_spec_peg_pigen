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

// "Input" is a special definition
Input             "input to parser"             = SourceFile

TERMINATOR        "terminator of line"          = SEMICOLON
                                                / newline

EOF               "end of file"                 = !.

// # The followings are based on the scraped EBNF ###################################

// # Source file organization ==============

SourceFile      "source file organization"      = PackageClause TERMINATOR ( ImportDecl TERMINATOR )* ( TopLevelDecl TERMINATOR )* EOF

// # 'package' clause ======================

PackageClause   "'package' clause"              = "package" PackageName
PackageName     "package name"                  = identifier

// # 'import' declarations =================

ImportDecl      "'import' declaration"          = "import" ( ImportSpec / LPAREN ( ImportSpec TERMINATOR )* RPAREN )
ImportSpec      "import specification"          = ( DOT / PackageName )? ImportPath
ImportPath      "import path"                   = string_lit

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

identifier        "identifier"                  = letter ( letter / unicode_digit )*

// # Integer literals =======================

int_lit           "integer literal"             = decimal_lit
                                                / binary_lit
                                                / octal_lit
                                                / hex_lit

decimal_lit       "decimal literal"             = "0" / ( [1-9] ) ( ( UBAR )? decimal_digits )?
binary_lit        "binary literal"              = "0" ( "b" / "B" ) ( UBAR )? binary_digits
octal_lit         "octal literal"               = "0" ( "o" / "O" ) ( UBAR )? octal_digits
hex_lit           "hexadecimal literal"         = "0" ( "x" / "X" ) ( UBAR )? hex_digits

decimal_digits    "decimal digits"              = decimal_digit ( ( UBAR )? decimal_digit )*
binary_digits     "binary digits"               = binary_digit ( ( UBAR )? binary_digit )*
octal_digits      "octal digits"                = octal_digit ( ( UBAR )? octal_digit )*
hex_digits        "hexadecimal digits"          = hex_digit ( ( UBAR ) hex_digit )*

// # Floating-point literals ================

float_lit         "float literal"               = decimal_float_lit
                                                / hex_float_lit

decimal_float_lit "decimal float literal"       = decimal_digits DOT ( decimal_digits )? ( decimal_exponent )?
                                                / decimal_digits decimal_exponent
                                                / DOT decimal_digits ( decimal_exponent )?

decimal_exponent  "decimal exponent"            = ( "e" / "E" ) ( PLUS / MINUS )? decimal_digits

hex_float_lit     "hexadecimal float literal"   = "0" ( "x" / "X" ) hex_mantissa hex_exponent
hex_mantissa      "hexadecimal mantissa"        = ( UBAR )? hex_digits DOT ( hex_digits )?
                                                / ( UBAR )? hex_digits
                                                / DOT hex_digits

hex_exponent      "hexadecimal exponent"        = ( "p" / "P" ) ( PLUS / MINUS )? decimal_digits

// # Imaginary literals =====================

imaginary_lit    "imaginary literal"            = (decimal_digits / int_lit / float_lit) "i"

// # Rune literals ==========================

rune_lit         "rune literal"                 = SQUO ( unicode_value / byte_value ) SQUO
unicode_value    "unicode value"                = unicode_char
                                                / little_u_value
                                                / big_u_value
                                                / escaped_char

byte_value       "byte value"                   = octal_byte_value
                                                / hex_byte_value

octal_byte_value "octal byte value"             = BQUO octal_digit octal_digit octal_digit
hex_byte_value   "hexadecimal byte value"       = BQUO "x" hex_digit hex_digit
little_u_value   "little u value"               = BQUO "u" hex_digit hex_digit hex_digit hex_digit
big_u_value      "big U value"                  = BQUO "U" hex_digit hex_digit hex_digit hex_digit
                                                          hex_digit hex_digit hex_digit hex_digit

escaped_char     "rune escaped char"            = BSLASH ( "a" / "b" / "f" / "n" / "r" / "t" / "v" / BSLASH / SQUO / DQUO )

// # String literals ========================

string_lit             "string literal"             = raw_string_lit
                                                    / interpreted_string_lit

raw_string_lit         "raw string literal"         = BQUO ( unicode_char / newline )* BQUO
interpreted_string_lit "interpreted string literal" = DQUO ( unicode_value / byte_value )* DQUO

// # Types ==================================

Type            "type"                          = TypeName
                                                / TypeLit
                                                / LPAREN Type RPAREN

TypeName        "type name"                     = identifier
                                                / QualifiedIdent

TypeLit         "type literal"                  = ArrayType
                                                / StructType
                                                / PointerType
                                                / FunctionType
                                                / InterfaceType
                                                / SliceType
                                                / MapType
                                                / ChannelType

// # Array types ----------------------------

ArrayType       "array type"                    = LBRACK ArrayLength RBRACK ElementType
ArrayLength     "array length"                  = Expression
ElementType     "element type"                  = Type

// # Slice types ----------------------------

SliceType       "slice type"                    = LBRACK RBRACK ElementType

// # Struct types ---------------------------

StructType      "struct type"                   = "struct" LBRACE ( FieldDecl TERMINATOR )* RBRACE
FieldDecl       "struct field declaration"      = (IdentifierList Type / EmbeddedField) ( Tag )?
EmbeddedField   "struct embedded field"         = ( ASTERISK )? TypeName
Tag             "struct tag"                    = string_lit

// # Pointer types --------------------------

PointerType     "pointer type"                  = ASTERISK BaseType
BaseType        "base type"                     = Type

// # Function types -------------------------

FunctionType    "function type"                   = "func" Signature
Signature       "function signature"              = Parameters ( Result )?
Result          "function result"                 = Parameters
                                                  / Type
Parameters      "function parameters"             = LPAREN ( ParameterList ( COMMA )? )? RPAREN
ParameterList   "function parameter list"         = ParameterDecl ( COMMA ParameterDecl )?
ParameterDecl   "function parameter declaration"  = ( IdentifierList )? ( ELLIPSIS )? Type

// # Interface types ------------------------

InterfaceType      "interface type"             = "interface" LBRACE ( MethodSpec TERMINATOR )? RBRACE
MethodSpec         "method specification"       = MethodName Signature
                                                / InterfaceTypeName

MethodName         "method name"                = identifier
InterfaceTypeName  "interface type name"        = TypeName

// # Map types ------------------------------

MapType         "map type"                      = "map" LBRACK KeyType RBRACK ElementType
KeyType         "map key type"                  = Type

// # Channel types --------------------------

ChannelType     "channel type"                  = ( "chan" / "chan" ARROW / ARROW "chan" ) ElementType

// # Blocks =================================

Block           "block"                         = LBRACE StatementList RBRACE
StatementList   "statement list"                = ( Statement TERMINATOR )*

// # Declarations and scope =================

Declaration     "declaration"                   = ConstDecl
                                                / TypeDecl
                                                / VarDecl

TopLevelDecl    "top-level declaration"         = Declaration
                                                / FunctionDecl
                                                / MethodDecl

// # Constant declarations ==================

ConstDecl       "constant declaration"          = "const" ( ConstSpec / LPAREN ( ConstSpec TERMINATOR )* RPAREN )
ConstSpec       "constant specification"        = IdentifierList ( ( Type )? EQUAL ExpressionList )?

IdentifierList  "identifier list"               = identifier ( COMMA identifier )*
ExpressionList  "expression list"               = Expression ( COMMA Expression )*

// # Type declarations ======================

TypeDecl        "type declaration"              = "type" ( TypeSpec / LPAREN ( TypeSpec TERMINATOR )? RPAREN )
TypeSpec        "type specification"            = AliasDecl
                                                / TypeDef

// # Alias declarations =====================

AliasDecl       "type alias declaration"        = identifier EQUAL Type

// # Type definitions =======================

TypeDef         "type definition"               = identifier Type

// # Variable declarations ==================

VarDecl         "variable declaration"          = "var" ( VarSpec / LPAREN ( VarSpec TERMINATOR )* RPAREN )
VarSpec         "variabpe specification"        = IdentifierList ( Type ( EQUAL ExpressionList )* / EQUAL ExpressionList )

// # Short variable declarations ============

ShortVarDecl    "short declaration of variable" = IdentifierList DEFINE ExpressionList

// # Function declarations ==================

FunctionDecl    "function declaration"          = "func" FunctionName Signature ( FunctionBody )?
FunctionName    "function name"                 = identifier
FunctionBody    "function body"                 = Block

// # Method declarations ====================

MethodDecl      "method declaration"            = "func" Receiver MethodName Signature ( FunctionBody )?
Receiver        "method receiver"               = Parameters

// # Expressions ============================

Operand         "operand"                       = Literal
                                                / OperandName
                                                / LPAREN Expression RPAREN

Literal         "literal"                       = BasicLit
                                                / CompositeLit
                                                / FunctionLit

BasicLit        "basic literal"                 = int_lit
                                                / float_lit
                                                / imaginary_lit
                                                / rune_lit
                                                / string_lit

OperandName     "operand name"                  = identifier
                                                / QualifiedIdent

// # Qualified identifiers ==================

QualifiedIdent  "qualified identifier"          = PackageName DOT identifier

// # Composite literals =====================

CompositeLit    "composite literal"             = LiteralType LiteralValue
LiteralType     "literal type"                  = StructType
                                                / ArrayType
                                                / LBRACK ELLIPSIS RBRACK ElementType
                                                / SliceType
                                                / MapType
                                                / TypeName

LiteralValue    "literal value"                 = LBRACE ( ElementList ( COMMA )? )? RBRACE
ElementList     "element list"                  = KeyedElement ( COMMA KeyedElement )*
KeyedElement    "keyed element"                 = ( Key COLON )? Element
Key             "key of element list"           = FieldName
                                                / Expression
                                                / LiteralValue

FieldName       "field name"                    = identifier
Element         "element of list"               = Expression
                                                / LiteralValue

// # Function literals =====================

FunctionLit     "function literal"              = "func" Signature FunctionBody

// # Primary expressions ===================

PrimaryExpr     "primary expression"            = Operand
                                                / Conversion
                                                / MethodExpr
                                                / PrimaryExpr Selector
                                                / PrimaryExpr Index
                                                / PrimaryExpr Slice
                                                / PrimaryExpr TypeAssertion
                                                / PrimaryExpr Arguments

Selector        "selector in expression"        = DOT identifier
Index           "index in expression"           = LBRACK Expression RBRACK
Slice           "slice in expression"           = LBRACK ( Expression )? COLON ( Expression )? RBRACK
                                                / LBRACK ( Expression )? COLON Expression COLON Expression RBRACK

TypeAssertion   "type assertion"                = DOT LPAREN Type RPAREN
Arguments       "arguments"                     = LPAREN ( ( ExpressionList / Type ( COMMA ExpressionList )? ) ( ELLIPSIS )? ( COMMA )? )? RPAREN

// # Method expressions ===================

MethodExpr      "method expression"             = ReceiverType DOT MethodName
ReceiverType    "receiver type"                 = Type

// # Operators ============================

// Precedence    Operator
//     5             *  /  %  <<  >>  &  &^
//     4             +  -  |  ^
//     3             ==  !=  <  <=  >  >=
//     2             &&
//     1             ||

Expression      "expression"                    = UnaryExpr
                                                / Expression binary_op Expression

UnaryExpr       "unary expression"              = PrimaryExpr
                                                / unary_op UnaryExpr

unary_op        "unary operator"                = PLUS          // "+"
                                                / MINUS         // "-"
                                                / NOT           // "!"
                                                / NEG           // "^"
                                                / ADDR          // "*"
                                                / REFR          // "&"
                                                / ARROW         // "<-"

binary_op       "binary operator"               = mul_op
                                                / add_op
                                                / rel_op
                                                / LAND          // "&&"
                                                / LOR           // "||"

rel_op          "comparison operator"           = EQL           // "=="
                                                / NEQ           // "!="
                                                / LSS           // "<"
                                                / LEQ           // "<="
                                                / GTR           // ">"
                                                / GEQ           // ">="

add_op          "addition operator"             = ADD           // "+"
                                                / SUB           // "-"
                                                / OR            // "|"
                                                / XOR           // "^"

mul_op          "multiplication operator"       = MUL           // "*"
                                                / QUO           // "/"
                                                / REM           // "%"
                                                / SHL           // "<<"
                                                / SHR           // ">>"
                                                / AND           // "&"
                                                / AND_NOT       // "&^"

// # Conversions ==========================

Conversion      "type conversion"               = Type LPAREN Expression ( COMMA )? RPAREN

// # Statements ===========================

Statement       "statement"                     = Declaration
                                                / LabeledStmt
                                                / SimpleStm
                                                / GoStmt
                                                / ReturnStmt
                                                / BreakStmt
                                                / ContinueStmt
                                                / GotoStm
                                                / FallthroughStmt
                                                / Block
                                                / IfStmt
                                                / SwitchStmt
                                                / SelectStmt
                                                / ForStm
                                                / DeferStmt

SimpleStmt      "simple statement"              = EmptyStmt
                                                / ExpressionStmt
                                                / SendStmt
                                                / IncDecStmt
                                                / Assignment
                                                / ShortVarDecl

// # Empty statements =====================

EmptyStmt       "empty statement"               = ""

// # Labeled statements ===================

LabeledStmt     "labeled statement"             = Label COLON Statement
Label           "label"                         = identifier

// # Expression statements ================

ExpressionStmt  "expression statement"          = Expression

// # Send statements ======================

SendStmt        "send-statement"                = Channel ARROW Expression
Channel         "channel"                       = Expression

// # IncDec statements ====================

IncDecStmt      "increment/decrement statement" = Expression ( INC / DEC )

// # Assignments ==========================

Assignment      "assignment"                    = ExpressionList assign_op ExpressionList

assign_op       "assignment operator"           = ( add_op / mul_op )? EQUAL

// # 'if' statements ======================

IfStmt  "'if' statement"  = "if" ( SimpleStmt TERMINATOR )? Expression Block ( "else" ( IfStmt / Block ) )?

// # 'switch' statements ==================

SwitchStmt      "'switch' statement"            = ExprSwitchStmt
                                                / TypeSwitchStmt

// # Expression switches ------------------

ExprSwitchStmt  "expression of 'switch' statement"  = "switch" ( SimpleStmt TERMINATOR )? ( Expression )? LBRACE ( ExprCaseClause )* RBRACE
ExprCaseClause  "expression of 'case' clause"       = ExprSwitchCase COLON StatementList
ExprSwitchCase  "expression of switch's 'case'"     = "case" ExpressionList
                                                    / "default"

// # Type switches -------------------------

TypeSwitchStmt  "type-switch statement"         = "switch" ( SimpleStmt TERMINATOR )? TypeSwitchGuard LBRACE ( TypeCaseClause )* RBRACE
TypeSwitchGuard "type-switch guard"             = ( identifier DEFINE ) PrimaryExpr DOT LPAREN "type" RPAREN
TypeCaseClause  "type-switch clause"            = TypeSwitchCase COLON StatementList
TypeSwitchCase  "type-switch case"              = "case" TypeList
                                                / "default"

TypeList        "type list"                     = Type ( COMMA Type )*

// # 'for' statements ======================

ForStmt         "'for' statement"               = "for" ( Condition / ForClause / RangeClause ) Block
Condition       "'for' condition"               = Expression

// # 'for' statements with 'for' clause ----

ForClause       "'for' clause"                  = ( InitStmt )? TERMINATOR ( Condition )? TERMINATOR ( PostStmt )?
InitStmt        "initial statament in 'for'"    = SimpleStmt
PostStmt        "post statement in 'for'"       = SimpleStmt

// # 'for' statements with 'range' clause --

RangeClause     "'range' clause"                = ( ExpressionList EQUAL / IdentifierList DEFINE )? "range" Expression

// # 'go' statements =======================

GoStmt          "'go' statement"                = "go" Expression

// # 'select' statements ===================

SelectStmt      "'select' statement"            = "select" LBRACE ( CommClause )* RBRACE
CommClause      "communication-clause"          = CommCase COLON StatementList
CommCase        "comminucation 'case'"          = "case" ( SendStmt / RecvStmt )
                                                / "default"

RecvStmt        "receive-statement"             = ( ExpressionList EQUAL / IdentifierList DEFINE )? RecvExpr
RecvExpr        "receive-expression"            = Expression

// # 'return' statements ===================

ReturnStmt      "'return' statement"            = "return" ( ExpressionList )?

// # 'break' statements ====================

BreakStmt       "'break' statement"             = "break" ( Label )?

// # 'continue' statements =================

ContinueStmt    "'continue' statement"          = "continue" ( Label )?

// # 'goto' statements =====================

GotoStmt        "'goto' statement"              = "goto" Label

// # 'fallthrough' statements ==============

FallthroughStmt "'fallthrough' statement"       = "fallthrough"

// # 'defer' statements ====================

DeferStmt       "'defer' statement"             = "defer" Expression

// # Operators and delimiters ##################################################
// via https://github.com/golang/go/src/go/token/token.go

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
