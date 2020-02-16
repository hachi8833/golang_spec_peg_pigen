// # Characters =============================

newline           "line feed"                   = [U+000A]
unicode_char      "unicode char"                = &[\pL] !newline
unicode_letter    "unicode letter"              = [\pL]
unicode_digit     "unicode digit"               = [\pNd]

// # Letters    =============================

letter            "letter"                      = unicode_letter / "_"
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

decimal_lit       "decimal literal"             = "0" / ( [1-9] ) ( ( "_" )? decimal_digits )?
binary_lit        "binary literal"              = "0" ( "b" / "B" ) ( "_" )? binary_digits
octal_lit         "octal literal"               = "0" ( "o" / "O" ) ( "_" )? octal_digits
hex_lit           "hexadecimal literal"         = "0" ( "x" / "X" ) ( "_" )? hex_digits

decimal_digits    "decimal digits"              = decimal_digit ( ( "_" )? decimal_digit )*
binary_digits     "binary digits"               = binary_digit ( ( "_" )? binary_digit )*
octal_digits      "octal digits"                = octal_digit ( ( "_" )? octal_digit )*
hex_digits        "hexadecimal digits"          = hex_digit ( ( "_" ) hex_digit )*

// # Floating-point literals ================

float_lit         "float literal"               = decimal_float_lit
                                                / hex_float_lit

decimal_float_lit "decimal float literal"       = decimal_digits "." ( decimal_digits )? ( decimal_exponent )?
                                                / decimal_digits decimal_exponent
                                                / "." decimal_digits ( decimal_exponent )?

decimal_exponent  "decimal exponent"            = ( "e" / "E" ) ( "+" / "-" )? decimal_digits

hex_float_lit     "hexadecimal float literal"   = "0" ( "x" / "X" ) hex_mantissa hex_exponent
hex_mantissa      "hexadecimal mantissa"        = ( "_" )? hex_digits "." ( hex_digits )?
                                                / ( "_" )? hex_digits
                                                / "." hex_digits

hex_exponent      "hexadecimal exponent"        = ( "p" / "P" ) ( "+" / "-" )? decimal_digits

// # Imaginary literals =====================

imaginary_lit    "imaginary literal"            = (decimal_digits / int_lit / float_lit) "i"

// # Rune literals ==========================

rune_lit         "rune literal"                 = "'" ( unicode_value / byte_value ) "'"
unicode_value    "unicode value"                = unicode_char
                                                / little_u_value
                                                / big_u_value
                                                / escaped_char

byte_value       "byte value"                   = octal_byte_value
                                                / hex_byte_value

octal_byte_value "octal byte value"             = `\` octal_digit octal_digit octal_digit
hex_byte_value   "hexadecimal byte value"       = `\` "x" hex_digit hex_digit
little_u_value   "little u value"               = `\` "u" hex_digit hex_digit hex_digit hex_digit
big_u_value      "big U value"                  = `\` "U" hex_digit hex_digit hex_digit hex_digit
                                                          hex_digit hex_digit hex_digit hex_digit

escaped_char     "rune escaped char"            = `\` ( "a" / "b" / "f" / "n" / "r" / "t" / "v" / `\` / "'" / `"` )

// # String literals ========================

string_lit             "string literal"             = raw_string_lit
                                                    / interpreted_string_lit

raw_string_lit         "raw string literal"         = "`" ( unicode_char / newline )* "`"
interpreted_string_lit "interpreted string literal" = `"` ( unicode_value / byte_value )* `"`

// # Types ==================================

Type            "type"                          = TypeName
                                                / TypeLit
                                                / "(" Type ")"

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

ArrayType       "array type"                    = "[" ArrayLength "]" ElementType
ArrayLength     "array length"                  = Expression
ElementType     "element type"                  = Type

// # Slice types ----------------------------

SliceType       "slice type"                    = "[" "]" ElementType

// # Struct types ---------------------------

StructType      "struct type"                   = "struct" "{" ( FieldDecl ";" )* "}"
FieldDecl       "struct field declaration"      = (IdentifierList Type / EmbeddedField) ( Tag )?
EmbeddedField   "struct embedded field"         = ( "*" )? TypeName
Tag             "struct tag"                    = string_lit

// # Pointer types --------------------------

PointerType     "pointer type"                  = "*" BaseType
BaseType        "base type"                     = Type

// # Function types -------------------------

FunctionType    "function type"                   = "func" Signature
Signature       "function signature"              = Parameters ( Result )?
Result          "function result"                 = Parameters
                                                  / Type
Parameters      "function parameters"             = "(" ( ParameterList ( "," )? )? ")"
ParameterList   "function parameter list"         = ParameterDecl ( "," ParameterDecl )?
ParameterDecl   "function parameter declaration"  = ( IdentifierList )? ( "..." )? Type

// # Interface types ------------------------

InterfaceType      "interface type"             = "interface" "{" ( MethodSpec ";" )? "}"
MethodSpec         "method specification"       = MethodName Signature
                                                / InterfaceTypeName

MethodName         "method name"                = identifier
InterfaceTypeName  "interface type name"        = TypeName

// # Map types ------------------------------

MapType         "map type"                      = "map" "[" KeyType "]" ElementType
KeyType         "map key type"                  = Type

// # Channel types --------------------------

ChannelType     "channel type"                  = ( "chan" / "chan" "<-" / "<-" "chan" ) ElementType

// # Blocks =================================

Block           "block"                         = "{" StatementList "}"
StatementList   "statement list"                = ( Statement ";" )*

// # Declarations and scope =================

Declaration     "declaration"                   = ConstDecl
                                                / TypeDecl
                                                / VarDecl

TopLevelDecl    "top-level declaration"         = Declaration
                                                / FunctionDecl
                                                / MethodDecl

// # Constant declarations ==================

ConstDecl       "constant declaration"          = "const" ( ConstSpec / "(" ( ConstSpec ";" )* ")" )
ConstSpec       "constant specification"        = IdentifierList ( ( Type )? "=" ExpressionList )?

IdentifierList  "identifier list"               = identifier ( "," identifier )*
ExpressionList  "expression list"               = Expression ( "," Expression )*

// # Type declarations ======================

TypeDecl        "type declaration"              = "type" ( TypeSpec / "(" ( TypeSpec ";" )? ")" )
TypeSpec        "type specification"            = AliasDecl
                                                / TypeDef

// # Alias declarations =====================

AliasDecl       "type alias declaration"        = identifier "=" Type

// # Type definitions =======================

TypeDef         "type definition"               = identifier Type

// # Variable declarations ==================

VarDecl         "variable declaration"          = "var" ( VarSpec / "(" ( VarSpec ";" )* ")" )
VarSpec         "variabpe specification"        = IdentifierList ( Type ( "=" ExpressionList )* / "=" ExpressionList )

// # Short variable declarations ============

ShortVarDecl    "short declaration of variable" = IdentifierList ":=" ExpressionList

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
                                                / "(" Expression ")"

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

QualifiedIdent  "qualified identifier"          = PackageName "." identifier

// # Composite literals =====================

CompositeLit    "composite literal"             = LiteralType LiteralValue
LiteralType     "literal type"                  = StructType
                                                / ArrayType
                                                / "[" "..." "]" ElementType
                                                / SliceType
                                                / MapType
                                                / TypeName

LiteralValue    "literal value"                 = "{" ( ElementList ( "," )? )? "}"
ElementList     "element list"                  = KeyedElement ( "," KeyedElement )*
KeyedElement    "keyed element"                 = ( Key ":" )? Element
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

Selector        "selector in expression"        = "." identifier
Index           "index in expression"           = "[" Expression "]"
Slice           "slice in expression"           = "[" ( Expression )? ":" ( Expression )? "]"
                                                / "[" ( Expression )? ":" Expression ":" Expression "]"

TypeAssertion   "type assertion"                = "." "(" Type ")"
Arguments       "arguments"                     = "(" ( ( ExpressionList / Type ( "," ExpressionList )? ) ( "..." )? ( "," )? )? ")"

// # Method expressions ===================

MethodExpr      "method expression"             = ReceiverType "." MethodName
ReceiverType    "receiver type"                 = Type

// # Operators ============================

Expression      "expression"                    = UnaryExpr
                                                / Expression binary_op Expression

UnaryExpr       "unary expression"              = PrimaryExpr
                                                / unary_op UnaryExpr

binary_op       "binary operator"               = "||"
                                                / "&&"
                                                / rel_op
                                                / add_op
                                                / mul_op

rel_op          "comparison operator"           = "=="
                                                / "!="
                                                / "<"
                                                / "<="
                                                / ">"
                                                / ">="

add_op          "addition operator"             = "+"
                                                / "-"
                                                / "|"
                                                / "^"

mul_op          "multiplication operator"       = "*"
                                                / "/"
                                                / "%"
                                                / "<<"
                                                / ">>"
                                                / "&"
                                                / "&^"


unary_op        "unary operator"                = "+"
                                                / "-"
                                                / "!"
                                                / "^"
                                                / "*"
                                                / "&"
                                                / "<-"

// # Conversions ==========================

Conversion      "type conversion"               = Type "(" Expression ( "," )? ")"

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

LabeledStmt     "labeled statement"             = Label ":" Statement
Label           "label"                         = identifier

// # Expression statements ================

ExpressionStmt  "expression statement"          = Expression

// # Send statements ======================

SendStmt        "send-statement"                = Channel "<-" Expression
Channel         "channel"                       = Expression

// # IncDec statements ====================

IncDecStmt      "increment/decrement statement" = Expression ( "++" / "--" )

// # Assignments ==========================

Assignment      "assignment"                    = ExpressionList assign_op ExpressionList

assign_op       "assignment operator"           = ( add_op / mul_op )? "="

// # 'if' statements ======================

IfStmt  "'if' statement"  = "if" ( SimpleStmt ";" )? Expression Block ( "else" ( IfStmt / Block ) )?

// # 'switch' statements ==================

SwitchStmt      "'switch' statement"            = ExprSwitchStmt
                                                / TypeSwitchStmt

// # Expression switches ------------------

ExprSwitchStmt  "expression of 'switch' statement"  = "switch" ( SimpleStmt ";" )? ( Expression )? "{" ( ExprCaseClause )* "}"
ExprCaseClause  "expression of 'case' clause"       = ExprSwitchCase ":" StatementList
ExprSwitchCase  "expression of switch's 'case'"     = "case" ExpressionList
                                                    / "default"

// # Type switches -------------------------

TypeSwitchStmt  "type-switch statement"   = "switch" ( SimpleStmt ";" )? TypeSwitchGuard "{" ( TypeCaseClause )* "}"
TypeSwitchGuard "type-switch guard"       = ( identifier ":=" ) PrimaryExpr "." "(" "type" ")"
TypeCaseClause  "type-switch clause"      = TypeSwitchCase ":" StatementList
TypeSwitchCase  "type-switch case"        = "case" TypeList
                                          / "default"
TypeList        "type list"               = Type ( "," Type )*

// # 'for' statements ======================

ForStmt         "'for' statement"               = "for" ( Condition / ForClause / RangeClause ) Block
Condition       "'for' condition"               = Expression

// # 'for' statements with 'for' clause ----

ForClause       "'for' clause"                  = ( InitStmt )? ";" ( Condition )? ";" ( PostStmt )?
InitStmt        "initial statament in 'for'"    = SimpleStmt
PostStmt        "post statement in 'for'"       = SimpleStmt

// # 'for' statements with 'range' clause --

RangeClause     "'range' clause"                = ( ExpressionList "=" / IdentifierList ":=" )? "range" Expression

// # 'go' statements =======================

GoStmt          "'go' statement"                = "go" Expression

// # 'select' statements ===================

SelectStmt      "'select' statement"            = "select" "{" ( CommClause )* "}"
CommClause      "communication-clause"          = CommCase ":" StatementList
CommCase        "comminucation 'case'"          = "case" ( SendStmt / RecvStmt )
                                                / "default"

RecvStmt        "receive-statement"             = ( ExpressionList "=" / IdentifierList ":=" )? RecvExpr
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

// # Source file organization ==============

SourceFile      "source file organization"      = PackageClause ";" ( ImportDecl ";" )* ( TopLevelDecl ";" )*

// # 'package' clause ======================

PackageClause   "'package' clause"              = "package" PackageName
PackageName     "package name"                  = identifier

// # 'import' declarations =================

ImportDecl      "'import' declaration"          = "import" ( ImportSpec / "(" ( ImportSpec ";" )* ")" )
ImportSpec      "import specification"          = ( "." / PackageName )? ImportPath
ImportPath      "import path"                   = string_lit
