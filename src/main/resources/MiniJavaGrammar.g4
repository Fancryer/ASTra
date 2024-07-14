grammar MiniJavaGrammar;

// Parser
program
    : mainClass classDeclaration* EOF
    ;

mainClass
    : CLASS identifier LBRACE PUBLIC STATIC VOID 'main'
      LPAREN 'String' LBRACK RBRACK identifier RPAREN LBRACE statement RBRACE RBRACE
    ;

classDeclaration
    : CLASS identifier (EXTENDS identifier)? LBRACE varDeclaration* methodDeclaration* RBRACE
    ;

methodDeclaration
    : PUBLIC type identifier LPAREN (type identifier (COMMA type identifier)* )? RPAREN
      LBRACE varDeclaration* statement* RETURN expression SEMI RBRACE
    ;

varDeclaration
    : type identifier SEMI
    ;

type
    : INT LBRACK RBRACK
    | BOOLEAN
    | INT
    ;

statement
    : LBRACE statement* RBRACE
    | IF LPAREN expression RPAREN statement ELSE statement
    | WHILE LPAREN expression RPAREN statement
    | 'System.out.println' LPAREN expression RPAREN SEMI
    | identifier ASSIGN expression SEMI
    | identifier LBRACK expression RBRACK ASSIGN expression SEMI
    ;

expression
    : expression (AND | LT | ADD | SUB | MUL) expression
    | expression LBRACK expression RBRACK
    | expression DOT 'length'
    | expression DOT identifier LPAREN (expression (COMMA expression)* )? RPAREN
    | INTEGER_LITERAL
    | 'true'
    | 'false'
    | identifier
    | THIS
    | NEW INT LBRACK expression RBRACK
    | NEW identifier LPAREN RPAREN
    | BANG expression
    | LPAREN expression RPAREN
    ;

identifier
    : IDENTIFIER
    ;

// Lexer

// Keywords
INT:                'int';
NEW:                'new';
ELSE:               'else';
IF:                 'if';
WHILE:              'while';
BOOLEAN:            'boolean';
PUBLIC:             'public';
THIS:               'this';
RETURN:             'return';
EXTENDS:            'extends';
VOID:               'void';
CLASS:              'class';
STATIC:             'static';

// Separators
LPAREN:             '(';
RPAREN:             ')';
LBRACE:             '{';
RBRACE:             '}';
LBRACK:             '[';
RBRACK:             ']';
SEMI:               ';';
COMMA:              ',';
DOT:                '.';

// Operators
ASSIGN:             '=';
AND:                '&&';
LT:                 '<';
BANG:               '!';
ADD:                '+';
SUB:                '-';
MUL:                '*';

// Whitespace and comments
WS:                 [ \t\r\n\u000C]+ -> channel(HIDDEN);
COMMENT:            '/*' .*? '*/'    -> channel(HIDDEN);
LINE_COMMENT:       '//' ~[\r\n]*    -> channel(HIDDEN);

// Identifiers
IDENTIFIER:         Letter LetterOrDigit*;

// Literalls
INTEGER_LITERAL:    ('0' | [1-9] (Digits? | '_'+ Digits)) [lL]?;

// Fragments
fragment Digits
    : [0-9] ([0-9_]* [0-9])?
    ;

fragment LetterOrDigit
    : Letter
    | [0-9]
    ;

fragment Letter
    : [a-zA-Z$_] // these are the "java letters" below 0x7F
    | ~[\u0000-\u007F\uD800-\uDBFF] // covers all characters above 0x7F which are not a surrogate
    | [\uD800-\uDBFF] [\uDC00-\uDFFF] // covers UTF-16 surrogate pairs encodings for U+10000 to U+10FFFF
    ;