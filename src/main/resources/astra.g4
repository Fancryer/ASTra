lexer grammar astra;

As: 'as';
Grammar: 'grammar';
Default: 'default';
Rule: 'rule';
True: 'true';
False: 'false';
Lookup: 'lookup';
Null: 'null';
Match: 'match';
To: 'to';
When: 'when';
Import: 'import';
If: 'if';
Else: 'else';
Package: 'package';

BacktickName: '`' Name '`';
Caret: '^';
Underscore: '_';
Elvis: '?:';
Arrow: '->';
Dot: '.';
SafeDot: '?.';
Colon: ':';
Semicolon: ';';
Equals: '=';
Bind: '::=';
RecBind: '<|>';
BracketStart: '[';
BracketEnd: ']';
BraceStart: '{';
BraceEnd: '}';
ParenStart: '(';
ParenEnd: ')';
LookupStart: '|?';
LookupEnd: '?|';
Comma: ',';
Less: '<';
Greater: '>';
Question: '?';
Sharp: '#';
Forward: '|->';
Pipe: '|';
DoubleQuestion: '??';
Star: '*';

BlockStart: '{#';
BlockEnd: '#}';
KtBlockStart: '{|';
KtBlockEnd: '|}';
Val: 'val';
Var: 'var';

Name: [a-zA-Z][a-zA-Z0-9_]*;
InfixOperator: [+\-*/%]
  | '&&'
  | '||'
  | Less
  | Greater
  | [=!<>] '='
  | '==='
  | '!=='
  | '..'
  | '..<';

// Literals
Int: (Digit+ DigitOrUnderscore*)? Digit [lL]?;
Float: IntDigitPart '.' IntDigitPart ExponentPart?;
STRING:     '"' ~'"'* '"';

// Whitespace and comments
WS:                 [ \t\r\n\u000C]+ -> channel(HIDDEN);
COMMENT:            '(*' .*? '*)'    -> channel(HIDDEN);

// Fragments
fragment Digit: [0-9];
fragment DigitOrUnderscore: Digit | '_';
fragment ExponentPart: [eE] [+-]? Digit+;
fragment IntDigitPart: (Digit+ DigitOrUnderscore*)? Digit+;

fragment Letter
    : [a-zA-Z$_] // these are the "java letters" below 0x7F
    | ~[\u0000-\u007F\uD800-\uDBFF] // covers all characters above 0x7F which are not a surrogate
    | [\uD800-\uDBFF] [\uDC00-\uDFFF] // covers UTF-16 surrogate pairs encodings for U+10000 to U+10FFFF
    ;