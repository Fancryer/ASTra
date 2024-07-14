lexer grammar astra;

Backtick: '`' -> pushMode(BacktickMode);

Src_pattern: '`' (~[`] | '\\`')* '`';

Rule: 'rule';


Lexem: [A-Z][a-zA-Z0-9_]*;
Field: [a-zA-Z0-9_]+;

// Literalls
INTEGER_LITERAL:    ('0' | [1-9] (Digits? | '_'+ Digits)) [lL]?;
STRING_LITERAL:     '\'' ~'\''* '\'';

// Whitespace and comments
WS:                 [ \t\r\n\u000C]+ -> channel(HIDDEN);
COMMENT:            '%(' .*? ')'    -> channel(HIDDEN);
LINE_COMMENT:       '%' ~[\r\n]*    -> channel(HIDDEN);

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

mode BacktickMode;