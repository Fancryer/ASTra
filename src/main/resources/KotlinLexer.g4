/**
 * Kotlin lexical grammar in ANTLR4 notation
 */

lexer grammar KotlinLexer;

import UnicodeClasses;

// SECTION: lexicalGeneral

ShebangLine: '#!' ~[\r\n]*;

DelimitedComment: '/*' (DelimitedComment | . )*? '*/';

LineComment: '//' ~[\r\n]*;

WS: [\u0020\u0009\u000C];

NL: '\n' | '\r' '\n'?;

// SECTION: separatorsAndOperations

DOT: '.';
COMMA: ',';
LPAREN: '(';
RPAREN: ')';
LSQUARE: '[';
RSQUARE: ']';
LCURL: '{';
/*
 * When using another programming language (not Java) to generate a parser,
 * please replace this code with the corresponding code of a programming language you are using.
 */
RCURL: '}' { if (!_modeStack.isEmpty()) { popMode(); } };
MULT: '*';
MOD: '%';
DIV: '/';
ADD: '+';
SUB: '-';
INCR: '++';
DECR: '--';
CONJ: '&&';
DISJ: '||';
EXCL_NO_WS: '!';
COLON: ':';
SEMICOLON: ';';
ASSIGNMENT: '=';
ADD_ASSIGNMENT: '+=';
SUB_ASSIGNMENT: '-=';
MULT_ASSIGNMENT: '*=';
DIV_ASSIGNMENT: '/=';
MOD_ASSIGNMENT: '%=';
ARROW: '->';
DOUBLE_ARROW: '=>';
RANGE: '..';
RANGE_UNTIL: '..<';
COLONCOLON: '::';
DOUBLE_SEMICOLON: ';;';
HASH: '#';
AT_NO_WS: '@';
QUEST_NO_WS: '?';
LANGLE: '<';
RANGLE: '>';
LE: '<=';
GE: '>=';
EXCL_EQ: '!=';
EXCL_EQEQ: '!==';
AS_SAFE: 'as?';
EQEQ: '==';
EQEQEQ: '===';
SINGLE_QUOTE: '\'';
AMP: '&';

// SECTION: keywords

RETURN_AT: 'return@' Identifier;
CONTINUE_AT: 'continue@' Identifier;
BREAK_AT: 'break@' Identifier;

THIS_AT: 'this@' Identifier;
SUPER_AT: 'super@' Identifier;

FILE: 'file';
FIELD: 'field';
PROPERTY: 'property';
GET: 'get';
SET: 'set';
RECEIVER: 'receiver';
PARAM: 'param';
SETPARAM: 'setparam';
DELEGATE: 'delegate';

PACKAGE: 'package';
IMPORT: 'import';
CLASS: 'class';
INTERFACE: 'interface';
FUN: 'fun';
OBJECT: 'object';
VAL: 'val';
VAR: 'var';
TYPE_ALIAS: 'typealias';
CONSTRUCTOR: 'constructor';
BY: 'by';
COMPANION: 'companion';
INIT: 'init';
THIS: 'this';
SUPER: 'super';
TYPEOF: 'typeof';
WHERE: 'where';
IF: 'if';
ELSE: 'else';
WHEN: 'when';
TRY: 'try';
CATCH: 'catch';
FINALLY: 'finally';
FOR: 'for';
DO: 'do';
WHILE: 'while';
THROW: 'throw';
RETURN: 'return';
CONTINUE: 'continue';
BREAK: 'break';
AS: 'as';
IS: 'is';
IN: 'in';
NOT_IS: '!is';
NOT_IN: '!in';
OUT: 'out';
DYNAMIC: 'dynamic';

// SECTION: lexicalModifiers

PUBLIC: 'public';
PRIVATE: 'private';
PROTECTED: 'protected';
INTERNAL: 'internal';
ENUM: 'enum';
SEALED: 'sealed';
ANNOTATION: 'annotation';
DATA: 'data';
INNER: 'inner';
VALUE: 'value';
TAILREC: 'tailrec';
OPERATOR: 'operator';
INLINE: 'inline';
INFIX: 'infix';
EXTERNAL: 'external';
SUSPEND: 'suspend';
OVERRIDE: 'override';
ABSTRACT: 'abstract';
FINAL: 'final';
OPEN: 'open';
CONST: 'const';
LATEINIT: 'lateinit';
VARARG: 'vararg';
NOINLINE: 'noinline';
CROSSINLINE: 'crossinline';
REIFIED: 'reified';
EXPECT: 'expect';
ACTUAL: 'actual';

// SECTION: literals

fragment DecDigit: '0'..'9';
fragment DecDigitNoZero: '1'..'9';
fragment DecDigitOrSeparator: DecDigit | '_';

fragment DecDigits: DecDigit DecDigitOrSeparator* DecDigit
    | DecDigit;

fragment DoubleExponent: [eE] [+-]? DecDigits;

RealLiteral: FloatLiteral
    | DoubleLiteral;

FloatLiteral: DoubleLiteral [fF]
    | DecDigits [fF];

DoubleLiteral: DecDigits? '.' DecDigits DoubleExponent?
    | DecDigits DoubleExponent;

IntegerLiteral: DecDigitNoZero DecDigitOrSeparator* DecDigit
    | DecDigit;

fragment HexDigit: [0-9a-fA-F];
fragment HexDigitOrSeparator: HexDigit | '_';

HexLiteral: '0' [xX] HexDigit HexDigitOrSeparator* HexDigit
    | '0' [xX] HexDigit;

fragment BinDigit: [01];
fragment BinDigitOrSeparator: BinDigit | '_';

BinLiteral: '0' [bB] BinDigit BinDigitOrSeparator* BinDigit
    | '0' [bB] BinDigit;

UnsignedLiteral: (IntegerLiteral | HexLiteral | BinLiteral) [uU] [lL]?;

LongLiteral: (IntegerLiteral | HexLiteral | BinLiteral) [lL];

BooleanLiteral: 'true'| 'false';

NullLiteral: 'null';

CharacterLiteral: '\'' (EscapeSeq | ~[\n\r'\\]) '\'';

// SECTION: lexicalIdentifiers

fragment UnicodeDigit: UNICODE_CLASS_ND;

Identifier: (Letter | '_') (Letter | '_' | UnicodeDigit)*
    | '`' ~([\r\n] | '`')+ '`';

IdentifierOrSoftKey: Identifier
    /* Soft keywords */
    | ABSTRACT
    | ANNOTATION
    | BY
    | CATCH
    | COMPANION
    | CONSTRUCTOR
    | CROSSINLINE
    | DATA
    | DYNAMIC
    | ENUM
    | EXTERNAL
    | FINAL
    | FINALLY
    | IMPORT
    | INFIX
    | INIT
    | INLINE
    | INNER
    | INTERNAL
    | LATEINIT
    | NOINLINE
    | OPEN
    | OPERATOR
    | OUT
    | OVERRIDE
    | PRIVATE
    | PROTECTED
    | PUBLIC
    | REIFIED
    | SEALED
    | TAILREC
    | VARARG
    | WHERE
    | GET
    | SET
    | FIELD
    | PROPERTY
    | RECEIVER
    | PARAM
    | SETPARAM
    | DELEGATE
    | FILE
    | EXPECT
    | ACTUAL
    | VALUE
    /* Strong keywords */
    | CONST
    | SUSPEND;

FieldIdentifier: '$' IdentifierOrSoftKey;

fragment UniCharacterLiteral: '\\' 'u' HexDigit HexDigit HexDigit HexDigit;

fragment EscapedIdentifier: '\\' ('t' | 'b' | 'r' | 'n' | '\'' | '"' | '\\' | '$');

fragment EscapeSeq: UniCharacterLiteral
    | EscapedIdentifier;

// SECTION: characters

fragment Letter: UNICODE_CLASS_LU
    | UNICODE_CLASS_LL
    | UNICODE_CLASS_LT
    | UNICODE_CLASS_LM
    | UNICODE_CLASS_LO;

// SECTION: strings

QUOTE_OPEN: '"';

TRIPLE_QUOTE_OPEN: '"""';

mode LineString;

QUOTE_CLOSE: '"' -> popMode;

LineStrRef: FieldIdentifier;

LineStrText: ~('\\' | '"' | '$')+ | '$';

LineStrEscapedChar: EscapedIdentifier | UniCharacterLiteral;

LineStrExprStart: '${';

mode MultiLineString;

TRIPLE_QUOTE_CLOSE: MultiLineStringQuote? '"""' -> popMode;

MultiLineStringQuote: '"'+;

MultiLineStrRef: FieldIdentifier;

MultiLineStrText:  ~('"' | '$')+ | '$';

MultiLineStrExprStart: '${';

mode DEFAULT_MODE;

ErrorCharacter: .;