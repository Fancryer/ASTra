parser grammar astrap;

options { tokenVocab = astra; }

program: stat* default EOF;

stat: sign | rule | funct;

sign: rule_sign | funct_sign;

rule_sign: Rule Field '::' Lexem '->' Lexem;

funct_sign: 'funct' Field '::' Lexem ('->' Lexem)+;

rule: Field ctx_pattern ';';

funct: Field Field+ '=' funct_body ';';

funct_body: exp;

exp: Src_pattern
	| 'let' Field '=' Src_pattern 'in' exp
;

ctx_pattern: '{' ctx_pattern_field (',' ctx_pattern_field)* '}';

ctx_pattern_field: field_equality | field_typed;

field_equality: Field '=' field_pattern;

field_pattern: stringLiteral | listLiteral;

listLiteral: '[' field_pattern (',' field_pattern)* ']';
stringLiteral: '"' ~('"' | '\\"')* '"';

field_typed: Field ':' Lexem;

default: 'default' Src_pattern;