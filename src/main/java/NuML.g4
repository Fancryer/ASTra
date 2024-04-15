grammar NuML;

compile_unit: module EOF;

module: 'module' ID import_stat* top_level_stat* 'end';

import_stat: 'import' STRING;

annotation: '@' ID;

top_level_stat: annotation* 'val' bind ';;'
	| 'sign' ID ':' type ';;'
	| annotation* 'namespace' ID top_level_stat* 'end';

stat: 'val' bind           #var_decl
	| 'sign' ID ':' type   #sign_decl
	| callee=exp param=exp #voidcall;

arg: ID | '(' ID ':' type ')';

block: (stat ';')* exp;

bind: ID (':' type)? '=' exp;

OP: '/' | '+' | '-' | '<>' | ('<' | '>' | '=') '='? | Asterisk | '++';
Asterisk: '*';

exp: callee=exp param=exp                               #call
	| atom                                              #atom_exp
    | variable_name                                     #variable_exp
	| '(' explist ')'                                   #tuple_exp
    | exp '.' ID                                        #field_access
    | 'not' exp                                         #not_exp
    | left=exp OP right=exp                             #infix_op
    | exp 'or' exp                                      #or_op
	| exp 'and' exp                                     #and_op
	| exp 'where' bind (',' bind)* 'end'                #where
	| lambda                                            #lambda_exp
	| lambda_full                                       #lambda_full_exp
    | 'if' pred=exp 'then' then=block 'else' else=block #branch
    | '[' explist? ']'                                  #array
    | '(' OP ')'                                        #operator
    | '{' ID ':' exp (',' ID ':' exp)* '}'              #record
    | match_to                                          #match
    ;

type: variable_name                             #flat_type
	| type Asterisk type                        #tuple_type
	| '[' type ']'                              #array_type
	| <right_assoc> input=type '->' output=type #funct_type
	| '(' type ')'                              #paren_type
	| '()'                                      #nothing_type
	| ID '[' type (',' type)*']'                #generic_type
	;

match_to: 'match' exp 'to' match_clause+ any_when_clause* else_clause?;

match_clause: '|' exp (',' exp)* ('when' exp)? '->' block;
any_when_clause: '|' '_' 'when' exp '->' block;
else_clause: '|' '_' '->' block;

variable_name: (namespace=(ID | NATIVE_VAR_PREFIX) '::')? name=ID;

lambda: 'funct' (arg | nothing='()') (':' type)? '==>' block;
lambda_full: 'funct' (arg | nothing='()')+ (':' type)? '==>' block;

explist: exp (',' exp)*;

atom: number | string | bool | nil | '()';

number: INT_MOD | FLOAT;
string: STRING;
bool: TRUE | FALSE;
nil: NIL;

NATIVE_VAR_PREFIX: '$$';

TRUE: 'true';
FALSE: 'false';
NIL: 'nil';

ID: [a-zA-Z_]+ [a-zA-Z0-9_]*;
WS: [ \t\n\r]+ -> channel(HIDDEN);
INT_MOD: SIGN? INT_POS;

FLOAT: INT_MOD [.] INT_POS EXP?     // 1.35, 1.35E-9, 0.3, -4.5
    | INT_MOD EXP                   // 1e10 -3e4
    | INT_MOD                       // -3, 45
    ;

STRING: QUOTE (~["\\] | QUOTE_ESCAPED)* QUOTE;

fragment INT_POS: [0] | DIGIT_POS [0-9]*; // no leading zeros
fragment EXP: [Ee] SIGN? INT_POS;
fragment SIGN: [+~];
fragment DIGIT_POS: [1-9];
fragment DIGIT: [0] | DIGIT_POS;
fragment QUOTE: ["];
fragment QUOTE_ESCAPED: [\\] QUOTE;

LINE_COMMENT: '//|' .*? [\n] -> channel(HIDDEN);
COMMENT: '/|' .*? '|\\' -> channel(HIDDEN);
