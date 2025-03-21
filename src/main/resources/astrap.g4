parser grammar astrap;

options { tokenVocab = astra; }

program: 'grammar' Name package? import_* rule_* EOF;

package: 'package' Name ('.' Name)* ';'?;
import_: 'import' Name ('.' Name)* ('.' '*')? ';'?;

rule_: mapper_rule | default;

mapper_rule: rule_name=name alias=name? from=name ',' to=name (ctx_name=name | '_') ('::=' | fix_name=name '<|>') (block | expr);

default: 'default' name? result;

result: block;

block: (stat ';')* expr;

match_clause: '|' (
  ':' name  //fast cast
  | pattern //full pattern
) ('when' expr)? '->' (block | expr);

stat: decl | expr;
decl: ('val' | 'var') name (':' type)? '=' expr;

type:
  name              #simple_type
  | type '->' type  #function_type
  | '[' type ']'    #array_type
  | '(' type ')'    #paren_type
  ;

expr:
  Int                                                           #int
  | 'null'                                                      #null
  | Float                                                       #float
  | STRING                                                      #string
  | name                                                        #variable
  | 'true'                                                      #true
  | 'false'                                                     #false
  | 'if' '(' expr ')' expr ('else' expr)?                       #if_else
  | expr type_args? '(' (expr (',' expr)*)? ')'                 #call
  | expr InfixOperator expr                                     #infix_call
  | expr '.' name                                               #access
  | expr '?.' name                                              #safe_access
  | expr type_args? '??' (expr | '(' (expr (',' expr)*)? ')')   #safe_call
  | expr '?:' expr                                              #elvis
  | lookup                                                      #lookup_call
  | '(' expr ')'                                                #paren
  | '{' (name ':' type (',' name ':' type)* '->')? expr '}'     #lambda
  | 'match' expr 'to' match_clause+ default_clause?             #match_to
  ;

lookup: '|?' name name '?|';

type_args: '<' type (',' type)* '>';

pattern:
  Int                                 #int_pattern
  // plain float
  | Float                             #float_pattern
  // plain string (may be interpolated)
  | STRING                            #raw_str_pattern
  | pattern '|->' pattern             #str_starts_with_pattern
  // plain nil = ()
  | ('null' | '?')                    #null_pattern
  // true, false
  | ('true' | 'false')                #logic_pattern
  // some unknown variable (named placeholder)
  | name                              #name_pattern
  // some variable from outer scope
  | 'val' name                        #val_pattern
  | arr_pattern                       #array_pattern
  | <assoc=right> pattern ',' pattern #tuple_pattern
  | '#' con_pattern                   #constructor_pattern
  | pattern 'as' name                 #alias_pattern
  | pattern ':' type                  #cast_pattern
  | '_'                               #wildcard
  | '(' pattern ')'                   #paren_pattern
  ;

default_clause: '|' '_' '->' (block | expr);

con_pattern: name   #singleton_constructor
  | name 'null'    #empty_constructor
  | name pattern+ #non_empty_constructor;

arr_pattern: '[' ']'                        #empty_array
  | '[' head=pattern (',' body=pattern)* (';' tail=pattern)? ']'  #non_empty_array
  ;

name: Name | BacktickName;