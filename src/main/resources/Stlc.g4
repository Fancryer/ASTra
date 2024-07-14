grammar Stlc;

t: x                                                #variable
  | t t                                             #application
  | <assoc=right> '\\' x ':' type ',' t             #abstraction
  | 'true'                                          #constant_true
  | 'false'                                         #constant_false
  | 'if' pred=t 'then' if_true=t 'else' if_false=t  #conditional
  | '(' t ')'                                       #parenthesis
  ;

x: ID;

type: ID          #flat_type
  | ID '->' type  #abstraction_type;

ID: [a-zA-Z][_a-zA-Z0-9]*;
WS: [ \t\n\r]+ -> skip;