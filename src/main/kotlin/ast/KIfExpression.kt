package ast

import arrow.core.some

data class KIfExpression(val expression:KExpression,val inner:KIfInner):KPrimaryExpression

/*
ifExpression (used by primaryExpression):
	'if' '(' expression ')' (
		controlStructureBody
		| (controlStructureBody? ';'? 'else' (controlStructureBody | ';'))
		| ';'
	)
	;
*/

fun KExpression.ifElse(ifTrue:KControlStructureBody,ifFalse:KControlStructureBodyOrSemicolon):KIfExpression=
	KIfExpression(this,KIfInnerFull(ifTrue.some(),ifFalse))

infix fun KExpression.ifTrue(ifTrue:KControlStructureBody):KIfExpression=
	ifElse(ifTrue,KSemicolon)

fun branch(pred:KExpression,ifTrue:KControlStructureBody,ifFalse:KControlStructureBodyOrSemicolon):KIfExpression=
	pred.ifElse(ifTrue,ifFalse)

