package org.fancryer.bf.ast

import arrow.core.some

data class KIfExpression(val expression:KExpression,val inner:KIfInner):KPrimaryExpression
{
	override val code:String
		get()=buildString {
			append("if (")
			append(expression.code)
			append(") ")
			append(inner.code)
		}
}

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