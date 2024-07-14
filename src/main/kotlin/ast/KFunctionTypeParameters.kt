package org.fancryer.bf.ast

import arrow.core.NonEmptyList
import arrow.core.None

data class KFunctionTypeParameters(val params:NonEmptyList<KFunctionTypeParameterInner>):KotlinAst
{
	override val code:String
		get()=params.joinToString(", ","(",")") {it.code}
}

infix fun KFunctionTypeParameters.leadsTo(type:KType):KFunctionType=
	KFunctionType(None,this,type)