package ast

import arrow.core.NonEmptyList
import arrow.core.None

infix fun NonEmptyList<KFunctionTypeParameterInner>.leadsTo(type:KType):KFunctionType=
	KFunctionType(None,this,type)