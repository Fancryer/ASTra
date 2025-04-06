package ast

import arrow.core.NonEmptyList

class KUserType(
	val types:NonEmptyList<KSimpleUserType>,
	questions:Int
):KUnescapedAnnotation,KDelegationSpecifier,KParenthesizedOrFlatUserType,KTypeReference(questions)

val KUserType.delegationSpecifier get()=KAnnotatedDelegationSpecifier(this)