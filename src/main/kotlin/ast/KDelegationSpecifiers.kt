package ast

import arrow.core.NonEmptyList

data class KDelegationSpecifiers(
	val specifiers:NonEmptyList<KAnnotatedDelegationSpecifier>
):KotlinAst