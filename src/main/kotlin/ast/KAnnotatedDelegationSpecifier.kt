package ast

data class KAnnotatedDelegationSpecifier(
	val delegationSpecifier:KDelegationSpecifier,
	val annotations:(List<KAnnotation>)=emptyList()
):KotlinAst