package ast

data class KTypeAlias(
	val modifiers:List<KModifier>,
	val identifier:KSimpleIdentifier,
	val typeParameters:List<KTypeParameter>,
	val type:KType
):KDeclaration