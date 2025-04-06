package ast

data class KTypeAlias(
	val modifiers:List<KModifier>,
	val identifier:KSimpleIdentifier,
	val typeParameters:KTypeParameters,
	val type:KType
):KDeclaration