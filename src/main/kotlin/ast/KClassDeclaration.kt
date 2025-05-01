package ast

import arrow.core.Option

data class KClassDeclaration(
	val modifiers:List<KModifiersInner>,
	val classHeaderType:KClassHeaderType,
	val identifier:KSimpleIdentifier,
	val typeParameters:List<KTypeParameter>,
	val primaryConstructor:Option<KPrimaryConstructor>,
	val delegationSpecifiers:Option<KDelegationSpecifiers>,
	val typeConstraints:List<KTypeConstraint>,
	val body:KClassOrEnumBody
):KDeclaration