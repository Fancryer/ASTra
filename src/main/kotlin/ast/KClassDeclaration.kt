package ast

import arrow.core.Option

data class KClassDeclaration(
	val modifiers:Option<KModifiers>,
	val classHeaderType:KClassHeaderType,
	val identifier:KSimpleIdentifier,
	val typeParameters:Option<KTypeParameters>,
	val primaryConstructor:Option<KPrimaryConstructor>,
	val delegationSpecifiers:Option<KDelegationSpecifiers>,
	val typeConstraints:Option<KTypeConstraints>,
	val body:KClassOrEnumBody
):KDeclaration