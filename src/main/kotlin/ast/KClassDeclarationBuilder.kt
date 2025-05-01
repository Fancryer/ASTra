package ast

import arrow.core.nel
import arrow.core.toNonEmptyListOrNone
import arrow.core.toOption
import ast.DelegationSpecifiersBuilder.Companion.kdelegationSpecifiers
import ast.KClassBodyBuilder.Companion.kclassBody
import org.fancryer.bf.id

class KClassDeclarationBuilder(private var name:KSimpleIdentifier)
{
	private var modifiers:(List<KModifiersInner>)=emptyList()
	private var classHeaderType:KClassHeaderType=KClassKeyword
	private var typeParameters:(List<KTypeParameter>)=emptyList()
	private var primaryConstructor:KPrimaryConstructor?=null
	private var delegationSpecifiers:KDelegationSpecifiers?=null
	private var typeConstraints:(List<KTypeConstraint>)=emptyList()
	private var body:KClassOrEnumBody?=null

	operator fun KModifiersInner.unaryPlus()
	{
		if(modifiers.isEmpty()) modifiers=nel()
		else modifiers=modifiers+this
	}

	fun modifiers(modifiers:List<KModifiersInner>)
	{
		this.modifiers+=modifiers
	}

	fun header(headerType:KClassHeaderType)
	{
		classHeaderType=headerType
	}

	fun typeParams(params:List<KTypeParameter>)
	{
		typeParameters=params
	}

	fun primaryConstructor(constructor:KPrimaryConstructor)
	{
		primaryConstructor=constructor
	}

	fun primaryConstructor(init:KPrimaryConstructorBuilder.()->Unit)=
		KPrimaryConstructorBuilder.kprimaryConstructor(init).also {primaryConstructor=it}

	fun delegationSpecifiers(specifiers:KDelegationSpecifiers)
	{
		delegationSpecifiers=specifiers
	}

	fun delegationSpecifiers(init:DelegationSpecifiersBuilder.()->Unit)
	{
		delegationSpecifiers=kdelegationSpecifiers(init)
	}

	fun typeConstraints(constraints:List<KTypeConstraint>)
	{
		typeConstraints=constraints
	}

	fun body(body:KClassOrEnumBody)
	{
		this.body=body
	}

	fun classBody(init:KClassBodyBuilder.()->Unit)=
		kclassBody(init).also {body=it}

	fun enumBody(init:KEnumClassBodyBuilder.()->Unit)=
		KEnumClassBodyBuilder.kenumClassBody(init).also {body=it}

	private fun build()=KClassDeclaration(
		modifiers,
		classHeaderType,
		name,
		typeParameters,
		primaryConstructor.toOption(),
		delegationSpecifiers.toOption(),
		typeConstraints,
		body ?: classBody {}
	)

	companion object
	{
		fun kclass(
			name:KSimpleIdentifier,
			vararg mods:KModifier,
			init:KClassDeclarationBuilder.()->Unit
		):KClassDeclaration=
			KClassDeclarationBuilder(name).apply {
				init()
				mods.toList()
					.toNonEmptyListOrNone()
					.onSome {
						modifiers(it)
					}
			}.build()

		fun kclass(name:String,vararg mods:KModifier,init:KClassDeclarationBuilder.()->Unit):KClassDeclaration=
			kclass(name.id,*mods,init=init)

		infix fun KSimpleIdentifier.kclass(init:KClassDeclarationBuilder.()->Unit)=kclass(this,init=init)
	}
}