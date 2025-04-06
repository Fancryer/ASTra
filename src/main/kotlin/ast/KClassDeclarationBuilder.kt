package ast

import arrow.core.nel
import arrow.core.toNonEmptyListOrNone
import arrow.core.toOption
import ast.KClassBodyBuilder.Companion.kclassBody
import org.fancryer.bf.id

class KClassDeclarationBuilder(private var name:KSimpleIdentifier)
{
	private var modifiers:KModifiers?=null
	private var classHeaderType:KClassHeaderType=KClassKeyword
	private var typeParameters:KTypeParameters?=null
	private var primaryConstructor:KPrimaryConstructor?=null
	private var delegationSpecifiers:KDelegationSpecifiers?=null
	private var typeConstraints:KTypeConstraints?=null
	private var body:KClassOrEnumBody?=null

	operator fun KModifiersInner.unaryPlus()
	{
		when(val mods=modifiers)
		{
			null->modifiers=KModifiers(nel())
			else->KModifiers(mods.mods+this)
		}
	}

	fun modifiers(modifiers:KModifiers)
	{
		this.modifiers=modifiers
	}

	fun header(headerType:KClassHeaderType)
	{
		classHeaderType=headerType
	}

	fun typeParams(params:KTypeParameters)
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

	fun typeConstraints(constraints:KTypeConstraints)
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
		modifiers.toOption(),
		classHeaderType,
		name,
		typeParameters.toOption(),
		primaryConstructor.toOption(),
		delegationSpecifiers.toOption(),
		typeConstraints.toOption(),
		body ?: classBody {}
	)

	companion object
	{
		fun kclass(
			name:KSimpleIdentifier,
			vararg mods:EClassModifier,
			init:KClassDeclarationBuilder.()->Unit
		):KClassDeclaration=
			KClassDeclarationBuilder(name).apply {
				init()
				mods.toList()
					.toNonEmptyListOrNone()
					.onSome {
						modifiers(KModifiers(it))
					}
			}.build()

		fun kclass(name:String,vararg mods:EClassModifier,init:KClassDeclarationBuilder.()->Unit):KClassDeclaration=
			kclass(name.id,*mods,init=init)
	}
}