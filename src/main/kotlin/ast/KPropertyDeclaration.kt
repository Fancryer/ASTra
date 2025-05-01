package ast

import arrow.core.None
import arrow.core.Option
import arrow.core.Some
import org.fancryer.bf.id
import org.fancryer.bf.list

data class KPropertyDeclaration(
	val modifiers:(List<KModifiersInner>)=emptyList(),
	val isVal:Boolean=true,
	val typeParameters:(List<KTypeParameter>)=emptyList(),
	val recieverType:(Option<KReceiverType>)=None,
	val declaration:KMultiOrSingleVariableDeclaration,
	val typeConstraints:(List<KTypeConstraint>)=emptyList(),
	val byDelegate:Boolean=false, // by expr
	val expr:KExpression,
	val getter:(Option<KGetter>)=None,
	val setter:(Option<KSetter>)=None
):KDeclaration
{
	companion object
	{
		fun KMultiOrSingleVariableDeclaration.property(
			expr:KExpression,
			modifiers:(List<KModifiersInner>)=emptyList(),
			isVal:Boolean=true,
			typeParameters:(List<KTypeParameter>)=emptyList(),
			typeConstraints:(List<KTypeConstraint>)=emptyList(),
			byDelegate:Boolean=false, // by expr
			getter:(Option<KGetter>)=None,
			setter:(Option<KSetter>)=None
		):KPropertyDeclaration=
			KPropertyDeclaration(
				declaration=this,
				expr=expr,
				modifiers=modifiers,
				isVal=isVal,
				typeParameters=typeParameters,
				typeConstraints=typeConstraints,
				byDelegate=byDelegate,
				getter=getter,
				setter=setter
			)
	}
}

class KPropertyDeclarationBuilder
{
	private var modifiers:(List<KModifiersInner>)=emptyList()
	private var _isVal:Boolean=true
	private var typeParameters:(List<KTypeParameter>)=emptyList()
	private var receiverType:(Option<KReceiverType>)=None
	private lateinit var declaration:KMultiOrSingleVariableDeclaration
	private var typeConstraints:(List<KTypeConstraint>)=emptyList()
	private var byDelegate:Boolean=false
	private lateinit var expr:KExpression
	private var getter:(Option<KGetter>)=None
	private var setter:(Option<KSetter>)=None

	val isPrivate:Unit
		get()
		{
			modifiers=(if(modifiers.none())
				EVisibilityModifier.Private.list
			else
				modifiers+EVisibilityModifier.Private)
		}

	fun mods(modifiers:List<KModifiersInner>)
	{
		this.modifiers=modifiers.ifEmpty {this.modifiers+modifiers}
	}

	fun mods(vararg modifiers:KModifiersInner)
	{
		mods(modifiers.toList())
	}

	fun clearMods()=apply {
		modifiers=emptyList()
	}

	operator fun KModifiersInner.unaryPlus()=mods(this)

	fun isVal(isVal:Boolean)
	{
		_isVal=isVal
	}

	val isVal:Unit
		get()
		{
			isVal(true)
		}

	val isVar:Unit
		get()
		{
			isVal(false)
		}

	fun typeParameters(typeParameters:List<KTypeParameter>)
	{
		this.typeParameters=typeParameters
	}

	fun receiverType(receiverType:KReceiverType)
	{
		this.receiverType=Some(receiverType)
	}

	fun declaration(declaration:KMultiOrSingleVariableDeclaration)
	{
		this.declaration=declaration
	}

	operator fun KMultiOrSingleVariableDeclaration.unaryPlus()=declaration(this)

	fun typeConstraints(typeConstraints:List<KTypeConstraint>)
	{
		this.typeConstraints=typeConstraints
	}

	fun byDelegate(byDelegate:Boolean)
	{
		this.byDelegate=byDelegate
	}

	fun expr(expr:KExpression)
	{
		this.expr=expr
	}

	val KExpression.expr get()=expr(this)

	operator fun KExpression.unaryPlus()=expr(this)


	fun getter(getter:KGetter)
	{
		this.getter=Some(getter)
	}

	fun setter(setter:KSetter)
	{
		this.setter=Some(setter)
	}

	private fun build():KPropertyDeclaration=KPropertyDeclaration(
		modifiers,
		_isVal,
		typeParameters,
		receiverType,
		declaration,
		typeConstraints,
		byDelegate,
		expr,
		getter,
		setter
	)

	companion object
	{
		fun kpropertyDeclaration(init:KPropertyDeclarationBuilder.()->Unit)=
			KPropertyDeclarationBuilder().apply(init).build()

		fun kval(name:String,init:KPropertyDeclarationBuilder.()->Unit)=
			KPropertyDeclarationBuilder().apply {
				declaration(name.id.variableDecl)
				isVal(true)
			}.apply(init).build()

		fun kval(name:String,exp:KExpression,init:KPropertyDeclarationBuilder.()->Unit={})=
			KPropertyDeclarationBuilder().apply {
				declaration(name.id.variableDecl)
				isVal(true)
				expr(exp)
			}.apply(init).build()
	}
}