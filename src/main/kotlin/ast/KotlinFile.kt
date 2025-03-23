package org.fancryer.bf.ast

import arrow.core.*
import org.fancryer.bf.*
import org.fancryer.bf.ast.FunctionDeclarationBuilder.Companion.kfun
import org.fancryer.bf.ast.KBlockBuilder.Companion.kblock
import org.fancryer.bf.ast.KClassParameterBuilder.Companion.kclassParameter
import org.fancryer.bf.ast.KEnumClassBodyBuilder.Companion.kenumClassBody
import org.fancryer.bf.ast.KEnumEntriesBuilder.Companion.kenumEntries
import org.fancryer.bf.ast.KPrimaryConstructorBuilder.Companion.kprimaryConstructor
import org.fancryer.bf.ast.KPropertyDeclarationBuilder.Companion.kpropertyDeclaration

val String.errf get()={this.err}
val String.err:Nothing get()=error(this)

data class KKotlinScript(
	val shebang:Option<String>,
	val annotations:List<KFileAnnotation>,
	val packageHeader:KPackageHeader,
	val importList:List<KImportHeader>,
	val statements:List<KStatement>
):KotlinAst

data class KFileAnnotation(val annotations:NonEmptyList<KUnescapedAnnotation>):KotlinAst

sealed interface KUnescapedAnnotation:KotlinAst

class KUserType(
	val types:NonEmptyList<KSimpleUserType>,
	questions:Int
):KUnescapedAnnotation,KDelegationSpecifier,KParenthesizedOrFlatUserType,KTypeReference(questions)
{
	override val code:String
		get()=types.joinToString(".") {it.code}
}

val KUserType.delegationSpecifier get()=KAnnotatedDelegationSpecifier(this)

data class KPackageHeader(
	val identifier:(Option<KIdentifier>)=None
):KotlinAst
{
	override val code:String=identifier.fold({""}) {it.code}
}

sealed interface KImportHeader:KotlinAst
{
	data class KSingleImport(val identifier:KIdentifier):KImportHeader
	{
		override val code:String="import ${identifier.code}"
	}

	data class KWildcardImport(val identifier:KIdentifier):KImportHeader
	{
		override val code:String="import ${identifier.code}.*"
	}

	data class KAliasImport(val identifier:KIdentifier,val alias:KSimpleIdentifier):KImportHeader
	{
		override val code:String="import ${identifier.code} as ${alias.code}"
	}
}

data class KTypeAlias(
	val modifiers:List<KModifier>,
	val identifier:KSimpleIdentifier,
	val typeParameters:KTypeParameters,
	val type:KType
):KDeclaration

sealed interface KDeclaration:KClassMemberDeclaration,KStatementInner

data class KClassDeclaration(
	val modifiers:Option<KModifiers>,
	val classHeaderType:KClassHeaderType,
	val identifier:KSimpleIdentifier,
	val typeParameters:Option<KTypeParameters>,
	val primaryConstructor:Option<KPrimaryConstructor>,
	val delegationSpecifiers:Option<KDelegationSpecifiers>,
	val typeConstraints:Option<KTypeConstraints>,
	val body:IKClassBody
):KDeclaration
{
	override val code:String
		get()=buildString {
			modifiers.onSome {append(it.code).append(' ')}
			append(classHeaderType.code)
			append(' ')
			append(identifier.code)
			append(typeParameters.fold({" "}) {it.code})
			primaryConstructor.onSome {append(it.code)}
			println(primaryConstructor)
			delegationSpecifiers.onSome {append(it.code)}
			typeConstraints.onSome {append(it.code)}
			append(body.code)
		}
}

class KClassDeclarationBuilder(private var name:KSimpleIdentifier)
{
	private var modifiers:KModifiers?=null
	private var classHeaderType:KClassHeaderType=KClassKeyword
	private var typeParameters:KTypeParameters?=null
	private var primaryConstructor:KPrimaryConstructor?=null
	private var delegationSpecifiers:KDelegationSpecifiers?=null
	private var typeConstraints:KTypeConstraints?=null
	private var body:IKClassBody?=null

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
		kprimaryConstructor(init).also {primaryConstructor=it}

	fun delegationSpecifiers(specifiers:KDelegationSpecifiers)
	{
		delegationSpecifiers=specifiers
	}

	fun typeConstraints(constraints:KTypeConstraints)
	{
		typeConstraints=constraints
	}

	fun body(body:IKClassBody)
	{
		this.body=body
	}

	fun classBody(init:ClassBodyBuilder.()->Unit)=
		ClassBodyBuilder().apply(init).build().also {body=it}

	fun enumBody(init:KEnumClassBodyBuilder.()->Unit)=
		kenumClassBody(init).also {body=it}

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

sealed interface KClassHeaderType:KotlinAst

sealed interface IKClassBody:KotlinAst

data class KClassBody(
	val memberDeclarations:(List<KClassMemberDeclaration>)=emptyList()
):IKClassBody
{
	override val code:String=
		if(memberDeclarations.isEmpty()) ""
		else "{${memberDeclarations.joinToString(";") {it.code}}}"
}

@ClassBuilderDsl
annotation class ClassBodyBuilderDsl

@ClassBodyBuilderDsl
class ClassBodyBuilder
{
	private val declarations=mutableListOf<KClassMemberDeclaration>()

	operator fun KClassMemberDeclaration.unaryPlus()
	{
		declarations+=this
	}

	fun property(init:KPropertyDeclarationBuilder.()->Unit)
	{
		declarations+=kpropertyDeclaration(init)
	}

	fun function(id:KSimpleIdentifier,init:FunctionDeclarationBuilder.()->Unit)
	{
		declarations+=kfun(id,init)
	}

	fun function(id:String,init:FunctionDeclarationBuilder.()->Unit)=
		function(id.id,init)

	infix fun String.funBlock(init:KBlockBuilder.()->Unit)=
		id funBlock init

	infix fun String.funExpr(expr:KExpression)=
		id funExpr expr

	infix fun KSimpleIdentifier.funBlock(init:KBlockBuilder.()->Unit)=
		function(this) {
			blockBody(init)
		}

	infix fun KSimpleIdentifier.funExpr(expr:KExpression)=
		function(this) {
			exprBody(expr)
		}

	fun build()=KClassBody(declarations)
}

data class KEnumClassBody(
	val entries:Option<KEnumEntries>,
	val memberDeclarations:List<KClassMemberDeclaration>
):IKClassBody

class KEnumClassBodyBuilder
{
	private var entries:KEnumEntries?=null
	private var memberDeclarations:(List<KClassMemberDeclaration>)=emptyList()

	fun entries(entries:KEnumEntries)
	{
		this.entries=entries
	}

	fun entries(init:KEnumEntriesBuilder.()->Unit)=
		kenumEntries(init).also {entries=it}

	fun memberDeclaration(declaration:KClassMemberDeclaration)
	{
		memberDeclarations+=declaration
	}

	private fun build()=KEnumClassBody(
		entries.toOption(),
		memberDeclarations
	)

	companion object
	{
		fun kenumClassBody(init:KEnumClassBodyBuilder.()->Unit)=
			KEnumClassBodyBuilder().apply(init).build()
	}
}

data class KPrimaryConstructor(
	val classParameters:List<KClassParameter>,
	val modifiers:(Option<KModifiers>)=None
):KotlinAst
{
	override val code:String
		get()=buildString {
			modifiers.onSome {append(it.code).append(' ')}
			append('(')
			append(classParameters.joinToString {it.code})
			append(')')
		}
}

class KPrimaryConstructorBuilder
{
	private var classParameters:(List<KClassParameter>)=emptyList()
	private var modifiers:KModifiers?=null

	fun param(id:KSimpleIdentifier,init:KClassParameterBuilder.()->Unit)=
		kclassParameter(id,init).also {classParameters+=it}

	fun param(id:String,init:KClassParameterBuilder.()->Unit)=
		param(id.id,init)

	infix fun KSimpleIdentifier.init(init:KClassParameterBuilder.()->Unit)=
		param(this,init)

	infix fun String.init(init:KClassParameterBuilder.()->Unit)=
		param(this,init)

	infix fun String.ofType(type:KType)=
		param(this) {type(type)}

	infix fun String.ofType(type:String)=
		this ofType type.type


	private fun build()=KPrimaryConstructor(classParameters,modifiers.toOption())

	companion object
	{
		fun kprimaryConstructor(init:KPrimaryConstructorBuilder.()->Unit)=
			KPrimaryConstructorBuilder().apply(init).build()
	}
}

class KClassParametersBuilder
{
	private val classParameters=mutableListOf<KClassParameter>()

	fun param(id:KSimpleIdentifier,init:KClassParameterBuilder.()->Unit)=
		kclassParameter(id,init).also {classParameters+=it}

	fun param(id:String,init:KClassParameterBuilder.()->Unit)=
		param(id.id,init)

	infix fun KSimpleIdentifier.init(init:KClassParameterBuilder.()->Unit)=
		param(this,init)

	infix fun String.init(init:KClassParameterBuilder.()->Unit)=
		param(this,init)

	infix fun String.ofType(type:KType)=
		param(this) {type(type)}

	infix fun String.ofType(type:String)=
		this ofType type.type

	private fun build()=classParameters.toList()

	companion object
	{
		fun kclassParameters(init:KClassParametersBuilder.()->Unit)=
			KClassParametersBuilder().apply(init).build()
	}
}

data class KClassParameter(
	val modifiers:Option<KModifiers>,
	val isVal:Boolean,
	val identifier:KSimpleIdentifier,
	val type:KType,
	val expression:Option<KExpression>
):KotlinAst
{
	override val code:String
		get()=buildString {
			modifiers.onSome {append(it.code).append(' ')}
			append(if(isVal) "val " else "var ")
			append(identifier.code)
			append(": ").append(type.code)
			expression.onSome {append(" = ").append(it.code)}
		}
}

class KClassParameterBuilder(id:KSimpleIdentifier)
{
	private var modifiers:KModifiers?=null
	private var isVal:Boolean=true
	private var identifier:KSimpleIdentifier=id
	private var type:KType?=null
	private var expression:KExpression?=null

	constructor(id:String):this(id.id)

	operator fun KModifiers.unaryPlus()
	{
		modifiers=this
	}

	fun type(type:KType)
	{
		this.type=type
	}

	fun type(type:String)=type(type.type)

	operator fun KType.unaryPlus()
	{
		type=this
	}

	fun expression(expression:KExpression)
	{
		this.expression=expression
	}

	operator fun KExpression.unaryPlus()
	{
		expression=this
	}

	fun isVal(isVal:Boolean)
	{
		this.isVal=isVal
	}

	fun isVal()
	{
		isVal=true
	}

	fun isVar()
	{
		isVal=false
	}

	private fun build()=KClassParameter(
		modifiers.toOption(),
		isVal,
		identifier,
		type ?: error("type must be defined"),
		expression.toOption()
	)

	companion object
	{
		fun kclassParameter(id:KSimpleIdentifier,init:KClassParameterBuilder.()->Unit)=
			KClassParameterBuilder(id).apply(init).build()

		fun kclassParameter(id:String,init:KClassParameterBuilder.()->Unit)=
			KClassParameterBuilder(id).apply(init).build()
	}
}

data class KDelegationSpecifiers(
	val specifiers:NonEmptyList<KAnnotatedDelegationSpecifier>
):KotlinAst
{
	override val code:String=specifiers.joinToString {it.code}
}

sealed interface KDelegationSpecifier:KotlinAst

data class KConstructorInvocation(
	val userType:KUserType,
	val valueArguments:KValueArguments
):KUnescapedAnnotation,KDelegationSpecifier

data class KAnnotatedDelegationSpecifier(
	val delegationSpecifier:KDelegationSpecifier,
	val annotations:(List<KAnnotation>)=emptyList()
):KotlinAst
{
	override val code:String=buildString {
		annotations.map {it.code}.forEach {append(it).append(' ')}
		append(delegationSpecifier.code)
	}
}

data class KFunctionDelegationSpecifier(
	val isSuspend:Boolean,
	val functionType:KFunctionType
):KDelegationSpecifier

data class KTypeParameters(val params:NonEmptyList<KTypeParameter>):KotlinAst
data class KTypeParameter(
	val modifiers:Option<KTypeParameterModifiers>,
	val identifier:KSimpleIdentifier,
	val type:Option<KType>
):KotlinAst

data class KTypeConstraints(val constraints:NonEmptyList<KTypeConstraint>):KotlinAst
data class KTypeConstraint(
	val annotations:List<KAnnotation>,
	val identifier:KSimpleIdentifier,
	val type:KType
):KotlinAst

sealed interface KClassMemberDeclaration:KotlinAst

data class KCompanionObject(
	val modifiers:Option<KModifiers>,
	val isData:Boolean,
	val identifier:Option<KSimpleIdentifier>,
	val delegationSpecifiers:Option<KDelegationSpecifiers>,
	val classBody:Option<KClassBody>
):KClassMemberDeclaration

data class KAnonymousInitializer(val block:KBlock):KClassMemberDeclaration

data class KFunctionValueParameter(
	val modifiers:(Option<KParameterModifiers>)=None,
	val parameter:KParameter,
	val expression:(Option<KExpression>)=None
):KotlinAst
{
	override val code:String=buildString {
		modifiers.onSome {append(it.code).append(' ')}
		append(parameter.code)
		expression.onSome {append(" = ").append(it.code)}
	}
}

data class KSecondaryConstructor(
	val modifiers:(Option<KModifiers>)=None,
	val functionValueParameters:List<KFunctionValueParameter>,
	val constructorDelegationCall:(Option<KConstructorDelegationCall>)=None,
	val block:KBlock
):KClassMemberDeclaration

data class KObjectDeclaration(
	val modifiers:(Option<KModifiers>)=None,
	val identifier:KSimpleIdentifier,
	val delegationSpecifiers:(Option<KDelegationSpecifiers>)=None,
	val classBody:(Option<KClassBody>)=None
):KDeclaration

data class KFunctionDeclaration(
	val modifiers:(Option<KModifiers>)=None,
	val typeParameters:(Option<KTypeParameters>)=None,
	val receiverType:(Option<KType>)=None,
	val identifier:KSimpleIdentifier,
	val functionValueParameters:List<KFunctionValueParameter>,
	val type:(Option<KType>)=None,
	val typeConstraints:(Option<KTypeConstraints>)=None,
	val functionBody:(Option<KFunctionBody>)=None
):KDeclaration
{
	override val code:String=buildString {
		modifiers.onSome {append(it.code).append(' ')}
		append("fun ")
		typeParameters.onSome {append(it.code).append(' ')}
		receiverType.onSome {append(it.code).append('.')}
		append(identifier.code)
		append('(')
		append(functionValueParameters.joinToString {it.code})
		append(')')
		type.onSome {append(": ").append(it.code).append(' ')}
		typeConstraints.onSome {append(it.code)}
		functionBody.onSome {append(it.code)}
	}
}

class FunctionDeclarationBuilder(private var identifier:KSimpleIdentifier)
{
	private var modifiers:(Option<KModifiers>)=None
	private var typeParameters:(Option<KTypeParameters>)=None
	private var receiverType:(Option<KType>)=None

	//Check
	private var valueParameters:(List<KFunctionValueParameter>)=emptyList()
	private var type:(Option<KType>)=None
	private var typeConstraints:(Option<KTypeConstraints>)=None
	private var functionBody:(Option<KFunctionBody>)=None

	operator fun KModifiers.unaryPlus()
	{
		modifiers=some()
	}

	fun typeParameters(params:KTypeParameters)
	{
		typeParameters=params.some()
	}

	fun recieverType(type:KType)
	{
		receiverType=type.some()
	}

	fun valueParameter(param:KFunctionValueParameter)
	{
		+param
	}

	operator fun KFunctionValueParameter.unaryPlus()
	{
		valueParameters+=this
	}

	operator fun List<KFunctionValueParameter>.unaryPlus()
	{
		valueParameters+=this
	}

	fun valueParameters(params:List<Pair<String,KType>>)
	{
		+params.map {(id,type)->
			type.let {id.id functionValueParameter it}
		}
	}

	infix fun String.ofType(type:KType)=
		+KFunctionValueParameter(parameter=this.id param type)

	infix fun String.ofType(type:String)=
		this ofType type.type

	infix fun KSimpleIdentifier.ofType(type:KType)=
		+KFunctionValueParameter(parameter=this param type)

	infix fun KSimpleIdentifier.ofType(type:String)=
		this ofType type.type

	fun type(type:KType)
	{
		this.type=type.some()
	}

	fun typeConstrants(constraints:KTypeConstraints)
	{
		typeConstraints=constraints.some()
	}

	fun body(b:KFunctionBody)
	{
		functionBody=b.some()
	}

	fun body(b:()->KFunctionBody)=body(b())

	fun blockBody(init:KBlockBuilder.()->Unit)=body(kblock(init))
	fun exprBody(expr:KExpression)=body(KFunctionAssignExpression(expr))

	operator fun KFunctionBody.unaryPlus()
	{
		functionBody=this.some()
	}

	fun bodyBlock(b:KBlock)
	{
		functionBody=b.some()
	}

	fun bodyExp(b:KExpression)
	{
		functionBody=KFunctionAssignExpression(b).some()
	}

	private fun build():KFunctionDeclaration=
		KFunctionDeclaration(
			modifiers,
			typeParameters,
			receiverType,
			identifier,
			valueParameters,
			type,
			typeConstraints,
			functionBody
		)

	companion object
	{
		fun kfun(
			identifier:KSimpleIdentifier,
			init:FunctionDeclarationBuilder.()->Unit
		)=
			FunctionDeclarationBuilder(identifier).apply(init).build()

		fun kfun(
			identifier:String,
			init:FunctionDeclarationBuilder.()->Unit
		)=kfun(identifier.id,init)
	}
}

val String.type
	get()=this.id
		.simpleUserType
		.userType
		.type

fun String.type(projection:KTypeProjection)=this.id
	.simpleGeneric(projection)
	.userType
	.type

fun String.type(how:(String)->String)=how(this).type

fun KSimpleIdentifier.type(how:String.()->String)=how(id).type
val KSimpleIdentifier.type get()=id.type

@DslMarker
annotation class ClassBuilderDsl

@ClassBuilderDsl
annotation class ModifiersBuilderDsl

@ClassBuilderDsl
class ClassDeclarationBuilder(private val identifier:KSimpleIdentifier)
{
	private var modifiers:(Option<KModifiers>)=None
	private var classHeaderType:(Option<KClassHeaderType>)=None
	private var typeParameters:(Option<KTypeParameters>)=None
	private var primaryConstructor:(Option<KPrimaryConstructor>)=None
	private var delegationSpecifiers:(Option<KDelegationSpecifiers>)=None
	private var typeConstraints:(Option<KTypeConstraints>)=None
	private var classBody:(Option<KClassBody>)=None

	fun headerType(type:KClassHeaderType)
	{
		classHeaderType=type.some()
	}

	fun body(init:ClassBodyBuilder.()->Unit)
	{
		classBody=ClassBodyBuilder().apply(init).build().some()
	}

	/*
	data class KClassDeclaration(
	val modifiers:Option<KModifiers>,
	val classHeaderType:KClassHeaderType,
	val typeParameters:Option<KTypeParameters>,
	val primaryConstructor:Option<KPrimaryConstructor>,
	val delegationSpecifiers:Option<KDelegationSpecifiers>,
	val typeConstraints:Option<KTypeConstraints>,
	val body:IKClassBody
):KDeclaration
	*/

	fun build():KClassDeclaration=
		classHeaderType.fold("classHeaderType must be defined".errf) {headerType->
			classBody.fold("classBody must be defined".errf) {
				KClassDeclaration(
					modifiers,
					headerType,
					identifier,
					typeParameters,
					primaryConstructor,
					delegationSpecifiers,
					typeConstraints,
					it
				)
			}
		}
}

fun klass(id:KSimpleIdentifier,init:ClassDeclarationBuilder.()->Unit)=
	ClassDeclarationBuilder(id).also(init).build()

sealed interface KFunctionBody:KotlinAst

data class KFunctionAssignExpression(val expression:KExpression):KFunctionBody
{
	override val code:String
		get()=" = ${expression.code}"
}

data class KVariableDeclaration(
	val annotations:(List<KAnnotation>)=emptyList(),
	val identifier:KSimpleIdentifier,
	val type:(Option<KType>)=None
):KMultiOrSingleVariableDeclaration,KLambdaParameter
{
	override val code:String
		get()=buildString {
			annotations.forEach {
				append(it.code)
				append(' ')
			}
			append(identifier.code)
			type.onSome {
				append(':')
				append(it.code)
			}
		}
}

val KVariableDeclaration.lambdaParams:List<KLambdaParameter> get()=nel()

data class KMultiVariableDeclaration(
	val variableDeclarations:NonEmptyList<KVariableDeclaration>
):KMultiOrSingleVariableDeclaration

sealed interface KMultiOrSingleVariableDeclaration:KotlinAst

data class KPropertyDeclaration(
	val modifiers:(Option<KModifiers>)=None,
	val isVal:Boolean=true,
	val typeParameters:(Option<KTypeParameters>)=None,
	val recieverType:(Option<KReceiverType>)=None,
	val declaration:KMultiOrSingleVariableDeclaration,
	val typeConstraints:(Option<KTypeConstraints>)=None,
	val byDelegate:Boolean=false, // by expr
	val expr:KExpression,
	val getter:(Option<KGetter>)=None,
	val setter:(Option<KSetter>)=None
):KDeclaration
{
	override val code:String
		get()=buildString {
			modifiers.onSome {append(it.code).append(' ')}
			append(if(isVal) "val " else "var ")
			typeParameters.onSome {append(it.code).append(' ')}
			recieverType.onSome {append(it.code).append('.')}
			append(declaration.code).append(' ')
			typeConstraints.onSome {append(it.code).append(' ')}
			append(if(byDelegate) "by " else "= ")
			append(expr.code)
			getter.onSome {append(it.code).append(' ')}
			setter.onSome {append(it.code)}
		}

	companion object
	{
		fun KMultiOrSingleVariableDeclaration.property(
			expr:KExpression,
			modifiers:(Option<KModifiers>)=None,
			isVal:Boolean=true,
			typeParameters:(Option<KTypeParameters>)=None,
			typeConstraints:(Option<KTypeConstraints>)=None,
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
	private var modifiers:(Option<KModifiers>)=None
	private var isVal:Boolean=true
	private var typeParameters:(Option<KTypeParameters>)=None
	private var receiverType:(Option<KReceiverType>)=None
	private lateinit var declaration:KMultiOrSingleVariableDeclaration
	private var typeConstraints:(Option<KTypeConstraints>)=None
	private var byDelegate:Boolean=false
	private lateinit var expr:KExpression
	private var getter:(Option<KGetter>)=None
	private var setter:(Option<KSetter>)=None

	val isPrivate:Unit
		get()
		{
			modifiers.fold({
				EVisibilityModifier.Private.nel()
			}) {
				it.mods+EVisibilityModifier.Private
			}.also {
				modifiers=KModifiers(it).some()
			}
		}

	fun modifiers(modifiers:KModifiers)
	{
		this.modifiers=Some(modifiers)
	}

	fun isVal(isVal:Boolean)
	{
		this.isVal=isVal
	}

	fun typeParameters(typeParameters:KTypeParameters)
	{
		this.typeParameters=Some(typeParameters)
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

	fun typeConstraints(typeConstraints:KTypeConstraints)
	{
		this.typeConstraints=Some(typeConstraints)
	}

	fun byDelegate(byDelegate:Boolean)
	{
		this.byDelegate=byDelegate
	}

	fun expr(expr:KExpression)
	{
		this.expr=expr
	}

	val KExpression.expr get()=apply {this@KPropertyDeclarationBuilder.expr=this}
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
		isVal,
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
	}
}


data class KGetter(
	val modifiers:Option<KModifiers>,
	val getterBody:Option<KGetterBody>
):KotlinAst

data class KGetterBody(
	val type:Option<KType>,
	val functionBody:KFunctionBody
):KotlinAst

data class KSetter(
	val modifiers:Option<KModifiers>,
	val setterBody:Option<KSetterBody>
):KotlinAst

data class KSetterBody(
	val param:KFunctionValueParameterWithOptionalType,
	val type:Option<KType>,
	val functionBody:KFunctionBody
):KotlinAst

data class KParametersWithOptionalType(
	val params:List<KFunctionValueParameterWithOptionalType>
):KotlinAst
{
	override val code:String
		get()=buildString {
			append("(")
			append(params.joinToString {it.code})
			append(")")
		}
}

data class KFunctionValueParameterWithOptionalType(
	val modifiers:(Option<KParameterModifiers>)=None,
	val parameter:KParameterWithOptionalType,
	val expression:(Option<KExpression>)=None
):KotlinAst
{
	override val code:String
		get()=buildString {
			modifiers.onSome {
				append(it.code)
				append(' ')
			}
			append(parameter.code)
			expression.onSome {
				append('=')
				append(it.code)
			}
		}
}

data class KParameterWithOptionalType(
	val identifier:KSimpleIdentifier,
	val type:(Option<KType>)=None
):KotlinAst
{
	override val code:String
		get()=buildString {
			append(identifier.code)
			type.onSome {
				append(':')
				append(it.code)
			}
		}
}

data class KParameter(
	val identifier:KSimpleIdentifier,
	val type:KType
):KFunctionTypeParameterInner
{
	override val code:String="${identifier.code} : ${type.code}"
}

infix fun KSimpleIdentifier.param(type:KType)=KParameter(this,type)

data class KConstructorDelegationCall(
	val delegationQualifier:EConstructorDelegationQualifier,
	val valueArguments:Option<KValueArguments>
):KotlinAst
{
	enum class EConstructorDelegationQualifier
	{
		THIS,
		SUPER
	}
}

data class KEnumEntries(
	val entries:NonEmptyList<KEnumEntry>
):KotlinAst

class KEnumEntriesBuilder
{
	private val entries=mutableListOf<KEnumEntry>()

	fun entry(entry:KEnumEntry)
	{
		entries+=entry
	}

	fun entries(entries:List<KEnumEntry>)
	{
		this.entries+=entries
	}

	private fun build()=
		KEnumEntries(entries.toNonEmptyListOrNull() ?: error("No enum entries"))

	companion object
	{
		fun kenumEntries(init:KEnumEntriesBuilder.()->Unit)=
			KEnumEntriesBuilder().apply(init).build()
	}
}

data class KEnumEntry(
	val modifiers:Option<KModifiers>,
	val identifier:KSimpleIdentifier,
	val valueArguments:Option<KValueArguments>,
	val classBody:Option<KClassBody>
):KotlinAst

data class KType(
	val modifiers:(List<KTypeModifier>)=emptyList(),
	val typeInner:KTypeInner
):KFunctionTypeParameterInner
{
	override val code:String
		get()=buildString {
			modifiers.forEachIndexed {i,it->
				append(it.code)
				if(i!=modifiers.size-1) append(' ')
			}
			if(modifiers.isNotEmpty()) append(' ')
			append(typeInner.code)
		}
}

val KType.functionTypeParameters:(NonEmptyList<KFunctionTypeParameterInner>)
	get()=nel()

sealed interface KTypeInner:KotlinAst

//rtype: typeModifier+? typeinner=rdefinitelyNonNullableType;

sealed class KTypeReference(questions:Int):KNullableType(questions),KRecieverTypeInner,KTypeInner
{
	data object Dynamic:KTypeReference(0)
}

sealed class KNullableType(val questions:Int):KTypeInner,KRecieverTypeInner

data class KSimpleUserType(
	val identifier:KSimpleIdentifier,
	val typeArguments:(Option<KTypeArguments>)=None
):KotlinAst
{
	override val code:String
		get()=buildString {
			append(identifier.code)
			typeArguments.onSome {
				append(it.code)
			}
		}
}

sealed interface KTypeProjection:KotlinAst
data class KConcreteTypeProjection(
	val type:KType,
	val modifiers:(List<KTypeProjectionModifier>)=emptyList()
):KTypeProjection
{
	override val code:String=buildString {
		modifiers.forEach {append(it.code).append(' ')}
		append(type.code)
	}
}

val KType.proj get()=KConcreteTypeProjection(this)

val NonEmptyList<KTypeProjection>.typeArgs get()=KTypeArguments(this)

data object KStarTypeProjection:KTypeProjection

sealed interface KTypeProjectionModifier:KotlinAst

data class KFunctionType(
	val recieverType:(Option<KReceiverType>)=None,
	val parameters:NonEmptyList<KFunctionTypeParameterInner>,
	val returnType:KType
):KTypeInner
{
	override val code:String
		get()=buildString {
			recieverType.onSome {
				append(it.code)
				append('.')
			}
			append(parameters.joinToString(", ","(",")") {it.code})
			append(" -> ")
			append(returnType.code)
		}
}

sealed interface KFunctionTypeParameterInner:KotlinAst

class KParenthesizedType(val type:KType,questions:Int):KNullableType(questions),KTypeInner,KRecieverTypeInner

sealed interface KRecieverTypeInner:KotlinAst

data class KReceiverType(
	val modifiers:List<KTypeModifier>,
	val type:KRecieverTypeInner
):KotlinAst

sealed interface KParenthesizedOrFlatUserType:KotlinAst

data class KParenthesizedUserType(val type:KParenthesizedOrFlatUserType):KParenthesizedOrFlatUserType

//TODO rename fields
data class KDefinitelyNonNullableType(
	val leftModifiers:List<KTypeModifier>,
	val left:KParenthesizedOrFlatUserType,
	val rightModifiers:List<KTypeModifier>,
	val right:KParenthesizedOrFlatUserType
):KTypeInner


sealed interface KLabelOrAnnotation:KotlinAst

sealed interface KStatementInner:KotlinAst

data class KLabel(val identifier:KSimpleIdentifier):KLabelOrAnnotation,KUnaryPrefix

sealed interface KControlStructureBody:KIfInner,KControlStructureBodyOrSemicolon

data class KBlock(val statements:(List<KStatement>)=emptyList()):KControlStructureBody,KFunctionBody,KStatementInner
{
	override val code:String
		get()=buildString {
			append('{')
			statements.asSequence()
				.map {it.code}
				.forEach {append(it).append(';')}
			append('}')
		}
}

class KBlockBuilder
{
	private var statements:(List<KStatement>)=emptyList()

	infix fun KSimpleIdentifier.declare(expression:KExpression)=KPropertyDeclaration(
		declaration=KVariableDeclaration(identifier=this),
		expr=expression
	)

	infix fun String.declare(expression:KExpression)=id declare expression

	fun stat(statement:KStatementInner)
	{
		+KStatement(emptyList(),statement)
	}

	fun stat(statement:KStatement)=apply {
		+statement
	}

	fun stats(stats:List<KStatement>)=apply {
		statements+=stats
	}

	operator fun List<KStatement>.unaryPlus()
	{
		stats(this)
	}

	operator fun KStatement.unaryPlus()
	{
		statements+=this
	}

	operator fun KStatementInner.unaryPlus()
	{
		stat(this)
	}

	private fun build()=KBlock(statements)

	companion object
	{
		fun kblock(init:KBlockBuilder.()->Unit)=
			KBlockBuilder().also(init).build()
	}
}

sealed interface KLoopStatement:KStatementInner

data class KForStatement(
	val annos:List<KAnnotation>,
	val declaration:KMultiOrSingleVariableDeclaration,
	val expression:KExpression,
	val controlStructureBody:Option<KControlStructureBody>
):KLoopStatement

data class KWhileStatement(
	val expression:KExpression,
	val body:KControlStructureBodyOrSemicolon
):KLoopStatement

data class KDoWhileStatement(
	val controlStructureBody:Option<KControlStructureBody>,
	val expression:KExpression
):KLoopStatement

sealed interface KAssignmentLeft:KStatementInner

data class KAssignment(val left:KAssignmentLeft,val right:KExpression):KStatementInner

data class KAssignableWithAndExpression(val left:KAssignableExpression,val right:EAssignmentAndOperator):KAssignmentLeft

sealed interface KExpression:KStatementInner,KWhenCondition
/*
expression
    : unaryPrefix* postfixUnaryExpression #prefixUnary
    | expression NL* asOperator NL* type #as
    | expression multiplicativeOperator NL* expression #muldivmod
    | expression additiveOperator NL* expression #addsub
    | expression (RANGE | RANGE_UNTIL) NL* expression #range
    | expression simpleIdentifier NL* expression #infix
    | expression NL* elvis NL* expression #elvis_e
    | expression (inOperator NL* expression | isOperator NL* type) #in_is
    | expression callSuffix #call
    | expression comparisonOperator NL* expression #comparison
    | expression equalityOperator NL* expression #equal
    | expression NL* CONJ NL* expression #and
    | expression NL* DISJ NL* expression #or
    ;
*/

/**
 * ||
 * */
data class KDisjunction(val left:KExpression,val right:KExpression):KExpression

/**
 * &&
 * */
data class KConjunction(val left:KExpression,val right:KExpression):KExpression

data class KEquality(
	val left:KExpression,
	val op:EEqualityOperator,
	val right:KExpression
):KExpression

class SepSeq<E,S>(private val first:E,private val rest:(List<Pair<S,E>>)=listOf())
{

	/**
	 * Добавляет элемент и разделитель в конец последовательности.
	 */
	fun append(separator:S,element:E):SepSeq<E,S>
	{
		val updatedRest=rest.toMutableList()
		updatedRest.add(Pair(separator,element))
		return SepSeq(first,updatedRest)
	}

	/**
	 * Возвращает строковое представление последовательности.
	 */
	override fun toString():String
	{
		val sb=StringBuilder()
		sb.append(first)
		rest.forEach {(separator,element)->
			sb.append(separator).append(element)
		}
		return sb.toString()
	}

	// Дополнительные методы, такие как size, isEmpty и т.д., могут быть добавлены по необходимости.
}

data class KComparison(
	val left:KExpression,
	val op:EComparisonOperator,
	val right:KExpression
):KExpression

data class KGenericCallLikeComparison(val left:KExpression,val right:KCallSuffix):KotlinAst

data class KInfixOperation(val left:KExpression,val right:KInfixOperationInner):KotlinAst

sealed interface KInfixOperationInner:KotlinAst
data class KInElvis(val left:EInOperator,val right:KExpression):KInfixOperationInner

data class KElvisExpression(val left:KExpression,val right:KExpression):KotlinAst

data class KInfixFunctionCall(val left:KExpression,val op:KSimpleIdentifier,val right:KExpression):KotlinAst

enum class ERangeOperator:KotlinAst
{
	Range, RangeUntil
}

data class KRangeExpression(val left:KExpression,val op:ERangeOperator,val right:KExpression):KExpression

data class KAdditiveExpression(
	val left:KExpression,
	val op:KAdditiveOperator,
	val right:KExpression
):KExpression
{
	override val code:String=
		"${left.code} ${op.code} ${right.code}"
}

data class KMultiplicativeExpression(
	val left:KExpression,
	val op:KMultiplicativeOperator,
	val right:KExpression
):KExpression
{
	override val code:String=
		"${left.code} ${op.code} ${right.code}"
}

data class KAsExpression(val left:KExpression,val op:EAsOperator,val type:KType):KExpression

data class KPrefixUnaryExpression(val prefix:KUnaryPrefix,val expression:KExpression):KAssignableExpression

sealed interface KUnaryPrefix:KotlinAst

data class KPostfixUnaryExpression(
	val primaryExpression:KPrimaryExpression,
	val suffixes:List<KPostfixUnarySuffix>
):KExpression
{
	override val code:String
		get()=buildString {
			append(primaryExpression.code)
			suffixes.asSequence()
				.map {it.code}
				.forEach(::append)
		}

	companion object
	{
		infix fun KPrimaryExpression.nav(suffix:KNavigationSuffix):KPostfixUnaryExpression=
			KPostfixUnaryExpression(this,suffix.list)

		infix fun KPostfixUnaryExpression.suffix(suffix:KPostfixUnarySuffix):KPostfixUnaryExpression=
			KPostfixUnaryExpression(primaryExpression,suffixes+suffix)

		infix fun KPrimaryExpression.index(suffix:KIndexingSuffix):KPostfixUnaryExpression=
			KPostfixUnaryExpression(this,suffix.list)

		infix fun KPrimaryExpression.index(expression:KExpression):KPostfixUnaryExpression=
			KPostfixUnaryExpression(this,KIndexingSuffix(expression.nel()).list)
	}
}

sealed interface KPostfixUnarySuffix:KotlinAst

sealed interface KDirectlyAssignableExpression:KAssignmentLeft,KExpression

data class KPostfixUnaryExpressionWithAssignableSuffix(
	val primaryExpression:KPrimaryExpression,
	val assignableSuffix:KAssignableSuffix
):KDirectlyAssignableExpression

data class KParenthesizedDirectlyAssignableExpression(
	val directlyAssignableExpression:KDirectlyAssignableExpression
):KDirectlyAssignableExpression,KAssignableExpression

sealed interface KAssignableExpression:KExpression

sealed interface KAssignableSuffix:KotlinAst

data class KIndexingSuffix(val expressionList:NonEmptyList<KExpression>):KAssignableSuffix,KPostfixUnarySuffix
{
	override val code:String
		get()=expressionList.joinToString(", ","[","]") {it.code}
}

data class KNavigationSuffix(
	val suffix:KNavigationSuffixInner,
	val memberAccessOperator:KMemberAccessOperator=KDot
):KAssignableSuffix,KPostfixUnarySuffix
{
	override val code:String=buildString {
		append(memberAccessOperator.code)
		append(suffix.code)
	}
}

sealed interface KNavigationSuffixInner:KotlinAst

data object KClassKeyword:KNavigationSuffixInner,KClassHeaderType,KCallableReferenceInner
{
	override val code:String="class"
}

data object KFunInterfaceKeyword:KClassHeaderType
data object KInterfaceKeyword:KClassHeaderType

data class KParenthesizedExpression(val expression:KExpression):KPrimaryExpression,KNavigationSuffixInner
{
	override val code:String get()="(${expression.code})"
}

data class KCallSuffix(
	val callSuffixInner:KCallSuffixInner,
	val args:(Option<KTypeArguments>)=None
):KPostfixUnarySuffix
{
	override val code:String
		get()=buildString {
			args.onSome {append(it.code)}
			append(callSuffixInner.code)
		}
}

sealed interface KCallSuffixInner:KotlinAst

data class KAnnotatedLambdaWithValueArguments(
	val valueArguments:Option<KValueArguments>,
	val lambda:KAnnotatedLambda
):KCallSuffixInner

data class KValueArguments(val args:(List<KValueArgument>)=emptyList()):KCallSuffixInner
{
	override val code:String
		get()=buildString {
			if(args.isEmpty()) return "()"
			val last=args.last()
			val preLast=args.dropLast(1)
			when(last.expression)
			{
				is KLambdaLiteral->
				{
					when(args.size)
					{
						1->append(last.code)
						else->
						{
							append('(')
							append(preLast.joinToString(", ") {it.code})
							append(')')
							append(last.code)
						}
					}
				}

				else->
				{
					append('(')
					append(args.joinToString(", ") {it.code})
					append(')')
				}
			}
		}
}

data class KAnnotatedLambda(val annos:List<KAnnotation>,val label:Option<KLabel>,val literal:KLambdaLiteral):KotlinAst
data class KTypeArguments(val projections:NonEmptyList<KTypeProjection>):KAssignableSuffix,KPostfixUnarySuffix
{
	override val code:String='<'+projections.joinToString {it.code}+'>'
}

data class KValueArgument(
	val anno:(Option<KAnnotation>)=None,
	val identifier:(Option<KSimpleIdentifier>)=None,
	val hasSpread:Boolean=false,
	val expression:KExpression
):KotlinAst
{
	override val code:String
		get()=buildString {
			anno.onSome {append(it.code).append(' ')}
			identifier.onSome {append(it.code)}
			if(hasSpread) append("...")
			append(expression.code)
		}
}

sealed interface KPrimaryExpression:KExpression

data class KCollectionLiteral(val exprs:List<KExpression>):KPrimaryExpression

sealed interface KLiteralConstant:KPrimaryExpression

enum class EBooleanLiteral(override val code:String):KLiteralConstant
{
	True("true"),
	False("false")
}

data class KIntegerLiteral(val value:Int):KLiteralConstant,KIntegerHexBin
{
	override val code:String=value.toString()
}

data class KHexLiteral(val value:Int):KLiteralConstant,KIntegerHexBin
data class KBinLiteral(val value:Int):KLiteralConstant,KIntegerHexBin
data class KCharacterLiteral(val value:Char):KLiteralConstant
sealed interface KRealLiteral:KLiteralConstant
data class KFloatLiteral(val value:Float):KRealLiteral
data class KDoubleLiteral(val value:Double):KRealLiteral
data object KNullLiteral:KLiteralConstant

data class KLongLiteral(val value:KIntegerHexBin):KLiteralConstant
data class KUnsignedLiteral(val value:KIntegerHexBin):KLiteralConstant

sealed interface KIntegerHexBin:KotlinAst

sealed interface KStringLiteral:KPrimaryExpression
sealed interface KLineStringContentOrExpression:KotlinAst

data class KLineStringLiteral(
	val content:(List<KLineStringContentOrExpression>)=emptyList()
):KStringLiteral
{
	constructor(str:String):this(KLineStrText(str).list)
	constructor(str:KLineStrText):this(str.list)

	override val code:String
		get()=buildString {
			append('"')
			content.asSequence()
				.map {it.code}
				.forEach(::append)
			append('"')
		}
}

val String.ast get()=KLineStringLiteral(this)

data class KMultiLineStringLiteral(val content:List<KMultiLineStringLiteralInner>):KStringLiteral

sealed interface KMultiLineStringLiteralInner:KotlinAst

sealed interface KLineStringContent:KLineStringContentOrExpression

/*
Check with:
	~('\\' | '"' | '$')+ | '$'
*/
data class KLineStrText(val text:String):KLineStringContent
{
	override val code:String get()=text
}

sealed interface KLineStrEscapedChar:KLineStringContent

/*
Check with:
	'\\' ('t' | 'b' | 'r' | 'n' | '\'' | '"' | '\\' | '$')
*/
data class KEscapedIdentifier(val id:String):KLineStrEscapedChar

/*
Check with:
	'\\' 'u' HexDigit{4}
*/
data class KUniCharacterLiteral(val char:String):KLineStrEscapedChar

data class KLineStrRef(val ref:KFieldIdentifier):KLineStringContent

data class KLineStringExpression(val expression:KExpression):KLineStringContentOrExpression

sealed interface KMultiLineStringContent:KMultiLineStringLiteralInner

/*
Check with:
	~('"' | '$')+ | '$'
*/
data class KMultiLineStrText(val text:String):KMultiLineStringContent

// '"'+
data class KMultiLineStringQuote(val quoteAmount:Int):KMultiLineStringContent,KMultiLineStringLiteralInner
data class KMultiLineStrRef(val ref:KFieldIdentifier):KMultiLineStringContent

data class KMultiLineStringExpression(val expression:KExpression):KMultiLineStringLiteralInner

data class KLambdaLiteral(
	val params:(List<KLambdaParameter>)=emptyList(),
	val stats:(List<KStatement>)=emptyList()
):KFunctionLiteral
{
	override val code:String
		get()=buildString {
			append("{")
			params.forEach {
				append(it.code)
				append(" -> ")
			}
			if(stats.size==1)
				append(stats.first().code)
			else
				stats.forEach {
					append(it.code)
					append(';')
				}
			append("}")
		}
}

infix fun List<KLambdaParameter>.literal(stats:NonEmptyList<KStatement>):KLambdaLiteral=
	KLambdaLiteral(this,stats)

infix fun List<KLambdaParameter>.literal(stat:KStatement):KLambdaLiteral=
	literal(stat.nel())

val List<KLambdaParameter>.literal:KLambdaLiteral
	get()=KLambdaLiteral(this,emptyList())

sealed interface KLambdaParameter:KotlinAst

data class KMultiVariableDeclarationWithType(
	val multiVariableDeclaration:KMultiVariableDeclaration,
	val type:Option<KType>
):KLambdaParameter

data class KAnonymousFunction(
	val isSuspend:Boolean=false,
	val recieverType:(Option<KType>)=None,
	val params:KParametersWithOptionalType,
	val toType:(Option<KType>)=None,
	val constraints:(Option<KTypeConstraints>)=None,
	val body:(Option<KFunctionBody>)=None
):KFunctionLiteral
{
	override val code:String
		get()=buildString {
			if(isSuspend) append("suspend ")
			append("fun ")
			recieverType.onSome {
				append(it.code)
				append('.')
			}
			append(params.code)
			toType.onSome {
				append(':')
				append(it.code)
			}
			constraints.onSome {
				append(it.code)
			}
			body.onSome {
				append(it.code)
			}
		}
}

class KObjectLiteralBuilder
{
	private var isData=false
	private var specifiers:(Option<KDelegationSpecifiers>)=None
	private var body:(Option<KClassBody>)=None

	val data get()=apply {isData=true}
	fun specifiers(specifiers:KDelegationSpecifiers)=apply {this.specifiers=Some(specifiers)}
	fun body(body:KClassBody)=apply {this.body=Some(body)}

	private fun build():KObjectLiteral=KObjectLiteral(isData,specifiers,body)

	companion object
	{
		fun kobjectLiteral(block:KObjectLiteralBuilder.()->Unit):KObjectLiteral=
			KObjectLiteralBuilder().apply(block).build()
	}
}

data class KObjectLiteral(
	val isData:Boolean,
	val specifiers:Option<KDelegationSpecifiers>,
	val body:Option<KClassBody>
):KPrimaryExpression
{
	override val code:String=buildString {
		if(isData) append("data ")
		append("object")
		specifiers.onSome {append(':').append(it.code)}
		body.onSome {append(it.code)}
	}
}

sealed interface KThisExpression:KPrimaryExpression

data object KThis:KThisExpression
data class KThisAt(val id:KIdentifier):KThisExpression

sealed interface KFunctionLiteral:KPrimaryExpression

sealed interface KSuperExpression:KPrimaryExpression
data class KTypeWithSimpleIdentifier(
	val type:Option<KType>,
	val id:Option<KSimpleIdentifier>
):KSuperExpression

data class KSuperAt(val id:KIdentifier):KSuperExpression

sealed interface KIfInner:KotlinAst

data object KSemicolon:KIfInner,KControlStructureBodyOrSemicolon
{
	override val code:String get()=";"
}

sealed interface KControlStructureBodyOrSemicolon:KotlinAst

data class KWhenSubject(val decl:Option<KVariableDeclarationWithAnnotations>,val expression:KExpression):KotlinAst

data class KVariableDeclarationWithAnnotations(val annos:List<KAnnotation>,val decl:KVariableDeclaration):KotlinAst

data class KWhenExpression(val subject:Option<KWhenSubject>,val entries:List<KWhenEntry>):KPrimaryExpression

sealed interface KWhenEntry:KotlinAst

data class KWhenEntryWithConditions(
	val conditions:NonEmptyList<KWhenCondition>,
	val body:KControlStructureBody
):KWhenEntry

data class KWhenElseEntry(val body:KControlStructureBody):KWhenEntry

sealed interface KWhenCondition:KotlinAst

data class KRangeTest(val op:EInOperator,val expression:KExpression):KWhenCondition
data class KTypeTest(val left:EIsOperator,val right:KType):KInfixOperationInner,KWhenCondition

data class KTryExpression(val block:KBlock,val clause:KTryExpressionInner):KPrimaryExpression

sealed interface KTryExpressionInner:KotlinAst

data class KCatchesWithFinally(
	val catches:NonEmptyList<KCatchBlock>,
	val finallyBlock:Option<KFinallyBlock>
):KTryExpressionInner

data class KCatchBlock(
	val annos:List<KAnnotation>,
	val id:KSimpleIdentifier,
	val type:KType,
	val block:KBlock
):KotlinAst

data class KFinallyBlock(val block:KBlock):KTryExpressionInner

sealed interface KJumpExpression:KPrimaryExpression

data class KThrowExpression(val expression:KExpression):KJumpExpression

data class KReturnExpression(
	val ret:KReturnInner,
	val expression:(Option<KExpression>)=None
):KJumpExpression
{
	override val code:String=buildString {
		append(ret.code)
		expression.onSome {
			append(' ')
			append(it.code)
		}
	}
}

val kreturn=KReturnExpression(KReturnKeyword)

sealed interface KReturnInner:KotlinAst
data object KReturnKeyword:KReturnInner
{
	override val code:String="return"
}

data class KReturnAt(val id:KIdentifier):KReturnInner
{
	override val code:String="return@${id.code}"
}

data object KContinue:KJumpExpression
data class KContinueAt(val id:KIdentifier):KJumpExpression

data object KBreak:KJumpExpression
data class KBreakAt(val id:KIdentifier):KJumpExpression

//T :: A
data class KCallableReference(val type:Option<KReceiverType>,val target:KCallableReferenceInner):KPrimaryExpression
sealed interface KCallableReferenceInner:KotlinAst

enum class EAssignmentAndOperator:KotlinAst
{
	/**
	 * +=
	 * */
	AddAssignment,

	/**
	 * -=
	 * */
	SubAssignment,

	/**
	 * *=
	 * */
	MultAssignment,

	/**
	 * /=
	 * */
	DivAssignment,

	/**
	 * %=
	 * */
	ModAssignment
}

enum class EEqualityOperator:KotlinAst
{
	/**
	 * !=
	 * */
	ExclEq,

	/**
	 * !==
	 * */
	ExclEqEq,

	/**
	 * ==
	 * */
	EqEq,

	/**
	 * ===
	 * */
	EqEqEq
}

infix fun KExpression.exclEq(expr:KExpression):KEquality=
	KEquality(this,EEqualityOperator.ExclEq,expr)

infix fun KExpression.exclEqEq(expr:KExpression):KEquality=
	KEquality(this,EEqualityOperator.ExclEqEq,expr)

infix fun KExpression.eqEq(expr:KExpression):KEquality=
	KEquality(this,EEqualityOperator.EqEq,expr)

infix fun KExpression.eqEqEq(expr:KExpression):KEquality=
	KEquality(this,EEqualityOperator.EqEqEq,expr)

enum class EComparisonOperator:KotlinAst
{
	/**
	 * >
	 * */
	Gt,

	/**
	 * <
	 */
	Lt,

	/**
	 * <=
	 */
	Le,

	/**
	 * $>=
	 */
	Ge
}

infix fun KExpression.greater(expr:KExpression):KComparison=
	KComparison(this,EComparisonOperator.Gt,expr)

infix fun KExpression.less(expr:KExpression):KComparison=
	KComparison(this,EComparisonOperator.Lt,expr)

infix fun KExpression.lessEq(expr:KExpression):KComparison=
	KComparison(this,EComparisonOperator.Le,expr)

infix fun KExpression.greaterEq(expr:KExpression):KComparison=
	KComparison(this,EComparisonOperator.Ge,expr)

enum class EInOperator:KotlinAst
{
	/**
	 * in
	 * */
	In,

	/**
	 * !in
	 * */
	NotIn
}

enum class EIsOperator:KotlinAst
{
	/**
	 * is
	 * */
	Is,

	/**
	 * !is
	 * */
	NotIs
}

sealed interface KAdditiveOperator:KotlinAst
sealed interface KMultiplicativeOperator:KotlinAst

enum class EAsOperator:KotlinAst
{
	/**
	 * as
	 * */
	As,

	/**
	 * as?
	 * */
	AsSafe
}

sealed interface KPrefixUnaryOperator:KUnaryPrefix

data object KAdd:KAdditiveOperator,KPrefixUnaryOperator
{
	override val code:String="+"
}

data object KSub:KAdditiveOperator,KPrefixUnaryOperator
{
	override val code:String="-"
}

data object KMult:KMultiplicativeOperator
{
	override val code:String="*"
}

data object KDiv:KMultiplicativeOperator
{
	override val code:String="/"
}

data object KMod:KMultiplicativeOperator
{
	override val code:String="%"
}

data object KExcl:KPrefixUnaryOperator
{
	override val code:String="!"
}

data object KIncr:KPrefixUnaryOperator,KPostfixUnaryOperator
{
	override val code:String="++"
}

data object KDecr:KPrefixUnaryOperator,KPostfixUnaryOperator
{
	override val code:String="--"
}

sealed interface KPostfixUnaryOperator:KPostfixUnarySuffix
data object KDoubleExcl:KPostfixUnaryOperator
{
	override val code:String="!!"
}

sealed interface KMemberAccessOperator:KotlinAst

operator fun KExpression.plus(other:KExpression):KAdditiveExpression=
	KAdditiveExpression(this,KAdd,other)

operator fun KExpression.minus(other:KExpression):KAdditiveExpression=
	KAdditiveExpression(this,KSub,other)

operator fun KExpression.times(other:KExpression):KMultiplicativeExpression=
	KMultiplicativeExpression(this,KMult,other)

operator fun KExpression.div(other:KExpression):KMultiplicativeExpression=
	KMultiplicativeExpression(this,KDiv,other)

operator fun KExpression.rem(other:KExpression):KMultiplicativeExpression=
	KMultiplicativeExpression(this,KMod,other)

infix fun KExcl.expr(expr:KExpression):KPrefixUnaryExpression=
	KPrefixUnaryExpression(this,expr)

val KExpression.excl
	get():KPrefixUnaryExpression=
		KPrefixUnaryExpression(KExcl,this)

data object KDot:KMemberAccessOperator
{
	override val code:String="."
}

//?.
data object KSafeNav:KMemberAccessOperator
{
	override val code="?."
}

data object KColonColon:KMemberAccessOperator
{
	override val code="::"
}

data class KModifiers(val mods:NonEmptyList<KModifiersInner>):KotlinAst
{
	override val code:String=mods.joinToString(" ") {it.code}
}

sealed interface KModifiersInner:KotlinAst

data class KParameterModifiers(val mods:NonEmptyList<KParameterModifiersInner>):KotlinAst
sealed interface KParameterModifiersInner:KotlinAst

sealed interface KModifier:KModifiersInner
sealed interface KPropertyModifier:KModifier
data object KConstModifier:KPropertyModifier

sealed interface KTypeModifier:KotlinAst
data object KSuspendKeyword:KTypeModifier,KSimpleIdentifier
{
	override val id="suspend"
}

enum class EClassModifier:KModifier
{
	Enum,
	Sealed,
	Annotation,
	Data,
	Inner,
	Value;

	override val code:String=toString().lowercase()
}

enum class EMemberModifier:KModifier
{
	Override,
	Lateinit;

	override val code:String=toString().lowercase()
}

enum class EVisibilityModifier:KModifier
{
	Public,
	Private,
	Internal,
	Protected;

	override val code:String=toString().lowercase()
}

enum class EVarianceModifier:KModifier,KTypeProjectionModifier,KTypeParameterModifier
{
	In,
	Out;

	override val code:String=toString().lowercase()
}

data class KTypeParameterModifiers(val mods:NonEmptyList<KTypeParameterModifier>):KotlinAst

sealed interface KTypeParameterModifier:KotlinAst

enum class EFunctionModifier:KModifier
{
	Tailrec,
	Operator,
	Infix,
	Inline,
	External,
	Suspend;

	override val code:String=toString().lowercase()
}

enum class EInheritanceModifier:KModifier
{
	Abstract,
	Final,
	Open;

	override val code:String=toString().lowercase()
}

enum class EParameterModifier:KModifier,KParameterModifiersInner
{
	Vararg,
	NoInline,
	CrossInline;

	override val code:String=toString().lowercase()
}

sealed interface KReificationModifier:KTypeParameterModifier

data object KReifiedKeyword:KReificationModifier,KSimpleIdentifier
{
	override val id="reified"
}

sealed interface KPlatformModifier:KModifier

data object KExpectKeyword:KPlatformModifier,KSimpleIdentifier
{
	override val id="expect"
}

data object KActualKeyword:KPlatformModifier,KSimpleIdentifier
{
	override val id="actual"
}

sealed interface KAnnotation:
		KTypeProjectionModifier,
		KLabelOrAnnotation,
		KUnaryPrefix,
		KModifiersInner,
		KParameterModifiersInner,
		KTypeModifier,
		KTypeParameterModifier

data class KSingleAnnotation(val left:KAnnotationLeft,val right:KUnescapedAnnotation):KAnnotation

fun anno(left:EAnnotationUseSiteTarget,right:KUnescapedAnnotation)=
	KSingleAnnotation(left,right)

fun anno(right:KUnescapedAnnotation)=
	KSingleAnnotation(KAt,right)

val KSingleAnnotation.mod get()=KModifiers(this.nel())

sealed interface KAnnotationLeft:KotlinAst

data class KMultiAnnotation(val left:KAnnotationLeft,val right:NonEmptyList<KUnescapedAnnotation>):KAnnotation

/**
 * @
 * */
data object KAt:KAnnotationLeft

enum class EAnnotationUseSiteTarget:KAnnotationLeft
{
	Field,
	Property,
	Get,
	Set,
	Receiver,
	Param,
	SetParam,
	Delegate
}

sealed interface KSimpleIdentifier:
		KPrimaryExpression,
		KCallableReferenceInner,
		KDirectlyAssignableExpression,
		KNavigationSuffixInner
{
	val id:String
}

fun KSimpleIdentifier.variableDecl(annos:List<KAnnotation>,type:KType):KVariableDeclaration=
	KVariableDeclaration(annos,this,type.some())

infix fun KSimpleIdentifier.variableDecl(type:KType):KVariableDeclaration=
	KVariableDeclaration(emptyList(),this,type.some())

val KSimpleIdentifier.variableDecl
	get()=KVariableDeclaration(identifier=this)

/*
Check with:
	(Letter | '_') (Letter | '_' | UnicodeDigit)*
    | '`' ~([\r\n] | '`')+ '`'
*/
class KIdentifierInner(name:String):KSimpleIdentifier
{
	val name=name //if(name.matches(Regex(""))) name else error("Bad identifier: $name")
	override val id:String=name
	override val code:String=name
}

data object KAbstractKeyword:KSimpleIdentifier
{
	override val id:String="abstract"
}

data object KAnnotationKeyword:KSimpleIdentifier
{
	override val id:String="annotation"
}

data object KByKeyword:KSimpleIdentifier
{
	override val id:String="by"
}

data object KCatchKeyword:KSimpleIdentifier
{
	override val id:String="catch"
}

data object KCompanionKeyword:KSimpleIdentifier
{
	override val id:String="companion"
}

data object KConstructorKeyword:KSimpleIdentifier
{
	override val id:String="constructor"
}

data object KCrossinlineKeyword:KSimpleIdentifier
{
	override val id:String="crossinline"
}

data object KDataKeyword:KSimpleIdentifier
{
	override val id:String="data"
}

data object KDynamicKeyword:KSimpleIdentifier
{
	override val id:String="dynamic"
}

data object KEnumKeyword:KSimpleIdentifier
{
	override val id:String="enum"
}

data object KExternalKeyword:KSimpleIdentifier
{
	override val id:String="external"
}

data object KFinalKeyword:KSimpleIdentifier
{
	override val id:String="final"
}

data object KFinallyKeyword:KSimpleIdentifier
{
	override val id="finally"
}

data object KGetKeyword:KSimpleIdentifier
{
	override val id="get"
}

data object KImportKeyword:KSimpleIdentifier
{
	override val id="import"
}

data object KInfixKeyword:KSimpleIdentifier
{
	override val id="infix"
}

data object KInitKeyword:KSimpleIdentifier
{
	override val id="init"
}

data object KInlineKeyword:KSimpleIdentifier
{
	override val id="inline"
}

data object KInnerKeyword:KSimpleIdentifier
{
	override val id="inner"
}

data object KInternalKeyword:KSimpleIdentifier
{
	override val id="internal"
}

data object KLateinitKeyword:KSimpleIdentifier
{
	override val id="lateinit"
}

data object KNoinlineKeyword:KSimpleIdentifier
{
	override val id="noinline"
}

data object KOpenKeyword:KSimpleIdentifier
{
	override val id="open"
}

data object KOperatorKeyword:KSimpleIdentifier
{
	override val id="operator"
}

data object KOutKeyword:KSimpleIdentifier
{
	override val id="out"
}

data object KOverrideKeyword:KSimpleIdentifier
{
	override val id="override"
}

data object KPrivateKeyword:KSimpleIdentifier
{
	override val id="private"
}

data object KProtectedKeyword:KSimpleIdentifier
{
	override val id="protected"
}

data object KPublicKeyword:KSimpleIdentifier
{
	override val id="public"
}

data object KSealedKeyword:KSimpleIdentifier
{
	override val id="sealed"
}

data object KTailrecKeyword:KSimpleIdentifier
{
	override val id="tailrec"
}

data object KSetKeyword:KSimpleIdentifier
{
	override val id="set"
}

data object KVarargKeyword:KSimpleIdentifier
{
	override val id="vararg"
}

data object KWhereKeyword:KSimpleIdentifier
{
	override val id="where"
}

data object KFieldKeyword:KSimpleIdentifier
{
	override val id="field"
}

data object KPropertyKeyword:KSimpleIdentifier
{
	override val id="property"
}

data object KReceiverKeyword:KSimpleIdentifier
{
	override val id="receiver"
}

data object KParamKeyword:KSimpleIdentifier
{
	override val id="param"
}

data object KSetparamKeyword:KSimpleIdentifier
{
	override val id="setparam"
}

data object KDelegateKeyword:KSimpleIdentifier
{
	override val id="delegate"
}

data object KFileKeyword:KSimpleIdentifier
{
	override val id="file"
}

data object KConstKeyword:KSimpleIdentifier
{
	override val id="const"
}

data object KValueKeyword:KSimpleIdentifier
{
	override val id="value"
}

data class KIdentifier(val ids:NonEmptyList<KSimpleIdentifier>):KotlinAst
{
	override val code:String
		get()=ids.joinToString(".") {it.code}
}

infix fun KSimpleIdentifier.dot(other:KSimpleIdentifier)=KIdentifier(this.nel()+other)

abstract class NonEmptyListBuilder<T>(first:T)
{
	private var ts=first.nel()

	operator fun T.unaryPlus()
	{
		ts=_ts(this)
	}

	private fun _ts(default:T)=
		(ts+default).also {ts=it}

	fun build()=ts
}

class IdentifierBuilder(first:KIdentifierInner)
{
	private var ids=first.nel()

	constructor(first:String):this(KIdentifierInner(first))

	operator fun KIdentifierInner.unaryPlus()
	{
		ids=_ids(this)
	}

	operator fun String.unaryPlus()
	{
		+KIdentifierInner(this)
	}

	private fun _ids(default:KIdentifierInner)=
		(ids+default).also {ids=it}

	fun build()=KIdentifier(ids)
}

fun identifier(first:KIdentifierInner,init:IdentifierBuilder.()->Unit)=
	IdentifierBuilder(first).also(init).build()

fun identifier(first:String,init:IdentifierBuilder.()->Unit)=
	IdentifierBuilder(first).also(init).build()

data class KFieldIdentifier(val id:KSimpleIdentifier):KotlinAst