package ast

import arrow.core.Option
import arrow.core.toOption
import ast.KClassParameterBuilder.Companion.kclassParameter
import org.fancryer.bf.id

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

	infix fun String.ofType(type:KType):KClassParameter=
		param(this) {type(type)}

	infix fun String.ofType(type:String):KClassParameter=
		this ofType type.type

	private fun build()=classParameters.toList()

	companion object
	{
		fun kclassParameters(init:KClassParametersBuilder.()->Unit)=
			KClassParametersBuilder().apply(init).build()
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

data class KClassParameter(
	val modifiers:Option<KModifiers>,
	val isVal:Boolean,
	val identifier:KSimpleIdentifier,
	val type:KType,
	val expression:Option<KExpression>
):KotlinAst