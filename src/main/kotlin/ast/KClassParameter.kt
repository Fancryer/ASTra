package ast

import arrow.core.*
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
	private var modifiers:(List<KModifiersInner>)=emptyList()
	private var _isVal:(Option<Boolean>)=Some(true)
	private var identifier:KSimpleIdentifier=id
	private var type:KType?=null
	private var expression:KExpression?=null

	constructor(id:String):this(id.id)

	operator fun KModifiersInner.unaryPlus()
	{
		modifiers+=this
	}

	val notMine:Unit
		get()
		{
			_isVal=None
		}

	val valueParam:Unit
		get()
		{
			notMine
		}

	fun type(type:KType)
	{
		this.type=type
	}

	fun type(type:String)=type(type.type)

	operator fun KType.unaryPlus()
	{
		type(this)
	}

	fun expression(expression:KExpression)
	{
		this.expression=expression
	}

	operator fun KExpression.unaryPlus()
	{
		expression(this)
	}

	fun isVal(isVal:Boolean)
	{
		this._isVal=isVal.some()
	}

	val isVal:Unit get()
	{
		_isVal=true.some()
	}

	val isVar:Unit get()
	{
		_isVal=false.some()
	}

	private fun build()=KClassParameter(
		modifiers,
		_isVal,
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
	val modifiers:List<KModifiersInner>,
	val isVal:Option<Boolean>, //When none, it is value-parameter
	val identifier:KSimpleIdentifier,
	val type:KType,
	val expression:Option<KExpression>
):KotlinAst