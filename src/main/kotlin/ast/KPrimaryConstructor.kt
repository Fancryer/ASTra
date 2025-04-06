package ast

import arrow.core.None
import arrow.core.Option
import arrow.core.toOption
import ast.KClassParameterBuilder.Companion.kclassParameter
import org.fancryer.bf.id

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

	infix fun String.ofType(type:KType):KClassParameter=
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

data class KPrimaryConstructor(
	val classParameters:List<KClassParameter>,
	val modifiers:(Option<KModifiers>)=None
):KotlinAst