package ast

import ast.KClassParameterBuilder.Companion.kclassParameter
import org.fancryer.bf.id

annotation class KPrimaryConstructorDsl

@KPrimaryConstructorDsl
class KPrimaryConstructorBuilder
{
	private var classParameters:(List<KClassParameter>)=emptyList()
	private var modifiers:(List<KModifiersInner>)=emptyList()

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


	private fun build()=KPrimaryConstructor(classParameters,modifiers)

	companion object
	{
		fun kprimaryConstructor(init:KPrimaryConstructorBuilder.()->Unit)=
			KPrimaryConstructorBuilder().apply(init).build()
	}
}

data class KPrimaryConstructor(
	val classParameters:List<KClassParameter>,
	val modifiers:(List<KModifiersInner>)=emptyList()
):KotlinAst