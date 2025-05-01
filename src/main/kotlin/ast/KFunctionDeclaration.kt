package ast

import arrow.core.None
import arrow.core.Option
import arrow.core.nel
import arrow.core.some
import org.fancryer.bf.functionValueParameter
import org.fancryer.bf.id
import org.fancryer.bf.type
import org.fancryer.bf.userType

data class KFunctionDeclaration(
	val modifiers:(List<KModifiersInner>)=emptyList(),
	val typeParameters:(List<KTypeParameter>)=emptyList(),
	val receiverType:(Option<KType>)=None,
	val identifier:KSimpleIdentifier,
	val functionValueParameters:List<KFunctionValueParameter>,
	val type:(Option<KType>)=None,
	val typeConstraints:(List<KTypeConstraint>)=emptyList(),
	val functionBody:(Option<KFunctionBody>)=None
):KDeclaration

class FunctionDeclarationBuilder(private var identifier:KSimpleIdentifier)
{
	private var modifiers:(List<KModifiersInner>)=emptyList()
	private var typeParameters:(List<KTypeParameter>)=emptyList()
	private var receiverType:(Option<KType>)=None

	//Check
	private var valueParameters:(List<KFunctionValueParameter>)=emptyList()
	private var type:(Option<KType>)=None
	private var typeConstraints:(List<KTypeConstraint>)=emptyList()
	private var functionBody:(Option<KFunctionBody>)=None


	operator fun KModifiersInner.unaryPlus()
	{
		modifiers=
			if(modifiers.none()) nel()
			else modifiers+this
	}

	fun typeParameters(params:List<KTypeParameter>)
	{
		typeParameters=params
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

	infix fun String.ofType(type:KSimpleUserType)=
		+KFunctionValueParameter(parameter=this.id param type.userType.type)

	infix fun String.ofType(type:String)=
		this ofType type.type

	infix fun KSimpleIdentifier.ofType(type:KType)=
		+KFunctionValueParameter(parameter=this param type)

	infix fun KSimpleIdentifier.ofType(type:String)=
		this ofType type.type

	fun returns(type:KType)
	{
		this.type=type.some()
	}

	fun typeConstrants(constraints:List<KTypeConstraint>)
	{
		typeConstraints=constraints
	}

	fun body(b:KFunctionBody)
	{
		functionBody=b.some()
	}

	fun body(b:()->KFunctionBody)=body(b())

	fun blockBody(init:KBlockBuilder.()->Unit)=body(KBlockBuilder.kblock(init))
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

		fun kfunBlock(
			identifier:KSimpleIdentifier,
			init:KBlockBuilder.()->Unit
		)=kfun(identifier) {blockBody(init)}

		fun kfunBlock(
			identifier:String,
			init:KBlockBuilder.()->Unit
		)=kfun(identifier) {blockBody(init)}
	}
}