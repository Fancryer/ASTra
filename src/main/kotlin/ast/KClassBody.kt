package ast

import ast.FunctionDeclarationBuilder.Companion.kfun
import ast.KPropertyDeclarationBuilder.Companion.kpropertyDeclaration
import org.fancryer.bf.id

@ClassBuilderDsl
annotation class ClassBodyBuilderDsl

@ClassBodyBuilderDsl
class KClassBodyBuilder
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

	private fun build()=KClassBody(declarations)

	companion object
	{
		fun kclassBody(init:KClassBodyBuilder.()->Unit)=
			KClassBodyBuilder().apply(init).build()
	}
}

data class KClassBody(
	val memberDeclarations:(List<KClassMemberDeclaration>)=emptyList()
):KClassOrEnumBody