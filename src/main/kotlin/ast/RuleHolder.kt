package ast

import org.antlr.v4.runtime.*
import org.antlr.v4.runtime.tree.ParseTree
import ast.rules.InoutLogger
import org.fancryer.bf.pipe
import kotlin.reflect.KClass
import kotlin.reflect.full.isSuperclassOf

data class RuleHolder<LT:Lexer,PT:Parser>(
	val defaultRule:DefaultRule<LT,PT>,
	val rules:List<TranspilationRule<out ParseTree,out KotlinAst>>,
	val logger:InoutLogger<out ParseTree,out KotlinAst,Int>
)
{
	inline fun <reified P:ParseTree> rulesWithFrom():(List<TranspilationRule<P,out KotlinAst>>)=
		rules.filterIsInstance<TranspilationRule<P,out KotlinAst>>()

	inline fun <reified P:ParseTree> rulesWithFromT(p:KClass<P>):(List<TranspilationRule<P,out KotlinAst>>)
	{
		val newRules=mutableListOf<TranspilationRule<P,out KotlinAst>>()
		rules.forEach {
			if(it.from.isSuperclassOf(p))
				@Suppress("UNCHECKED_CAST")
				newRules.add(it as TranspilationRule<P,out KotlinAst>)
		}
		return newRules
	}

	inline fun <reified N:KotlinAst> rulesWithTo():(List<TranspilationRule<out ParseTree,N>>)=
		rules.filterIsInstance<TranspilationRule<out ParseTree,N>>()

	inline fun <reified P:ParseTree,reified N:KotlinAst> rulesWithFromAndTo():(List<TranspilationRule<P,N>>)=
		rules.filterIsInstance<TranspilationRule<P,N>>()

	inline fun <reified C:ParserRuleContext> transpile(
		src:String,
		lexerConstructor:(CharStream)->LT,
		parserConstructor:(TokenStream)->PT,
		parserRuleMethod:PT.()->C
	):KotlinAst=
		src pipe
				CharStreams::fromString pipe
				lexerConstructor pipe
				::CommonTokenStream pipe
				parserConstructor pipe
				parserRuleMethod pipe
				RuleVisitor(this)::visit
}

fun <LT:Lexer,PT:Parser> rules(init:RuleHolderBuilder<LT,PT>.()->Unit):(RuleHolder<LT,PT>)=
	RuleHolderBuilder<LT,PT>().apply(init).build()