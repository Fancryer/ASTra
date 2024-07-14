package ast

import org.antlr.v4.runtime.tree.ParseTree
import org.fancryer.bf.ast.KotlinAst
import kotlin.reflect.KClass
import kotlin.reflect.full.isSuperclassOf

data class RuleHolder(val defaultRule:DefaultRule,val rules:List<TranspilationRule<out ParseTree,out KotlinAst>>)
{
	inline fun <reified P:ParseTree> rulesWithFrom():(List<TranspilationRule<P,out KotlinAst>>)
	{
		return rules.filterIsInstance<TranspilationRule<P,out KotlinAst>>()
	}

	inline fun <reified P:ParseTree> rulesWithFromT(p:KClass<P>):(List<TranspilationRule<P,out KotlinAst>>)
	{
		val newRules=mutableListOf<TranspilationRule<P,out KotlinAst>>()
		rules.forEach {
			if(it.from.isSuperclassOf(p))
				@Suppress("UNCHECKED_CAST") newRules.add(it as TranspilationRule<P,out KotlinAst>)
		}
		return newRules
	}

	inline fun<reified N:KotlinAst> rulesWithTo():(List<TranspilationRule<out ParseTree,N>>)=
		rules.filterIsInstance<TranspilationRule<out ParseTree,N>>()

	inline fun<reified P:ParseTree,reified N:KotlinAst> rulesWithFromAndTo():(List<TranspilationRule<P,N>>)=
		rules.filterIsInstance<TranspilationRule<P,N>>()
}

fun rules(init:RuleHolderBuilder.()->Unit):RuleHolder=
	RuleHolderBuilder().apply(init).build()