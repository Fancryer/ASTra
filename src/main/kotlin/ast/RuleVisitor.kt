package ast

import org.antlr.v4.runtime.tree.ParseTree
import org.fancryer.bf.Distance
import org.fancryer.bf.Distance.UnrealDistance.rd
import org.fancryer.bf.ast.KotlinAst
import org.fancryer.bf.distanceFromAncestor
import java.security.Provider
import kotlin.reflect.KClass
import kotlin.reflect.full.isSuperclassOf

inline fun <reified T:ParseTree,R:KotlinAst,T1:ParseTree> Collection<TranspilationRule<out T,out R>>.filterInstancesOf(
):List<TranspilationRule<T1,out R>>
{
	val newList=mutableListOf<TranspilationRule<T1,R>>()
	forEach {
		if(T::class.isSuperclassOf(it::class)) newList.add(it as TranspilationRule<T1,R>)
	}
	return newList
}

class RuleVisitor(val holder:RuleHolder)
{
	inline fun <reified P:ParseTree> visit1(tree:P):KotlinAst=
		//check if tree is null
		tree.let {t->
			//get holder rules
			holder.rules
				//find rule that gets non-null tree of `from` class
				//if found then convert ot with `how` function, else null
				.firstOrNull {
					t::class.isSuperclassOf(it.from)
				}
				?.let {
					@Suppress("TYPE_MISMATCH")
					it.how(t)
				}
		} ?: holder.defaultRule.how(tree)

	inline fun <reified P:ParseTree> visit(tree:P):KotlinAst
	{
		//check if tree is null
		val t=tree

		val wellTypedRules=holder.rules.filterIsInstance<TranspilationRule<P,KotlinAst>>()

		val rulesWithDistanceToFrom=wellTypedRules.map {distanceFromAncestor(P::class,it.from) to it}
			.filter {it.first<=5.rd}
			.sortedBy {it.first}
			.map {it.second}

		//		val leastRules=rulesWithDistanceToFrom.map {distanceFromAncestor(KotlinAst::class,it.second.nodeClass) to it}
		//			.sortedByDescending {it.first}
		//			.map {it.second}

		//get holder rules
		return rulesWithDistanceToFrom.firstOrNull()
				   //if found then convert ot with `how` function, else null
				   ?.let {
					   //@Suppress("TYPE_MISMATCH")
					   it.how(t)
				   } ?: holder.defaultRule.how(tree)
	}
}

public inline fun <reified R> Iterable<*>.firstInstanceOrNull():R?
{
	return filterIsInstance<R>().firstOrNull()
}