package ast

import Distance.UnrealDistance.rd
import distanceFromAncestor
import org.antlr.v4.runtime.Lexer
import org.antlr.v4.runtime.Parser
import org.antlr.v4.runtime.tree.ParseTree
import kotlin.reflect.full.isSuperclassOf

inline fun <reified T:ParseTree,R:KotlinAst,T1:ParseTree>
		Collection<TranspilationRule<out T,out R>>.filterInstancesOf(
):List<TranspilationRule<T1,out R>>
{
	val newList=mutableListOf<TranspilationRule<T1,R>>()
	forEach {
		if(T::class.isSuperclassOf(it::class)) newList.add(it as TranspilationRule<T1,R>)
	}
	return newList
}

/***
 * This enum determines the behavior of the rule visitor when a rule is not found
 */
enum class RuleVisitorBehaviour
{
	Throw,
	Fall
}

class RuleVisitor<LT:Lexer,PT:Parser>(val holder:RuleHolder<LT,PT>)
{
	//	inline fun <reified P:ParseTree> visit1(tree:P):KotlinAst=
	//		//check if tree is null
	//		tree.let {t->
	//			//get holder rules
	//			holder.rules
	//				//find rule that gets non-null tree of `from` class
	//				//if found then convert ot with `how` function, else null
	//				.firstOrNull {
	//					t::class.isSuperclassOf(it.from)
	//				}
	//				?.let {
	//					@Suppress("TYPE_MISMATCH")
	//					it.how( t)
	//				}
	//		} ?: holder.defaultRule.how(tree)

	inline fun <reified P:ParseTree> visit(tree:P):KotlinAst
	{
		//check if tree is null
		val t=tree

		return holder.rules
				   .asSequence()
				   .filterIsInstance<TranspilationRule<P,KotlinAst>>()
				   .map {distanceFromAncestor(P::class,it.from) to it}
				   .filter {it.first<=5.rd}
				   .sortedBy {it.first}
				   .map {it.second}.firstOrNull()
				   //if found then convert ot with `how` function, else null
				   ?.let {rule->
					   @Suppress("TYPE_MISMATCH")
					   if(holder.logger.useBefore(t)) holder.logger.before(t)
					   rule.logger.before(t)
					   rule.how(t).also {
						   rule.logger.after(it)
						   @Suppress("TYPE_MISMATCH")
						   if(holder.logger.useAfter(it)) holder.logger.after(it)
					   }
				   } ?: holder.defaultRule.how(tree)
	}
}

inline fun <reified R> Iterable<*>.firstInstanceOrNull():R?
{
	return filterIsInstance<R>().firstOrNull()
}