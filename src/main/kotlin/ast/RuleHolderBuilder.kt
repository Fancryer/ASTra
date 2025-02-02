package ast

import arrow.core.*
import org.antlr.v4.runtime.Lexer
import org.antlr.v4.runtime.Parser
import org.antlr.v4.runtime.tree.ParseTree
import org.fancryer.bf.ast.KotlinAst
import org.fancryer.bf.ast.errf
import org.fancryer.bf.ast.rules.InoutLogger
import org.fancryer.bf.ast.rules.InoutLoggerImpl
import kotlin.math.log

class RuleHolderBuilder<LT:Lexer,PT:Parser>
{
	private var defaultRule:(Option<DefaultRule<LT,PT>>)=none()
	val rules=mutableListOf<TranspilationRule<out ParseTree,out KotlinAst>>()
//	val logger:(InoutLogger<out ParseTree,out KotlinAst,Int>)=
//		InoutLoggerImpl(
//			{it::class.simpleName?.run {length>15} ?: false},
//			{it::class.simpleName?.run {length>15} ?: false}
//		)

	fun default(init:(ParseTree)->KotlinAst)
	{
		defaultRule=DefaultRule<LT,PT>(init).some()
	}

	fun default(init:DefaultRule<LT,PT>):RuleHolderBuilder<LT,PT>
	{
		defaultRule=init.some()
		return this
	}

	fun <P:ParseTree,K:KotlinAst> rule(name:String,init:RuleBuilder<P,K>.()->Unit):(TranspilationRule<P,K>)=
		RuleBuilder<P,K>().also(init)
			.apply {name(name)}
			.build()
			.also {rules.add(it)}

	fun <P:ParseTree,K:KotlinAst> rule(init:RuleBuilder<P,K>.()->Unit):(TranspilationRule<P,K>)=
		RuleBuilder<P,K>().also(init)
			.build()
			.also {rules.add(it)}

	/**
	 * This function tries to find rule with signature:
	 *
	 * ```
	 * K1 <: K, P1 <: P |- P -> K1
	 * ```
	 *
	 * @return null if it can't find it or found rule otherwise
	 */
	inline fun <reified P:ParseTree,reified K:KotlinAst> RuleHolderBuilder<LT,PT>.lookup():(TranspilationRule<P,K>)?=
		rules.filterIsInstance<TranspilationRule<P,K>>()
			.firstOrNull {
				it.from==P::class
				//				distanceFromAncestor(it.from,P::class).isZero()
			}

	//			.filter {
	//			P::class.isSuperclassOf(it.from)
	//		}.filter {
	//			K::class.isSuperclassOf(it.nodeClass)
	//		}.map {
	//			it
	//		}
	//			.map {
	//			distanceFromAncestor(it.from,P::class) to it
	//		}.minByOrNull {it.first}
	//			?.second

	/**
	 * This function behaves almost like [lookup],
	 * but if it can't find rule with given signature, it returns given rule
	 * @return rule
	 */
	inline fun <reified P:ParseTree,reified K:KotlinAst> lookupOrGet(
		get:()->TranspilationRule<P,K>
	):(TranspilationRule<P,K>)=lookup<P,K>() ?: get()

	/**
	 * This function behaves as [lookupOrGet] but it also adds rule to holder if it can't find such rule
	 */
	inline fun <reified P:ParseTree,reified K:KotlinAst> lookupOrUpdate(
		update:()->TranspilationRule<P,K>
	):(TranspilationRule<P,K>)=lookup<P,K>() ?: update().also {rules.add(it)}

	/**
	 * The function takes a mapping function 'm' that transforms a TranspilationRule<P, K> to K.
	 * It looks up the corresponding TranspilationRule<P, K> using lookup<P, K>()
	 * and applies the mapping function 'm' to it.
	 * @return result of applying the mapping function to the found TranspilationRule<P, K>.
	 */
	inline fun <reified P:ParseTree,reified K:KotlinAst> mapWith(m:(TranspilationRule<P,K>)->K)=
		lookup<P,K>()?.let {m(it)}

	inline fun <reified P:ParseTree,reified K:KotlinAst> P.mapWith(m:(TranspilationRule<P,K>)->K)=
		lookup<P,K>()?.let {m(it)}

	/**
	 * A function that maps a Kotlin AST element to another Kotlin AST element
	 * using a given transformation function and a mapping function,
	 * then looks up a specific key in a map and applies the mapping function to it if found,
	 * @return result or null.
	 */
	inline fun <reified P:ParseTree,reified K:KotlinAst,reified T:KotlinAst> mapWithThen(
		m:(K)->T,
		h:(TranspilationRule<P,K>)->K
	):T?=lookup<P,K>()?.let {m(h(it))}

	/**
	 * A function that maps a TranspilationRule to a result of type K,
	 * with a fallback to the 'orElse' value if the mapping result is null.
	 */
	inline fun <reified P:ParseTree,reified K:KotlinAst> RuleHolderBuilder<LT,PT>.mapWithOrElse(
		m:(TranspilationRule<P,K>)->K,
		orElse:()->K
	)=mapWith(m) ?: orElse()

	/**
	 * A function that maps with the provided function or throws an exception if the result is null.
	 * @param m A function that takes a TranspilationRule and returns a KotlinAst.
	 * @param e A function that returns a Throwable.
	 * @return The result of mapping with the provided function or throws an exception.
	 */
	inline fun <reified P:ParseTree,reified K:KotlinAst> mapWithOrThrow(
		m:(TranspilationRule<P,K>)->K,
		e:()->Throwable
	)=mapWith<P,K>(m) ?: throw e()

	/**
	 * A function that takes a transpilation rule, a mapping function, and an error function,
	 * applies the mapping function to the transpilation rule, and throws an error if the result is null.
	 */
	inline fun <reified P:ParseTree,reified K:KotlinAst,reified T:KotlinAst> mapThenOrThrow(
		h:(TranspilationRule<P,K>)->K,
		m:(K)->T,
		e:()->Throwable
	):T=mapWithThen<P,K,T>(m,h) ?: throw e()

	fun build():(RuleHolder<LT,PT>)=
		defaultRule.let {
			it.fold("No default".errf) {
				val logger=InoutLoggerImpl<ParseTree,KotlinAst>({true}){true}
				if(rules.isNotEmpty()) RuleHolder(it,rules,logger)
				else RuleHolder(it,emptyList(),logger)
			}
		}
}