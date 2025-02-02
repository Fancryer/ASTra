package ast

import org.antlr.v4.runtime.tree.ParseTree
import org.fancryer.bf.ast.KotlinAst
import org.fancryer.bf.ast.err
import org.fancryer.bf.ast.rules.RuleLogger
import org.fancryer.bf.ast.rules.RuleLoggerImpl
import kotlin.reflect.KClass
import kotlin.reflect.jvm.ExperimentalReflectionOnLambdas
import kotlin.reflect.jvm.reflect

//@OptIn(ExperimentalReflectionOnLambdas::class)
//open class TranspilationRule<F:ParseTree,N:KotlinAst>(
//	val from:KClass<F>,
//	val how:(F)->N,
//	val name:String,
//	@Suppress("UNCHECKED_CAST")
//	val nodeClass:(KClass<N>)=how.reflect()?.returnType?.classifier as KClass<N>
//)
//{
//	override fun toString()=
//		"TranspilationRule($name ${from.simpleName} -> ${nodeClass.simpleName})"
//}

@OptIn(ExperimentalReflectionOnLambdas::class)
class TranspilationRule<P:ParseTree,A:KotlinAst>(
	val from:KClass<P>,
	how:(P)->A,
	val name:String,
	@Suppress("UNCHECKED_CAST")
//	val nodeClass:(KClass<A>?)=how.reflect()?.returnType?.classifier as? KClass<A>,
	val demands:Requirements<P>,
	val ensures:Requirements<A>,
	val logger:(RuleLogger<P,A>)=RuleLoggerImpl()
):(P)->A
{
	val how=how.wrapHow(demands,ensures)

	override fun invoke(f:P)=how(f)

	override fun toString()=
		"TranspilationRule($name ${from.simpleName} -> \${nodeClass.simpleName} $demands $ensures)"

	companion object
	{
		fun <F:ParseTree,N:KotlinAst> ((F)->N).wrapHow(
			demands:Requirements<F>,
			ensures:Requirements<N>
		)={f:F->
			if(!demands.all {it(f)}) "Demand contract violated".err
			this(f).let {ret->
				if(!ensures.all {it(ret)}) "Ensures contract violated".err
				ret
			}
		}
	}
}

typealias Requirements<T>
		=List<(T)->Boolean>