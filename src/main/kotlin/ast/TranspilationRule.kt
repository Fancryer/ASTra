package ast

import arrow.core.Option
import org.antlr.v4.runtime.tree.ParseTree
import org.fancryer.bf.ast.KotlinAst
import org.fancryer.bf.ast.rules.RuleLogger
import org.fancryer.bf.ast.rules.RuleLoggerImpl
import kotlin.reflect.KClass
import kotlin.reflect.jvm.ExperimentalReflectionOnLambdas

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
	val how:(P)->A,
	val name:String,
	@Suppress("UNCHECKED_CAST")
	//	val nodeClass:(KClass<A>?)=how.reflect()?.returnType?.classifier as? KClass<A>,
	val demands:Option<(P)->Boolean>,
	val ensures:Option<(A)->Boolean>,
	val unwinds:Option<(Throwable)->A>,
	val logger:(RuleLogger<P,A>)=RuleLoggerImpl()
):(P)->A
{

	override fun invoke(f:P):A=
		try
		{
			assert(demands.fold({true}) {it(f)}) {
				"Demand contract violated"
			}
			how(f).apply {
				assert(ensures.fold({true}) {it(this)}) {
					"Ensure contract violated"
				}
			}
		}
		catch(e:Throwable)
		{
			unwinds.fold({throw RuntimeException("Unwind was not provided, so rule just fell.",e)}) {it(e)}
		}

	override fun toString()=
		"TranspilationRule($name ${from.simpleName} -> \${nodeClass.simpleName} $demands $ensures)"
}

typealias Requirements<T>
		=List<(T)->Boolean>