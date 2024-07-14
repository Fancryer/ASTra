package ast

import org.antlr.v4.runtime.tree.ParseTree
import org.fancryer.bf.ast.KotlinAst
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
class TranspilationRule<F:ParseTree,N:KotlinAst>(
	val from:KClass<F>,
	how:(F)->N,
	val name:String,
	@Suppress("UNCHECKED_CAST")
	val nodeClass:(KClass<N>?)=null, //how.reflect()?.returnType?.classifier as? KClass<N>,
	val demands:Requirements<F>,
	val ensures:Requirements<N>
):(F)->N
{
	val how=how.wrapHow(demands,ensures)

	override fun invoke(f:F)=how(f)

	override fun toString()=
		"TranspilationRule($name ${from.simpleName} -> \${nodeClass.simpleName} $demands $ensures)"

	companion object
	{
		fun <F:ParseTree,N:KotlinAst> ((F)->N).wrapHow(
			demands:Requirements<F>,
			ensures:Requirements<N>
		)={f:F->
			if(!demands.all {it(f)}) throw Exception("Demand contract violated")
			this(f).let {ret->
				if(!ensures.all {it(ret)}) throw Exception("Ensures contract violated")
				ret
			}
		}
	}
}

typealias Requirements<T>
		=List<(T)->Boolean>