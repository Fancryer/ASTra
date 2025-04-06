package ast

import org.antlr.v4.runtime.tree.ParseTree
import ast.rules.RuleLogger
import ast.rules.RuleLoggerImpl
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
	val logger:(RuleLogger<P,A>)=RuleLoggerImpl()
):(P)->A
{

	override fun invoke(f:P):A=how(f)

	override fun toString()=
		"TranspilationRule($name ${from.simpleName} -> \${nodeClass.simpleName})"
}

typealias Requirements<T>
		=List<(T)->Boolean>