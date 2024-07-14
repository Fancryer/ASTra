package ast

import org.antlr.v4.runtime.tree.ParseTree
import org.fancryer.bf.ast.KotlinAst

data class SusNodeHolder<H:KotlinAst>(val node:H?)
{
	fun fall(react:()->Throwable)=node ?: throw react()
	fun trust()=fall {RuntimeException("Nullable node cannot be extracted from SusNodeHolder")}
	fun another(a:()->H)=node ?: a()
}

fun <F:ParseTree,H:KotlinAst> TranspilationRule<F,H>?.act(f:F)=SusNodeHolder(this?.how?.let {it(f)})