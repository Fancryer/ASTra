package org.fancryer.bf.ast.rules

import org.antlr.v4.runtime.tree.ParseTree
import org.fancryer.bf.ast.KotlinAst

interface InoutLogger<I,O,T>
{
	var ident:T
	fun before(from:I)=Unit
	fun after(to:O)=Unit
	val useBefore:(I)->Boolean
	val useAfter:(O)->Boolean
}

class InoutLoggerImpl<I:Any,O:Any>(
	override val useBefore:(I)->Boolean,
	override val useAfter:(O)->Boolean
):InoutLogger<I,O,Int>
{
	override var ident=0

	override fun before(from:I)=Unit
//		println("${"  ".repeat(++ident)} INOUT before ${from::class.simpleName}")

	override fun after(to:O)=Unit
//		println("${"  ".repeat(ident--)} INOUT after ${to::class.simpleName}")

	companion object
	{
		fun <I:Any,O:Any> inoutLogger(before:(I)->Unit,after:(O)->Unit)=
			object:InoutLogger<I,O,Int>
			{
				override var ident:Int=0
				override val useBefore:(I)->Boolean={true}
				override val useAfter:(O)->Boolean={true}

				override fun before(from:I)=before(from)
				override fun after(to:O)=after(to)
			}
	}
}

interface RuleLogger<P:ParseTree,A:KotlinAst>:InoutLogger<P,A,Int>
{
	override fun before(from:P)=Unit
	override fun after(to:A)=Unit
}

class RuleLoggerImpl<P:ParseTree,A:KotlinAst>:RuleLogger<P,A>
{
	override var ident=0

	override fun before(from:P)=Unit
//		println("${"  ".repeat(ident++)} RULE before ${from::class.simpleName}")

	override fun after(to:A)=Unit
//		println("${"  ".repeat(--ident)} RULE after ${to::class.simpleName}")
	override val useBefore:(P)->Boolean={true}
	override val useAfter:(A)->Boolean={true}
}