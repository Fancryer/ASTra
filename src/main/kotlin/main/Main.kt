package main

import gen.NuMLLexer
import gen.NuMLParser
import gen.NuMLParser.ExpContext
import gen.NuMLParser.ExplistContext
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.tree.ParseTree


fun main()
{
	val src="[[],[[],[[],[]]]]"
	val parser=getParser(src)
	val ruleHolder=parser.rules({tree-> KotlinErrorNode(Sourcifier().sourcify(tree))}) {holderBuilder->
		//		rule {
		//			name("Array")
		//			from(ArrayContext::class)
		//			how {ctx:ArrayContext->
		//				val m=mapWithOrThrow<ExplistContext,KotlinExplistNode>(
		//
		//				)
		//				lookup<ExplistContext,KotlinExplistNode>()
		//					?.let {rule->
		//						ctx.explist()?.let {
		//							KotlinListNode(rule.how(it).list)
		//						} ?: KotlinListNode()
		//					} ?: throw RuntimeException("No rule found for ${getNameUnsafe()}->KotlinExplistNode")
		//			}
		//		}
		rule {
			name("Explist")
			from(ExplistContext::class)
			how {ctx:ExplistContext->
				lookup<ExpContext,KotlinExprNode>()?.let {it:TranspilationRule<ExpContext,KotlinExprNode>->
					KotlinExplistNode(ctx.exp().map(it.how))
				} ?: throw RuntimeException("No rule found for ${getNameUnsafe()}->KotlinExprNode")
			}
		}
		//		rule<ExpContext,KotlinExprNode> {
		//			name("Exp")
		//			from(ExpContext::class)
		//			how {ctx:ExpContext->
		//				when(ctx)
		//				{
		//					is ArrayContext->
		//					{
		//						mapWithOrThrow<ArrayContext,KotlinListNode>(ctx) {
		//							RuntimeException("No rule found for ${getNameUnsafe()}->KotlinListNode")
		//						}
		//					}
		//
		//					else->TODO("Only arrays of arrays supported")
		//				}
		//			}
		//		}
	}
	println(RuleVisitor(ruleHolder).visit(parser.exp()).transpile())
}

fun <F:ParseTree,H:KotlinNode> TranspilationRule<F,H>?.act(f:F)=SusNodeHolder(this?.how?.let {it(f)})

data class SusNodeHolder<H:KotlinNode>(val node:H?)
{
	fun fall(react:()->Throwable)=node ?: throw react()
	fun trust()=fall {RuntimeException("Nullable node cannot be extracted from SusNodeHolder")}
	fun another(a:()->H)=node ?: a()
}

sealed class KotlinExprNode:KotlinNode()
data class KotlinMathNode(val left:KotlinExprNode,val right:KotlinExprNode,val op:String):KotlinExprNode()
{
	override fun transpile()="($left $op $right)"
}

data class KotlinIdNode(val id:String):KotlinExprNode()
{
	override fun transpile():String=id
}

data class KotlinIntegerNode(val value:Int):KotlinExprNode()
{
	override fun transpile():String=value.toString()
}

data class KotlinErrorNode(val error:String):KotlinNode()
{
	override fun toString()="KotlinErrorNode($error)"
	override fun transpile():String="throw Exception($error)"
}

data class KotlinDebugNode(val debug:String):KotlinNode()
{
	override fun toString()="KotlinDebugNode($debug)"
	override fun transpile():String=""
}

fun getParser(src:String)=
	NuMLParser(
		CommonTokenStream(
			NuMLLexer(
				CharStreams.fromString(src)
			)
		)
	)