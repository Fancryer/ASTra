package main

class KotlinListNode(val exps:(List<KotlinExprNode>)=emptyList()):KotlinExprNode()
{
	override fun transpile():String="listOf(${exps.joinToString(",") {it.transpile()}})"
}