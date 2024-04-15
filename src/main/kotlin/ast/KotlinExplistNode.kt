package main

class KotlinExplistNode(val list:(List<KotlinExprNode>)=emptyList()):KotlinNode()
{
	override fun transpile()=list.joinToString(" "){it.transpile()}
}