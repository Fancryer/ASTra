package main

data class KotlinMathNode(val left:KotlinExprNode,val right:KotlinExprNode,val op:String):KotlinExprNode()
{
	override fun transpile()="($left $op $right)"
}