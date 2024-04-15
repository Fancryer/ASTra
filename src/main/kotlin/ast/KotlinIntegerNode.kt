package main

data class KotlinIntegerNode(val value:Int):KotlinExprNode()
{
	override fun transpile():String=value.toString()
}