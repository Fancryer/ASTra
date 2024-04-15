package main

data class KotlinIdNode(val id:String):KotlinExprNode()
{
	override fun transpile():String=id
}