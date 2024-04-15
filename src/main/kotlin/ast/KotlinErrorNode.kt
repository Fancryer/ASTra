package main

data class KotlinErrorNode(val error:String):KotlinNode()
{
	override fun toString()="KotlinErrorNode($error)"
	override fun transpile():String="throw Exception($error)"
}