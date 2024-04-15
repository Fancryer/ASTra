package main

data class KotlinDebugNode(val debug:String):KotlinNode()
{
	override fun toString()="KotlinDebugNode($debug)"
	override fun transpile():String=""
}