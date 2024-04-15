package main

sealed class KotlinNode
{
	abstract fun transpile():String
}