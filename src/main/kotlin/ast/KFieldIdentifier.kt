package ast

data class KFieldIdentifier(val id:KSimpleIdentifier):KotlinAst
{
	companion object
	{
		val KSimpleIdentifier.field get()=KFieldIdentifier(this)
	}
}