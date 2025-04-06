package ast

/*
Check with:
	(Letter | '_') (Letter | '_' | UnicodeDigit)*
    | '`' ~([\r\n] | '`')+ '`'
*/
class KIdentifierInner(name:String):KSimpleIdentifier
{
	val name=name //if(name.matches(Regex(""))) name else error("Bad identifier: $name")
	override val id:String=name
}