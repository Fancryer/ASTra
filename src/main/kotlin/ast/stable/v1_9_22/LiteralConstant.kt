package ast.stable.v1_9_22

sealed interface LiteralConstant:PrimaryExpression
{
	data class BooleanLiteral(val value:Boolean):LiteralConstant
	data class IntegerLiteral(val value:Int):LiteralConstant
	data class HexLiteral(val value:String):LiteralConstant
	data class BinLiteral(val value:String):LiteralConstant
	data class CharacterLiteral(val value:Char):LiteralConstant
	data class RealLiteral(val value:Real):LiteralConstant
	{
		sealed interface Real
		{
			data class Float(val value:kotlin.Float):Real
			data class Double(val value:kotlin.Double):Real
		}
	}
	data object NullLiteral:LiteralConstant
	data class LongLiteral(val value:String):LiteralConstant
	data class UnsignedLiteral(val value:String):LiteralConstant
}