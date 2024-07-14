import ast.RuleHolder
import ast.RuleVisitor
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.fancryer.bf.*
import org.fancryer.bf.ast.*
import org.fancryer.bf.examples.stlcRuleHolder
import org.junit.jupiter.api.Test
import stlc.gen.StlcLexer
import stlc.gen.StlcParser
import kotlin.test.assertEquals

//	@Test

class StlcTest
{
	// The variable "x".
	@Test
	fun testVariable()
	{
		assertEquals("x",tryT(holder,"x").code)
	}

	// The identity function for booleans.
	@Test
	fun testIdentity()
	{
		val idF="\\x:Bool, x"
		assertEquals("{x: Boolean -> x;}",tryT(holder,idF).code)
	}

	// The identity function for booleans, applied to the boolean true.
	@Test
	fun testIdentityAppliedToTrue()
	{
		val idTrueF="(\\x:Bool, x) true"
		assertEquals("(({x: Boolean -> x;}))(true)",tryT(holder,idTrueF).code)
	}

	// The boolean "not" function.
	@Test
	fun testNot()
	{
		val notF="\\x:Bool, if x then false else true"
		assertEquals("{x: Boolean -> if (x) {false;}{true;};}",tryT(holder,notF).code)
	}

	// The constant function that takes every (boolean) argument to true.
	@Test
	fun testTrue()
	{
		val trueF="\\x:Bool, true"
		assertEquals("{x: Boolean -> true;}",tryT(holder,trueF).code)
	}

	// A two-argument function that takes two booleans and returns the first one.
	// (As in Coq, a two-argument function in the lambda-calculus is really a
	// one-argument function whose body is also a one-argument function.)
	@Test
	fun testFirst()
	{
		val firstF="\\x:Bool, \\y:Bool, x"
		assertEquals("{x: Boolean -> {y: Boolean -> x;};}",tryT(holder,firstF).code)
	}

	// A two-argument function that takes two booleans and returns the first one, applied to the booleans false and true.
	// As in Coq, application associates to the left -- i.e.,
	// this expression is parsed as ((\x:Bool, \y:Bool, x) false) true.
	@Test
	fun testFirstFalseTrue()
	{
		val firstFalseTrueF="(\\x:Bool, \\y:Bool, x) false true"
		assertEquals("((({x: Boolean -> {y: Boolean -> x;};}))(false))(true)",tryT(holder,firstFalseTrueF).code)
	}

	// A higher-order function that takes a function f (from booleans to booleans) as an argument,
	// applies f to true, and applies f again to the result.
	@Test
	fun testFF()
	{
		val ffF="\\f:Bool -> Bool, f (f true)"
		assertEquals("{f: ( Bool) ->  Boolean -> (f)(((f)(true)));}",tryT(holder,ffF).code)
	}

	// The same higher-order function, applied to the constantly false function.
	@Test
	fun testFFFalse()
	{
		val ffFalseF="(\\f:Bool -> Bool, f (f true)) (\\x:Bool, false)"
		assertEquals("(({f: ( Bool) ->  Boolean -> (f)(((f)(true)));}))(({x: Boolean -> false;}))",tryT(holder,ffFalseF).code)
	}

	fun tryT(holder:RuleHolder,src:String):KotlinAst=
		src pipe
				CharStreams::fromString pipe
				::StlcLexer pipe
				::CommonTokenStream pipe
				::StlcParser pipe
				StlcParser::t pipe
				RuleVisitor(holder)::visit1

	companion object
	{
		val holder=stlcRuleHolder
	}
}