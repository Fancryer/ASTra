package org.fancryer.bf

import arrow.core.*
import ast.RuleHolder
import ast.RuleVisitor
import gen.LispLexer
import gen.LispParser
import gen.LispParser.LispContext
import gen.MiniJavaGrammarLexer
import gen.MiniJavaGrammarParser
import org.antlr.v4.runtime.*
import org.fancryer.bf.ast.*
import org.fancryer.bf.examples.lispRuleHolder
import org.fancryer.bf.examples.stlcRuleHolder
import stlc.gen.StlcLexer
import stlc.gen.StlcParser
import java.util.stream.IntStream.range
import kotlin.time.Duration
import kotlin.time.measureTime

fun call(expr:KPrimaryExpression,suffix:KPostfixUnarySuffix)=
	KPostfixUnaryExpression(expr,listOf(suffix))

val <T> T?.alsoPrintln get()=this.also {println(it)}

fun main()
{
	//	measureTime {
	//		loxRuleHolder.transpile(
	//			"var a = 4 * 4 * 4 * 4 * 4 * 4;",
	//			::LoxLexer,
	//			::LoxParser,
	//			LoxParser::declaration
	//		).code.alsoPrintln
	//	}.alsoPrintln
	val varF="x"

	//The identity function for booleans.
	val idF="\\x:Bool, x"

	//The identity function for booleans, applied to the boolean true.
	val idTrueF="(\\x:Bool, x) true"

	//The boolean "not" function.
	val notF="\\x:Bool, if x then false else true"

	//The constant function that takes every (boolean) argument to true.
	val trueF="\\x:Bool, true"

	//A two-argument function that takes two booleans and returns the first one.
	// (As in Coq, a two-argument function in the lambda-calculus is really a
	// one-argument function whose body is also a one-argument function.)
	val firstF="\\x:Bool, \\y:Bool, x"

	//A two-argument function that takes two booleans and returns the first one, applied to the booleans false and true.
	//As in Coq, application associates to the left -- i.e.,
	// this expression is parsed as ((\x:Bool, \y:Bool, x) false) true.
	val firstFalseTrueF="(\\x:Bool, \\y:Bool, x) false true"

	//A higher-order function that takes a function f (from booleans to booleans) as an argument,
	// applies f to true, and applies f again to the result.
	val ffF="\\f:Bool -> Bool, f (f true)"

	//The same higher-order function, applied to the constantly false function.
	val ffFalseF="(\\f:Bool -> Bool, f (f true)) (\\x:Bool, false)"

	//	val srcs=listOf(varF,idF,idTrueF,notF,trueF,firstF,firstFalseTrueF,ffF,ffFalseF)

	val examples=sequenceOf(varF,idF,idTrueF,notF,trueF,firstF,firstFalseTrueF,ffF,ffFalseF)
	val names=listOf("varF","idF","idTrueF","notF","trueF","firstF","firstFalseTrueF","ffF","ffFalseF")

	measureTime {
		examples.map {tryT(stlcRuleHolder,it).code}
			.forEachIndexed {i,it-> println("${names[i]}: $it")}
	}.let {
		println("Stlc lexed, parsed and transpiled in $it")
	}

	/*
	let x = 10 in
	let square = fun n -> n * n in
	square x

	(let x 10 (
	  let square (lambda n (* n n)) (
	    square x
	  )
	))
	*/
	val src="""
    (let
      x
      10
      (let
        square
        (lambda n int (* n n))
        (square x)
      )
    )
	(define
	  add
	  x int
	  y int
	  (int
	    (let
	      res
	      (+ x y)
	      (let
	        p
	        (println res)
	        res
	      )
	    )
	  )
	)
	""".trimIndent()

	val visitor=RuleVisitor(lispRuleHolder)
	val times=(0..<1000).asSequence().map {
		val tuple:Tuple7<Duration,Duration,Duration,Duration,Duration,Duration,Duration>
		measureTime {
			buildString {
				append("[$it] {")
				val charStream:CharStream
				val charStreamCreationTime=measureTime {
					charStream=CharStreams.fromString(src)
				}
				append("\tcharStreamCreationTime: $charStreamCreationTime\n")
				val lexer:LispLexer
				val lexerCreationTime=measureTime {
					lexer=LispLexer(charStream)
				}
				append("\tlexerCreationTime: $lexerCreationTime\n")
				val tokenStream:CommonTokenStream
				val tokenStreamCreationTime=measureTime {
					tokenStream=CommonTokenStream(lexer)
				}
				append("\ttokenStreamCreationTime: $tokenStreamCreationTime\n")
				val parser:LispParser
				val parserCreationTime=measureTime {
					parser=LispParser(tokenStream)
				}
				append("\tparserCreationTime: $parserCreationTime\n")
				val tree:LispContext
				val treeParsingTime=measureTime {
					tree=parser.lisp()
				}
				append("\ttreeParsingTime: $treeParsingTime\n")
				val visited:KotlinAst
				val visitedTime=measureTime {
					visited=visitor.visit(tree)
				}
				append("\tvisitedTime: $visitedTime\n")
				val code:String
				val codeTime=measureTime {
					code=visited.code
				}
				append("\tcodeTime: $codeTime\n")
				append("}")
				tuple=Tuple7(
					charStreamCreationTime,
					lexerCreationTime,
					tokenStreamCreationTime,
					parserCreationTime,
					treeParsingTime,
					visitedTime,
					codeTime
				)
			}
			//			.let {
			//			"[$n] Lisp lexed, parsed and transpiled in $it" to it
			//		}
		}to tuple
	}

	times.forEachIndexed { index, (it,tuple) ->
		val (a,b,c,d,e,f,g)=tuple
		println("index: $index, time: $it {")
		println("\tcharStreamCreationTime: $a")
		println("\tlexerCreationTime: $b")
		println("\ttokenStreamCreationTime: $c")
		println("\tparserCreationTime: $d")
		println("\ttreeParsingTime: $e")
		println("\tvisitedTime: $f")
		println("\tcodeTime: $g")
		println("\tavgTime: ${a+b+c+d+e+f+g}")
		println("\tminTime: ${sequenceOf(a,b,c,d,e,f,g).min()}")
		println("\tmaxTime: ${sequenceOf(a,b,c,d,e,f,g).max()}")
		println("\ttotalTime: ${a+b+c+d+e+f+g}")
		println("}")
	}

	val avg=(0..<1000).asSequence().map {n->
		measureTime {
			//			val file=
			RuleVisitor(lispRuleHolder).visit(
				(src pipe
						CharStreams::fromString pipe
						::LispLexer pipe
						::CommonTokenStream pipe
						::LispParser).lisp()
			).code
			//			println("```kotlin")
			//			println(file)
			//			println("```")
		}.let {
			"[$n] Lisp lexed, parsed and transpiled in $it" to it
		}
	}.onEach {(s,_)->
		println(s)
	}.map {
		it.second
	}.reduceOrNull {acc,duration->
		acc+duration
	}?.div(1000)
			?: Duration.ZERO
	println("Avg time: $avg")
}

fun add(x:Int,y:Int):Int=
	(x+y).let {res->
		(println(res)).let {p-> res}
	}

fun tryT(holder:RuleHolder<StlcLexer,StlcParser>,src:String):KotlinAst=
	RuleVisitor(holder).visit(
		(src pipe
				CharStreams::fromString pipe
				::StlcLexer pipe
				::CommonTokenStream pipe
				::StlcParser).t()
	)

fun <T,R> fix(f:((T)->R)->(T)->R):(T)->R=
	{x-> f(fix(f))(x)}

inline val KIdentifierInner.full get()=KIdentifier(this.nel())

val Boolean.ast get()=if(this) EBooleanLiteral.True else EBooleanLiteral.False

val KExpression.paren get()=KParenthesizedExpression(this)

val KExpression.valueArg get()=KValueArgument(None,None,false,this)
val KValueArgument.args get()=KValueArguments(nel().some())

val KStatement.lambda get()=KLambdaLiteral(None,nel().some())

val KStatementInner.stat get()=KStatement(emptyList(),this)
val KStatementInner.block get()=stat.block

infix fun <T> List<T>.len(n:Int)=size==n

inline infix fun <T,R> T.pipe(f:(T)->R)=let(f)

fun <T,R,C> T.pipeWith(g:C,f:(Pair<T,C>)->R)=f(this to g)

fun <P:Parser,L:Lexer> getParser(src:String,p:(TokenStream)->P,l:(CharStream)->L)=
	CharStreams.fromString(src) pipe l pipe ::CommonTokenStream pipe p

fun getParser(src:String)=getParser(src,::MiniJavaGrammarParser,::MiniJavaGrammarLexer)

val KIdentifier.packageHeader get()=KPackageHeader(this.some())

val <T> T.list get()=listOf(this)

val Int.ast get()=KIntegerLiteral(this)

val KExpression.strExpr:KLineStringExpression
	get()=KLineStringExpression(this)

fun KPrimaryExpression.call(suffix:KCallSuffixInner,args:(Option<KTypeArguments>)=none())=
	KPostfixUnaryExpression(
		this,
		KCallSuffix(args,suffix).list
	)

fun KPrimaryExpression.call(
	valueArgs:NonEmptyList<KValueArgument>,
	args:(Option<KTypeArguments>)=None
)=
	KPostfixUnaryExpression(
		this,
		KCallSuffix(args,KValueArguments(valueArgs.some())).list
	)

fun KExpression.getProp(prop:KNavigationSuffixInner,op:KMemberAccessOperator=KDot)=
	KPostfixUnaryExpression(
		if(this is KPrimaryExpression) this else this.paren,
		KNavigationSuffix(op,prop).list
	)

val KDeclaration.topLevel get()=KTopLevelObject(this)

val String.id get()=KIdentifierInner(this)

val KSimpleIdentifier.simpleUserType get()=KSimpleUserType(this,none())
fun KSimpleIdentifier.simpleGeneric(projection:org.fancryer.bf.ast.KTypeProjection)=
	KSimpleUserType(
		this,
		KTypeArguments(projection.nel()).some()
	)

val KSimpleUserType.userType get()=KUserType(this.nel())

val KSimpleUserType.anno get()=anno(KUserType(this.nel()))

val KConcreteTypeProjection.args get()=KTypeArguments(this.nel())

val KTypeInner.type get()=KType(None,this)

val KType.concreteTypeProjection get()=KConcreteTypeProjection(None,this)
fun KType.concreteTypeProjection(modifiers:KTypeProjectionModifiers)=
	KConcreteTypeProjection(modifiers.some(),this)

infix fun KSimpleIdentifier.functionValueParameter(type:KType)=
	KFunctionValueParameter(None,KParameter(this,type),None)