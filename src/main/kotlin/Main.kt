package org.fancryer.bf

import arrow.core.*
import ast.*
import ast.EMemberModifier.Lateinit
import ast.EMemberModifier.Override
import ast.FunctionDeclarationBuilder.Companion.kfun
import ast.KAssignment.Companion.subAssign
import ast.KClassDeclarationBuilder.Companion.kclass
import ast.KPropertyDeclarationBuilder.Companion.kval
import ast.KWhenExpressionBuilder.Companion.kwhen
import ast.KWhileStatement.Companion.kwhile
import ast.KotlinFileBuilder.Companion.kotlinFile
import ast.LineStringLiteralBuilder.Companion.stringLiteral
import astra.astra
import astra.astrap
import emitter.KotlinEmitter
import examples.lispRuleHolder
import examples.stlcRuleHolder
import gen.LispLexer
import gen.LispParser
import gen.MiniJavaGrammarLexer
import gen.MiniJavaGrammarParser
import org.antlr.v4.runtime.*
import stlc.gen.StlcLexer
import stlc.gen.StlcParser
import java.io.File
import kotlin.time.measureTime

fun call(expr:KPrimaryExpression,suffix:KPostfixUnarySuffix)=
	KPostfixUnaryExpression(expr,listOf(suffix))

val <T> T.alsoPrintln get()=this.also {println(it)}

fun main()
{
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
		examples.map {
			val str:String
			measureTime {
				str=KotlinEmitter().emitKExpression(tryT(stlcRuleHolder,it))
			}.let {str to it}
		}
			.forEachIndexed {i,(it,time)-> println("${names[i]} [$time]: $it")}
	}.let {
		println("Stlc lexed, parsed and transpiled in $it")
	}

	val declaration1=kval("x") {
		+Lateinit // import EMemberModifier.Lateinit
		+Override // import EMemberModifier.Override
		isVar
		+10.ast
	}

	// Объявления равнозначны

	val declaration2=kval("x") {
		mods(Lateinit,Override)
		isVal(false)
		expr(10.ast)
	}

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
	measureTime {
		buildString {
			val charStream=CharStreams.fromString(src)
			val lexer=LispLexer(charStream)
			val tokenStream=CommonTokenStream(lexer)
			val parser=LispParser(tokenStream)
			val tree=parser.lisp()
			println("[[[")
			val visited=visitor.visit(tree)
			println(visited)
			println("|||")
			val code=KotlinEmitter().emitAst(visited)
			println(code)
			println("]]]")
		}
	}
	measureTime {
		buildString {
			val charStream=CharStreams.fromFileName("src/main/resources/stlc.astra")
			val lexer=astra(charStream)
			val tokenStream=CommonTokenStream(lexer)
			val parser=astrap(tokenStream)
			val tree=parser.program()
			val visited=AstralToKotlinMapper().visitProgram(tree)
			val code=KotlinEmitter().emitKKotlinFile(visited)
			println(code)
			File("src/main/kotlin/gen/Stlc.kt") //.writeText(code)
		}
	}

	KotlinEmitter().emitKExpression(
		KWhenExpression(
			KWhenSubject(expression="x".id).some(),
			listOf(
				KWhenEntryWithConditions(
					KTypeTest(EIsOperator.Is,"Int".type).nel(),
					"Int".ast.stat
				),
				KWhenEntryWithConditions(
					KTypeTest(EIsOperator.Is,"String".type).nel(),
					"String".ast.stat
				),
				KWhenElseEntry("Unknown".ast.stat)
			)
		)
	).alsoPrintln

	KotlinEmitter().emitKExpression(
		kwhen("x".id) {
			"Int".type caseIs "Int".ast
			"String".type caseIs "String".ast
			"Unknown".ast.stat.caseElse
		}
	).alsoPrintln

	kotlinFile {
		+kclass("Character",EInheritanceModifier.Abstract) {
			primaryConstructor {
				"name" ofType "String"
				"health" init {
					isVar
					type("Int")
				}
			}
			classBody {
				function("attack") {
					+EInheritanceModifier.Abstract
					"target" ofType "Character"
				}
				function("isAlive") {
					returns("Boolean".type)
					exprBody("health".id greater 0.ast)
				}
				function("takeDamage") {
					+EInheritanceModifier.Open
					"amount" ofType "Int"
					blockBody {
						+"health".id.subAssign("amount".id)
						+"println".id(
							stringLiteral {
								ref("name")
								text(" получил ")
								ref("amount")
								text(" урона. Осталось ")
								ref("health")
								text(" HP.")
							}
						)
						+"health".id
							.lessEq(0.ast)
							.ifTrue("println".id("\$name пал в бою...".ast).stat)

					}
				}
			}
		}

		+kclass("Hero") {
			primaryConstructor {
				"name" init {
					notMine
					type("String")
				}
				"health" init {
					notMine
					type("Int")
				}
				"power" ofType "Int"
			}
			delegationSpecifiers {
				"Character".id.simpleUserType.userType constructorInvocation
						listOf("name".id.valueArg,"health".id.valueArg)
			}
			classBody {
				function("attack") {
					+Override
					"target" ofType "Character"
					blockBody {
						+"println".id(
							stringLiteral {
								ref("name")
								text(" атакует ")
								expr("target".id["name".id])
								text("!")
							}
						)
						+"target"["takeDamage".id]("power".id)
					}
				}
			}
		}

		+kclass("Monster") {
			primaryConstructor {
				"name" init {
					notMine
					type("String")
				}
				"health" init {
					notMine
					type("Int")
				}
				"damage" ofType "Int"
			}
			delegationSpecifiers {
				"Character".id.simpleUserType.userType constructorInvocation
						listOf("name".id.valueArg,"health".id.valueArg)
			}
			classBody {
				function("attack") {
					+Override
					"target" ofType "Character"
					blockBody {
						+"println".id(
							stringLiteral {
								ref("name")
								text(" кусает ")
								expr("target".id["name".id])
								text("!")
							}
						)
						+"target"["takeDamage".id]("damage".id)
					}
				}
			}
		}

		+kfun("main") {
			blockBody {
				+kval("hero","Hero".id("Алиса".ast,100.ast,20.ast))
				+kval("goblin","Monster".id("Гоблин".ast,100.ast,20.ast))

				+"println".id("⚔️ Битва начинается!".ast)

				+("hero"["isAlive".id]() and "goblin"["isAlive".id]()).kwhile {
					+"hero"["attack".id]("goblin".id)
					+"goblin"["isAlive".id]().ifTrue("goblin"["attack".id]("hero".id).stat)
					+"println".id()
				}

				+"println".id("🏁 Битва окончена!".ast)
			}
		}
	}.alsoPrintln
		.let(KotlinEmitter()::emitKKotlinFile)
		.alsoPrintln
}

operator fun KExpression.invoke(vararg args:KExpression):KPostfixUnaryExpression=
	when(this)
	{
		is KPostfixUnaryExpression->invoke(*args)
		is KPrimaryExpression->invoke(*args)
		else->primary(*args)
	}

val KExpression.primary:KPrimaryExpression
	get()=when(this)
	{
		is KPrimaryExpression->this
		is KPostfixUnaryExpression->this.primaryExpression
		else->this.paren
	}

fun tryT(holder:RuleHolder<StlcLexer,StlcParser>,src:String)=
	src.let(CharStreams::fromString)
		.let(::StlcLexer)
		.let(::CommonTokenStream)
		.let(::StlcParser)
		.t()
		.let(RuleVisitor(holder)::visit)
		.let {it as KExpression}

fun <T,R> fix(f:((T)->R)->(T)->R):(T)->R=
	{x-> f(fix(f))(x)}

inline val KIdentifierInner.full get()=KIdentifier(this.nel())

val Boolean.ast get()=if(this) EBooleanLiteral.True else EBooleanLiteral.False

val KExpression.paren get()=KParenthesizedExpression(this)

val KExpression.valueArg get()=KValueArgument(None,None,false,this)
val KValueArgument.args get()=KValueArguments(list)

val KStatement.lambda get()=KLambdaLiteral(emptyList(),list)

val KStatementInner.stat get()=KStatement(emptyList(),this)
val KStatementInner.block get()=stat.block

infix fun <T> List<T>.len(n:Int)=size==n

inline infix fun <T,R> T.pipe(f:(T)->R)=let(f)

fun <T,R,C> T.pipeWith(g:C,f:(Pair<T,C>)->R)=f(this to g)

fun <P:Parser,L:Lexer> getParser(src:String,p:(TokenStream)->P,l:(CharStream)->L)=
	src.let(CharStreams::fromString)
		.let(l)
		.let(::CommonTokenStream)
		.let(p)

fun getParser(src:String)=getParser(src,::MiniJavaGrammarParser,::MiniJavaGrammarLexer)

val KIdentifier.packageHeader get()=KPackageHeader(this.some())

val <T> T.list get()=listOf(this)

val Int.ast get()=KIntegerLiteral(this)

val KExpression.strExpr:KLineStringExpression
	get()=KLineStringExpression(this)

/// CALLS START

fun KPostfixUnaryExpression.invoke(
	suffix:KCallSuffixInner,
	args:(Option<KTypeArguments>)=none()
)=
	KPostfixUnaryExpression(
		this.primary,
		this.suffixes+KCallSuffix(suffix,args)
	)

fun KPostfixUnaryExpression.invoke(
	valueArgs:NonEmptyList<KValueArgument>,
	args:(Option<KTypeArguments>)=None
)=
	KPostfixUnaryExpression(
		this.primary,
		this.suffixes+KCallSuffix(KValueArguments(valueArgs),args)
	)

//

operator fun KPostfixUnaryExpression.invoke(args:(Option<KTypeArguments>)=none())=
	invoke(KValueArguments(emptyList()),args)

operator fun KPostfixUnaryExpression.invoke(vararg valueArgs:KValueArgument):KPostfixUnaryExpression=
	invoke(valueArgs.toList().toNonEmptyListOrNull() ?: error("No arguments"))

operator fun KPostfixUnaryExpression.invoke(vararg valueArgs:KExpression):KPostfixUnaryExpression=
	invoke(valueArgs.map {it.valueArg}.toNonEmptyListOrNull() ?: error("No arguments"))

operator fun KPostfixUnaryExpression.invoke(valueArg:KExpression):KPostfixUnaryExpression=
	invoke(valueArg.valueArg)

operator fun KPostfixUnaryExpression.invoke(valueArgs:NonEmptyList<KValueArgument>):KPostfixUnaryExpression=
	invoke(valueArgs,None)

operator fun KPostfixUnaryExpression.invoke(suffix:KCallSuffixInner):KPostfixUnaryExpression=
	invoke(suffix,None)
////

operator fun KPrimaryExpression.invoke(vararg valueArgs:KValueArgument):KPostfixUnaryExpression=
	invoke(valueArgs.toList().toNonEmptyListOrNull() ?: error("No arguments"))

operator fun KPrimaryExpression.invoke(vararg valueArgs:KExpression):KPostfixUnaryExpression=
	invoke(valueArgs.map {it.valueArg}.toNonEmptyListOrNull() ?: error("No arguments"))

infix operator fun KPrimaryExpression.invoke(valueArg:KExpression):KPostfixUnaryExpression=
	invoke(valueArg.valueArg)

infix fun KPrimaryExpression.invoke(valueArgs:NonEmptyList<KValueArgument>):KPostfixUnaryExpression=
	invoke(valueArgs,None)

infix fun KPrimaryExpression.invoke(suffix:KCallSuffixInner):KPostfixUnaryExpression=
	invoke(suffix,None)

///MID


operator fun KPrimaryExpression.invoke(
	suffix:KCallSuffixInner,
	args:(Option<KTypeArguments>)=none()
)=
	KPostfixUnaryExpression(
		this,
		KCallSuffix(suffix,args).list
	)

operator fun KPrimaryExpression.invoke(args:(Option<KTypeArguments>)=none())=
	invoke(KValueArguments(emptyList()),args)

operator fun KPrimaryExpression.invoke(
	valueArgs:NonEmptyList<KValueArgument>,
	args:(Option<KTypeArguments>)=None
)=
	KPostfixUnaryExpression(
		this,
		KCallSuffix(KValueArguments(valueArgs),args).list
	)

/// CALLS END

fun KExpression.getProp(
	prop:KNavigationSuffixInner,
	op:KMemberAccessOperator=KMemberAccessOperator.Dot
):KPostfixUnaryExpression=
	KPostfixUnaryExpression(
		if(this is KPrimaryExpression) this else this.paren,
		KNavigationSuffix(prop,op).list
	)

operator fun KExpression.get(prop:KNavigationSuffixInner):KPostfixUnaryExpression=
	getProp(prop)

operator fun String.get(prop:KNavigationSuffixInner):KPostfixUnaryExpression=
	id[prop]

fun KExpression.getProp(prop:String,op:KMemberAccessOperator=KMemberAccessOperator.Dot):KPostfixUnaryExpression=
	getProp(prop.id,op)

val String.id get()=KIdentifierInner(this)

val KSimpleIdentifier.simpleUserType get()=KSimpleUserType(this,none())
fun KSimpleIdentifier.simpleGeneric(projection:KTypeProjection)=
	KSimpleUserType(
		this,
		KTypeArguments(projection.nel()).some()
	)

fun KSimpleIdentifier.simpleGeneric(type:KType)=simpleGeneric(type.proj)
fun KSimpleIdentifier.simpleGeneric(type:String)=simpleGeneric(type.type)

val KSimpleUserType.userType get()=KUserType(nel(),0)

val KSimpleUserType.anno get()=anno(KUserType(nel(),0))

val KConcreteTypeProjection.args get()=KTypeArguments(nel())

val KTypeInner.type get()=KType(emptyList(),this)

infix fun KSimpleIdentifier.functionValueParameter(type:KType)=
	KFunctionValueParameter(None,KParameter(this,type),None)