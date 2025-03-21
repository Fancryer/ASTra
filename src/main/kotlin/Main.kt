package org.fancryer.bf

import arrow.core.*
import ast.RuleHolder
import ast.RuleVisitor
import astra.astra
import astra.astrap
import gen.LispLexer
import gen.LispParser
import gen.MiniJavaGrammarLexer
import gen.MiniJavaGrammarParser
import org.antlr.v4.runtime.*
import org.fancryer.bf.ast.*
import org.fancryer.bf.ast.FunctionDeclarationBuilder.Companion.kfun
import org.fancryer.bf.ast.KBlockBuilder.Companion.kblock
import org.fancryer.bf.ast.KClassDeclarationBuilder.Companion.kclass
import org.fancryer.bf.ast.KPostfixUnaryExpression.Companion.index
import org.fancryer.bf.ast.KPropertyDeclaration.Companion.property
import org.fancryer.bf.ast.KPropertyDeclarationBuilder.Companion.kval
import org.fancryer.bf.ast.KotlinFileBuilder.Companion.kotlinFile
import org.fancryer.bf.examples.lispRuleHolder
import org.fancryer.bf.examples.stlcRuleHolder
import stlc.gen.StlcLexer
import stlc.gen.StlcParser
import java.io.File
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
		examples.map {
			val str:String
			measureTime {
				str=tryT(stlcRuleHolder,it).code
			}.let {str to it}
		}
			.forEachIndexed {i,(it,time)-> println("${names[i]} [$time]: $it")}
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
	measureTime {
		buildString {
			val charStream=CharStreams.fromString(src)
			val lexer=LispLexer(charStream)
			val tokenStream=CommonTokenStream(lexer)
			val parser:LispParser=LispParser(tokenStream)
			val tree=parser.lisp()
			val visited=visitor.visit(tree)
			val code=visited.code
			println(code)
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
			val code=visited.code
			println(code)
			File("src/main/kotlin/gen/Stlc.kt") //.writeText(code)
		}
	}

	kclass("HelloWorld") {
		classBody {
			function("sortByLength") {
				"strings" ofType "List".id.simpleGeneric("String").userType.type
				blockBody {
					+"strings".id.call(
						KObjectLiteral(
							false,
							KDelegationSpecifiers(
								KAnnotatedDelegationSpecifier(
									emptyList(),
									"Comparator".id.simpleGeneric("String").userType
								).nel()
							).some(),
							KClassBody(
								kfun("compare") {
									modifiers(KModifiers(EMemberModifier.Override.nel()))
									type("Int".type)
									exprBody("a"["length".id]-"b"["length".id])
								}.list
							).some()
						)
					)
				}
			}
		}

		/*
		class HelloWorld {
		  fun sortByLength(strings: List<String>) {
			strings.sortedWith(object : Comparator<String> {
			  override fun compare(a: String, b: String): Int = a.length - b.length
			})
		  }
		}
		*/
	}.also {
		println(it.code)
	}
	//	println(codegenExample().code)
}

fun codegenExample()=kotlinFile {
	nonEmptyListOf("java".id,"util".id).let(::KIdentifier)
		.let(KImportHeader::KWildcardImport)
		.also(::import)

	+kclass("Task",EClassModifier.Data) {
		+EClassModifier.Data
		primaryConstructor {
			"id" ofType "Int"
			"description" ofType "String"
			"isDone" init {
				isVar()
				type("Boolean")
				expression(false.ast)
			}
		}
	}

	+kclass("TaskManager") {
		classBody {
			+kval("tasks") {
				isPrivate
				expr(
					"mutableListOf".id.call(
						"Task".type.proj.nel().typeArgs.some()
					)
				)
			}
			+kval("nextId") {
				isPrivate
				isVal(false)
				expr(1.ast)
			}
			/*
			private var nextId=1
			 */
			function("addTask") {
				+KFunctionValueParameter(
					parameter="description".id param "String".type
				)
				blockBody {
					+kval("task") {
						"Task".id.call("nextId".id.incr,"description".id).expr
					}
					+"tasks"["add".id].primary.call("task".id)
					+"println".id.call("Задача добавлена: \$task".ast)
				}
			}
			"listTasks" funBlock {
				stat(
					"tasks"["isEmpty".id].primary.call() ifTrue
							kblock {
								+"println".id.call("Нет задач.".ast)
								+kreturn
							}
				)
				+"println".id.call("Список задач:".ast)
				"tasks"["forEach".id].primary.call(
					KLambdaLiteral(
						"task".id.variableDecl.lambdaParams,
						listOf(
							"status".id.variableDecl.property(
								"task"["isDone".id].ifElse("[✓]".ast.stat,"[ ]".ast.stat)
							).stat,
							"println".id.call(
								"\$status \${task.id}: \${task.description}".ast
							).stat
						)
					)
				)
			}
			/*
			fun listTasks()
			{
				if(tasks.isEmpty())
				{
					println("Нет задач.")
					return
				}
				println("Список задач:")
				tasks.forEach {task->
					val status=if(task.isDone) "[✓]" else "[ ]"
					println("$status ${task.id}: ${task.description}")
				}
			}
			*/
		}
	}
}

val KExpression.primary:KPrimaryExpression
	get()=when(this)
	{
		is KPrimaryExpression->this
		else->this.paren
	}

fun add(x:Int,y:Int):Int=
	(x+y).let {res->
		(println(res)).let {p-> res}
	}

fun tryT(holder:RuleHolder<StlcLexer,StlcParser>,src:String):KotlinAst=
	src.let(CharStreams::fromString)
		.let(::StlcLexer)
		.let(::CommonTokenStream)
		.let(::StlcParser)
		.t()
		.let(RuleVisitor(holder)::visit)

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

fun KPrimaryExpression.call(
	suffix:KCallSuffixInner,
	args:(Option<KTypeArguments>)=none()
)=
	KPostfixUnaryExpression(
		this,
		KCallSuffix(suffix,args).list
	)

fun KPrimaryExpression.call(args:(Option<KTypeArguments>)=none())=
	call(KValueArguments(emptyList()),args)

fun KPrimaryExpression.call(
	valueArgs:NonEmptyList<KValueArgument>,
	args:(Option<KTypeArguments>)=None
)=
	KPostfixUnaryExpression(
		this,
		KCallSuffix(KValueArguments(valueArgs),args).list
	)

fun KPrimaryExpression.call(vararg valueArgs:KValueArgument):KPostfixUnaryExpression=
	call(valueArgs.toList().toNonEmptyListOrNull() ?: error("No arguments"))

fun KPrimaryExpression.call(vararg valueArgs:KExpression):KPostfixUnaryExpression=
	call(valueArgs.map {it.valueArg}.toNonEmptyListOrNull() ?: error("No arguments"))

infix fun KPrimaryExpression.call(valueArgs:NonEmptyList<KValueArgument>):KPostfixUnaryExpression=
	call(valueArgs,None)

infix fun KPrimaryExpression.call(suffix:KCallSuffixInner):KPostfixUnaryExpression=
	call(suffix,None)

val KPrimaryExpression.incr:KPostfixUnaryExpression
	get()=KPostfixUnaryExpression(this,KIncr.list)

fun KExpression.getProp(prop:KNavigationSuffixInner,op:KMemberAccessOperator=KDot):KPostfixUnaryExpression=
	KPostfixUnaryExpression(
		if(this is KPrimaryExpression) this else this.paren,
		KNavigationSuffix(prop,op).list
	)

operator fun KExpression.get(prop:KNavigationSuffixInner):KPostfixUnaryExpression=
	getProp(prop)

operator fun String.get(prop:KNavigationSuffixInner):KPostfixUnaryExpression=
	id[prop]

fun KExpression.getProp(prop:String,op:KMemberAccessOperator=KDot):KPostfixUnaryExpression=
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

val KSimpleUserType.userType get()=KUserType(this.nel(),0)

val KSimpleUserType.anno get()=anno(KUserType(this.nel(),0))

val KConcreteTypeProjection.args get()=KTypeArguments(this.nel())

val KTypeInner.type get()=KType(emptyList(),this)

infix fun KSimpleIdentifier.functionValueParameter(type:KType)=
	KFunctionValueParameter(None,KParameter(this,type),None)