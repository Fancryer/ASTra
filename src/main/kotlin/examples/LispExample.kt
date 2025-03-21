package org.fancryer.bf.examples

import arrow.core.*
import ast.Sourcifier
import ast.rules
import ast.sourcify
import gen.LispLexer
import gen.LispParser
import gen.LispParser.*
import org.fancryer.bf.*
import org.fancryer.bf.ast.*
import org.fancryer.bf.ast.FunctionDeclarationBuilder.Companion.kfun
import org.fancryer.bf.ast.KPostfixUnaryExpression.Companion.suffix
import org.fancryer.bf.ast.KPropertyDeclaration.Companion.property

sealed interface SExp
sealed interface AtomicSymbol
data class AId(val id:String):AtomicSymbol
data class AInt(val i:Int):AtomicSymbol
data class STuple(val l:SExp,val r:SExp):SExp
data class SList(val l:NonEmptyList<SExp>):SExp
data object SNil:SExp
/*
s_expression:
  atomic_symbol
  | '(' s_expression '.' s_expression ')'
  | list
  | '(' ')'
  ;
*/

val lispRuleHolder=rules<LispLexer,LispParser> {

	default {
		println("default for ${it::class.simpleName}")
		"TODO".id call KLineStringLiteral().valueArg.args
	}

	val rLisp=
		rule("lisp") {
			var dummyCounter=0
			from(LispContext::class)
			how {ctx:LispContext->
				val sexp=lookup<S_expressionContext,KExpression>()!!
				ctx.s_expression()
					.asSequence()
					.map {
						it.list()?.let {list->
							list.s_expression()
								.split()
								?.let {(tail,head)->
									val atomic=lookup<Atomic_symbolContext,KPrimaryExpression>()
											   ?: error("unknown sexp")
									head.atomic_symbol()
										?.let(atomic::invoke)
										?.let {expr->
											if(expr !is KSimpleIdentifier||expr.id!="define") null
											else
											{
												assert(tail.size>2) {
													"define must have at least 3 parts:"+
													"[!0] `define`"+
													"[!1] name"+
													"[?2] arg:(id type)*"+
													"([?3] type [!4] expr)"
												}
												val name=tail.first()
													.atomic_symbol()
													.let(atomic::invoke).let {
														it as? KSimpleIdentifier
														?: error("define name must be an identifier, not ${it::class}")
													}
												val args=tail.drop(1)
													.dropLast(1)
													.chunked(2) {(l,r)->
														sexp(l) to sexp(r)
													}.filterIsInstance<Pair<KSimpleIdentifier,KSimpleIdentifier>>()
													.map {(k,v)->
														k.id to v.simpleUserType.userType.type
													}
												val (exp,type)=
													tail.last()
														?.list()
														?.s_expression()
														?.split()
													?: error("define must have type and expression")
												val asType=sexp(type)
												val funType=(asType as? KSimpleIdentifier)?.let {id->
													id.type {
														replaceFirstChar {it.uppercase()}
													}
												} ?: error(
													"define type must be an identifier, not ${
														asType::class
													}; ${Sourcifier().run {list.sourcify}}"
												)

												kfun(name) {
													valueParameters(args)
													type(funType)
													bodyExp(sexp(exp[0]))
												}
											}
										}
								}
						}
						?: "_${dummyCounter++}".id.variableDecl.property(sexp(it))
					}
					.toList()
					.let {
						KKotlinFile(topLevelObjects=it)
					}
			}
		}

	val rSexp=
		rule<S_expressionContext,KExpression>("sexp") {
			from(S_expressionContext::class)
			howCtx {
				atomic_symbol()?.let {
					lookup<Atomic_symbolContext,KPrimaryExpression>()!!(it)
				} ?: list()?.let {
					lookup<ListContext,KExpression>()!!(it)
				} ?: error("unknown sexp")
			}
		}

	val rAtomic=
		rule<Atomic_symbolContext,KPrimaryExpression>("atomic_symbol") {
			from(Atomic_symbolContext::class)
			howCtx {
				//+*/=<>%-
				Id()?.text?.let {
					if(it.contains(Regex("[+*/=<>%-]"))) "`$it`"
					else it
				}?.id
				?: Int().text.toInt().ast
			}
		}

	val rList=
		rule<ListContext,KExpression>("list") {
			from(ListContext::class)
			howCtx {
				val (tail,head)=s_expression().split()!!
				head.atomic_symbol()
					?.let(rAtomic::invoke)
					?.let {expr->
						when(expr)
						{
							is KSimpleIdentifier->
								when(expr.id)
								{
									"let"->
									{
										assert(tail.size==3) {
											"let binding must have 4 parts: `let`, id, expr, expr)"
										}
										val id=tail[0]?.atomic_symbol()
										checkNotNull(id) {
											"id in let binding must be an identifier"
										}
										val what=rSexp(tail[1])
										val where=rSexp(tail[2])
										what["let".id] suffix
												KCallSuffix(
													id.text
														.id
														.variableDecl
														.nel()
														.literal(where.stat)
														.valueArg
														.list
														.let(::KValueArguments)
												)
									}

									"lambda"->
									{
										//(lambda n int (* n n))
										assert(tail.size>2) {
											"lambda must have at least 3 parts:"+
											"[!0] `lambda`"+
											"[!1:n-1] arg:(id type)+"+
											"[!n] expr"
										}
										val args=tail.dropLast(1)
											.chunked(2) {
												rSexp(it[0]) to rSexp(it[1])
											}.filterIsInstance<Pair<KSimpleIdentifier,KSimpleIdentifier>>()
										check(args.size==tail.dropLast(1).chunked(2).size) {
											"""
											lambda arguments (except last) must be pairs id type,
											where id is an identifier and type is an identifier
											""".trimIndent()
										}


										val body=rSexp(tail.last())

										args.map {(id,type)->
											type.id
												.replaceFirstChar {it.uppercase()}
												.type
												.let {id variableDecl it}
										}.let {KLambdaLiteral(it,body.stat.list)}
									}

									//+*/=<>%-

									"`+`","`*`","`/`","`=`","`<`","`>`","`%`","`-`"->
									{
										val op=expr.id.removeSurrounding("`")
										assert(tail.size>2) {
											"operator $op must have exactly 2 arguments: `$op`, left, right"
										}
										val l=rSexp(tail[0])
										val r=rSexp(tail[1])
										when(op)
										{
											"+"->l+r
											"-"->l-r
											"*"->l*r
											"/"->l/r
											"%"->l%r
											"="->l eqEq r
											"<"->l less r
											">"->l greater r
											else->error("unknown operator $op")
										}
									}

									else->tail.map(rSexp)
										.map(KExpression::valueArg)
										.toNonEmptyListOrNull()!!
										.let(expr::call)
								}

							else->TODO()
						}
					} ?: error("unknown list")
			}
		}
}