package org.fancryer.bf.examples

import ast.rules
import org.fancryer.bf.*
import org.fancryer.bf.ast.*
import stlc.gen.StlcLexer
import stlc.gen.StlcParser
import stlc.gen.StlcParser.*


inline infix fun <reified T:Any,reified R:Any> T.castOr(r:(T)->R):R=
	if(this is R) this else r(this)

val stlcRuleHolder=rules<StlcLexer,StlcParser> {
	default {
		println("default for ${it::class.simpleName}")
		"TODO".id.call(KLineStringLiteral(emptyList()).valueArg.args)
	}

	val rX=
		rule<XContext,KSimpleIdentifier>("x") {
			from(XContext::class)
			how {ctx:XContext-> ctx.ID().text.id}
		}

	val rVariable=
		rule("variable") {
			from(VariableContext::class)
			how {ctx:VariableContext-> rX(ctx.x())}
		}

	val rTrue=
		rule("true") {
			from(Constant_trueContext::class)
			how {true.ast}
		}

	val rFalse=
		rule("false") {
			from(Constant_falseContext::class)
			how {false.ast}
		}

	val rParenthesis=
		rule("parenthesis") {
			from(ParenthesisContext::class)
			how {ctx:ParenthesisContext->
				val tToExp=lookup<TContext,KExpression>()
						   ?: "ASTra doesn't know how to get KExpression from Parenthesis".err
				tToExp(ctx.t()).paren
			}
		}

	val rConditional=
		rule("conditional") {
			from(ConditionalContext::class)
			how {ctx:ConditionalContext->
				val tToExp=lookup<TContext,KExpression>()
						   ?: "ASTra doesn't know how to get KExpression from T".err
				tToExp(ctx.pred).ifElse(
					tToExp(ctx.if_true).block,
					tToExp(ctx.if_false).block
				)
			}
		}

	val rT=
		rule("t") {
			from(TContext::class)
			how {ctx:TContext->
				when(ctx)
				{
					is VariableContext->rVariable(ctx)
					is AbstractionContext->
						lookup<AbstractionContext,KFunctionLiteral>()?.invoke(ctx)
						?: "ASTra doesn't know how to get KLambdaLiteral from Abstraction".err

					is ApplicationContext->
						lookup<ApplicationContext,KExpression>()?.invoke(ctx)
						?: "ASTra doesn't know how to get KExpression from ApplicationContext".err

					is Constant_trueContext->rTrue(ctx)
					is Constant_falseContext->rFalse(ctx)
					is ConditionalContext->rConditional(ctx)
					is ParenthesisContext->rParenthesis(ctx)
					else->"ASTra doesn't know how to get KExpression from ${ctx::class.simpleName}".err
				}
			}
		}

	val rApplication=
		rule<ApplicationContext,KExpression>("application") {
			from(ApplicationContext::class)
			howCtx {
				val exp=t(1)
				t(0).let(rT)
					.castOr<KExpression,KPrimaryExpression> {it.paren}
					.call(rT(exp).valueArg.args)
			}
		}

	val rType=
		rule("type") {
			from(TypeContext::class)
			howFixed {rType->
				{ctx:TypeContext->
					val typeNameToKType={name:String->
						name.id.simpleUserType.userType.type
					}
					val boolify={name:String->
						if(name=="Bool") "Boolean" else name
					}
					when(ctx)
					{
						is Flat_typeContext->typeNameToKType(boolify(ctx.ID().text))

						is Abstraction_typeContext->
							typeNameToKType(boolify(ctx.ID().text)).functionTypeParameters
								.leadsTo(rType(ctx.type()))
								.type

						else->"ASTra doesn't know how to get other KType from ${ctx::class.simpleName}".err
					}
				}
			}
		}

	val rAbstraction=
		rule<AbstractionContext,KLambdaLiteral>("abstraction") {
			from(AbstractionContext::class)
			howCtx {
				val id=rX(x())
				val type=rType(type())
				id.variableDecl(type).lambdaParams.literal(rT(t()).stat)
			}
		}
}