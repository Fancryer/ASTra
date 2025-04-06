package examples

import arrow.core.nel
import ast.*
import org.fancryer.bf.*
import stlc.gen.StlcLexer
import stlc.gen.StlcParser
import stlc.gen.StlcParser.*


inline infix fun <reified T:Any,reified R:Any> T.castOr(r:(T)->R):R=
	if(this is R) this else r(this)

val stlcRuleHolder=rules<StlcLexer,StlcParser> {
	default {
		println("default for ${it::class.simpleName}")
		"TODO".id.call()
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
						   ?: error("ASTra doesn't know how to get KExpression from Parenthesis")
				tToExp(ctx.t()).paren
			}
		}

	val rConditional=
		rule("conditional") {
			from(ConditionalContext::class)
			how {ctx:ConditionalContext->
				val tToExp=lookup<TContext,KExpression>()
						   ?: error("ASTra doesn't know how to get KExpression from T")
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
						?: error("ASTra doesn't know how to get KLambdaLiteral from Abstraction")

					is ApplicationContext->
						lookup<ApplicationContext,KExpression>()?.invoke(ctx)
						?: error("ASTra doesn't know how to get KExpression from ApplicationContext")

					is Constant_trueContext->rTrue(ctx)
					is Constant_falseContext->rFalse(ctx)
					is ConditionalContext->rConditional(ctx)
					is ParenthesisContext->rParenthesis(ctx)
					else->error("ASTra doesn't know how to get KExpression from ${ctx::class.simpleName}")
				}
			}
		}

		val rApplication=
			rule<ApplicationContext,KExpression>("application") {
				from(ApplicationContext::class)
				how {ctx->
					val exp=ctx.t(1)
					ctx.t(0).let(rT).primary call rT(exp).valueArg.nel()
				}
			}

		val rType=
			rule("type") {
				from(TypeContext::class)
				howFixed {rType->
					{ctx:TypeContext->
						val boolify={name:String->
							if(name=="Bool") "Boolean" else name
						}
						when(ctx)
						{
							is Flat_typeContext->boolify(ctx.ID().text).type

							is Abstraction_typeContext->
								boolify(ctx.ID().text).type
									.functionTypeParameters
									.leadsTo(rType(ctx.type()))
									.type

							else->error("ASTra doesn't know how to get other KType from ${ctx::class.simpleName}")
						}
					}
				}
			}

		val rAbstraction=
			rule<AbstractionContext,KLambdaLiteral>("abstraction") {
				from(AbstractionContext::class)
				how {ctx->
					rX(ctx.x()).variableDecl(rType(ctx.type()))
						.lambdaParams
						.literal(rT(ctx.t()).stat)
				}
			}
	}