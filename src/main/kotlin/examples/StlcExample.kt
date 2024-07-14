package org.fancryer.bf.examples

import ast.rules
import org.fancryer.bf.*
import org.fancryer.bf.ast.*
import stlc.gen.StlcParser.*

val stlcRuleHolder=rules {
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
		rule<VariableContext,KSimpleIdentifier>("variable") {
			from(VariableContext::class)
			how {ctx:VariableContext-> rX(ctx.x())}
		}

	val rTrue=
		rule<Constant_trueContext,EBooleanLiteral>("true") {
			from(Constant_trueContext::class)
			how {_:Constant_trueContext-> true.ast}
		}

	val rFalse=
		rule<Constant_falseContext,EBooleanLiteral>("false") {
			from(Constant_falseContext::class)
			how {_:Constant_falseContext-> false.ast}
		}

	val rParenthesis=
		rule<ParenthesisContext,KParenthesizedExpression>("parenthesis") {
			from(ParenthesisContext::class)
			how {ctx:ParenthesisContext->
				val tToExp=lookup<TContext,KExpression>()
						   ?: error("ASTra doesn't know how to get KExpression from Parenthesis")
				tToExp(ctx.t()).paren
			}
		}

	val rConditional=
		rule<ConditionalContext,KIfExpression>("conditional") {
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
		rule<TContext,KExpression>("t") {
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
			how {ctx:ApplicationContext->
				val exp=ctx.t(1)
				rT(ctx.t(0)).paren.call(rT(exp).valueArg.args)
			}
		}

	val rType=
		rule<TypeContext,KType>("type") {
			from(TypeContext::class)
			howFixed {getType:(TypeContext)->KType->
				{ctx:TypeContext->
					val typeNameToKType={name:String->
						name.id.simpleUserType.userType.type
					}
					when(ctx)
					{
						is Flat_typeContext->
							when(val typeName=ctx.ID().text)
							{
								"Bool"->typeNameToKType("Boolean")
								else->typeNameToKType(typeName)
							}

						is Abstraction_typeContext->
							typeNameToKType(ctx.ID().text).functionTypeParameters
								.leadsTo(getType(ctx.type()))
								.type

						else->error("ASTra doesn't know how to get other KType from ${ctx::class.simpleName}")
					}
				}
			}
		}

	val rAbstraction=
		rule<AbstractionContext,KLambdaLiteral>("abstraction") {
			from(AbstractionContext::class)
			how {ctx:AbstractionContext->
				val id=rX.how(ctx.x())
				val type=rType.how(ctx.type())
				(id variableDecl type).lambdaParams literal rT.how(ctx.t()).stat
			}
		}
}