package org.fancryer.bf

import arrow.core.*
import ast.RuleHolder
import ast.RuleVisitor
import ast.rules
import gen.MiniJavaGrammarLexer
import gen.MiniJavaGrammarParser
import org.antlr.v4.runtime.*
import org.fancryer.bf.ast.*
import org.fancryer.bf.examples.stlcRuleHolder
import stlc.gen.StlcLexer
import stlc.gen.StlcParser
import stlc.gen.StlcParser.*

fun call(expr:KPrimaryExpression,suffix:KPostfixUnarySuffix)=
	KPostfixUnaryExpression(expr,listOf(suffix))

fun main()
{
}

fun <T,R> fix(f:((T)->R)->(T)->R):(T)->R=
	{x-> f(fix(f))(x)}

inline val KIdentifierInner.full get()=KIdentifier(this.nonEmptyList)

val Boolean.ast get()=if(this) EBooleanLiteral.True else EBooleanLiteral.False

val KExpression.paren get()=KParenthesizedExpression(this)

val KExpression.valueArg get()=KValueArgument(none(),none(),false,this)
val KValueArgument.args get()=KValueArguments(nonEmptyList.some())

val KStatement.lambda get()=KLambdaLiteral(none(),nonEmptyList.some())

val KStatementInner.stat get()=KStatement(emptyList(),this)
val KStatementInner.block get()=stat.block

infix fun <T> List<T>.len(n:Int)=size==n

inline infix fun <T,R> T.pipe(f:(T)->R)=let(f)

fun <T,R,C> T.pipeWith(g:C,f:(Pair<T,C>)->R)=f(this to g)

fun <P:Parser,L:Lexer> getParser(src:String,p:(TokenStream)->P,l:(CharStream)->L)=
	CharStreams.fromString(src) pipe l pipe ::CommonTokenStream pipe p

fun getParser(src:String)=getParser(src,::MiniJavaGrammarParser,::MiniJavaGrammarLexer)

val KIdentifier.packageHeader get()=KPackageHeader(this.some())

fun <T> T.list()=listOf(this)
val <T> T.nonEmptyList get()=nonEmptyListOf(this)

val Int.ast get()=KIntegerLiteral(this)

fun KExpression.add(other:KExpression):KExpression=KAdditiveExpression(this,KAdd,other)
fun KExpression.sub(other:KExpression):KExpression=KAdditiveExpression(this,KSub,other)

val KExpression.strExpr:KLineStringExpression
	get()=KLineStringExpression(this)

fun KPrimaryExpression.call(suffix:KCallSuffixInner,args:(Option<KTypeArguments>)=none())=
	KPostfixUnaryExpression(
		this,
		KCallSuffix(args,suffix).list()
	)

fun KPrimaryExpression.call(
	valueArgs:NonEmptyList<KValueArgument>,
	args:(Option<KTypeArguments>)=none()
)=
	KPostfixUnaryExpression(
		this,
		KCallSuffix(args,KValueArguments(valueArgs.some())).list()
	)

fun KPrimaryExpression.getProp(prop:KNavigationSuffixInner,op:KMemberAccessOperator=KDot)=
	KPostfixUnaryExpression(
		this,
		KNavigationSuffix(op,prop).list()
	)

val KDeclaration.topLevel get()=KTopLevelObject(this)

val String.id get()=KIdentifierInner(this)

val KIdentifierInner.simpleUserType get()=KSimpleUserType(this,none())
fun KIdentifierInner.simpleGeneric(projection:org.fancryer.bf.ast.KTypeProjection)=
	KSimpleUserType(
		this,
		KTypeArguments(projection.nonEmptyList).some()
	)

val KSimpleUserType.userType get()=KUserType(this.nonEmptyList)

val KSimpleUserType.anno get()=anno(KUserType(this.nonEmptyList))

val KConcreteTypeProjection.args get()=KTypeArguments(this.nonEmptyList)

val KTypeInner.type get()=KType(None,this)

val KType.concreteTypeProjection get()=KConcreteTypeProjection(none(),this)
fun KType.concreteTypeProjection(modifiers:KTypeProjectionModifiers)=
	KConcreteTypeProjection(modifiers.some(),this)

fun KSimpleIdentifier.functionValueParameter(type:KType)=
	KFunctionValueParameter(none(),KParameter(this,type),none())