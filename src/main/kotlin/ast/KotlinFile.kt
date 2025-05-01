package ast

import arrow.core.*
import ast.KAdditiveOperator.Add
import ast.KAdditiveOperator.Sub
import ast.KBlockBuilder.Companion.kblock
import ast.KFieldIdentifier.Companion.field
import org.fancryer.bf.*

data class KTypeParameter(
	val modifiers:Option<KTypeParameterModifiers>,
	val identifier:KSimpleIdentifier,
	val type:Option<KType>
):KotlinAst

data class KTypeConstraint(
	val annotations:List<KAnnotation>,
	val identifier:KSimpleIdentifier,
	val type:KType
):KotlinAst

sealed interface KClassMemberDeclaration:KotlinAst

data class KCompanionObject(
	val modifiers:List<KModifiersInner>,
	val isData:Boolean,
	val identifier:Option<KSimpleIdentifier>,
	val delegationSpecifiers:Option<KDelegationSpecifiers>,
	val classBody:Option<KClassBody>
):KClassMemberDeclaration

data class KAnonymousInitializer(val block:KBlock):KClassMemberDeclaration

data class KFunctionValueParameter(
	val modifiers:(Option<KParameterModifiers>)=None,
	val parameter:KParameter,
	val expression:(Option<KExpression>)=None
):KotlinAst

data class KSecondaryConstructor(
	val modifiers:(List<KModifiersInner>)=emptyList(),
	val functionValueParameters:List<KFunctionValueParameter>,
	val constructorDelegationCall:(Option<KConstructorDelegationCall>)=None,
	val block:KBlock
):KClassMemberDeclaration

data class KObjectDeclaration(
	val modifiers:(List<KModifiersInner>)=emptyList(),
	val identifier:KSimpleIdentifier,
	val delegationSpecifiers:(Option<KDelegationSpecifiers>)=None,
	val classBody:(Option<KClassBody>)=None
):KDeclaration

val String.type
	get()=this.id
		.simpleUserType
		.userType
		.type

fun String.returns(projection:KTypeProjection)=this.id
	.simpleGeneric(projection)
	.userType
	.type

fun String.type(how:(String)->String)=how(this).type

fun KSimpleIdentifier.type(how:String.()->String)=how(id).type
val KSimpleIdentifier.type get()=id.type

@DslMarker
annotation class ClassBuilderDsl

sealed interface KFunctionBody:KotlinAst

data class KFunctionAssignExpression(val expression:KExpression):KFunctionBody

data class KVariableDeclaration(
	val annotations:(List<KAnnotation>)=emptyList(),
	val identifier:KSimpleIdentifier,
	val type:(Option<KType>)=None
):KMultiOrSingleVariableDeclaration,KLambdaParameter

val KVariableDeclaration.lambdaParams:List<KLambdaParameter> get()=nel()

data class KMultiVariableDeclaration(
	val variableDeclarations:NonEmptyList<KVariableDeclaration>
):KMultiOrSingleVariableDeclaration

sealed interface KMultiOrSingleVariableDeclaration:KotlinAst


data class KGetter(
	val modifiers:List<KModifiersInner>,
	val getterBody:Option<KGetterBody>
):KotlinAst

data class KGetterBody(
	val type:Option<KType>,
	val functionBody:KFunctionBody
):KotlinAst

data class KSetter(
	val modifiers:List<KModifiersInner>,
	val setterBody:Option<KSetterBody>
):KotlinAst

data class KSetterBody(
	val param:KFunctionValueParameterWithOptionalType,
	val type:Option<KType>,
	val functionBody:KFunctionBody
):KotlinAst

data class KParametersWithOptionalType(
	val params:List<KFunctionValueParameterWithOptionalType>
):KotlinAst

data class KFunctionValueParameterWithOptionalType(
	val modifiers:(Option<KParameterModifiers>)=None,
	val parameter:KParameterWithOptionalType,
	val expression:(Option<KExpression>)=None
):KotlinAst

data class KParameterWithOptionalType(
	val identifier:KSimpleIdentifier,
	val type:(Option<KType>)=None
):KotlinAst

data class KParameter(
	val identifier:KSimpleIdentifier,
	val type:KType
):KFunctionTypeParameterInner

infix fun KSimpleIdentifier.param(type:KType)=KParameter(this,type)

data class KConstructorDelegationCall(
	val delegationQualifier:EConstructorDelegationQualifier,
	val valueArguments:Option<KValueArguments>
):KotlinAst
{
	enum class EConstructorDelegationQualifier
	{
		THIS,
		SUPER
	}
}

data class KEnumEntries(
	val entries:NonEmptyList<KEnumEntry>
):KotlinAst

class KEnumEntriesBuilder
{
	private val entries=mutableListOf<KEnumEntry>()

	fun entry(entry:KEnumEntry)
	{
		entries+=entry
	}

	fun entries(entries:List<KEnumEntry>)
	{
		this.entries+=entries
	}

	private fun build()=
		KEnumEntries(entries.toNonEmptyListOrNull() ?: error("No enum entries"))

	companion object
	{
		fun kenumEntries(init:KEnumEntriesBuilder.()->Unit)=
			KEnumEntriesBuilder().apply(init).build()
	}
}

data class KEnumEntry(
	val modifiers:List<KModifiersInner>,
	val identifier:KSimpleIdentifier,
	val valueArguments:Option<KValueArguments>,
	val classBody:Option<KClassBody>
):KotlinAst

data class KType(
	val modifiers:(List<KTypeModifier>)=emptyList(),
	val typeInner:KTypeInner
):KFunctionTypeParameterInner

val KType.functionTypeParameters:(NonEmptyList<KFunctionTypeParameterInner>)
	get()=nel()

sealed interface KTypeInner:KotlinAst

//rtype: typeModifier+? typeinner=rdefinitelyNonNullableType;

sealed class KTypeReference(questions:Int):KNullableType(questions),KRecieverTypeInner,KTypeInner
{
	data object Dynamic:KTypeReference(0)
}

sealed class KNullableType(val questions:Int):KTypeInner,KRecieverTypeInner

data class KSimpleUserType(
	val identifier:KSimpleIdentifier,
	val typeArguments:(Option<KTypeArguments>)=None
):KotlinAst

sealed interface KTypeProjection:KotlinAst
data class KConcreteTypeProjection(
	val type:KType,
	val modifiers:(List<KTypeProjectionModifier>)=emptyList()
):KTypeProjection

val KType.proj get()=KConcreteTypeProjection(this)

val NonEmptyList<KTypeProjection>.typeArgs get()=KTypeArguments(this)

data object KStarTypeProjection:KTypeProjection

sealed interface KTypeProjectionModifier:KotlinAst

data class KFunctionType(
	val recieverType:(Option<KReceiverType>)=None,
	val parameters:NonEmptyList<KFunctionTypeParameterInner>,
	val returnType:KType
):KTypeInner

sealed interface KFunctionTypeParameterInner:KotlinAst

class KParenthesizedType(val type:KType,questions:Int):KNullableType(questions),KTypeInner,KRecieverTypeInner

sealed interface KRecieverTypeInner:KotlinAst

data class KReceiverType(
	val modifiers:List<KTypeModifier>,
	val type:KRecieverTypeInner
):KotlinAst

sealed interface KParenthesizedOrFlatUserType:KotlinAst

data class KParenthesizedUserType(val type:KParenthesizedOrFlatUserType):KParenthesizedOrFlatUserType

//TODO rename fields
data class KDefinitelyNonNullableType(
	val leftModifiers:List<KTypeModifier>,
	val left:KParenthesizedOrFlatUserType,
	val rightModifiers:List<KTypeModifier>,
	val right:KParenthesizedOrFlatUserType
):KTypeInner


sealed interface KLabelOrAnnotation:KotlinAst

sealed interface KStatementInner:KotlinAst

data class KLabel(val identifier:KSimpleIdentifier):KLabelOrAnnotation,KUnaryPrefix

sealed interface KControlStructureBody:KIfInner,KControlStructureBodyOrSemicolon

data class KBlock(val statements:(List<KStatement>)=emptyList()):KControlStructureBody,KFunctionBody,KStatementInner
class KBlockBuilder
{
	private var statements:(List<KStatement>)=emptyList()

	infix fun KSimpleIdentifier.declare(expression:KExpression)=KPropertyDeclaration(
		declaration=KVariableDeclaration(identifier=this),
		expr=expression
	)

	infix fun String.declare(expression:KExpression)=id declare expression

	fun stat(statement:KStatementInner)
	{
		+KStatement(emptyList(),statement)
	}

	fun stat(statement:KStatement)=apply {
		+statement
	}

	fun stats(stats:List<KStatement>)=apply {
		statements+=stats
	}

	operator fun List<KStatement>.unaryPlus()
	{
		stats(this)
	}

	operator fun KStatement.unaryPlus()
	{
		statements+=this
	}

	operator fun KStatementInner.unaryPlus()
	{
		stat(this)
	}

	private fun build()=KBlock(statements)

	companion object
	{
		fun kblock(init:KBlockBuilder.()->Unit)=
			KBlockBuilder().also(init).build()
	}
}

sealed interface KLoopStatement:KStatementInner

data class KForStatement(
	val annos:List<KAnnotation>,
	val declaration:KMultiOrSingleVariableDeclaration,
	val expression:KExpression,
	val controlStructureBody:Option<KControlStructureBody>
):KLoopStatement

data class KWhileStatement(
	val expression:KExpression,
	val body:KControlStructureBodyOrSemicolon
):KLoopStatement
{
	companion object
	{
		infix fun KExpression.kwhile(body:KControlStructureBody)=
			KWhileStatement(this,body)

		infix fun KExpression.kwhile(init:KBlockBuilder.()->Unit)=
			KWhileStatement(this,kblock(init))
	}
}

data class KDoWhileStatement(
	val controlStructureBody:Option<KControlStructureBody>,
	val expression:KExpression
):KLoopStatement

sealed interface KAssignmentLeft:KStatementInner

data class KAssignment(val left:KAssignmentLeft,val right:KExpression):KStatementInner
{
	companion object
	{
		fun KIdentifierInner.subAssign(exp:KExpression):KAssignment=KAssignment(
			KAssignableWithAndExpression(
				postfix.prefix,
				EAssignmentAndOperator.SubAssignment
			),
			exp
		)
	}
}

data class KAssignableWithAndExpression(
	val left:KAssignableExpression,
	val right:EAssignmentAndOperator
):KAssignmentLeft

sealed interface KExpression:KStatementInner,KWhenCondition

val KExpression.postfix:KPostfixUnaryExpression get()=KPostfixUnaryExpression(this.primary)
/*
expression
    : unaryPrefix* postfixUnaryExpression #prefixUnary
    | expression NL* asOperator NL* type #as
    | expression multiplicativeOperator NL* expression #muldivmod
    | expression additiveOperator NL* expression #addsub
    | expression (RANGE | RANGE_UNTIL) NL* expression #range
    | expression simpleIdentifier NL* expression #infix
    | expression NL* elvis NL* expression #elvis_e
    | expression (inOperator NL* expression | isOperator NL* type) #in_is
    | expression callSuffix #call
    | expression comparisonOperator NL* expression #comparison
    | expression equalityOperator NL* expression #equal
    | expression NL* CONJ NL* expression #and
    | expression NL* DISJ NL* expression #or
    ;
*/

/**
 * ||
 * */
data class KDisjunction(val left:KExpression,val right:KExpression):KExpression

/**
 * &&
 * */
data class KConjunction(val left:KExpression,val right:KExpression):KExpression

infix fun KExpression.and(expression:KExpression)=KConjunction(this,expression)
infix fun KExpression.or(expression:KExpression)=KDisjunction(this,expression)

data class KEquality(
	val left:KExpression,
	val op:EEqualityOperator,
	val right:KExpression
):KExpression

class SepSeq<E,S>(private val first:E,private val rest:(List<Pair<S,E>>)=listOf())
{

	/**
	 * Добавляет элемент и разделитель в конец последовательности.
	 */
	fun append(separator:S,element:E):SepSeq<E,S>
	{
		val updatedRest=rest.toMutableList()
		updatedRest.add(Pair(separator,element))
		return SepSeq(first,updatedRest)
	}

	/**
	 * Возвращает строковое представление последовательности.
	 */
	override fun toString():String
	{
		val sb=StringBuilder()
		sb.append(first)
		rest.forEach {(separator,element)->
			sb.append(separator).append(element)
		}
		return sb.toString()
	}

	// Дополнительные методы, такие как size, isEmpty и т.д., могут быть добавлены по необходимости.
}

data class KComparison(
	val left:KExpression,
	val op:EComparisonOperator,
	val right:KExpression
):KExpression

data class KGenericCallLikeComparison(val left:KExpression,val right:KCallSuffix):KotlinAst

data class KInfixOperation(val left:KExpression,val right:KInfixOperationInner):KotlinAst

sealed interface KInfixOperationInner:KotlinAst
data class KInElvis(val left:EInOperator,val right:KExpression):KInfixOperationInner

data class KElvisExpression(val left:KExpression,val right:KExpression):KotlinAst

data class KInfixFunctionCall(val left:KExpression,val op:KSimpleIdentifier,val right:KExpression):KotlinAst

enum class ERangeOperator:KotlinAst
{
	Range, RangeUntil
}

data class KRangeExpression(val left:KExpression,val op:ERangeOperator,val right:KExpression):KExpression

data class KBinaryExpression(
	val left:KExpression,
	val op:KBinaryOperator,
	val right:KExpression
):KExpression

data class KAsExpression(val left:KExpression,val op:EAsOperator,val type:KType):KExpression

data class KPrefixUnaryExpression(
	val expression:KPostfixUnaryExpression,
	val prefixes:(List<KUnaryPrefix>)=emptyList()
):KAssignableExpression

val KPostfixUnaryExpression.prefix get()=KPrefixUnaryExpression(this)

sealed interface KUnaryPrefix:KotlinAst

data class KPostfixUnaryExpression(
	val primaryExpression:KPrimaryExpression,
	val suffixes:(List<KPostfixUnarySuffix>)=emptyList()
):KExpression
{
	companion object
	{
		infix fun KPrimaryExpression.nav(suffix:KNavigationSuffix):KPostfixUnaryExpression=
			KPostfixUnaryExpression(this,suffix.list)

		infix fun KPostfixUnaryExpression.suffix(suffix:KPostfixUnarySuffix):KPostfixUnaryExpression=
			KPostfixUnaryExpression(primaryExpression,suffixes+suffix)

		infix fun KPrimaryExpression.index(suffix:KIndexingSuffix):KPostfixUnaryExpression=
			KPostfixUnaryExpression(this,suffix.list)

		infix fun KPrimaryExpression.index(expression:KExpression):KPostfixUnaryExpression=
			KPostfixUnaryExpression(this,KIndexingSuffix(expression.nel()).list)
	}
}

sealed interface KPostfixUnarySuffix:KotlinAst

sealed interface KDirectlyAssignableExpression:KAssignmentLeft,KExpression

data class KPostfixUnaryExpressionWithAssignableSuffix(
	val primaryExpression:KPrimaryExpression,
	val assignableSuffix:KAssignableSuffix
):KDirectlyAssignableExpression

data class KParenthesizedDirectlyAssignableExpression(
	val directlyAssignableExpression:KDirectlyAssignableExpression
):KDirectlyAssignableExpression,KAssignableExpression

sealed interface KAssignableExpression:KExpression

sealed interface KAssignableSuffix:KotlinAst

data class KIndexingSuffix(val expressionList:NonEmptyList<KExpression>):KAssignableSuffix,KPostfixUnarySuffix

data class KNavigationSuffix(
	val suffix:KNavigationSuffixInner,
	val memberAccessOperator:KMemberAccessOperator=KMemberAccessOperator.Dot
):KAssignableSuffix,KPostfixUnarySuffix

sealed interface KNavigationSuffixInner:KotlinAst

data object KClassKeyword:KNavigationSuffixInner,KClassHeaderType,KCallableReferenceInner

data object KFunInterfaceKeyword:KClassHeaderType
data object KInterfaceKeyword:KClassHeaderType

data class KParenthesizedExpression(val expression:KExpression):KPrimaryExpression,KNavigationSuffixInner

data class KCallSuffix(
	val callSuffixInner:KCallSuffixInner,
	val args:(Option<KTypeArguments>)=None
):KPostfixUnarySuffix

sealed interface KCallSuffixInner:KotlinAst

data class KAnnotatedLambdaWithValueArguments(
	val valueArguments:Option<KValueArguments>,
	val lambda:KAnnotatedLambda
):KCallSuffixInner

data class KValueArguments(val args:(List<KValueArgument>)=emptyList()):KCallSuffixInner

data class KAnnotatedLambda(val annos:List<KAnnotation>,val label:Option<KLabel>,val literal:KLambdaLiteral):KotlinAst
data class KTypeArguments(val projections:NonEmptyList<KTypeProjection>):KAssignableSuffix,KPostfixUnarySuffix

data class KValueArgument(
	val anno:(Option<KAnnotation>)=None,
	val identifier:(Option<KSimpleIdentifier>)=None,
	val hasSpread:Boolean=false,
	val expression:KExpression
):KotlinAst

sealed interface KPrimaryExpression:KExpression

data class KCollectionLiteral(val exprs:List<KExpression>):KPrimaryExpression

sealed interface KLiteralConstant:KPrimaryExpression

enum class EBooleanLiteral:KLiteralConstant
{
	True, False
}

data class KIntegerLiteral(val value:Int):KLiteralConstant,KIntegerHexBin

data class KHexLiteral(val value:Int):KLiteralConstant,KIntegerHexBin
data class KBinLiteral(val value:Int):KLiteralConstant,KIntegerHexBin
data class KCharacterLiteral(val value:Char):KLiteralConstant
sealed interface KRealLiteral:KLiteralConstant
data class KFloatLiteral(val value:Float):KRealLiteral
data class KDoubleLiteral(val value:Double):KRealLiteral
data object KNullLiteral:KLiteralConstant

data class KLongLiteral(val value:KIntegerHexBin):KLiteralConstant
data class KUnsignedLiteral(val value:KIntegerHexBin):KLiteralConstant

sealed interface KIntegerHexBin:KotlinAst

sealed interface KStringLiteral:KPrimaryExpression
sealed interface KLineStringContentOrExpression:KotlinAst

class LineStringLiteralBuilder
{
	private var content:(List<KLineStringContentOrExpression>)=emptyList()

	fun ref(field:KFieldIdentifier)
	{
		content+=KLineStrRef(field)
	}

	fun ref(field:KSimpleIdentifier)
	{
		ref(field.field)
	}

	fun ref(field:String)
	{
		ref(field.id)
	}

	val KFieldIdentifier.ref get()=ref(this)
	val KSimpleIdentifier.ref get()=ref(this)
	val String.ref get()=ref(this)

	fun text(str:String)
	{
		content+=KLineStrText(str)
	}

	val String.text get()=text(this)

	fun expr(expr:KExpression)
	{
		content+=KLineStringExpression(expr)
	}

	val KExpression.expr get()=expr(this)

	private fun build():KLineStringLiteral=KLineStringLiteral(content)

	companion object
	{
		fun stringLiteral(init:LineStringLiteralBuilder.()->Unit)=
			LineStringLiteralBuilder().apply(init).build()
	}
}

data class KLineStringLiteral(
	val content:(List<KLineStringContentOrExpression>)=emptyList()
):KStringLiteral
{
	constructor(str:String):this(KLineStrText(str).list)
	constructor(str:KLineStrText):this(str.list)
}

val String.ast get()=KLineStringLiteral(this)

data class KMultiLineStringLiteral(val content:List<KMultiLineStringLiteralInner>):KStringLiteral

sealed interface KMultiLineStringLiteralInner:KotlinAst

sealed interface KLineStringContent:KLineStringContentOrExpression

/*
Check with:
	~('\\' | '"' | '$')+ | '$'
*/
data class KLineStrText(val text:String):KLineStringContent

sealed interface KLineStrEscapedChar:KLineStringContent

/*
Check with:
	'\\' ('t' | 'b' | 'r' | 'n' | '\'' | '"' | '\\' | '$')
*/
data class KEscapedIdentifier(val id:String):KLineStrEscapedChar

/*
Check with:
	'\\' 'u' HexDigit{4}
*/
data class KUniCharacterLiteral(val char:String):KLineStrEscapedChar

data class KLineStrRef(val ref:KFieldIdentifier):KLineStringContent

data class KLineStringExpression(val expression:KExpression):KLineStringContentOrExpression

sealed interface KMultiLineStringContent:KMultiLineStringLiteralInner

/*
Check with:
	~('"' | '$')+ | '$'
*/
data class KMultiLineStrText(val text:String):KMultiLineStringContent

// '"'+
data class KMultiLineStringQuote(val quoteAmount:Int):KMultiLineStringContent,KMultiLineStringLiteralInner
data class KMultiLineStrRef(val ref:KFieldIdentifier):KMultiLineStringContent

data class KMultiLineStringExpression(val expression:KExpression):KMultiLineStringLiteralInner

data class KLambdaLiteral(
	val params:(List<KLambdaParameter>)=emptyList(),
	val stats:(List<KStatement>)=emptyList()
):KFunctionLiteral

infix fun List<KLambdaParameter>.literal(stats:NonEmptyList<KStatement>):KLambdaLiteral=
	KLambdaLiteral(this,stats)

infix fun List<KLambdaParameter>.literal(stat:KStatement):KLambdaLiteral=
	literal(stat.nel())

val List<KLambdaParameter>.literal:KLambdaLiteral
	get()=KLambdaLiteral(this,emptyList())

sealed interface KLambdaParameter:KotlinAst

data class KMultiVariableDeclarationWithType(
	val multiVariableDeclaration:KMultiVariableDeclaration,
	val type:Option<KType>
):KLambdaParameter

data class KAnonymousFunction(
	val isSuspend:Boolean=false,
	val recieverType:(Option<KType>)=None,
	val params:KParametersWithOptionalType,
	val toType:(Option<KType>)=None,
	val constraints:(List<KTypeConstraint>)=emptyList(),
	val body:(Option<KFunctionBody>)=None
):KFunctionLiteral

sealed interface KThisExpression:KPrimaryExpression

data object KThis:KThisExpression
data class KThisAt(val id:KIdentifier):KThisExpression

sealed interface KFunctionLiteral:KPrimaryExpression

sealed interface KSuperExpression:KPrimaryExpression
data class KTypeWithSimpleIdentifier(
	val type:Option<KType>,
	val id:Option<KSimpleIdentifier>
):KSuperExpression

data class KSuperAt(val id:KIdentifier):KSuperExpression

sealed interface KIfInner:KotlinAst

data object KSemicolon:KIfInner,KControlStructureBodyOrSemicolon

sealed interface KControlStructureBodyOrSemicolon:KotlinAst

data class KWhenSubject(
	val decl:(Option<KVariableDeclarationWithAnnotations>)=None,
	val expression:KExpression
):KotlinAst

data class KVariableDeclarationWithAnnotations(val annos:List<KAnnotation>,val decl:KVariableDeclaration):KotlinAst

sealed interface KWhenEntry:KotlinAst

data class KWhenEntryWithConditions(
	val conditions:NonEmptyList<KWhenCondition>,
	val body:KControlStructureBody
):KWhenEntry

data class KWhenElseEntry(val body:KControlStructureBody):KWhenEntry

sealed interface KWhenCondition:KotlinAst

data class KRangeTest(val op:EInOperator,val expression:KExpression):KWhenCondition
data class KTypeTest(val left:EIsOperator,val right:KType):KInfixOperationInner,KWhenCondition

data class KTryExpression(val block:KBlock,val clause:KTryExpressionInner):KPrimaryExpression

sealed interface KTryExpressionInner:KotlinAst

data class KCatchesWithFinally(
	val catches:NonEmptyList<KCatchBlock>,
	val finallyBlock:Option<KFinallyBlock>
):KTryExpressionInner

data class KCatchBlock(
	val annos:List<KAnnotation>,
	val id:KSimpleIdentifier,
	val type:KType,
	val block:KBlock
):KotlinAst

data class KFinallyBlock(val block:KBlock):KTryExpressionInner

sealed interface KJumpExpression:KPrimaryExpression

data class KThrowExpression(val expression:KExpression):KJumpExpression

data class KReturnExpression(
	val ret:KReturnInner,
	val expression:(Option<KExpression>)=None
):KJumpExpression

val kreturn=KReturnExpression(KReturnKeyword)

sealed interface KReturnInner:KotlinAst
data object KReturnKeyword:KReturnInner

data class KReturnAt(val id:KIdentifier):KReturnInner

data object KContinue:KJumpExpression
data class KContinueAt(val id:KIdentifier):KJumpExpression

data object KBreak:KJumpExpression
data class KBreakAt(val id:KIdentifier):KJumpExpression

//T :: A
data class KCallableReference(val type:Option<KReceiverType>,val target:KCallableReferenceInner):KPrimaryExpression
sealed interface KCallableReferenceInner:KotlinAst

enum class EAssignmentAndOperator:KotlinAst
{
	/**
	 * +=
	 * */
	AddAssignment,

	/**
	 * -=
	 * */
	SubAssignment,

	/**
	 * *=
	 * */
	MultAssignment,

	/**
	 * /=
	 * */
	DivAssignment,

	/**
	 * %=
	 * */
	ModAssignment
}

enum class EEqualityOperator:KotlinAst
{
	/**
	 * !=
	 * */
	ExclEq,

	/**
	 * !==
	 * */
	ExclEqEq,

	/**
	 * ==
	 * */
	EqEq,

	/**
	 * ===
	 * */
	EqEqEq
}

infix fun KExpression.exclEq(expr:KExpression):KEquality=
	KEquality(this,EEqualityOperator.ExclEq,expr)

infix fun KExpression.exclEqEq(expr:KExpression):KEquality=
	KEquality(this,EEqualityOperator.ExclEqEq,expr)

infix fun KExpression.eqEq(expr:KExpression):KEquality=
	KEquality(this,EEqualityOperator.EqEq,expr)

infix fun KExpression.eqEqEq(expr:KExpression):KEquality=
	KEquality(this,EEqualityOperator.EqEqEq,expr)

enum class EComparisonOperator:KotlinAst
{
	/**
	 * >
	 * */
	Gt,

	/**
	 * <
	 */
	Lt,

	/**
	 * <=
	 */
	Le,

	/**
	 * $>=
	 */
	Ge
}

infix fun KExpression.greater(expr:KExpression):KComparison=
	KComparison(this,EComparisonOperator.Gt,expr)

infix fun KExpression.less(expr:KExpression):KComparison=
	KComparison(this,EComparisonOperator.Lt,expr)

infix fun KExpression.lessEq(expr:KExpression):KComparison=
	KComparison(this,EComparisonOperator.Le,expr)

infix fun KExpression.greaterEq(expr:KExpression):KComparison=
	KComparison(this,EComparisonOperator.Ge,expr)

enum class EInOperator:KotlinAst
{
	/**
	 * in
	 * */
	In,

	/**
	 * !in
	 * */
	NotIn
}

enum class EIsOperator:KotlinAst
{
	/**
	 * is
	 * */
	Is,

	/**
	 * !is
	 * */
	NotIs
}

sealed interface KBinaryOperator:KotlinAst

enum class KAdditiveOperator:KPrefixUnaryOperator,KBinaryOperator
{
	Add, Sub
}

enum class KMultiplicativeOperator:KBinaryOperator
{
	Mult, Div, Mod
}

enum class EAsOperator:KotlinAst
{
	/**
	 * as
	 * */
	As,

	/**
	 * as?
	 * */
	AsSafe
}

sealed interface KPrefixUnaryOperator:KUnaryPrefix

data object KExcl:KPrefixUnaryOperator

enum class KPostfixUnaryOperator:KPostfixUnarySuffix
{
	Incr, Decr, DoubleExcl
}

enum class KMemberAccessOperator:KotlinAst
{
	Dot, SafeNav, ColonColon
}


operator fun KExpression.plus(other:KExpression):KBinaryExpression=
	KBinaryExpression(this,Add,other)

operator fun KExpression.minus(other:KExpression):KBinaryExpression=
	KBinaryExpression(this,Sub,other)

operator fun KExpression.times(other:KExpression):KBinaryExpression=
	KBinaryExpression(this,KMultiplicativeOperator.Mult,other)

operator fun KExpression.div(other:KExpression):KBinaryExpression=
	KBinaryExpression(this,KMultiplicativeOperator.Div,other)

operator fun KExpression.rem(other:KExpression):KBinaryExpression=
	KBinaryExpression(this,KMultiplicativeOperator.Mod,other)

infix fun KExcl.expr(expr:KExpression):KPrefixUnaryExpression=
	KPrefixUnaryExpression(expr.postfix,this.list)

val KExpression.excl
	get():KPrefixUnaryExpression=
		KPrefixUnaryExpression(this.postfix,KExcl.list)

//data class KModifiers(val mods:NonEmptyList<KModifiersInner>):KotlinAst

sealed interface KModifiersInner:KotlinAst

data class KParameterModifiers(val mods:NonEmptyList<KParameterModifiersInner>):KotlinAst
sealed interface KParameterModifiersInner:KotlinAst

sealed interface KModifier:KModifiersInner
sealed interface KPropertyModifier:KModifier
data object KConstModifier:KPropertyModifier

sealed interface KTypeModifier:KotlinAst
data object KSuspendKeyword:KTypeModifier,KSimpleIdentifier
{
	override val id="suspend"
}

enum class EClassModifier:KModifier
{
	Enum,
	Sealed,
	Annotation,
	Data,
	Inner,
	Value
}

enum class EMemberModifier:KModifier
{
	Override, Lateinit
}

enum class EVisibilityModifier:KModifier
{
	Public,
	Private,
	Internal,
	Protected
}

enum class EVarianceModifier:KModifier,KTypeProjectionModifier,KTypeParameterModifier
{
	In, Out
}

data class KTypeParameterModifiers(val mods:NonEmptyList<KTypeParameterModifier>):KotlinAst

sealed interface KTypeParameterModifier:KotlinAst

enum class EFunctionModifier:KModifier
{
	Tailrec,
	Operator,
	Infix,
	Inline,
	External,
	Suspend
}

enum class EInheritanceModifier:KModifier
{
	Abstract, Final, Open
}

enum class EParameterModifier:KModifier,KParameterModifiersInner
{
	Vararg, NoInline, CrossInline
}

sealed interface KReificationModifier:KTypeParameterModifier

data object KReifiedKeyword:KReificationModifier,KSimpleIdentifier
{
	override val id="reified"
}

sealed interface KPlatformModifier:KModifier

data object KExpectKeyword:KPlatformModifier,KSimpleIdentifier
{
	override val id="expect"
}

data object KActualKeyword:KPlatformModifier,KSimpleIdentifier
{
	override val id="actual"
}

sealed interface KAnnotation:
		KTypeProjectionModifier,
		KLabelOrAnnotation,
		KUnaryPrefix,
		KModifiersInner,
		KParameterModifiersInner,
		KTypeModifier,
		KTypeParameterModifier

data class KSingleAnnotation(val left:KAnnotationLeft,val right:KUnescapedAnnotation):KAnnotation

fun anno(left:EAnnotationUseSiteTarget,right:KUnescapedAnnotation)=
	KSingleAnnotation(left,right)

fun anno(right:KUnescapedAnnotation)=
	KSingleAnnotation(KAt,right)

val KSingleAnnotation.mod get()=this.nel()

sealed interface KAnnotationLeft:KotlinAst

data class KMultiAnnotation(val left:KAnnotationLeft,val right:NonEmptyList<KUnescapedAnnotation>):KAnnotation

/**
 * @
 * */
data object KAt:KAnnotationLeft

enum class EAnnotationUseSiteTarget:KAnnotationLeft
{
	Field,
	Property,
	Get,
	Set,
	Receiver,
	Param,
	SetParam,
	Delegate
}

fun KSimpleIdentifier.variableDecl(annos:List<KAnnotation>,type:KType):KVariableDeclaration=
	KVariableDeclaration(annos,this,type.some())

infix fun KSimpleIdentifier.variableDecl(type:KType):KVariableDeclaration=
	KVariableDeclaration(emptyList(),this,type.some())

val KSimpleIdentifier.variableDecl
	get()=KVariableDeclaration(identifier=this)

data object KExternalKeyword:KSimpleIdentifier
{
	override val id:String="external"
}

data object KFinalKeyword:KSimpleIdentifier
{
	override val id:String="final"
}

data object KFinallyKeyword:KSimpleIdentifier
{
	override val id="finally"
}

data object KGetKeyword:KSimpleIdentifier
{
	override val id="get"
}

data object KImportKeyword:KSimpleIdentifier
{
	override val id="import"
}

data object KInfixKeyword:KSimpleIdentifier
{
	override val id="infix"
}

data object KInitKeyword:KSimpleIdentifier
{
	override val id="init"
}

data object KInlineKeyword:KSimpleIdentifier
{
	override val id="inline"
}

data object KInnerKeyword:KSimpleIdentifier
{
	override val id="inner"
}

data object KInternalKeyword:KSimpleIdentifier
{
	override val id="internal"
}

data object KLateinitKeyword:KSimpleIdentifier
{
	override val id="lateinit"
}

data object KNoinlineKeyword:KSimpleIdentifier
{
	override val id="noinline"
}

data object KOpenKeyword:KSimpleIdentifier
{
	override val id="open"
}

data object KOperatorKeyword:KSimpleIdentifier
{
	override val id="operator"
}

data object KOutKeyword:KSimpleIdentifier
{
	override val id="out"
}

data object KOverrideKeyword:KSimpleIdentifier
{
	override val id="override"
}

data object KPrivateKeyword:KSimpleIdentifier
{
	override val id="private"
}

data object KProtectedKeyword:KSimpleIdentifier
{
	override val id="protected"
}

data object KPublicKeyword:KSimpleIdentifier
{
	override val id="public"
}

data object KSealedKeyword:KSimpleIdentifier
{
	override val id="sealed"
}

data object KTailrecKeyword:KSimpleIdentifier
{
	override val id="tailrec"
}

data object KSetKeyword:KSimpleIdentifier
{
	override val id="set"
}

data object KVarargKeyword:KSimpleIdentifier
{
	override val id="vararg"
}

data object KWhereKeyword:KSimpleIdentifier
{
	override val id="where"
}

data object KFieldKeyword:KSimpleIdentifier
{
	override val id="field"
}

data object KPropertyKeyword:KSimpleIdentifier
{
	override val id="property"
}

data object KReceiverKeyword:KSimpleIdentifier
{
	override val id="receiver"
}

data object KParamKeyword:KSimpleIdentifier
{
	override val id="param"
}

data object KSetparamKeyword:KSimpleIdentifier
{
	override val id="setparam"
}

data object KDelegateKeyword:KSimpleIdentifier
{
	override val id="delegate"
}

data object KFileKeyword:KSimpleIdentifier
{
	override val id="file"
}

data object KConstKeyword:KSimpleIdentifier
{
	override val id="const"
}

data object KValueKeyword:KSimpleIdentifier
{
	override val id="value"
}

infix fun KSimpleIdentifier.dot(other:KSimpleIdentifier)=KIdentifier(this.nel()+other)

class IdentifierBuilder(first:KIdentifierInner)
{
	private var ids=first.nel()

	constructor(first:String):this(KIdentifierInner(first))

	operator fun KIdentifierInner.unaryPlus()
	{
		ids=_ids(this)
	}

	operator fun String.unaryPlus()
	{
		+KIdentifierInner(this)
	}

	private fun _ids(default:KIdentifierInner)=
		(ids+default).also {ids=it}

	fun build()=KIdentifier(ids)
}

fun identifier(first:KIdentifierInner,init:IdentifierBuilder.()->Unit)=
	IdentifierBuilder(first).also(init).build()

fun identifier(first:String,init:IdentifierBuilder.()->Unit)=
	IdentifierBuilder(first).also(init).build()