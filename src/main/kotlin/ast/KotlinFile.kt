package org.fancryer.bf.ast

import arrow.core.*
import org.fancryer.bf.nonEmptyList

data class KKotlinFile(
	val shebang:(Option<String>)=none(),
	val annotations:(List<KFileAnnotation>)=emptyList(),
	val packageHeader:KPackageHeader,
	val importList:(List<KImportHeader>)=emptyList(),
	val topLevelObjectList:(List<KTopLevelObject>)=emptyList()
):KotlinAst

data class KKotlinScript(
	val shebang:Option<String>,
	val annotations:List<KFileAnnotation>,
	val packageHeader:KPackageHeader,
	val importList:List<KImportHeader>,
	val statements:List<KStatement>
):KotlinAst

data class KFileAnnotation(val annotations:NonEmptyList<KUnescapedAnnotation>):KotlinAst

sealed interface KUnescapedAnnotation:KotlinAst

data class KUserType(
	val types:NonEmptyList<KSimpleUserType>
):KUnescapedAnnotation,KDelegationSpecifier,KParenthesizedOrFlatUserType,KTypeReference
{
	override val code:String
		get()=types.joinToString(".") {it.code}
}

data class KPackageHeader(val identifier:Option<KIdentifier>):KotlinAst

sealed interface KImportHeader:KotlinAst
{
	data class KMultImport(val identifier:KIdentifier):KImportHeader
	data class KAliasImport(val identifier:KIdentifier,val alias:KImportAlias):KImportHeader
	data class KImportAlias(val identifier:KSimpleIdentifier)
}

data class KTopLevelObject(val declaration:KDeclaration):KotlinAst

data class KTypeAlias(
	val modifiers:List<KModifier>,
	val identifier:KSimpleIdentifier,
	val typeParameters:KTypeParameters,
	val type:KType
):KDeclaration

sealed interface KDeclaration:KClassMemberDeclaration,KStatementInner

data class KClassDeclaration(
	val modifiers:Option<KModifiers>,
	val classHeaderType:KClassHeaderType,
	val identifier:KSimpleIdentifier,
	val typeParameters:Option<KTypeParameters>,
	val primaryConstructor:Option<KPrimaryConstructor>,
	val delegationSpecifiers:Option<KDelegationSpecifiers>,
	val typeConstraints:Option<KTypeConstraints>,
	val body:IKClassBody
):KDeclaration

sealed interface KClassHeaderType:KotlinAst

sealed interface IKClassBody:KotlinAst

data class KClassBody(val memberDeclarations:KClassMemberDeclarations):IKClassBody

@ClassBuilderDsl
annotation class ClassBodyBuilderDsl

@ClassBodyBuilderDsl
class ClassBodyBuilder
{
	private val declarations=mutableListOf<KClassMemberDeclaration>()

	//	operator fun KClassMemberDeclaration.unaryPlus()
	//	{
	//		declarations+=this
	//	}

	fun funct(id:KSimpleIdentifier,init:FunctionDeclarationBuilder.()->Unit)
	{
		declarations+=FunctionDeclarationBuilder(id).also(init).build()
	}

	fun build()=KClassBody(KClassMemberDeclarations(declarations))
}

data class KEnumClassBody(
	val entries:Option<KEnumEntries>,
	val memberDeclarations:Option<KClassMemberDeclarations>
):IKClassBody

data class KPrimaryConstructor(
	val modifiers:Option<KModifiers>,
	val classParameters:KClassParameters
):KotlinAst

data class KClassParameters(
	val classParameterList:Option<NonEmptyList<KClassParameter>>
):KotlinAst

data class KClassParameter(
	val modifiers:Option<KModifiers>,
	val isVal:Boolean,
	val identifier:KSimpleIdentifier,
	val type:KType,
	val expression:Option<KExpression>
):KotlinAst

data class KDelegationSpecifiers(
	val specifiers:NonEmptyList<KAnnotatedDelegationSpecifier>
):KotlinAst

sealed interface KDelegationSpecifier:KotlinAst

data class KConstructorInvocation(
	val userType:KUserType,
	val valueArguments:KValueArguments
):KUnescapedAnnotation,KDelegationSpecifier

data class KAnnotatedDelegationSpecifier(
	val annotations:List<KAnnotation>,
	val delegationSpecifier:KDelegationSpecifier
):KotlinAst

data class KFunctionDelegationSpecifier(
	val isSuspend:Boolean,
	val functionType:KFunctionType
):KDelegationSpecifier

data class KTypeParameters(val params:NonEmptyList<KTypeParameter>):KotlinAst
data class KTypeParameter(
	val modifiers:Option<KTypeParameterModifiers>,
	val identifier:KSimpleIdentifier,
	val type:Option<KType>
):KotlinAst

data class KTypeConstraints(val constraints:NonEmptyList<KTypeConstraint>):KotlinAst
data class KTypeConstraint(
	val annotations:List<KAnnotation>,
	val identifier:KSimpleIdentifier,
	val type:KType
):KotlinAst

data class KClassMemberDeclarations(val memberDeclarations:List<KClassMemberDeclaration>):KotlinAst

sealed interface KClassMemberDeclaration:KotlinAst

data class KCompanionObject(
	val modifiers:Option<KModifiers>,
	val isData:Boolean,
	val identifier:Option<KSimpleIdentifier>,
	val delegationSpecifiers:Option<KDelegationSpecifiers>,
	val classBody:Option<KClassBody>
):KClassMemberDeclaration

data class KAnonymousInitializer(val block:KBlock):KClassMemberDeclaration

data class KFunctionValueParameters(val params:Option<NonEmptyList<KFunctionValueParameter>>):KotlinAst

data class KFunctionValueParameter(
	val modifiers:Option<KParameterModifiers>,
	val parameter:KParameter,
	val expression:Option<KExpression>
):KotlinAst

data class KSecondaryConstructor(
	val modifiers:Option<KModifiers>,
	val functionValueParameters:KFunctionValueParameters,
	val constructorDelegationCall:Option<KConstructorDelegationCall>,
	val block:KBlock
):KClassMemberDeclaration

data class KObjectDeclaration(
	val modifiers:Option<KModifiers>,
	val identifier:KSimpleIdentifier,
	val delegationSpecifiers:Option<KDelegationSpecifiers>,
	val classBody:Option<KClassBody>
):KDeclaration

//robjectDeclaration: modifiers?
//      rsimpleIdentifier
//      rdelegationSpecifiers?
//      rclassBody?;

data class KFunctionDeclaration(
	val modifiers:(Option<KModifiers>)=none(),
	val typeParameters:(Option<KTypeParameters>)=none(),
	val receiverType:(Option<KType>)=none(),
	val identifier:KSimpleIdentifier,
	val functionValueParameters:KFunctionValueParameters,
	val type:(Option<KType>)=none(),
	val typeConstraints:(Option<KTypeConstraints>)=none(),
	val functionBody:(Option<KFunctionBody>)=none()
):KDeclaration

class FunctionDeclarationBuilder(private var identifier:KSimpleIdentifier)
{
	private var modifiers:(Option<KModifiers>)=none()
	private var typeParameters:(Option<KTypeParameters>)=none()
	private var receiverType:(Option<KType>)=none()

	//Check
	private var functionValueParameters:KFunctionValueParameters=KFunctionValueParameters(none())
	private var type:(Option<KType>)=none()
	private var typeConstraints:(Option<KTypeConstraints>)=none()
	private var functionBody:(Option<KFunctionBody>)=none()

	fun modifiers(init:KModifiersBuilder.()->Unit)
	{
		modifiers=KModifiersBuilder().apply(init).build().some()
	}

	operator fun KModifiers.unaryPlus()
	{
		modifiers=this.some()
	}

	fun typeParameters(params:KTypeParameters)
	{
		typeParameters=params.some()
	}

	fun recieverType(type:KType)
	{
		receiverType=type.some()
	}

	fun functionValueParameters(params:KFunctionValueParameters)
	{
		functionValueParameters=params
	}

	fun type(type:KType)
	{
		this.type=type.some()
	}

	fun typeConstrants(constraints:KTypeConstraints)
	{
		typeConstraints=constraints.some()
	}

	fun body(b:KFunctionBody)
	{
		functionBody=b.some()
	}

	fun body(b:()->KFunctionBody)
	{
		body(b())
	}

	fun build():KFunctionDeclaration=
		KFunctionDeclaration(
			modifiers,
			typeParameters,
			receiverType,
			identifier,
			functionValueParameters,
			type,
			typeConstraints,
			functionBody
		)
}

@DslMarker
annotation class ClassBuilderDsl

@ClassBuilderDsl
annotation class ModifiersBuilderDsl

@ModifiersBuilderDsl
class KModifiersBuilder
{
	private var mods:(List<KModifiersInner>)=listOf()

	operator fun KModifiersInner.unaryPlus()
	{
		mods+=this
	}

	fun build():KModifiers
	{
		return KModifiers(mods.toNonEmptyListOrNull() ?: throw Exception("No modifiers"))
	}
}

@ClassBuilderDsl
class ClassDeclarationBuilder(private val identifier:KSimpleIdentifier)
{
	private var modifiers:(Option<KModifiers>)=none()
	private var classHeaderType:(Option<KClassHeaderType>)=none()
	private var typeParameters:(Option<KTypeParameters>)=none()
	private var primaryConstructor:(Option<KPrimaryConstructor>)=none()
	private var delegationSpecifiers:(Option<KDelegationSpecifiers>)=none()
	private var typeConstraints:(Option<KTypeConstraints>)=none()
	private var classBody:(Option<KClassBody>)=none()

	fun headerType(type:KClassHeaderType)
	{
		classHeaderType=type.some()
	}

	fun body(init:ClassBodyBuilder.()->Unit)
	{
		classBody=ClassBodyBuilder().apply(init).build().some()
	}

	/*
	data class KClassDeclaration(
	val modifiers:Option<KModifiers>,
	val classHeaderType:KClassHeaderType,
	val typeParameters:Option<KTypeParameters>,
	val primaryConstructor:Option<KPrimaryConstructor>,
	val delegationSpecifiers:Option<KDelegationSpecifiers>,
	val typeConstraints:Option<KTypeConstraints>,
	val body:IKClassBody
):KDeclaration
	*/

	fun build():KClassDeclaration
	{
		return when(val classHeaderType=this.classHeaderType)
		{
			is None->throw Exception("classHeaderType must be defined")
			is Some->
				when(val classBody=this.classBody)
				{
					is None->throw Exception("classBody must be defined")
					is Some->
					{
						KClassDeclaration(
							modifiers,
							classHeaderType.value,
							identifier,
							typeParameters,
							primaryConstructor,
							delegationSpecifiers,
							typeConstraints,
							classBody.value
						)
					}
				}
		}
	}
}

fun klass(id:KSimpleIdentifier,init:ClassDeclarationBuilder.()->Unit)=
	ClassDeclarationBuilder(id).also(init).build()

sealed interface KFunctionBody:KotlinAst

data class KFunctionAssignExpression(val expression:KExpression):KFunctionBody
{
	override val code:String
		get()=" = ${expression.code}"
}

data class KVariableDeclaration(
	val annotations:List<KAnnotation>,
	val identifier:KSimpleIdentifier,
	val type:Option<KType>
):KMultiOrSingleVariableDeclaration,KLambdaParameter
{
	override val code:String
		get()=buildString {
			annotations.forEach {
				append(it.code)
				append(' ')
			}
			append(identifier.code)
			type.onSome {
				append(':')
				append(it.code)
			}
		}
}

val KVariableDeclaration.lambdaParams:KLambdaParameters
	get()=KLambdaParameters(nonEmptyList)

data class KMultiVariableDeclaration(
	val variableDeclarations:NonEmptyList<KVariableDeclaration>
):KMultiOrSingleVariableDeclaration

sealed interface KMultiOrSingleVariableDeclaration:KotlinAst
data class KPropertyDeclaration(
	val modifiers:Option<KModifiers>,
	val isVal:Boolean,
	val typeParameters:Option<KTypeParameters>,
	val declaration:KMultiOrSingleVariableDeclaration,
	val typeConstraints:Option<KTypeConstraints>,
	val byDelegate:Boolean, // by expr
	val expr:KExpression,
	val getter:Option<KGetter>,
	val setter:Option<KSetter>
):KDeclaration

data class KGetter(
	val modifiers:Option<KModifiers>,
	val getterBody:Option<KGetterBody>
):KotlinAst

data class KGetterBody(
	val type:Option<KType>,
	val functionBody:KFunctionBody
):KotlinAst

data class KSetter(
	val modifiers:Option<KModifiers>,
	val setterBody:Option<KSetterBody>
):KotlinAst

data class KSetterBody(
	val param:KFunctionValueParameterWithOptionalType,
	val type:Option<KType>,
	val functionBody:KFunctionBody
):KotlinAst

data class KParametersWithOptionalType(
	val params:Option<NonEmptyList<KFunctionValueParameterWithOptionalType>>
):KotlinAst
{
	override val code:String
		get()=buildString {
			append("(")
			params.onSome {params->
				append(params.joinToString {it.code})
			}
			append(")")
		}
}

data class KFunctionValueParameterWithOptionalType(
	val modifiers:(Option<KParameterModifiers>)=None,
	val parameter:KParameterWithOptionalType,
	val expression:(Option<KExpression>)=None
):KotlinAst
{
	override val code:String
		get()=buildString {
			modifiers.onSome {
				append(it.code)
				append(' ')
			}
			append(parameter.code)
			expression.onSome {
				append('=')
				append(it.code)
			}
		}
}

data class KParameterWithOptionalType(
	val identifier:KSimpleIdentifier,
	val type:(Option<KType>)=None
):KotlinAst
{
	override val code:String
		get()=buildString {
			append(identifier.code)
			type.onSome {
				append(':')
				append(it.code)
			}
		}
}

data class KParameter(
	val identifier:KSimpleIdentifier,
	val type:KType
):KFunctionTypeParameterInner

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

data class KEnumEntry(
	val modifiers:Option<KModifiers>,
	val identifier:KSimpleIdentifier,
	val valueArguments:Option<KValueArguments>,
	val classBody:Option<KClassBody>
):KotlinAst

data class KType(
	val modifiers:(Option<NonEmptyList<KTypeModifier>>)=None,
	val typeInner:KTypeInner
):KFunctionTypeParameterInner
{
	override val code:String
		get()=buildString {
			val mods=modifiers.map {it.toList()}.getOrElse {emptyList()}
			mods.forEachIndexed {i,it->
				append(it.code)
				if(i!=mods.size-1) append(' ')
			}
			append(' ')
			append(typeInner.code)
		}
}

val KType.functionTypeParameters get()=KFunctionTypeParameters(nonEmptyList)

sealed interface KTypeInner:KotlinAst

//rtype: typeModifier+? typeinner=rdefinitelyNonNullableType;

sealed interface KTypeReference:KNullableType,KRecieverTypeInner,KTypeInner
{
	data object Dynamic:KTypeReference
}

sealed interface KNullableType:KTypeInner,KRecieverTypeInner

data class KSimpleUserType(
	val identifier:KSimpleIdentifier,
	val typeArguments:Option<KTypeArguments>
):KotlinAst
{
	override val code:String
		get()=buildString {
			append(identifier.code)
			typeArguments.onSome {
				append(it.code)
			}
		}
}

sealed interface KTypeProjection:KotlinAst
data class KConcreteTypeProjection(
	val modifiers:Option<KTypeProjectionModifiers>,
	val type:KType
):KTypeProjection

data object KStarTypeProjection:KTypeProjection

data class KTypeProjectionModifiers(
	val modifiers:NonEmptyList<KTypeProjectionModifier>
):KotlinAst

sealed interface KTypeProjectionModifier:KotlinAst

data class KFunctionType(
	val recieverType:(Option<KRecieverType>)=None,
	val parameters:KFunctionTypeParameters,
	val returnType:KType
):KTypeInner
{
	override val code:String
		get()=buildString {
			recieverType.onSome {
				append(it.code)
				append('.')
			}
			append(parameters.code)
			append(" -> ")
			append(returnType.code)
		}
}

sealed interface KFunctionTypeParameterInner:KotlinAst

data class KParenthesizedType(val type:KType):KNullableType,KTypeInner,KRecieverTypeInner

sealed interface KRecieverTypeInner:KotlinAst

data class KRecieverType(
	val modifiers:Option<NonEmptyList<KTypeModifier>>,
	val type:KRecieverTypeInner
):KotlinAst

sealed interface KParenthesizedOrFlatUserType:KotlinAst

data class KParenthesizedUserType(val type:KParenthesizedOrFlatUserType):KParenthesizedOrFlatUserType

//TODO rename fields
data class KDefinitelyNonNullableType(
	val leftModifiers:Option<NonEmptyList<KTypeModifier>>,
	val left:KParenthesizedOrFlatUserType,
	val rightModifiers:Option<NonEmptyList<KTypeModifier>>,
	val right:KParenthesizedOrFlatUserType
):KTypeInner


sealed interface KLabelOrAnnotation:KotlinAst

sealed interface KStatementInner:KotlinAst

data class KLabel(val identifier:KSimpleIdentifier):KLabelOrAnnotation,KUnaryPrefix

sealed interface KControlStructureBody:KIfInner,KControlStructureBodyOrSemicolon

data class KBlock(val statements:Option<NonEmptyList<KStatement>>):KControlStructureBody,KFunctionBody,KStatementInner
{
	override val code:String
		get()=buildString {
			append('{')
			statements.onSome {stats->
				stats.toList()
					.asSequence()
					.map {it.code}
					.forEach {append(it).append(';')}
			}
			append('}')
		}
}

class BlockBuilder
{
	private var statements:(Option<NonEmptyList<KStatement>>)=none()

	operator fun KStatement.unaryPlus()
	{
		statements=stats(this).some()
	}

	operator fun KStatementInner.unaryPlus()
	{
		+KStatement(emptyList(),this)
	}

	private fun stats(default:KStatement)=
		when(val stats=statements)
		{
			is None->
			{
				nonEmptyListOf(default).also {statements=it.some()}
			}

			is Some->
			{
				(stats.value+default).also {statements=it.some()}
			}
		}

	fun build()=KBlock(statements)
}

fun block(init:BlockBuilder.()->Unit)=
	BlockBuilder().also(init).build()

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

data class KDoWhileStatement(
	val controlStructureBody:Option<KControlStructureBody>,
	val expression:KExpression
):KLoopStatement

sealed interface KAssignmentLeft:KStatementInner

data class KAssignment(val left:KAssignmentLeft,val right:KExpression):KStatementInner

data class KAssignableWithAndExpression(val left:KAssignableExpression,val right:EAssignmentAndOperator):KAssignmentLeft

sealed interface KExpression:KStatementInner,KWhenCondition
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

data class KAdditiveExpression(val left:KExpression,val op:KAdditiveOperator,val right:KExpression):KExpression

data class KMultiplicativeExpression(val left:KExpression,val op:KMultiplicativeOperator,val right:KExpression):KExpression

data class KAsExpression(val left:KExpression,val op:EAsOperator,val type:KType):KExpression

data class KPrefixUnaryExpression(val prefix:KUnaryPrefix,val expression:KExpression):KAssignableExpression

sealed interface KUnaryPrefix:KotlinAst

data class KPostfixUnaryExpression(
	val primaryExpression:KPrimaryExpression,
	val suffixes:List<KPostfixUnarySuffix>
):KExpression
{
	override val code:String
		get()=buildString {
			append(primaryExpression.code)
			suffixes.asSequence()
				.map {it.code}
				.forEach(::append)
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
	val memberAccessOperator:KMemberAccessOperator,
	val suffix:KNavigationSuffixInner
):KAssignableSuffix,KPostfixUnarySuffix

sealed interface KNavigationSuffixInner:KotlinAst

data object KClassKeyword:KNavigationSuffixInner,KClassHeaderType,KCallableReferenceInner
data object KFunInterfaceKeyword:KClassHeaderType
data object KInterfaceKeyword:KClassHeaderType
/*
directlyAssignableExpression: done postfixUnaryExpression assignableSuffix
    | todo rsimpleIdentifier
    | done parenthesizedDirectlyAssignableExpression;
*/

data class KParenthesizedExpression(val expression:KExpression):KPrimaryExpression,KNavigationSuffixInner
{
	override val code:String get()="(${expression.code})"
}

data class KCallSuffix(val args:Option<KTypeArguments>,val callSuffixInner:KCallSuffixInner):KPostfixUnarySuffix
{
	override val code:String
		get()=buildString {
			args.onSome {append(it.code)}
			append(callSuffixInner.code)
		}
}

sealed interface KCallSuffixInner:KotlinAst

data class KAnnotatedLambdaWithValueArguments(val valueArguments:Option<KValueArguments>,val lambda:KAnnotatedLambda):KCallSuffixInner

data class KAnnotatedLambda(val annos:List<KAnnotation>,val label:Option<KLabel>,val literal:KLambdaLiteral):KotlinAst

data class KTypeArguments(val projections:NonEmptyList<KTypeProjection>):KAssignableSuffix,KPostfixUnarySuffix
data class KValueArguments(val args:Option<NonEmptyList<KValueArgument>>):KCallSuffixInner
{
	override val code:String
		get()=buildString {
			append('(')
			args.onSome {arguments->
				arguments.toList()
					.asSequence()
					.map {it.code}
					.forEach(::append)
			}
			append(')')
		}
}

data class KValueArgument(
	val anno:(Option<KAnnotation>)=None,
	val identifier:(Option<KSimpleIdentifier>)=None,
	val hasSpread:Boolean=false,
	val expression:KExpression
):KotlinAst
{
	override val code:String
		get()=buildString {
			anno.onSome {append(it.code).append(' ')}
			identifier.onSome {append(it.code)}
			if(hasSpread) append("...")
			append(expression.code)
		}
}

sealed interface KPrimaryExpression:KExpression

data class KCollectionLiteral(val exprs:Option<NonEmptyList<KExpression>>):KPrimaryExpression

sealed interface KLiteralConstant:KPrimaryExpression

enum class EBooleanLiteral:KLiteralConstant
{
	True
	{
		override val code:String get()="true"
	},
	False
	{
		override val code:String get()="false"
	}
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

data class KLineStringLiteral(val content:List<KLineStringContentOrExpression>):KStringLiteral
{
	override val code:String
		get()=buildString {
			append('"')
			content.asSequence()
				.map {it.code}
				.forEach(::append)
			append('"')
		}
}

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
	val params:Option<KLambdaParameters>,
	val stats:Option<NonEmptyList<KStatement>>
):KFunctionLiteral
{
	override val code:String
		get()=buildString {
			append("{")
			params.onSome {
				append(it.code)
				append(" -> ")
			}
			stats.onSome {stats->
				stats.forEach {
					append(it.code)
					append(';')
				}
			}
			append("}")
		}
}

data class KLambdaParameters(val params:NonEmptyList<KLambdaParameter>):KotlinAst
{
	override val code:String
		get()=params.joinToString {it.code}
}

infix fun KLambdaParameters.literal(stats:NonEmptyList<KStatement>):KLambdaLiteral=
	KLambdaLiteral(some(),stats.some())

infix fun KLambdaParameters.literal(stat:KStatement):KLambdaLiteral=
	literal(stat.nonEmptyList)

val KLambdaParameters.literal:KLambdaLiteral
	get()=KLambdaLiteral(some(),none())

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
	val constraints:(Option<KTypeConstraints>)=None,
	val body:(Option<KFunctionBody>)=None
):KFunctionLiteral
{
	override val code:String
		get()=buildString {
			if(isSuspend) append("suspend ")
			append("fun ")
			recieverType.onSome {
				append(it.code)
				append('.')
			}
			append(params.code)
			toType.onSome {
				append(':')
				append(it.code)
			}
			constraints.onSome {
				append(it.code)
			}
			body.onSome {
				append(it.code)
			}
		}
}

data class KObjectLiteral(
	val isData:Boolean,
	val specifiers:Option<KDelegationSpecifiers>,
	val body:Option<KClassBody>
):KPrimaryExpression

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
{
	override val code:String get()=";"
}

sealed interface KControlStructureBodyOrSemicolon:KotlinAst

data class KWhenSubject(val decl:Option<KVariableDeclarationWithAnnotations>,val expression:KExpression):KotlinAst

data class KVariableDeclarationWithAnnotations(val annos:List<KAnnotation>,val decl:KVariableDeclaration):KotlinAst

data class KWhenExpression(val subject:Option<KWhenSubject>,val entries:List<KWhenEntry>):KPrimaryExpression

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

data class KReturnExpression(val ret:KReturnInner,val expression:Option<KExpression>):KJumpExpression

sealed interface KReturnInner:KotlinAst
data object KReturnKeyword:KReturnInner
data class KReturnAt(val id:KIdentifier):KReturnInner

data object KContinue:KJumpExpression
data class KContinueAt(val id:KIdentifier):KJumpExpression

data object KBreak:KJumpExpression
data class KBreakAt(val id:KIdentifier):KJumpExpression

//T :: A
data class KCallableReference(val type:Option<KRecieverType>,val target:KCallableReferenceInner):KPrimaryExpression
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
	 * >=
	 */
	Ge
}

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

sealed interface KAdditiveOperator:KotlinAst
sealed interface KMultiplicativeOperator:KotlinAst

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

data object KAdd:KAdditiveOperator,KPrefixUnaryOperator
data object KSub:KAdditiveOperator,KPrefixUnaryOperator
data object KMult:KMultiplicativeOperator
data object KDiv:KMultiplicativeOperator
data object KMod:KMultiplicativeOperator
data object KExcl:KPrefixUnaryOperator
data object KIncr:KPrefixUnaryOperator,KPostfixUnaryOperator
data object KDecr:KPrefixUnaryOperator,KPostfixUnaryOperator
sealed interface KPostfixUnaryOperator:KPostfixUnarySuffix
data object KDoubleExcl:KPostfixUnaryOperator
sealed interface KMemberAccessOperator:KotlinAst

data object KDot:KMemberAccessOperator

//?.
data object KSafeNav:KMemberAccessOperator
data object KColonColon:KMemberAccessOperator

data class KModifiers(val mods:NonEmptyList<KModifiersInner>):KotlinAst
sealed interface KModifiersInner:KotlinAst

data class KParameterModifiers(val mods:NonEmptyList<KParameterModifiersInner>):KotlinAst
sealed interface KParameterModifiersInner:KotlinAst

sealed interface KModifier:KModifiersInner
sealed interface KPropertyModifier:KModifier
data object KConstModifier:KPropertyModifier

sealed interface KTypeModifier:KotlinAst
data object KSuspendKeyword:KTypeModifier,KSimpleIdentifier

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
	Override,
	Lateinit
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
	In,
	Out
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
	Abstract,
	Final,
	Open
}

enum class EParameterModifier:KModifier,KParameterModifiersInner
{
	Vararg,
	NoInline,
	CrossInline
}

sealed interface KReificationModifier:KTypeParameterModifier

data object KReifiedKeyword:KReificationModifier,KSimpleIdentifier


sealed interface KPlatformModifier:KModifier

data object KExpectKeyword:KPlatformModifier,KSimpleIdentifier
data object KActualKeyword:KPlatformModifier,KSimpleIdentifier

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

val KSingleAnnotation.mod get()=KModifiers(nonEmptyListOf(this))

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

sealed interface KSimpleIdentifier:
		KPrimaryExpression,
		KCallableReferenceInner,
		KDirectlyAssignableExpression,
		KNavigationSuffixInner

fun KSimpleIdentifier.variableDecl(annos:List<KAnnotation>,type:KType):KVariableDeclaration=
	KVariableDeclaration(annos,this,type.some())

infix fun KSimpleIdentifier.variableDecl(type:KType):KVariableDeclaration=
	KVariableDeclaration(emptyList(),this,type.some())

val KSimpleIdentifier.variableDecl
	get()=KVariableDeclaration(emptyList(),this,None)

/*
Check with:
	(Letter | '_') (Letter | '_' | UnicodeDigit)*
    | '`' ~([\r\n] | '`')+ '`'
*/
data class KIdentifierInner(val name:String):KSimpleIdentifier
{
	override val code:String
		get()=name
}

data object KAbstractKeyword:KSimpleIdentifier
data object KAnnotationKeyword:KSimpleIdentifier
data object KByKeyword:KSimpleIdentifier
data object KCatchKeyword:KSimpleIdentifier
data object KCompanionKeyword:KSimpleIdentifier
data object KConstructorKeyword:KSimpleIdentifier
data object KCrossinlineKeyword:KSimpleIdentifier
data object KDataKeyword:KSimpleIdentifier
data object KDynamicKeyword:KSimpleIdentifier
data object KEnumKeyword:KSimpleIdentifier
data object KExternalKeyword:KSimpleIdentifier

data object KFinalKeyword:KSimpleIdentifier
data object KFinallyKeyword:KSimpleIdentifier
data object KGetKeyword:KSimpleIdentifier
data object KImportKeyword:KSimpleIdentifier
data object KInfixKeyword:KSimpleIdentifier
data object KInitKeyword:KSimpleIdentifier
data object KInlineKeyword:KSimpleIdentifier
data object KInnerKeyword:KSimpleIdentifier
data object KInternalKeyword:KSimpleIdentifier
data object KLateinitKeyword:KSimpleIdentifier
data object KNoinlineKeyword:KSimpleIdentifier
data object KOpenKeyword:KSimpleIdentifier
data object KOperatorKeyword:KSimpleIdentifier
data object KOutKeyword:KSimpleIdentifier
data object KOverrideKeyword:KSimpleIdentifier
data object KPrivateKeyword:KSimpleIdentifier
data object KProtectedKeyword:KSimpleIdentifier
data object KPublicKeyword:KSimpleIdentifier
data object KSealedKeyword:KSimpleIdentifier
data object KTailrecKeyword:KSimpleIdentifier
data object KSetKeyword:KSimpleIdentifier
data object KVarargKeyword:KSimpleIdentifier
data object KWhereKeyword:KSimpleIdentifier
data object KFieldKeyword:KSimpleIdentifier
data object KPropertyKeyword:KSimpleIdentifier
data object KReceiverKeyword:KSimpleIdentifier
data object KParamKeyword:KSimpleIdentifier
data object KSetparamKeyword:KSimpleIdentifier
data object KDelegateKeyword:KSimpleIdentifier
data object KFileKeyword:KSimpleIdentifier
data object KConstKeyword:KSimpleIdentifier
data object KValueKeyword:KSimpleIdentifier

data class KIdentifier(val ids:NonEmptyList<KSimpleIdentifier>):KotlinAst
{
	override val code:String
		get()=ids.joinToString(".") {it.code}
}

abstract class NonEmptyListBuilder<T>(first:T)
{
	private var ts=nonEmptyListOf(first)

	operator fun T.unaryPlus()
	{
		ts=_ts(this)
	}

	private fun _ts(default:T)=
		(ts+default).also {ts=it}

	fun build()=ts
}

class IdentifierBuilder(first:KIdentifierInner)
{
	private var ids=nonEmptyListOf(first)

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

data class KFieldIdentifier(val id:KSimpleIdentifier):KotlinAst