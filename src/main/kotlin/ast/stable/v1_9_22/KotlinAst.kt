import arrow.core.Either
import arrow.core.NonEmptyList
import arrow.core.Option
import arrow.core.none

@Target(AnnotationTarget.EXPRESSION)
@Retention(AnnotationRetention.SOURCE)
annotation class CommentBefore(val text:String)

@Target(AnnotationTarget.EXPRESSION)
@Retention(AnnotationRetention.SOURCE)
annotation class CommentAfter(val text:String)

sealed interface KotlinAst

data class KotlinFile(
	val shebangLine:Option<String>,
	val fileAnnotations:List<FileAnnotation>,
	val packageHeader:PackageHeader,
	val importList:List<ImportHeader>,
	val topLevelObjects:List<TopLevelObject>
):KotlinAst

data class Script(
	val shebangLine:(Option<String>)=none(),
	val fileAnnotations:List<FileAnnotation>,
	val packageHeader:PackageHeader,
	val importList:List<ImportHeader>,
	val statements:List<Semi>
):KotlinAst

data class FileAnnotation(
	//left means an array of annotations
	//right means a single annotation
	val annotations:Either<List<UnescapedAnnotation>,UnescapedAnnotation>
):KotlinAst

data class PackageHeader(val identifier:String):KotlinAst

data class ImportHeader(
	val identifier:Identifier,
	val importType:(Option<ImportType>)=none()
):KotlinAst
{
	sealed class ImportType(val text:String)
	{
		data object MULT:ImportType(".*")
		data class AS(val identifier:ESimpleIdentifier):ImportType("as $identifier")
	}
}

data class TopLevelObject(val declaration:Declaration):KotlinAst

typealias Modifiers=NonEmptyList<Either<Annotation,Modifier>>

data class TypeAlias(
	val modifiers:Modifiers,
	val simpleIdentifier:ESimpleIdentifier,
	val typeParameters:List<TypeParameter>,
	val type_:Type
):Declaration

sealed interface Declaration:ClassMemberDeclaration,StatementType

//declaration
//    : classDeclaration
//    | objectDeclaration@TODO
//    | functionDeclaration@TODO
//    | propertyDeclaration@TODO
//    | typeAlias
//    ;
//
//// SECTION: classes


data class ClassDeclaration(
	val declarationType:EClassDeclarationType,
	val name:ESimpleIdentifier,
	val typeParameters:List<TypeParameter>,
	val primaryConstructor:Option<PrimaryConstructor>,
	val delegationSpecifiers:Option<DelegationSpecifiers>,
	val typeConstraints:Option<TypeConstraints>,
	val body:Option<ClassDeclarationBody>
):Declaration
{
	enum class EClassDeclarationType
	{
		CLASS,
		FUN_INTERFACE
	}

	sealed class ClassDeclarationBody
	{
		data class ClassBody(val members:List<ClassMemberDeclaration>):ClassDeclarationBody()
		data class EnumBody(
			val entries:List<EnumEntry>,
			val members:List<ClassMemberDeclaration>
		):ClassDeclarationBody()
	}
}

data class PrimaryConstructor(
	val modifiers:Option<Modifiers>,
	val parameters:List<ClassParameter>
):KotlinAst

data class ClassParameter(
	val modifiers:Option<Modifiers>,
	val valVar:ValVar,
	val name:ESimpleIdentifier,
	val type:Type,
	val expr:Option<Expression>
):KotlinAst

sealed class ValVar:KotlinAst
{
	data object VAL:ValVar()
	data object VAR:ValVar()
}

typealias DelegationSpecifiers=NonEmptyList<AnnotatedDelegationSpecifier>

sealed class DelegationSpecifier:KotlinAst
{
	data class ConstructorInvocation(val type:UserType,val valueArguments:ValueArguments):DelegationSpecifier()
	data class ExplicitDelegation(
		val type:Either<UserType,FunctionType>,
		val expression:Expression
	):DelegationSpecifier()

	data class UserType(val userTypes:List<SimpleUserType>):DelegationSpecifier(),TypeReference
	data class FunctionType(val type:Type):DelegationSpecifier()
	data class SuspendFunctionType(val type:Type):DelegationSpecifier()
}

typealias ValueArguments=NonEmptyList<ValueArgument>

data class AnnotatedDelegationSpecifier(
	val annotations:List<Annotation>,
	val delegation:DelegationSpecifier
):KotlinAst

data class TypeParameter(
	val typeParameterModifiers:List<TypeParameterModifier>,
	val name:ESimpleIdentifier,
	val type:Option<Type>
):KotlinAst

//@TODO
typealias TypeConstraints=NonEmptyList<TypeConstraint>

data class TypeConstraint(
	val annotations:List<Annotation>,
	val name:ESimpleIdentifier,
	val type:Type
):KotlinAst

//delegationSpecifier
//    : constructorInvocation
//    | explicitDelegation
//    | userType
//    | functionType@TODO
//    | SUSPEND NL* functionType@TODO
//    ;
//
//// SECTION: classMembers

sealed interface ClassMemberDeclaration:KotlinAst

//classMemberDeclaration
//    : declaration
//    | companionObject
//    | anonymousInitializer@TODO
//    | secondaryConstructor@TODO
//    ;

data class AnonymousInitializer(val block:Block):ClassMemberDeclaration

data class CompanionObject(
	val modifiers:Option<Modifiers>,
	val isData:Boolean,
	val name:Option<ESimpleIdentifier>,
	val delegationSpecifiers:Option<DelegationSpecifiers>,
	val classBody:Option<ClassDeclaration.ClassDeclarationBody.ClassBody>
):ClassMemberDeclaration

typealias FunctionValueParameters=List<FunctionValueParameter>

data class FunctionValueParameter(
	val modifiers:Option<ParameterModifiers>
):KotlinAst
//functionValueParameter
//    : parameterModifiers? parameter (NL* ASSIGNMENT NL* expression)?
//    ;
//
//functionDeclaration
//    : modifiers?
//      FUN (NL* typeParameters)? (NL* receiverType NL* DOT)? NL* simpleIdentifier
//      NL* functionValueParameters
//      (NL* COLON NL* type)?
//      (NL* typeConstraints)?
//      (NL* functionBody)?
//    ;
//
//functionBody
//    : block
//    | ASSIGNMENT NL* expression
//    ;
//
//variableDeclaration
//    : annotation* NL* simpleIdentifier (NL* COLON NL* type)?
//    ;
//
//multiVariableDeclaration
//    : LPAREN NL* variableDeclaration (NL* COMMA NL* variableDeclaration)* (NL* COMMA)? NL* RPAREN
//    ;
//
//propertyDeclaration
//    : modifiers? (VAL | VAR)
//      (NL* typeParameters)?
//      (NL* receiverType NL* DOT)?
//      (NL* (multiVariableDeclaration | variableDeclaration))
//      (NL* typeConstraints)?
//      (NL* (ASSIGNMENT NL* expression | propertyDelegate))?
//      (NL* SEMICOLON)? NL* (getter? (NL* semi? setter)? | setter? (NL* semi? getter)?)
//    ;
//
//propertyDelegate
//    : BY NL* expression
//    ;
//
//getter
//    : modifiers? GET
//      (NL* LPAREN NL* RPAREN (NL* COLON NL* type)? NL* functionBody)?
//    ;
//
//setter
//    : modifiers? SET
//      (NL* LPAREN NL* functionValueParameterWithOptionalType (NL* COMMA)? NL* RPAREN (NL* COLON NL* type)? NL* functionBody)?
//    ;
//
//parametersWithOptionalType
//    : LPAREN NL* (functionValueParameterWithOptionalType (NL* COMMA NL* functionValueParameterWithOptionalType)* (NL* COMMA)?)? NL* RPAREN
//    ;
//
//functionValueParameterWithOptionalType
//    : parameterModifiers? parameterWithOptionalType (NL* ASSIGNMENT NL* expression)?
//    ;
//
//parameterWithOptionalType
//    : simpleIdentifier NL* (COLON NL* type)?
//    ;
//
//parameter
//    : simpleIdentifier NL* COLON NL* type
//    ;
//
//objectDeclaration
//    : modifiers? OBJECT
//      NL* simpleIdentifier
//      (NL* COLON NL* delegationSpecifiers)?
//      (NL* classBody)?
//    ;
//
//secondaryConstructor
//    : modifiers? CONSTRUCTOR NL* functionValueParameters (NL* COLON NL* constructorDelegationCall)? NL* block?
//    ;
//
//constructorDelegationCall
//    : (THIS | SUPER) NL* valueArguments
//    ;
//
//// SECTION: enumClasses
//
//enumClassBody
//    : LCURL NL* enumEntries? (NL* SEMICOLON NL* classMemberDeclarations)? NL* RCURL
//    ;
//
//enumEntries
//    : enumEntry (NL* COMMA NL* enumEntry)* NL* COMMA?
//    ;
//
//enumEntry
//    : (modifiers NL*)? simpleIdentifier (NL* valueArguments)? (NL* classBody)?
//    ;
//
//// SECTION: types
//
data class Type(
	val modifiers:List<TypeModifier>,
	val type:TypeType,
):KotlinAst

sealed interface TypeType

sealed interface TypeReference:TypeType,RecieverTypeType

data object DynamicType:TypeReference

data class ParenthesizedType(val type:Type):TypeType,RecieverTypeType
data class NullableType(val type:Either<TypeReference,ParenthesizedType>):TypeType,RecieverTypeType

data class SimpleUserType(val name:ESimpleIdentifier,val args:List<TypeArgument>)

//type
//    : typeModifiers? (functionType | parenthesizedType | nullableType | typeReference | definitelyNonNullableType)
//    ;

sealed interface TypeProjection:KotlinAst
data class SimpleTypeProjection(val modifiers:List<TypeProjectionModifier>,val type:Type):TypeProjection
data object StarTypeProjection:TypeProjection

typealias TypeProjectionModifier=Either<VarianceModifier,Annotation>

data class FunctionType(
	val receiverType:Option<RecieverType>,
	val parameters:FunctionTypeParameters,
	val returnType:Type
):TypeType

typealias FunctionTypeParameters=List<Either<Parameter,Type>>

data class RecieverType(
	val modifiers:List<TypeModifier>,
	val type:RecieverTypeType
):KotlinAst

sealed interface RecieverTypeType:KotlinAst

data class ParenthesizedUserType(val type:Either<DelegationSpecifier.UserType,ParenthesizedUserType>)

data class DefinitelyNonNullableType(
	val leftTypeModifiers:List<TypeModifier>,
	val leftType:Either<DelegationSpecifier.UserType,ParenthesizedUserType>,
	val rightTypeModifiers:List<TypeModifier>,
	val rightType:Either<DelegationSpecifier.UserType,ParenthesizedUserType>
):TypeType

//// SECTION: statements
//

typealias Statements=NonEmptyList<Statement>

data class Statement(
	val labelOrAnnotation:Either<Label,Annotation>,
	val body:StatementType
):KotlinAst

sealed interface StatementType:KotlinAst

//statement
//    : (label | annotation)* ( declaration | assignment | loopStatement | expression)
//    ;
//

data class Label(val name:ESimpleIdentifier):KotlinAst

//label
//    : simpleIdentifier (AT_NO_WS | AT_POST_WS) NL*
//    ;
//
//controlStructureBody
//    : block
//    | statement
//    ;
//

data class Block(val statements:NonEmptyList<Statement>):KotlinAst

//loopStatement
//    : forStatement
//    | whileStatement
//    | doWhileStatement
//    ;
//
//forStatement
//    : FOR NL* LPAREN annotation* (variableDeclaration | multiVariableDeclaration)
//      IN expression RPAREN NL* controlStructureBody?
//    ;
//
//whileStatement
//    : WHILE NL* LPAREN expression RPAREN NL* (controlStructureBody | SEMICOLON)
//    ;
//
//doWhileStatement
//    : DO NL* controlStructureBody? NL* WHILE NL* LPAREN expression RPAREN
//    ;
//
//assignment
//    : (directlyAssignableExpression ASSIGNMENT | assignableExpression assignmentAndOperator) NL* expression
//    ;
//
//semi
//    : (SEMICOLON | NL) NL*
//    ;
//
//semis
//    : (SEMICOLON | NL)+
//    ;
//
//// SECTION: expressions
//
//expression
//    : disjunction
//    ;
//
//disjunction
//    : conjunction (NL* DISJ NL* conjunction)*
//    ;
//
//conjunction
//    : equality (NL* CONJ NL* equality)*
//    ;
//
//equality
//    : comparison (equalityOperator NL* comparison)*
//    ;
//
//comparison
//    : genericCallLikeComparison (comparisonOperator NL* genericCallLikeComparison)*
//    ;
//
//genericCallLikeComparison
//    : infixOperation callSuffix*
//    ;
//
//infixOperation
//    : elvisExpression (inOperator NL* elvisExpression | isOperator NL* type)*
//    ;
//
//elvisExpression
//    : infixFunctionCall (NL* elvis NL* infixFunctionCall)*
//    ;
//
//elvis
//    : QUEST_NO_WS COLON
//    ;
//
//infixFunctionCall
//    : rangeExpression (simpleIdentifier NL* rangeExpression)*
//    ;
//
//rangeExpression
//    : additiveExpression ((RANGE | RANGE_UNTIL) NL* additiveExpression)*
//    ;
//
//additiveExpression
//    : multiplicativeExpression (additiveOperator NL* multiplicativeExpression)*
//    ;
//
//multiplicativeExpression
//    : asExpression (multiplicativeOperator NL* asExpression)*
//    ;
//
//asExpression
//    : prefixUnaryExpression (NL* asOperator NL* type)*
//    ;
//
//prefixUnaryExpression
//    : unaryPrefix* postfixUnaryExpression
//    ;
//
//unaryPrefix
//    : annotation
//    | label
//    | prefixUnaryOperator NL*
//    ;
//
//postfixUnaryExpression
//    : primaryExpression postfixUnarySuffix*
//    ;
//
//postfixUnarySuffix
//    : postfixUnaryOperator
//    | typeArguments
//    | callSuffix
//    | indexingSuffix
//    | navigationSuffix
//    ;
//
//directlyAssignableExpression
//    : postfixUnaryExpression assignableSuffix
//    | simpleIdentifier
//    | parenthesizedDirectlyAssignableExpression
//    ;
//
//parenthesizedDirectlyAssignableExpression
//    : LPAREN NL* directlyAssignableExpression NL* RPAREN
//    ;
//
//assignableExpression
//    : prefixUnaryExpression
//    | parenthesizedAssignableExpression
//    ;
//
//parenthesizedAssignableExpression
//    : LPAREN NL* assignableExpression NL* RPAREN
//    ;
//
//assignableSuffix
//    : typeArguments
//    | indexingSuffix
//    | navigationSuffix
//    ;
//
//indexingSuffix
//    : LSQUARE NL* expression (NL* COMMA NL* expression)* (NL* COMMA)? NL* RSQUARE
//    ;
//
//navigationSuffix
//    : memberAccessOperator NL* (simpleIdentifier | parenthesizedExpression | CLASS)
//    ;
//
//callSuffix
//    : typeArguments? (valueArguments? annotatedLambda | valueArguments)
//    ;
//
//annotatedLambda
//    : annotation* label? NL* lambdaLiteral
//    ;
//
//typeArguments
//    : LANGLE NL* typeProjection (NL* COMMA NL* typeProjection)* (NL* COMMA)? NL* RANGLE
//    ;
//
//valueArguments
//    : LPAREN NL* (valueArgument (NL* COMMA NL* valueArgument)* (NL* COMMA)? NL*)? RPAREN
//    ;
//
//valueArgument
//    : annotation? NL* (simpleIdentifier NL* ASSIGNMENT NL*)? MULT? NL* expression
//    ;
//
//primaryExpression
//    : parenthesizedExpression
//    | simpleIdentifier
//    | literalConstant
//    | stringLiteral
//    | callableReference
//    | functionLiteral
//    | objectLiteral
//    | collectionLiteral
//    | thisExpression
//    | superExpression
//    | ifExpression
//    | whenExpression
//    | tryExpression
//    | jumpExpression
//    ;
//
//parenthesizedExpression
//    : LPAREN NL* expression NL* RPAREN
//    ;
//
//collectionLiteral
//    : LSQUARE NL* (expression (NL* COMMA NL* expression)* (NL* COMMA)? NL*)? RSQUARE
//    ;
//
//literalConstant
//    : BooleanLiteral
//    | IntegerLiteral
//    | HexLiteral
//    | BinLiteral
//    | CharacterLiteral
//    | RealLiteral
//    | NullLiteral
//    | LongLiteral
//    | UnsignedLiteral
//    ;
//
//stringLiteral
//    : lineStringLiteral
//    | multiLineStringLiteral
//    ;
//
//lineStringLiteral
//    : QUOTE_OPEN (lineStringContent | lineStringExpression)* QUOTE_CLOSE
//    ;
//
//multiLineStringLiteral
//    : TRIPLE_QUOTE_OPEN (multiLineStringContent | multiLineStringExpression | MultiLineStringQuote)* TRIPLE_QUOTE_CLOSE
//    ;
//
//lineStringContent
//    : LineStrText
//    | LineStrEscapedChar
//    | LineStrRef
//    ;
//
//lineStringExpression
//    : LineStrExprStart NL* expression NL* RCURL
//    ;
//
//multiLineStringContent
//    : MultiLineStrText
//    | MultiLineStringQuote
//    | MultiLineStrRef
//    ;
//
//multiLineStringExpression
//    : MultiLineStrExprStart NL* expression NL* RCURL
//    ;
//
//lambdaLiteral
//    : LCURL NL* (lambdaParameters? NL* ARROW NL*)? statements NL* RCURL
//    ;
//
//lambdaParameters
//    : lambdaParameter (NL* COMMA NL* lambdaParameter)* (NL* COMMA)?
//    ;
//
//lambdaParameter
//    : variableDeclaration
//    | multiVariableDeclaration (NL* COLON NL* type)?
//    ;
//
//anonymousFunction
//    : SUSPEND?
//      NL*
//      FUN
//      (NL* type NL* DOT)?
//      NL* parametersWithOptionalType
//      (NL* COLON NL* type)?
//      (NL* typeConstraints)?
//      (NL* functionBody)?
//    ;
//
//functionLiteral
//    : lambdaLiteral
//    | anonymousFunction
//    ;
//
//objectLiteral
//    : DATA? NL* OBJECT (NL* COLON NL* delegationSpecifiers NL*)? (NL* classBody)?
//    ;
//
//thisExpression
//    : THIS
//    | THIS_AT
//    ;
//
//superExpression
//    : SUPER (LANGLE NL* type NL* RANGLE)? (AT_NO_WS simpleIdentifier)?
//    | SUPER_AT
//    ;
//
//ifExpression
//    : IF NL* LPAREN NL* expression NL* RPAREN NL*
//      ( controlStructureBody
//      | controlStructureBody? NL* SEMICOLON? NL* ELSE NL* (controlStructureBody | SEMICOLON)
//      | SEMICOLON)
//    ;
//
//whenSubject
//    : LPAREN (annotation* NL* VAL NL* variableDeclaration NL* ASSIGNMENT NL*)? expression RPAREN
//    ;
//
//whenExpression
//    : WHEN NL* whenSubject? NL* LCURL NL* (whenEntry NL*)* NL* RCURL
//    ;
//
//whenEntry
//    : whenCondition (NL* COMMA NL* whenCondition)* (NL* COMMA)? NL* ARROW NL* controlStructureBody semi?
//    | ELSE NL* ARROW NL* controlStructureBody semi?
//    ;
//
//whenCondition
//    : expression
//    | rangeTest
//    | typeTest
//    ;
//
//rangeTest
//    : inOperator NL* expression
//    ;
//
//typeTest
//    : isOperator NL* type
//    ;
//
//tryExpression
//    : TRY NL* block ((NL* catchBlock)+ (NL* finallyBlock)? | NL* finallyBlock)
//    ;
//
//catchBlock
//    : CATCH NL* LPAREN annotation* simpleIdentifier COLON type (NL* COMMA)? RPAREN NL* block
//    ;
//
//finallyBlock
//    : FINALLY NL* block
//    ;
//
//jumpExpression
//    : THROW NL* expression
//    | (RETURN | RETURN_AT) expression?
//    | CONTINUE
//    | CONTINUE_AT
//    | BREAK
//    | BREAK_AT
//    ;
//
//callableReference
//    : receiverType? COLONCOLON NL* (simpleIdentifier | CLASS)
//    ;
//
//assignmentAndOperator
//    : ADD_ASSIGNMENT
//    | SUB_ASSIGNMENT
//    | MULT_ASSIGNMENT
//    | DIV_ASSIGNMENT
//    | MOD_ASSIGNMENT
//    ;
//
//equalityOperator
//    : EXCL_EQ
//    | EXCL_EQEQ
//    | EQEQ
//    | EQEQEQ
//    ;
//
//comparisonOperator
//    : LANGLE
//    | RANGLE
//    | LE
//    | GE
//    ;
//
//inOperator
//    : IN
//    | NOT_IN
//    ;
//
//isOperator
//    : IS
//    | NOT_IS
//    ;
//
//additiveOperator
//    : ADD
//    | SUB
//    ;
//
//multiplicativeOperator
//    : MULT
//    | DIV
//    | MOD
//    ;
//
//asOperator
//    : AS
//    | AS_SAFE
//    ;
//
//prefixUnaryOperator
//    : INCR
//    | DECR
//    | SUB
//    | ADD
//    | excl
//    ;
//
//postfixUnaryOperator
//    : INCR
//    | DECR
//    | EXCL_NO_WS excl
//    ;
//
//excl
//    : EXCL_NO_WS
//    | EXCL_WS
//    ;
//
//memberAccessOperator
//    : NL* DOT
//    | NL* safeNav
//    | COLONCOLON
//    ;
//
//safeNav
//    : QUEST_NO_WS DOT
//    ;
//
//// SECTION: modifiers
//
//modifiers
//    : (annotation | modifier)+
//    ;
//

typealias ParameterModifiers=NonEmptyList<Either<Annotation,ParameterModifier>>

//modifier
//    : (classModifier
//    | memberModifier
//    | visibilityModifier
//    | functionModifier
//    | propertyModifier
//    | inheritanceModifier
//    | parameterModifier
//    | platformModifier) NL*
//    ;
//
//typeModifiers
//    : typeModifier+
//    ;
//

sealed interface TypeModifier
//typeModifier
//    : annotation
//    | SUSPEND NL*
//    ;
//
//classModifier
//    : ENUM
//    | SEALED
//    | ANNOTATION
//    | DATA
//    | INNER
//    | VALUE
//    ;
//
//memberModifier
//    : OVERRIDE
//    | LATEINIT
//    ;
//
//visibilityModifier
//    : PUBLIC
//    | PRIVATE
//    | INTERNAL
//    | PROTECTED
//    ;
//
//varianceModifier
//    : IN
//    | OUT
//    ;
//
//typeParameterModifiers
//    : typeParameterModifier+
//    ;
//
//typeParameterModifier
//    : reificationModifier NL*
//    | varianceModifier NL*
//    | annotation
//    ;
//
//functionModifier
//    : TAILREC
//    | OPERATOR
//    | INFIX
//    | INLINE
//    | EXTERNAL
//    | SUSPEND
//    ;
//
//propertyModifier
//    : CONST
//    ;
//
//inheritanceModifier
//    : ABSTRACT
//    | FINAL
//    | OPEN
//    ;
//
//parameterModifier
//    : VARARG
//    | NOINLINE
//    | CROSSINLINE
//    ;
//
//reificationModifier
//    : REIFIED
//    ;
//
//platformModifier
//    : EXPECT
//    | ACTUAL
//    ;
//
//// SECTION: annotations
//
typealias Annotation=Either<SingleAnnotation,MultiAnnotation>

data class SingleAnnotation(val targetOrAt:Either<AnnotationUseSiteTarget,AT>,val anno:UnescapedAnnotation):KotlinAst
data object AT:AnnotationUseSiteTarget

data class MultiAnnotation(
	val targetOrAt:Either<AnnotationUseSiteTarget,AT>,
	val annos:NonEmptyList<UnescapedAnnotation>
):KotlinAst

sealed interface AnnotationUseSiteTarget
{
	data object FIELD:AnnotationUseSiteTarget
	data object PROPERTY:AnnotationUseSiteTarget
	data object GET:AnnotationUseSiteTarget
	data object SET:AnnotationUseSiteTarget
	data object RECEIVER:AnnotationUseSiteTarget
	data object PARAM:AnnotationUseSiteTarget
	data object SETPARAM:AnnotationUseSiteTarget
	data object DELEGATE:AnnotationUseSiteTarget
}

sealed interface unescapedAnnotation
//unescapedAnnotation
//    : constructorInvocation
//    | userType
//    ;
//
//// SECTION: identifiers
//

sealed class ESimpleIdentifier(val text:String)
{
	data class IDENTIFIER(val identifier:String):ESimpleIdentifier(identifier)
	data object ABSTRACT:ESimpleIdentifier("abstract")
	data object ANNOTATION:ESimpleIdentifier("annotation")
	data object BY:ESimpleIdentifier("by")
	data object CATCH:ESimpleIdentifier("catch")
	data object COMPANION:ESimpleIdentifier("companion")
	data object CONSTRUCTOR:ESimpleIdentifier("constructor")
	data object CROSSINLINE:ESimpleIdentifier("crossinline")
	data object DATA:ESimpleIdentifier("data")
	data object DYNAMIC:ESimpleIdentifier("dynamic")
	data object ENUM:ESimpleIdentifier("enum")
	data object EXTERNAL:ESimpleIdentifier("external")
	data object FINAL:ESimpleIdentifier("final")
	data object FINALLY:ESimpleIdentifier("finally")
	data object GET:ESimpleIdentifier("get")
	data object IMPORT:ESimpleIdentifier("import")
	data object INFIX:ESimpleIdentifier("infix")
	data object INIT:ESimpleIdentifier("init")
	data object INLINE:ESimpleIdentifier("inline")
	data object INNER:ESimpleIdentifier("inner")
	data object INTERNAL:ESimpleIdentifier("internal")
	data object LATEINIT:ESimpleIdentifier("lateinit")
	data object NOINLINE:ESimpleIdentifier("noinline")
	data object OPEN:ESimpleIdentifier("open")
	data object OPERATOR:ESimpleIdentifier("operator")
	data object OUT:ESimpleIdentifier("out")
	data object OVERRIDE:ESimpleIdentifier("override")
	data object PRIVATE:ESimpleIdentifier("private")
	data object PROTECTED:ESimpleIdentifier("protected")
	data object PUBLIC:ESimpleIdentifier("public")
	data object REIFIED:ESimpleIdentifier("reified")
	data object SEALED:ESimpleIdentifier("sealed")
	data object TAILREC:ESimpleIdentifier("tailrec")
	data object SET:ESimpleIdentifier("set")
	data object VARARG:ESimpleIdentifier("vararg")
	data object WHERE:ESimpleIdentifier("where")
	data object FIELD:ESimpleIdentifier("field")
	data object PROPERTY:ESimpleIdentifier("property")
	data object RECEIVER:ESimpleIdentifier("receiver")
	data object PARAM:ESimpleIdentifier("param")
	data object SETPARAM:ESimpleIdentifier("setparam")
	data object DELEGATE:ESimpleIdentifier("delegate")
	data object FILE:ESimpleIdentifier("file")
	data object EXPECT:ESimpleIdentifier("expect")
	data object ACTUAL:ESimpleIdentifier("actual")
	data object CONST:ESimpleIdentifier("const")
	data object SUSPEND:ESimpleIdentifier("suspend")
	data object VALUE:ESimpleIdentifier("value")
}

data class Identifier(val simpleIdentifiers:List<ESimpleIdentifier>)
//
//identifier
//    : simpleIdentifier (NL* DOT simpleIdentifier)*
//    ;