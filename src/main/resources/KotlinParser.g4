/**
 * Kotlin syntax grammar in ANTLR4 notation
 */

parser grammar KotlinParser;

options { tokenVocab = KotlinLexer; }

// SECTION: general

rkotlinFile: ShebangLine? rfileAnnotation* rpackageHeader importList=rimportHeader* rtopLevelObject*;

rscript: ShebangLine? rfileAnnotation* rpackageHeader importList=rimportHeader* rstatement*;

//@file:[anno1, anno2,...]
//@file:anno
rfileAnnotation: runescapedAnnotation+;

//package aboba
rpackageHeader: ridentifier?;

//.* or importAlias
rimportHeader: ridentifier (DOT MULT | rimportAlias)?;

rimportAlias: rsimpleIdentifier;

rtopLevelObject: rdeclaration;

rtypeAlias: modifiers? rsimpleIdentifier rtypeParameters? rtype;

rdeclaration: rclassDeclaration
    | robjectDeclaration
    | rfunctionDeclaration
    | rpropertyDeclaration
    | rtypeAlias;

// SECTION: classes

rclassDeclaration: modifiers? (CLASS | FUN? INTERFACE) rsimpleIdentifier
      rtypeParameters? rprimaryConstructor?
      rdelegationSpecifiers?
      rtypeConstraints?
      (rclassBody | renumClassBody)?;


rprimaryConstructor: modifiers? rclassParameters;

rclassBody: rclassMemberDeclarations;
rclassParameters: rclassParameter+?;
rclassParameter: modifiers? (VAL | VAR)? rsimpleIdentifier rtype (expression)?;

rdelegationSpecifiers: rannotatedDelegationSpecifier+;

rdelegationSpecifier: rconstructorInvocation
    | rexplicitDelegation
    | ruserType
    | rfunctionType
    | SUSPEND rfunctionType;

rconstructorInvocation: ruserType valueArguments;

rannotatedDelegationSpecifier: rannotation* rdelegationSpecifier;

rexplicitDelegation: (ruserType | rfunctionType) expression;

rtypeParameters: rtypeParameter+;
rtypeParameter: typeParameterModifiers? rsimpleIdentifier rtype?;
rtypeConstraints: rtypeConstraint+;
rtypeConstraint: rannotation* rsimpleIdentifier rtype;

// SECTION: classMembers

rclassMemberDeclarations: rclassMemberDeclaration*;

rclassMemberDeclaration: rdeclaration
    | rcompanionObject
    | ranonymousInitializer
    | rsecondaryConstructor;

ranonymousInitializer: rblock;

rcompanionObject: modifiers? DATA?
      rsimpleIdentifier?
      rdelegationSpecifiers?
      rclassBody?;

rfunctionValueParameters: rfunctionValueParameter+?;

rfunctionValueParameter: parameterModifiers? rparameter expression?;

rfunctionDeclaration: modifiers?
      rtypeParameters? rreceiverType? rsimpleIdentifier
      rfunctionValueParameters
      rtype?
      rtypeConstraints?
      rfunctionBody?;

rfunctionBody: rblock | ASSIGNMENT expression;

rvariableDeclaration: rannotation* rsimpleIdentifier rtype?;

rmultiVariableDeclaration: rvariableDeclaration+;

rpropertyDeclaration: modifiers? (VAL | VAR)
      rtypeParameters?
      rreceiverType? //A.
      (rmultiVariableDeclaration | rvariableDeclaration)
      rtypeConstraints?
      (expression //= 4
      | rpropertyDelegate //by expr
      )?
      rgetter? rsetter?;


//by expr
rpropertyDelegate: expression;

rgetter: modifiers? (rtype? rfunctionBody)?;

rsetter: modifiers? (rfunctionValueParameterWithOptionalType rtype? rfunctionBody)?;

rparametersWithOptionalType: rfunctionValueParameterWithOptionalType+?;

rfunctionValueParameterWithOptionalType: parameterModifiers? rparameterWithOptionalType expression?;

rparameterWithOptionalType: rsimpleIdentifier rtype?;

rparameter: rsimpleIdentifier rtype;


robjectDeclaration: modifiers?
      rsimpleIdentifier
      rdelegationSpecifiers?
      rclassBody?;

rsecondaryConstructor: modifiers? CONSTRUCTOR rfunctionValueParameters rconstructorDelegationCall? rblock?;

rconstructorDelegationCall: (THIS | SUPER) valueArguments;

// SECTION: enumClasses

renumClassBody: renumEntries? rclassMemberDeclarations?;

renumEntries: renumEntry+;

renumEntry: modifiers? rsimpleIdentifier valueArguments? rclassBody?;

// SECTION: types


rtype: typeModifier+? (rfunctionType | rparenthesizedType | rnullableType | rtypeReference | rdefinitelyNonNullableType);

rtypeReference: ruserType | DYNAMIC;

//A?

rnullableType: rtypeReference | rparenthesizedType;

ruserType: rsimpleUserType+;

rsimpleUserType: rsimpleIdentifier typeArguments?;

rtypeProjection: rtypeProjectionModifiers? rtype | MULT;

rtypeProjectionModifiers: rtypeProjectionModifier+;

rtypeProjectionModifier: varianceModifier | rannotation;

//A.<B,C> -> D
rfunctionType: rreceiverType? rfunctionTypeParameters rtype;

rfunctionTypeParameters: (rparameter | rtype)*;

//(A)
rparenthesizedType: rtype;

rreceiverType: typeModifier+? (rparenthesizedType | rnullableType | rtypeReference);

rparenthesizedUserType: ruserType | rparenthesizedUserType;

rdefinitelyNonNullableType: typeModifier+? (ruserType | rparenthesizedUserType) typeModifier+? (ruserType | rparenthesizedUserType);

// SECTION: statements

rstatement: (rlabel | rannotation)* ( rdeclaration | rassignment | rloopStatement | expression);

rlabel: rsimpleIdentifier AT_NO_WS;

rcontrolStructureBody: rblock | rstatement;

rblock: rstatement+?;

rloopStatement: rforStatement
    | rwhileStatement
    | rdoWhileStatement;


rforStatement: rannotation* (rvariableDeclaration | rmultiVariableDeclaration)
      expression rcontrolStructureBody?;

rwhileStatement: expression (rcontrolStructureBody | SEMICOLON);

rdoWhileStatement: rcontrolStructureBody? expression;

rassignment: (directlyAssignableExpression | assignableExpression assignmentAndOperator) expression;

// SECTION: expressions

expression: disjunction;

disjunction: conjunction (DISJ conjunction)*;

conjunction: equality (CONJ equality)*;

equality: comparison (equalityOperator comparison)*;

comparison: genericCallLikeComparison (comparisonOperator genericCallLikeComparison)*;

genericCallLikeComparison: infixOperation callSuffix*;

infixOperation: elvisExpression (inOperator elvisExpression | typeTest)*;

elvisExpression: infixFunctionCall*;

infixFunctionCall: rangeExpression (rsimpleIdentifier rangeExpression)*;

rangeRangeUntil: RANGE | RANGE_UNTIL;

rangeExpression: additiveExpression (rangeRangeUntil additiveExpression)*;

additiveExpression: multiplicativeExpression (additiveOperator multiplicativeExpression)*;

multiplicativeExpression: asExpression (multiplicativeOperator asExpression)*;

asExpression: prefixUnaryExpression (asOperator rtype)*;

prefixUnaryExpression: unaryPrefix* postfixUnaryExpression;

unaryPrefix: rannotation
    | rlabel
    | prefixUnaryOperator;

postfixUnaryExpression: primaryExpression postfixUnarySuffix*;

postfixUnarySuffix: postfixUnaryOperator
    | typeArguments
    | callSuffix
    | indexingSuffix
    | navigationSuffix;

directlyAssignableExpression: postfixUnaryExpression assignableSuffix
    | rsimpleIdentifier
    | parenthesizedDirectlyAssignableExpression;

parenthesizedDirectlyAssignableExpression: LPAREN directlyAssignableExpression RPAREN;

assignableExpression: prefixUnaryExpression
    | parenthesizedAssignableExpression;

parenthesizedAssignableExpression: LPAREN assignableExpression RPAREN;

assignableSuffix: typeArguments
    | indexingSuffix
    | navigationSuffix;

indexingSuffix: expression+;

navigationSuffix: memberAccessOperator (rsimpleIdentifier | parenthesizedExpression | CLASS);

callSuffix: typeArguments? (valueArguments? annotatedLambda | valueArguments);

annotatedLambda: rannotation* rlabel? lambdaLiteral;

typeArguments: rtypeProjection+;

valueArguments: valueArgument+; //opt

valueArgument: rannotation? (rsimpleIdentifier ASSIGNMENT)? MULT? expression;

primaryExpression: parenthesizedExpression
    | rsimpleIdentifier
    | literalConstant
    | stringLiteral
    | callableReference
    | functionLiteral
    | objectLiteral
    | collectionLiteral
    | thisExpression
    | superExpression
    | ifExpression
    | whenExpression
    | tryExpression
    | jumpExpression;

parenthesizedExpression: expression;

collectionLiteral: expression+?;

literalConstant: BooleanLiteral
    | IntegerLiteral
    | HexLiteral
    | BinLiteral
    | CharacterLiteral
    | RealLiteral
    | NullLiteral
    | LongLiteral
    | UnsignedLiteral;

stringLiteral: lineStringLiteral | multiLineStringLiteral;

lineStringLiteral: (lineStringContent | lineStringExpression)*;

multiLineStringLiteral: (multiLineStringContent | multiLineStringExpression | MultiLineStringQuote)*;

lineStringContent: LineStrText
    | LineStrEscapedChar
    | LineStrRef;

lineStringExpression: expression;

multiLineStringContent: MultiLineStrText
    | MultiLineStringQuote
    | MultiLineStrRef;

multiLineStringExpression: expression;

lambdaLiteral: lambdaParameters? rstatement+?;

lambdaParameters: lambdaParameter+;

lambdaParameter: rvariableDeclaration
    | rmultiVariableDeclaration rtype?;

anonymousFunction: SUSPEND?
      rtype?
      rparametersWithOptionalType
      rtype?
      rtypeConstraints?
      rfunctionBody?;

functionLiteral: lambdaLiteral | anonymousFunction;

objectLiteral: DATA? (COLON rdelegationSpecifiers)? rclassBody?;

thisExpression: THIS | THIS_AT;

superExpression: rtype? rsimpleIdentifier? | SUPER_AT;

ifExpression: expression
      ( rcontrolStructureBody
      | rcontrolStructureBody? ELSE (rcontrolStructureBody | SEMICOLON)
      | SEMICOLON);

whenSubject: (rannotation* rvariableDeclaration)? expression;

whenExpression: whenSubject? whenEntry*;

whenEntry: whenCondition+ rcontrolStructureBody
    | rcontrolStructureBody;

whenCondition: expression
    | rangeTest
    | typeTest;

rangeTest: inOperator expression;

typeTest: isOperator rtype;

tryExpression: rblock (catchBlock+ finallyBlock? | finallyBlock);

catchBlock: rannotation* rsimpleIdentifier rtype rblock;

finallyBlock: rblock;

jumpExpression: THROW expression //throw
    | (RETURN | RETURN_AT) expression? //return
    | CONTINUE //continue
    | CONTINUE_AT //continueAt
    | BREAK //break
    | BREAK_AT; //breakAt

callableReference: rreceiverType? (rsimpleIdentifier | CLASS);

assignmentAndOperator: ADD_ASSIGNMENT
    | SUB_ASSIGNMENT
    | MULT_ASSIGNMENT
    | DIV_ASSIGNMENT
    | MOD_ASSIGNMENT;

equalityOperator: EXCL_EQ
    | EXCL_EQEQ
    | EQEQ
    | EQEQEQ;

comparisonOperator: LANGLE | RANGLE | LE | GE;

inOperator: IN | NOT_IN;
isOperator: IS | NOT_IS;

additiveOperator: ADD | SUB;
multiplicativeOperator: MULT | DIV | MOD;

asOperator: AS | AS_SAFE;

prefixUnaryOperator: INCR
    | DECR
    | SUB
    | ADD
    | EXCL_NO_WS;

postfixUnaryOperator: INCR
    | DECR
    | EXCL_NO_WS EXCL_NO_WS;

memberAccessOperator: DOT
    | safeNav
    | COLONCOLON;

//?.
safeNav:;

// SECTION: modifiers
modifiers: (rannotation | modifier)+;

parameterModifiers: (rannotation | rparameterModifier)+;

modifier: classModifier
    | memberModifier
    | visibilityModifier
    | functionModifier
    | CONST
    | rinheritanceModifier
    | rparameterModifier
    | rplatformModifier;

typeModifier: rannotation | SUSPEND;

classModifier: ENUM
    | SEALED
    | ANNOTATION
    | DATA
    | INNER
    | VALUE;

memberModifier: OVERRIDE
    | LATEINIT;

visibilityModifier: PUBLIC
    | PRIVATE
    | INTERNAL
    | PROTECTED;

varianceModifier: IN | OUT;


typeParameterModifiers: typeParameterModifier+;

typeParameterModifier: rreificationModifier
    | varianceModifier
    | rannotation;

functionModifier: TAILREC
    | OPERATOR
    | INFIX
    | INLINE
    | EXTERNAL
    | SUSPEND;

rinheritanceModifier: ABSTRACT
    | FINAL
    | OPEN;

rparameterModifier: VARARG
    | NOINLINE
    | CROSSINLINE;

rreificationModifier: REIFIED;

rplatformModifier: EXPECT | ACTUAL;

// SECTION: annotations

rannotation: rsingleAnnotation | rmultiAnnotation;

rsingleAnnotation: (rannotationUseSiteTarget | AT_NO_WS) runescapedAnnotation;

rmultiAnnotation: (rannotationUseSiteTarget | AT_NO_WS) runescapedAnnotation+;

rannotationUseSiteTarget: AT_NO_WS (FIELD | PROPERTY | GET | SET | RECEIVER | PARAM | SETPARAM | DELEGATE) COLON;

runescapedAnnotation: rconstructorInvocation | ruserType;

// SECTION: identifiers

//todo
rsimpleIdentifier: Identifier
    | ABSTRACT
    | ANNOTATION
    | BY
    | CATCH
    | COMPANION
    | CONSTRUCTOR
    | CROSSINLINE
    | DATA
    | DYNAMIC
    | ENUM
    | EXTERNAL
    | FINAL
    | FINALLY
    | GET
    | IMPORT
    | INFIX
    | INIT
    | INLINE
    | INNER
    | INTERNAL
    | LATEINIT
    | NOINLINE
    | OPEN
    | OPERATOR
    | OUT
    | OVERRIDE
    | PRIVATE
    | PROTECTED
    | PUBLIC
    | REIFIED
    | SEALED
    | TAILREC
    | SET
    | VARARG
    | WHERE
    | FIELD
    | PROPERTY
    | RECEIVER
    | PARAM
    | SETPARAM
    | DELEGATE
    | FILE
    | EXPECT
    | ACTUAL
    | CONST
    | SUSPEND
    | VALUE;

ridentifier: rsimpleIdentifier+;