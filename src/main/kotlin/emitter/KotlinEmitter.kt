package emitter

import arrow.core.NonEmptyList
import arrow.core.toNonEmptyListOrNone
import ast.*

class KotlinEmitter:CodeEmitter<String>
{
	override fun emitKClassBody(node:KClassBody):String=
		if(node.memberDeclarations.isEmpty()) ""
		else node.memberDeclarations.joinToString(
			";",
			"{",
			"}",
			transform=::emitKClassMemberDeclaration
		)

	override fun emitKFunctionValueParameter(node:KFunctionValueParameter):String=buildString {
		node.modifiers.onSome {append(emitKParameterModifiers(it)).append(' ')}
		append(emitKParameter(node.parameter))
		node.expression.onSome {append(" = ").append(emitKExpression(it))}
	}

	override fun emitKFunctionDeclaration(node:KFunctionDeclaration):String=node.run {
		buildString {
			modifiers.toNonEmptyListOrNone().onSome {
				append(emitKModifiers(it))
				append(' ')
			}
			append("fun ")
			typeParameters.toNonEmptyListOrNone().onSome {
				append(it.joinToString(", ","<",">") {emitKTypeParameter(it)})
				append(' ')
			}
			receiverType.onSome {
				append(emitKType(it))
				append('.')
			}
			append(emitKSimpleIdentifier(identifier))
			functionValueParameters.joinToString("","(",")") {
				emitKFunctionValueParameter(it)
			}.also(::append)
			type.onSome {
				append(": ")
				append(emitKType(it))
				append(' ')
			}
			if(typeConstraints.isNotEmpty())
			{
				append(typeConstraints.joinToString(", ","where "," ") {emitKTypeConstraint(it)})
			}
			functionBody.onSome {
				append(emitKFunctionBody(it))
			}
		}
	}

	override fun emitKWhenExpression(node:KWhenExpression):String=buildString {
		//'when' whenSubject? '{' whenEntry* '}'
		append("when")
		node.subject.onSome {
			append(emitKWhenSubject(it))
		}
		append('{')
		node.entries.forEach {
			append(emitKWhenEntry(it))
		}
		append('}')
	}

	override fun emitKWhenEntry(node:KWhenEntry):String=when(node)
	{
		is KWhenElseEntry->emitKWhenElseEntry(node)
		is KWhenEntryWithConditions->emitKWhenEntryWithConditions(node)
	}

	override fun emitKWhenEntryWithConditions(node:KWhenEntryWithConditions):String=buildString {
		append(node.conditions.joinToString {
			emitKWhenCondition(it)
		})
		append(" -> ")
		append(emitKControlStructureBody(node.body))
		append(';')
	}

	override fun emitKWhenCondition(node:KWhenCondition):String=when(node)
	{
		is KExpression->emitKExpression(node)
		is KRangeTest->emitKRangeTest(node)
		is KTypeTest->emitKTypeTest(node)
	}

	override fun emitKTypeTest(node:KTypeTest):String=
		emitEIsOperator(node.left)+emitKType(node.right)

	override fun emitEIsOperator(node:EIsOperator):String=when(node)
	{
		EIsOperator.Is->"is"
		EIsOperator.NotIs->"!is"
	}

	override fun emitKWhenElseEntry(node:KWhenElseEntry):String=
		"else -> ${emitKControlStructureBody(node.body)};"

	override fun emitKWhenSubject(node:KWhenSubject):String=buildString {
		append('(')
		node.decl.onSome {
			append(emitKVariableDeclarationWithAnnotations(it))
		}
		append(emitKExpression(node.expression))
		append(')')
	}

	override fun emitKFunctionAssignExpression(node:KFunctionAssignExpression):String=
		"= ${emitKExpression(node.expression)}"

	override fun emitKVariableDeclaration(node:KVariableDeclaration):String=buildString {
		node.annotations.forEach {
			append(emitKAnnotation(it))
			append(' ')
		}
		append(emitKSimpleIdentifier(node.identifier))
		node.type.onSome {
			append(':')
			append(emitKType(it))
		}
	}

	override fun emitKPropertyDeclaration(node:KPropertyDeclaration):String=buildString {
		node.run {
			modifiers.toNonEmptyListOrNone().onSome {
				append(emitKModifiers(it))
				append(' ')
			}
			append(if(isVal) "val " else "var ")
			typeParameters.toNonEmptyListOrNone().onSome {
				append(it.joinToString(", ","<",">") {emitKTypeParameter(it)})
				append(' ')
			}
			recieverType.onSome {
				append(emitKReceiverType(it))
				append('.')
			}
			append(emitKMultiOrSingleVariableDeclaration(declaration))
			append(' ')
			if(typeConstraints.isNotEmpty())
			{
				append(typeConstraints.joinToString(", ","where "," ") {emitKTypeConstraint(it)})
			}
			append(if(byDelegate) "by " else "= ")
			append(emitKExpression(expr))
			getter.onSome {
				append(emitKGetter(it))
				append(' ')
			}
			setter.onSome {
				append(emitKSetter(it))
			}
		}
	}

	override fun emitKMultiVariableDeclaration(
		node:KMultiVariableDeclaration
	):String=node.variableDeclarations.joinToString(
		",",
		"(",
		")",
		transform=::emitKVariableDeclaration
	)

	override fun emitKParametersWithOptionalType(node:KParametersWithOptionalType):String=
		node.params.joinToString(",","(",")") {
			emitKFunctionValueParameterWithOptionalType(it)
		}

	override fun emitKFunctionValueParameterWithOptionalType(
		node:KFunctionValueParameterWithOptionalType
	):String=buildString {
		node.modifiers.onSome {
			append(emitKParameterModifiers(it))
			append(' ')
		}
		append(emitKParameterWithOptionalType(node.parameter))
		node.expression.onSome {
			append('=')
			append(emitKExpression(it))
		}
	}

	override fun emitKParameterWithOptionalType(node:KParameterWithOptionalType):String=buildString {
		append(emitKSimpleIdentifier(node.identifier))
		node.type.onSome {
			append(':')
			append(emitKType(it))
		}
	}

	override fun emitKParameter(node:KParameter):String=
		"${emitKSimpleIdentifier(node.identifier)} : ${emitKType(node.type)}"

	override fun emitKType(node:KType):String=buildString {
		node.modifiers.forEachIndexed {i,mod->
			append(emitKTypeModifier(mod))
			if(i!=node.modifiers.size-1) append(' ')
		}
		if(node.modifiers.isNotEmpty()) append(' ')
		append(emitKTypeInner(node.typeInner))
	}

	override fun emitKClassDeclaration(node:KClassDeclaration):String=buildString {
		node.run {
			modifiers.toNonEmptyListOrNone().onSome {
				append(emitKModifiers(it))
				append(' ')
			}
			append(emitKClassHeaderType(classHeaderType))
			append(' ')
			append(emitKSimpleIdentifier(identifier))
			append(' ')
			typeParameters.toNonEmptyListOrNone().onSome {
				append(it.joinToString(", ","<",">") {emitKTypeParameter(it)})
				append(' ')
			}
			primaryConstructor.onSome {
				append(emitKPrimaryConstructor(it))
			}
			delegationSpecifiers.onSome {
				append(':')
				append(emitKDelegationSpecifiers(it))
			}
			if(typeConstraints.isNotEmpty())
			{
				append(typeConstraints.joinToString(", ","where ") {emitKTypeConstraint(it)})
			}
			append(emitKClassOrEnumBody(body))
		}
	}

	override fun emitKIdentifierInner(node:KIdentifierInner):String=node.name

	override fun emitKIfExpression(node:KIfExpression):String=buildString {
		append("if (")
		append(emitKExpression(node.expression))
		append(") ")
		append(emitKIfInner(node.inner))
	}

	override fun emitKIfInnerFull(node:KIfInnerFull):String=buildString {
		node.ifTrue.onSome {
			append(emitKControlStructureBody(it))
		}
		if(node.ifFalse !is KSemicolon)
		{
			append(" else ")
			append(emitKControlStructureBodyOrSemicolon(node.ifFalse))
		}
	}

	override fun emitKStatement(node:KStatement):String=buildString {
		node.labelOrAnnotations.asSequence()
			.map {emitKLabelOrAnnotation(it)}
			.forEach {append(it).append(' ')}
		append(emitKStatementInner(node.stat))
	}

	override fun emitKUserType(node:KUserType):String=
		node.types.joinToString(".") {emitKSimpleUserType(it)}

	override fun emitKSimpleUserType(node:KSimpleUserType):String=buildString {
		append(emitKSimpleIdentifier(node.identifier))
		node.typeArguments.onSome {
			append(emitKTypeArguments(it))
		}
	}

	override fun emitKConcreteTypeProjection(node:KConcreteTypeProjection):String=buildString {
		node.modifiers.forEach {
			append(emitKTypeProjectionModifier(it))
			append(' ')
		}
		append(emitKType(node.type))
	}

	override fun emitKFunctionType(node:KFunctionType):String=buildString {
		node.recieverType.onSome {
			append(emitKReceiverType(it))
			append('.')
		}
		append(node.parameters.joinToString(", ","(",")") {
			append(emitKFunctionTypeParameterInner(it))
		})
		append(" -> ")
		append(emitKType(node.returnType))
	}

	override fun emitKBlock(node:KBlock):String=buildString {
		append('{')
		node.statements.asSequence()
			.map {emitKStatement((it))}
			.forEach {append(it).append(';')}
		append('}')
	}

	override fun emitKBinaryExpression(node:KBinaryExpression):String=
		"${emitKExpression(node.left)} ${emitKBinaryOperator(node.op)} ${emitKExpression(node.right)}"

	override fun emitKPostfixUnaryExpression(node:KPostfixUnaryExpression):String=buildString {
		append(emitKPrimaryExpression(node.primaryExpression))
		node.suffixes.asSequence()
			.map {emitKPostfixUnarySuffix(it)}
			.forEach(::append)
	}

	override fun emitKNavigationSuffix(node:KNavigationSuffix):String=
		emitKMemberAccessOperator(node.memberAccessOperator)+
		emitKNavigationSuffixInner(node.suffix)

	override fun emitKIndexingSuffix(node:KIndexingSuffix):String=
		node.expressionList.joinToString(", ","[","]") {emitKExpression(it)}

	override fun emitKClassKeyword(node:KClassKeyword):String="class"

	override fun emitKParenthesizedExpression(node:KParenthesizedExpression):String=
		"(${emitKExpression(node.expression)})"

	override fun emitKCallSuffix(node:KCallSuffix):String=buildString {
		node.args.onSome {append(emitKTypeArguments(it))}
		append(emitKCallSuffixInner(node.callSuffixInner))
	}

	override fun emitKValueArguments(node:KValueArguments):String=buildString {
		if(node.args.isEmpty()) return "()"
		val last=node.args.last()
		val preLast=node.args.dropLast(1)
		when(last.expression)
		{
			is KLambdaLiteral->
				when(node.args.size)
				{
					1->append(emitKValueArgument(last))
					else->
					{
						append(preLast.joinToString(", ","(",")") {emitKValueArgument(it)})
						append(emitKValueArgument(last))
					}
				}

			else->
				append(node.args.joinToString(", ","(",")") {emitKValueArgument(it)})
		}
	}

	override fun emitKTypeArguments(node:KTypeArguments):String=
		node.projections.joinToString(", ","<",">") {
			emitKTypeProjection(it)
		}

	override fun emitKValueArgument(node:KValueArgument):String=buildString {
		node.anno.onSome {
			append(emitKAnnotation(it))
			append(' ')
		}
		node.identifier.onSome {emitKSimpleIdentifier(it)}
		if(node.hasSpread) append("...")
		append(emitKExpression(node.expression))
	}

	override fun emitKIntegerLiteral(node:KIntegerLiteral):String=node.value.toString()

	override fun emitKLineStringLiteral(node:KLineStringLiteral):String=
		node.content.joinToString("","\"","\"") {
			emitKLineStringContentOrExpression(it)
		}

	override fun emitKLineStrText(node:KLineStrText):String=node.text

	override fun emitKLambdaLiteral(node:KLambdaLiteral):String=buildString {
		append("{")
		node.params.forEach {
			append(emitKLambdaParameter(it))
			append(" -> ")
		}
		if(node.stats.size==1)
			append(emitKStatement(node.stats.first()))
		else
			node.stats.forEach {
				append(emitKStatement(it))
				append(';')
			}
		append("}")
	}

	override fun emitKAnonymousFunction(node:KAnonymousFunction):String=buildString {
		node.run {
			if(isSuspend) append("suspend ")
			append("fun ")
			recieverType.onSome {
				append(emitKType(it))
				append('.')
			}
			append(emitKParametersWithOptionalType(params))
			toType.onSome {
				append(':')
				append(emitKType(it))
			}
			if(constraints.isNotEmpty())
			{
				append(constraints.joinToString(", ","where ") {emitKTypeConstraint(it)})
			}
			body.onSome {
				append(emitKFunctionBody(it))
			}
		}
	}

	override fun emitKObjectLiteral(node:KObjectLiteral):String=buildString {
		if(node.isData) append("data ")
		append("object")
		node.specifiers.onSome {
			append(':')
			append(emitKDelegationSpecifiers(it))
		}
		node.body.onSome {
			append(emitKClassBody(it))
		}
	}

	override fun emitKSemicolon(node:KSemicolon):String=";"

	override fun emitKReturnExpression(node:KReturnExpression):String=buildString {
		append(emitKReturnInner(node.ret))
		node.expression.onSome {
			append(' ')
			append(emitKExpression(it))
		}
	}

	override fun emitKReturnKeyword(node:KReturnKeyword):String="return"

	override fun emitKReturnAt(node:KReturnAt):String="return@${emitKIdentifier(node.id)}"

	override fun emitKExcl(node:KExcl):String="!"

	fun emitKModifiers(node:NonEmptyList<KModifiersInner>):String=
		node.joinToString(" ") {emitKModifiersInner(it)}

	override fun emitEClassModifier(node:EClassModifier):String=node.toString().lowercase()

	override fun emitEMemberModifier(node:EMemberModifier):String=node.toString().lowercase()
	override fun emitEVisibilityModifier(node:EVisibilityModifier):String=node.toString().lowercase()
	override fun emitEVarianceModifier(node:EVarianceModifier):String=node.toString().lowercase()
	override fun emitEFunctionModifier(node:EFunctionModifier):String=node.toString().lowercase()
	override fun emitEInheritanceModifier(node:EInheritanceModifier):String=node.toString().lowercase()
	override fun emitEParameterModifier(node:EParameterModifier):String=node.toString().lowercase()

	override fun emitKKotlinFile(node:KKotlinFile):String=buildString {
		node.run {
			shebang.onSome(::appendLine)
			annotations.map(::emitKFileAnnotation).forEach(::appendLine)
			appendLine(emitKPackageHeader(packageHeader))
			imports.map(::emitKImportHeader).forEach(::appendLine)
			topLevelObjects.map(::emitKDeclaration).forEach(::appendLine)
		}
	}

	override fun emitKIdentifier(node:KIdentifier):String=
		node.ids.joinToString(".",transform=::emitKSimpleIdentifier)

	override fun emitKPackageHeader(node:KPackageHeader):String=node.identifier.fold({""}) {
		"package ${emitKIdentifier(it)}\n"
	}

	override fun emitKAnnotatedDelegationSpecifier(node:KAnnotatedDelegationSpecifier):String=buildString {
		node.annotations.map {emitKAnnotation(it)+' '}.forEach(::append)
		append(emitKDelegationSpecifier(node.delegationSpecifier))
	}

	override fun emitKClassParameter(node:KClassParameter):String=buildString {
		node.modifiers.toNonEmptyListOrNone().onSome {
			append(emitKModifiers(it))
			append(' ')
		}
		node.isVal.onSome {
			append(if(it) "val " else "var ")
		}
		append(emitKSimpleIdentifier(node.identifier))
		append(": ")
		append(emitKType(node.type))
		node.expression.onSome {
			append(" = ")
			append(emitKExpression(it))
		}
	}

	override fun emitKDelegationSpecifiers(node:KDelegationSpecifiers):String=
		node.specifiers.joinToString {emitKAnnotatedDelegationSpecifier(it)}

	override fun emitKSingleImport(node:KImportHeader.KSingleImport):String=
		"import ${emitKIdentifier(node.identifier)}"

	override fun emitKWildcardImport(node:KImportHeader.KWildcardImport):String=
		"import ${emitKIdentifier(node.identifier)}.*"

	override fun emitKAliasImport(node:KImportHeader.KAliasImport):String=
		"import ${emitKIdentifier(node.identifier)} as ${emitKSimpleIdentifier(node.alias)}"

	override fun emitKPrimaryConstructor(node:KPrimaryConstructor):String=buildString {
		node.modifiers.toNonEmptyListOrNone().onSome {
			append(emitKModifiers(it))
			append(' ')
		}
		append(node.classParameters.joinToString(", ","(",")") {emitKClassParameter(it)})
	}

	override fun emitEBooleanLiteral(node:EBooleanLiteral):String=
		if(node==EBooleanLiteral.True) "true" else "false"

	override fun emitKMemberAccessOperator(node:KMemberAccessOperator):String=when(node)
	{
		KMemberAccessOperator.Dot->"."
		KMemberAccessOperator.SafeNav->"?."
		KMemberAccessOperator.ColonColon->"::"
	}

	override fun emitKBinaryOperator(node:KBinaryOperator):String=when(node)
	{
		KAdditiveOperator.Add->"+"
		KAdditiveOperator.Sub->"-"
		KMultiplicativeOperator.Mult->"*"
		KMultiplicativeOperator.Div->"/"
		KMultiplicativeOperator.Mod->"%"
	}

	override fun emitKLineStrRef(node:KLineStrRef):String=emitKFieldIdentifier(node.ref)

	override fun emitKFieldIdentifier(node:KFieldIdentifier):String="\$${emitKSimpleIdentifier(node.id)}"

	override fun emitKComparison(node:KComparison):String=
		"${emitKExpression(node.left)} ${emitEComparisonOperator(node.op)} ${emitKExpression(node.right)}"

	override fun emitEComparisonOperator(node:EComparisonOperator):String=when(node)
	{
		EComparisonOperator.Gt->">"
		EComparisonOperator.Lt->"<"
		EComparisonOperator.Le->"<="
		EComparisonOperator.Ge->">="
	}

	override fun emitEAssignmentAndOperator(node:EAssignmentAndOperator):String=when(node)
	{
		EAssignmentAndOperator.AddAssignment->"+="
		EAssignmentAndOperator.SubAssignment->"-="
		EAssignmentAndOperator.MultAssignment->"*="
		EAssignmentAndOperator.DivAssignment->"/="
		EAssignmentAndOperator.ModAssignment->"%="
	}

	override fun emitKAssignment(node:KAssignment):String=
		"${emitKAssignmentLeft(node.left)} ${emitKExpression(node.right)}"

	override fun emitKAssignableWithAndExpression(node:KAssignableWithAndExpression):String=
		"${emitKAssignableExpression(node.left)} ${emitEAssignmentAndOperator(node.right)}"

	override fun emitKParenthesizedDirectlyAssignableExpression(node:KParenthesizedDirectlyAssignableExpression):String=
		"(${emitKDirectlyAssignableExpression(node.directlyAssignableExpression)})"

	override fun emitKPrefixUnaryExpression(node:KPrefixUnaryExpression):String=
		node.prefixes.joinToString("") {emitKUnaryPrefix(it)}+
		emitKPostfixUnaryExpression(node.expression)

	override fun emitKConstructorInvocation(node:KConstructorInvocation):String=
		"${emitKUserType(node.userType)}${emitKValueArguments(node.valueArguments)}"

	override fun emitKLineStringExpression(node:KLineStringExpression):String=
		"\${${emitKExpression(node.expression)}}"

	override fun emitKWhileStatement(node:KWhileStatement):String=buildString {
		append("while(")
		append(emitKExpression(node.expression))
		append(')')
		append(emitKControlStructureBodyOrSemicolon(node.body))
	}

	override fun emitKConjunction(node:KConjunction):String=
		"${emitKExpression(node.left)}&&${emitKExpression(node.right)}"

	override fun emitKDisjunction(node:KDisjunction):String=
		"${emitKExpression(node.left)}||${emitKExpression(node.right)}"
}