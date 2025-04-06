package emitter

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
			modifiers.onSome {
				append(emitKModifiers(it))
				append(' ')
			}
			append("fun ")
			typeParameters.onSome {
				append(emitKTypeParameters(it))
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
			typeConstraints.onSome {
				append(emitKTypeConstraints(it))
			}
			functionBody.onSome {
				append(emitKFunctionBody(it))
			}
		}
	}

	override fun emitKFunctionAssignExpression(node:KFunctionAssignExpression):String=
		emitKExpression(node.expression)

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
			modifiers.onSome {
				append(emitKModifiers(it))
				append(' ')
			}
			append(if(isVal) "val " else "var ")
			typeParameters.onSome {
				append(emitKTypeParameters(it))
				append(' ')
			}
			recieverType.onSome {
				append(emitKReceiverType(it))
				append('.')
			}
			append(emitKMultiOrSingleVariableDeclaration(declaration))
			append(' ')
			typeConstraints.onSome {
				append(emitKTypeConstraints(it))
				append(' ')
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
			modifiers.onSome {
				append(emitKModifiers(it)).append(' ')
			}
			append(emitKClassHeaderType(classHeaderType))
			append(' ')
			append(emitKSimpleIdentifier(identifier))
			append(typeParameters.fold({" "}) {
				emitKTypeParameters(it)
			})
			primaryConstructor.onSome {
				append(emitKPrimaryConstructor(it))
			}
			delegationSpecifiers.onSome {
				append(emitKDelegationSpecifiers(it))
			}
			typeConstraints.onSome {
				emitKTypeConstraints(it)
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

	override fun emitKAdditiveExpression(node:KAdditiveExpression):String=
		"${emitKExpression(node.left)} ${emitKAdditiveOperator(node.op)} ${emitKExpression(node.right)}"

	override fun emitKMultiplicativeExpression(node:KMultiplicativeExpression):String=
		"${emitKExpression(node.left)} ${emitKMultiplicativeOperator(node.op)} ${emitKExpression(node.right)}"

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
			constraints.onSome {
				append(emitKTypeConstraints(it))
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

	override fun emitKModifiers(node:KModifiers):String=
		node.mods.joinToString(" ") {emitKModifiersInner(it)}

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
		node.modifiers.onSome {
			emitKModifiers(it)
			append(' ')
		}
		append(if(node.isVal) "val " else "var ")
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
		node.modifiers.onSome {
			emitKModifiers(it)
			append(' ')
		}
		append(node.classParameters.joinToString("","(",")") {emitKClassParameter(it)})
	}
}