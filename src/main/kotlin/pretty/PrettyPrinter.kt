package org.fancryer.bf.pretty

import arrow.core.Some
import org.fancryer.bf.ast.*

fun KotlinAst.code():String=
	when(this)
	{
		is KKotlinFile->
			buildString {
				if(shebang is Some) append(shebang.value).append('\n')
				annotations.map {it.code}.forEach {append(it).append('\n')}
				append(packageHeader.code)
				imports.map {it.code}.forEach {append(it).append('\n')}
				topLevelObjects.map {it.code}.forEach {append(it).append('\n')}
			}

		is KPackageHeader->if(identifier is Some) "package ${identifier.value.code}\n" else ""

		is KIdentifier->
			buildString {
				ids.map(KSimpleIdentifier::code).forEachIndexed {index,code->
					append(code)
					if(index<ids.size-1) append('.')
				}
			}

		is KFunctionValueParameter->
			buildString {
				if(modifiers is Some) append(modifiers.value.code).append(' ')
				append(parameter.code)
				if(expression is Some) append(" = ").append(expression.value.code)
			}

		is KParameter->"${identifier.code}: ${type.code}"

		is KIdentifierInner->this.name

		KClassKeyword->"class"
		KFunInterfaceKeyword->"fun interface"
		KInterfaceKeyword->"interface"

		KAbstractKeyword->"abstract"
		KActualKeyword->"actual"
		KAnnotationKeyword->"annotation"
		KByKeyword->"by"
		KCatchKeyword->"catch"
		KCompanionKeyword->"companion"
		KConstKeyword->"const"
		KConstructorKeyword->"constructor"
		KCrossinlineKeyword->"crossinline"
		KDataKeyword->"data"
		KDelegateKeyword->"delegate"
		KDynamicKeyword->"dynamic"
		KEnumKeyword->"enum"
		KExpectKeyword->"expect"
		KExternalKeyword->"external"
		KFieldKeyword->"field"
		KFileKeyword->"file"
		KFinalKeyword->"final"
		KFinallyKeyword->"finally"
		KGetKeyword->"get"
		KImportKeyword->"import"
		KInfixKeyword->"infix"
		KInitKeyword->"init"
		KInlineKeyword->"inline"
		KInnerKeyword->"inner"
		KInternalKeyword->"internal"
		KLateinitKeyword->"lateinit"
		KNoinlineKeyword->"noinline"
		KOpenKeyword->"open"
		KOperatorKeyword->"operator"
		KOutKeyword->"out"
		KOverrideKeyword->"override"
		KParamKeyword->"param"
		KPrivateKeyword->"private"
		KPropertyKeyword->"property"
		KProtectedKeyword->"protected"
		KPublicKeyword->"public"
		KReceiverKeyword->"receiver"
		KReifiedKeyword->"reified"
		KSealedKeyword->"sealed"
		KSetKeyword->"set"
		KSetparamKeyword->"setparam"
		KSuspendKeyword->"suspend"
		KTailrecKeyword->"tailrec"
		KValueKeyword->"value"
		KVarargKeyword->"vararg"
		KWhereKeyword->"where"

		is KClassDeclaration->
			buildString {
				if(modifiers is Some) append(modifiers.value.code).append(' ')
				append(classHeaderType.code).append(' ')
				append(identifier.code)
				if(typeParameters is Some) append(typeParameters.value.code)
				if(primaryConstructor is Some) append(primaryConstructor.value.code)
				if(delegationSpecifiers is Some) append(delegationSpecifiers.value.code)
				if(typeConstraints is Some) append(typeConstraints.value.code)
				append(body.code)
			}

		is KFunctionDeclaration->
			buildString {
				if(modifiers is Some) append(modifiers.value.code).append(' ')
				append("fun ")
				if(typeParameters is Some) append(typeParameters.value.code).append(' ')
				if(receiverType is Some) append(receiverType.value.code).append('.')
				append(identifier.code)
				append(functionValueParameters.joinToString {it.code})
				if(type is Some) append(':').append(type.value.code)
				if(typeConstraints is Some) append(typeConstraints.value.code)
				if(functionBody is Some) append(functionBody.value.code)
			}

		is KObjectDeclaration->"ё"
		is KPropertyDeclaration->"ё"
		is KTypeAlias->"ё"

		is KClassBody->
			buildString {
				append('{')
				append(memberDeclarations.joinToString(";") {it.code})
				append('}')
			}

		is KType->
			buildString {

			}

		is KBlock->
			buildString {
				append('{')
				statements.forEach {append(it.code)}
				append('}')
			}

		is KStatement->
			buildString {
				labelOrAnnotations.forEach {append(it.code).append(' ')}
				append(stat.code).append(';')
			}

		is KPostfixUnaryExpression->
			buildString {
				append(primaryExpression.code)
				suffixes.forEach {append(it.code)}
			}

		is KCallSuffix->
			buildString {
				if(args is Some) append(args.value.code)
				append(callSuffixInner.code)
			}

		is KValueArguments->
			buildString {
				append('(')
				append(args.joinToString(",") {it.code})
				append(')')
			}

		is KValueArgument->
			buildString {
				if(anno is Some) append(anno.value.code)
				if(identifier is Some) append(identifier.value.code)
				if(hasSpread) append('*')
				append(expression.code)
			}

		is KLineStringLiteral->
			buildString {
				append('"')
				content.forEach {append(it.code)}
				append('"')
			}

		is KLineStringExpression->
			buildString {

			}

		is KAdditiveExpression->"${left.code}${op.code}${right.code}"

		is KAdd->"+"
		is KSub->"-"
		is KIntegerLiteral->value.toString()

		else->"ё${this::class.simpleName}ё"
	}