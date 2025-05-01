package ast

import arrow.core.NonEmptyList
import arrow.core.toNonEmptyListOrNull
import org.fancryer.bf.list

class DelegationSpecifiersBuilder
{
	private var specifiers=listOf<KAnnotatedDelegationSpecifier>()

	fun specifier(specifier:KAnnotatedDelegationSpecifier)
	{
		specifiers+=specifier
	}

	operator fun KAnnotatedDelegationSpecifier.unaryPlus()
	{
		specifier(this)
	}

	infix fun KUserType.constructorInvocation(valueArguments:KValueArguments)
	{
		+KAnnotatedDelegationSpecifier(KConstructorInvocation(this,valueArguments))
	}

	infix fun KUserType.constructorInvocation(valueArguments:List<KValueArgument>)
	{
		+KAnnotatedDelegationSpecifier(KConstructorInvocation(this,KValueArguments(valueArguments)))
	}

	infix fun KUserType.constructorInvocation(valueArgument:KValueArgument)
	{
		+KAnnotatedDelegationSpecifier(KConstructorInvocation(this,KValueArguments(valueArgument.list)))
	}

	fun KUserType.constructorInvocation()
	{
		this constructorInvocation KValueArguments()
	}

	operator fun List<KAnnotatedDelegationSpecifier>.unaryPlus()
	{
		specifiers+=this
	}

	private fun build()=
		specifiers.toNonEmptyListOrNull()?.let(::KDelegationSpecifiers)
		?: error("No specifiers")

	companion object
	{
		fun kdelegationSpecifiers(init:DelegationSpecifiersBuilder.()->Unit)=
			DelegationSpecifiersBuilder().apply(init).build()
	}
}

data class KDelegationSpecifiers(
	val specifiers:NonEmptyList<KAnnotatedDelegationSpecifier>
):KotlinAst