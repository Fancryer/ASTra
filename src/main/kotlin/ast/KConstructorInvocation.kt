package ast

data class KConstructorInvocation(
	val userType:KUserType,
	val valueArguments:KValueArguments
):KUnescapedAnnotation,KDelegationSpecifier