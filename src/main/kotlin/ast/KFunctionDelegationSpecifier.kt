package ast

data class KFunctionDelegationSpecifier(
	val isSuspend:Boolean,
	val functionType:KFunctionType
):KDelegationSpecifier