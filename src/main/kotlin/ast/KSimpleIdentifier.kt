package ast

sealed interface KSimpleIdentifier:
		KPrimaryExpression,
		KCallableReferenceInner,
		KDirectlyAssignableExpression,
		KNavigationSuffixInner
{
	val id:String
}