package ast.stable.v1_9_22

data class PostfixUnaryExpression(
	val expr:PrimaryExpression,
	val suffixes:List<PostfixUnarySuffix>
):Expression