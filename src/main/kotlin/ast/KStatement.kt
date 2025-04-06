package ast

import org.fancryer.bf.list

data class KStatement(
	val labelOrAnnotations:(List<KLabelOrAnnotation>)=emptyList(),
	val stat:KStatementInner
):KControlStructureBody

val KStatement.block get()=KBlock(list)