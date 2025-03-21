package org.fancryer.bf.ast

import arrow.core.nel
import arrow.core.some
import org.fancryer.bf.list

data class KStatement(
	val labelOrAnnotations:(List<KLabelOrAnnotation>)=emptyList(),
	val stat:KStatementInner
):KControlStructureBody
{
	override val code:String
		get()=buildString {
			labelOrAnnotations.asSequence()
				.map {it.code}
				.forEach {append(it).append(' ')}
			append(stat.code)
		}
}

val KStatement.block get()=KBlock(list)