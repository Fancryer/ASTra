package org.fancryer.bf.ast

import arrow.core.some
import org.fancryer.bf.nonEmptyList

data class KStatement(val labelOrAnnotations:List<KLabelOrAnnotation>,val stat:KStatementInner):KControlStructureBody
{
	override val code:String
		get()=buildString {
			labelOrAnnotations.asSequence()
				.map {it.code}
				.forEach {append(it).append(' ')}
			append(stat.code)
		}
}

val KStatement.block get()=KBlock(nonEmptyList.some())