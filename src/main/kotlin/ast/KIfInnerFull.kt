package org.fancryer.bf.ast

import arrow.core.Option

data class KIfInnerFull(
	val ifTrue:Option<KControlStructureBody>,
	val ifFalse:KControlStructureBodyOrSemicolon
):KIfInner
{
	override val code:String
		get()=buildString {
			ifTrue.onSome {append(it.code)}
			if(ifFalse !is KSemicolon)
			{
				append(" else ")
				append(ifFalse.code)
			}
		}
}