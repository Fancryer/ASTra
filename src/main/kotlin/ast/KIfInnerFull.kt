package ast

import arrow.core.Option

data class KIfInnerFull(
	val ifTrue:Option<KControlStructureBody>,
	val ifFalse:KControlStructureBodyOrSemicolon
):KIfInner