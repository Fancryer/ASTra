package ast

import arrow.core.NonEmptyList

data class KIdentifier(val ids:NonEmptyList<KSimpleIdentifier>):KotlinAst