package ast

import arrow.core.NonEmptyList

data class KFileAnnotation(val annos:NonEmptyList<KUnescapedAnnotation>):KotlinAst