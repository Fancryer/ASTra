package ast

import arrow.core.None
import arrow.core.Option

data class KPackageHeader(val identifier:(Option<KIdentifier>)=None):KotlinAst