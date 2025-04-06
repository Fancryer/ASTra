package ast

import arrow.core.Option
import ast.*

data class KKotlinScript(
	val shebang:Option<String>,
	val annotations:List<KFileAnnotation>,
	val packageHeader:KPackageHeader,
	val importList:List<KImportHeader>,
	val statements:List<KStatement>
):KotlinAst