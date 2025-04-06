package ast

sealed interface KImportHeader:KotlinAst
{
	data class KSingleImport(val identifier:KIdentifier):KImportHeader
	data class KWildcardImport(val identifier:KIdentifier):KImportHeader
	data class KAliasImport(val identifier:KIdentifier,val alias:KSimpleIdentifier):KImportHeader
}