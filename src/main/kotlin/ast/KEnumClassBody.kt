package ast

import arrow.core.Option
import arrow.core.toOption
import ast.KEnumEntriesBuilder.Companion.kenumEntries

class KEnumClassBodyBuilder
{
	private var entries:KEnumEntries?=null
	private var memberDeclarations:(List<KClassMemberDeclaration>)=emptyList()

	fun entries(entries:KEnumEntries)
	{
		this.entries=entries
	}

	fun entries(init:KEnumEntriesBuilder.()->Unit)=
		kenumEntries(init).also {entries=it}

	fun memberDeclaration(declaration:KClassMemberDeclaration)
	{
		memberDeclarations+=declaration
	}

	private fun build()=KEnumClassBody(
		entries.toOption(),
		memberDeclarations
	)

	companion object
	{
		fun kenumClassBody(init:KEnumClassBodyBuilder.()->Unit)=
			KEnumClassBodyBuilder().apply(init).build()
	}
}

data class KEnumClassBody(
	val entries:Option<KEnumEntries>,
	val memberDeclarations:List<KClassMemberDeclaration>
):KClassOrEnumBody