package ast

import arrow.core.None
import arrow.core.Option
import arrow.core.nel
import arrow.core.some
import org.fancryer.bf.stat

data class KWhenExpression(
	val subject:(Option<KWhenSubject>)=None,
	val entries:(List<KWhenEntry>)=emptyList()
):KPrimaryExpression

class KWhenExpressionBuilder
{
	private var subject:(Option<KWhenSubject>)=None
	private var entries:(List<KWhenEntry>)=emptyList()

	fun subject(subject:KWhenSubject)
	{
		this.subject=subject.some()
	}

	val KWhenEntry.entry get()=entries(this)

	val List<KWhenEntry>.entries get()=entries(this)

	fun entries(entries:List<KWhenEntry>)
	{
		this.entries=entries
	}

	fun entries(vararg entries:KWhenEntry)
	{
		entries(entries.toList())
	}

	infix fun KType.caseIs(expr:KExpression)=caseIs(expr.stat)

	infix fun KType.caseIs(body:KControlStructureBody)=
		KWhenEntryWithConditions(
			KTypeTest(EIsOperator.Is,this).nel(),
			body
		).also {
			entries+=it
		}

	val KControlStructureBody.caseElse
		get()=KWhenElseEntry(this).also {entries+=it}

	private fun build()=KWhenExpression(subject,entries)

	companion object
	{
		fun kwhen(init:KWhenExpressionBuilder.()->Unit)=
			KWhenExpressionBuilder().apply(init).build()

		fun kwhen(subject:KWhenSubject,init:KWhenExpressionBuilder.()->Unit)=
			KWhenExpressionBuilder().apply {subject(subject)}
				.apply(init)
				.build()

		fun kwhen(subject:KExpression,init:KWhenExpressionBuilder.()->Unit)=
			KWhenExpressionBuilder().apply {subject(KWhenSubject(expression=subject))}
				.apply(init)
				.build()
	}
}