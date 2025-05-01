package ast

import arrow.core.None
import arrow.core.Option
import arrow.core.Some
import arrow.core.nel
import org.fancryer.bf.userType

data class KObjectLiteral(
	val isData:Boolean,
	val specifiers:Option<KDelegationSpecifiers>,
	val body:Option<KClassBody>
):KPrimaryExpression

class KObjectLiteralBuilder
{
	private var isData=false
	private var specifiers:(Option<KDelegationSpecifiers>)=None
	private var body:(Option<KClassBody>)=None

	val data get()=apply {isData=true}
	fun specifiers(specifiers:KDelegationSpecifiers)=apply {this.specifiers=Some(specifiers)}
	fun extends(type:KSimpleUserType)=specifiers(
		type.userType
			.delegationSpecifier
			.nel()
			.let(::KDelegationSpecifiers)
	)

	fun body(body:KClassBody)=apply {this.body=Some(body)}
	fun body(init:KClassBodyBuilder.()->Unit)=body(KClassBodyBuilder.kclassBody(init))

	private fun build():KObjectLiteral=KObjectLiteral(isData,specifiers,body)

	companion object
	{
		fun kobjectLiteral(block:KObjectLiteralBuilder.()->Unit):KObjectLiteral=
			KObjectLiteralBuilder().apply(block).build()
	}
}
