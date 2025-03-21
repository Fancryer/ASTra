package org.fancryer.bf.ast

import arrow.core.*
import astra.astrap.*
import astra.astrapBaseVisitor
import org.fancryer.bf.*
import org.fancryer.bf.ast.KImportHeader.KSingleImport
import org.fancryer.bf.ast.KImportHeader.KWildcardImport
import org.fancryer.bf.ast.KPropertyDeclarationBuilder.Companion.kpropertyDeclaration

class AstralToKotlinMapper:astrapBaseVisitor<KotlinAst>()
{
	override fun visitProgram(ctx:ProgramContext):KKotlinFile
	{
		//'grammar' Name package? import_* rule* EOF
		val grammarName=ctx.Name().text
		//'package' Name ('.' Name)* ';'?;
		val package_=ctx.package_()?.let(::visitPackage).toOption()
		val imports=ctx.import_().map(::visitImport_)
		val rules=ctx.rule_().map(::visitRule_)
		val holder=kpropertyDeclaration {
			declaration(KVariableDeclaration(identifier="${grammarName}_RuleHolder".id))
			"rules".id.call(
				KLambdaLiteral(
					stats=rules.map {KStatement(stat=it)}
				).valueArg.nel(),
				nonEmptyListOf("Lexer","Parser")
					.map {"$grammarName$it".type}
					.map(::KConcreteTypeProjection)
					.toNonEmptyList()
					.typeArgs
					.some()
			).expr
		}
		return KKotlinFile(
			packageHeader=KPackageHeader(package_),
			imports=imports,
			topLevelObjects=holder.list
		)
	}

	override fun visitPackage(ctx:PackageContext):KIdentifier
	{
		val (tail,head)=ctx.Name().map {it.text.id}.split()!!
		return identifier(head) {tail.forEach {+it}}
	}

	override fun visitImport_(ctx:Import_Context):KImportHeader
	{
		val (tail,head)=ctx.Name().map {it.text.id}.split()!!
		val id=identifier(head) {tail.forEach {+it}}
		return when(ctx.Star())
		{
			null->KSingleImport(id)
			else->KWildcardImport(id)
		}
	}

	override fun visitRule_(ctx:Rule_Context):KPropertyDeclaration=
		ctx.mapper_rule()?.let(::visitMapper_rule)
		?: ctx.default_().let(::visitDefault)

	override fun visitMapper_rule(ctx:Mapper_ruleContext):KPropertyDeclaration
	{
		/*
		rule_name=name alias=name? from=name ',' to=name (ctx_name=name | '_') ('::=' | fix_name=name '<|>') (block | expr)
		*/
		val ruleName=visitName(ctx.rule_name)
		val alias=ctx.alias?.let(::visitName)
		val fromType=(visitName(ctx.from).id+"Context").type
		val toType=visitName(ctx.to).type
		val ctxName=ctx.ctx_name?.let(::visitName)
		return kpropertyDeclaration {
			declaration(ruleName.variableDecl)
			"rule".id.call(
				args=nonEmptyListOf(
					KConcreteTypeProjection(fromType),
					KConcreteTypeProjection(toType)
				).typeArgs.some(),
				valueArgs=nonEmptyListOf((ctxName ?: "_".id).valueArg,KLambdaLiteral().valueArg)
			).expr
		}
	}

	override fun visitDefault(ctx:DefaultContext):KPropertyDeclaration
	{
		return kpropertyDeclaration {
			declaration("default".id.variableDecl)
			KLambdaLiteral().expr
		}
	}

	override fun visitName(ctx:NameContext):KSimpleIdentifier=
		ctx.Name()?.text?.id
		?: ctx.BacktickName().text.removeSurrounding("`").id
}