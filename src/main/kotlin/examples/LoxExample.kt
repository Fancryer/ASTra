package org.fancryer.bf.examples

import ast.Sourcifier
import ast.rules
import ast.sourcify
import lox.LoxLexer
import lox.LoxParser
import lox.LoxParser.ClassDeclContext
import lox.LoxParser.DeclarationContext
import org.fancryer.bf.args
import org.fancryer.bf.ast.KLineStringLiteral
import org.fancryer.bf.call
import org.fancryer.bf.id
import org.fancryer.bf.valueArg

val loxRuleHolder=rules<LoxLexer,LoxParser> {
	default {
		println("default for ${it::class.simpleName}")
		"TODO".id.call(KLineStringLiteral(emptyList()).valueArg.args)
	}

	rule("declaration") {
		from(DeclarationContext::class)
		how {ctx:DeclarationContext->
			ctx.classDecl()?.let {TODO("class")}
			?: ctx.funDecl()?.let {TODO("fun")}
			?: ctx.varDecl()?.let {TODO("var")}
			?: ctx.statement()?.let {TODO("statement")}
			?: error(ctx::class.simpleName ?: "???")
		}
	}
}