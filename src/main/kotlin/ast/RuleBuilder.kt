package ast

import arrow.core.*
import org.antlr.v4.runtime.tree.ParseTree
import org.fancryer.bf.ast.KLambdaLiteral
import org.fancryer.bf.ast.KotlinAst
import org.fancryer.bf.ast.err
import org.fancryer.bf.ast.errf
import org.fancryer.bf.ast.rules.RuleLogger
import org.fancryer.bf.ast.rules.RuleLoggerImpl
import org.fancryer.bf.fix
import stlc.gen.StlcParser.AbstractionContext
import kotlin.reflect.KClass

class RuleBuilder<F:ParseTree,N:KotlinAst>
{
	private var from:(Option<KClass<F>>)=none()
	private var how:(Option<((F)->N)>)=none()
	private var name:(Option<String>)=none()
	private var demands:(Option<(F)->Boolean>)=None
	private var ensures:(Option<(N)->Boolean>)=None
	private var unwinds:(Option<(Throwable)->N>)=None
	private var logger:(RuleLogger<F,N>)=RuleLoggerImpl()

	fun from(init:()->KClass<F>)
	{
		from(init())
	}

	fun from(init:KClass<F>)
	{
		from=init.some()
	}

	fun how(init:(F)->N)
	{
		how=init.some()
	}

	fun howCtx(init:F.()->N):Unit=how {
		init.run {
			this(it)
		}
	}

	fun howFixed(init:((F)->N)->(F)->N)
	{
		how=fix(init).some()
	}

	fun name(init:String)
	{
		name=init.some()
	}

	fun demand(init:(F)->Boolean)
	{
		demands=init.some()
	}

	fun ensure(init:(N)->Boolean)
	{
		ensures=init.some()
	}

	fun unwind(init:(Throwable)->N)
	{
		unwinds=init.some()
	}

	fun logger(constructor:()->RuleLogger<F,N>,init:RuleLogger<F,N>.()->Unit)=
		constructor().also(init)

	fun build():TranspilationRule<F,N>
	{
		val from=from.getOrNull() ?: "from is not set}".err
		val how=how.getOrNull() ?: "how is not set".err
		val name=name.getOrNull() ?: "name is not set".err
		return TranspilationRule(
			from,
			how,
			name,
			demands,
			ensures,
			unwinds,
			logger
		)
	}
}