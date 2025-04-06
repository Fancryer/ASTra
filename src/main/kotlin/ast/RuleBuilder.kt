package ast

import arrow.core.Option
import arrow.core.getOrElse
import arrow.core.none
import arrow.core.some
import ast.rules.RuleLogger
import ast.rules.RuleLoggerImpl
import org.antlr.v4.runtime.tree.ParseTree
import org.fancryer.bf.fix
import kotlin.reflect.KClass

class RuleBuilder<F:ParseTree,N:KotlinAst>
{
	private var from:(Option<KClass<F>>)=none()
	private var how:(Option<((F)->N)>)=none()
	private var name:(Option<String>)=none()
	private var logger:(RuleLogger<F,N>)=RuleLoggerImpl()

	fun from(init:()->KClass<F>)=from(init())

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

	fun logger(constructor:()->RuleLogger<F,N>,init:RuleLogger<F,N>.()->Unit)=
		constructor().also(init)

	fun build():(TranspilationRule<F,N>)=TranspilationRule(
		from.getOrElse {error("from is not set}")},
		how.getOrElse {error("how is not set")},
		name.getOrElse {error("name is not set")},
		logger
	)
}