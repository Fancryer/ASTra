package ast

import arrow.core.*
import org.antlr.v4.runtime.tree.ParseTree
import org.fancryer.bf.ast.KotlinAst
import org.fancryer.bf.fix
import kotlin.reflect.KClass

class RuleBuilder<F:ParseTree,N:KotlinAst>
{
	private var from:(Option<KClass<F>>)=none()
	private var how:(Option<((F)->N)>)=none()
	private var name:(Option<String>)=none()
	private var demands:(List<(F)->Boolean>)=emptyList()
	private var ensures:(List<(N)->Boolean>)=emptyList()

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
		demands+=init
	}

	fun ensure(init:(N)->Boolean)
	{
		ensures+=init
	}

	fun build():TranspilationRule<F,N>
	{
		val from=from.getOrNull() ?: throw Exception("from is not set")
		val how=how.getOrNull() ?: throw Exception("how is not set")
		val name=name.getOrNull() ?: throw Exception("name is not set")
		return TranspilationRule(
			from,
			how,
			name,
			demands=demands,
			ensures=ensures
		)
	}
}