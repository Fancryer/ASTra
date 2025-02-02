package ast

import org.antlr.v4.runtime.RuleContext
import org.antlr.v4.runtime.tree.TerminalNode
import org.antlr.v4.runtime.tree.Tree

class Sourcifier
{
	fun sourcify(tree:Tree?,stringBuilder:StringBuilder=StringBuilder()):String
	{
		if(tree==null) return ""
		val childCount=tree.childCount
		(0..childCount).map(tree::getChild)
			.forEach {
				if(it is TerminalNode)
					stringBuilder.append(it).append(" ")
				sourcify(it,stringBuilder)
			}
		return stringBuilder.toString().trim().replace("<EOF>","");
	}

	fun sourcify(trees:List<Tree?>):(List<String>)=
		trees.map {sourcify(it)}
}

val <T:Tree> T?.sourcify get()=Sourcifier().sourcify(this)
val <T:Tree> List<T?>.sourcify get()=Sourcifier().sourcify(this)