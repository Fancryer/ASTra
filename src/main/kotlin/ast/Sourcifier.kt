package ast

import org.antlr.v4.runtime.tree.TerminalNode
import org.antlr.v4.runtime.tree.Tree

class Sourcifier
{
	fun sourcify(tree:Tree?)=sourcify(tree,StringBuilder())

	fun sourcify(trees:List<Tree?>):(List<String>)=trees.map(this::sourcify)

	fun sourcify(tree:Tree?,stringBuilder:StringBuilder):String
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
}

val Tree?.sourcify get()=Sourcifier().sourcify(this)