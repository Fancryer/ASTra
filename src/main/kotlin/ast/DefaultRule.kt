package ast

import org.antlr.v4.runtime.tree.ParseTree
import org.fancryer.bf.ast.KotlinAst

class DefaultRule(val how:(ParseTree)->KotlinAst)