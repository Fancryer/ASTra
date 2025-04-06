package ast

import org.antlr.v4.runtime.Lexer
import org.antlr.v4.runtime.Parser
import org.antlr.v4.runtime.tree.ParseTree

class DefaultRule<L:Lexer,P:Parser>(val how:(ParseTree)->KotlinAst)