// Generated from A:/Downloads/KAST/src/main/resources/Stlc.g4 by ANTLR 4.13.1
package stlc.gen;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link StlcParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface StlcVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by the {@code constant_true}
	 * labeled alternative in {@link StlcParser#t}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitConstant_true(StlcParser.Constant_trueContext ctx);
	/**
	 * Visit a parse tree produced by the {@code conditional}
	 * labeled alternative in {@link StlcParser#t}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitConditional(StlcParser.ConditionalContext ctx);
	/**
	 * Visit a parse tree produced by the {@code application}
	 * labeled alternative in {@link StlcParser#t}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitApplication(StlcParser.ApplicationContext ctx);
	/**
	 * Visit a parse tree produced by the {@code abstraction}
	 * labeled alternative in {@link StlcParser#t}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAbstraction(StlcParser.AbstractionContext ctx);
	/**
	 * Visit a parse tree produced by the {@code variable}
	 * labeled alternative in {@link StlcParser#t}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitVariable(StlcParser.VariableContext ctx);
	/**
	 * Visit a parse tree produced by the {@code parenthesis}
	 * labeled alternative in {@link StlcParser#t}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitParenthesis(StlcParser.ParenthesisContext ctx);
	/**
	 * Visit a parse tree produced by the {@code constant_false}
	 * labeled alternative in {@link StlcParser#t}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitConstant_false(StlcParser.Constant_falseContext ctx);
	/**
	 * Visit a parse tree produced by {@link StlcParser#x}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitX(StlcParser.XContext ctx);
	/**
	 * Visit a parse tree produced by the {@code flat_type}
	 * labeled alternative in {@link StlcParser#type}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFlat_type(StlcParser.Flat_typeContext ctx);
	/**
	 * Visit a parse tree produced by the {@code abstraction_type}
	 * labeled alternative in {@link StlcParser#type}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAbstraction_type(StlcParser.Abstraction_typeContext ctx);
}