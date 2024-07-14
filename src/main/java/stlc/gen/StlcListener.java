// Generated from A:/Downloads/KAST/src/main/resources/Stlc.g4 by ANTLR 4.13.1
package stlc.gen;
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link StlcParser}.
 */
public interface StlcListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by the {@code constant_true}
	 * labeled alternative in {@link StlcParser#t}.
	 * @param ctx the parse tree
	 */
	void enterConstant_true(StlcParser.Constant_trueContext ctx);
	/**
	 * Exit a parse tree produced by the {@code constant_true}
	 * labeled alternative in {@link StlcParser#t}.
	 * @param ctx the parse tree
	 */
	void exitConstant_true(StlcParser.Constant_trueContext ctx);
	/**
	 * Enter a parse tree produced by the {@code conditional}
	 * labeled alternative in {@link StlcParser#t}.
	 * @param ctx the parse tree
	 */
	void enterConditional(StlcParser.ConditionalContext ctx);
	/**
	 * Exit a parse tree produced by the {@code conditional}
	 * labeled alternative in {@link StlcParser#t}.
	 * @param ctx the parse tree
	 */
	void exitConditional(StlcParser.ConditionalContext ctx);
	/**
	 * Enter a parse tree produced by the {@code application}
	 * labeled alternative in {@link StlcParser#t}.
	 * @param ctx the parse tree
	 */
	void enterApplication(StlcParser.ApplicationContext ctx);
	/**
	 * Exit a parse tree produced by the {@code application}
	 * labeled alternative in {@link StlcParser#t}.
	 * @param ctx the parse tree
	 */
	void exitApplication(StlcParser.ApplicationContext ctx);
	/**
	 * Enter a parse tree produced by the {@code abstraction}
	 * labeled alternative in {@link StlcParser#t}.
	 * @param ctx the parse tree
	 */
	void enterAbstraction(StlcParser.AbstractionContext ctx);
	/**
	 * Exit a parse tree produced by the {@code abstraction}
	 * labeled alternative in {@link StlcParser#t}.
	 * @param ctx the parse tree
	 */
	void exitAbstraction(StlcParser.AbstractionContext ctx);
	/**
	 * Enter a parse tree produced by the {@code variable}
	 * labeled alternative in {@link StlcParser#t}.
	 * @param ctx the parse tree
	 */
	void enterVariable(StlcParser.VariableContext ctx);
	/**
	 * Exit a parse tree produced by the {@code variable}
	 * labeled alternative in {@link StlcParser#t}.
	 * @param ctx the parse tree
	 */
	void exitVariable(StlcParser.VariableContext ctx);
	/**
	 * Enter a parse tree produced by the {@code parenthesis}
	 * labeled alternative in {@link StlcParser#t}.
	 * @param ctx the parse tree
	 */
	void enterParenthesis(StlcParser.ParenthesisContext ctx);
	/**
	 * Exit a parse tree produced by the {@code parenthesis}
	 * labeled alternative in {@link StlcParser#t}.
	 * @param ctx the parse tree
	 */
	void exitParenthesis(StlcParser.ParenthesisContext ctx);
	/**
	 * Enter a parse tree produced by the {@code constant_false}
	 * labeled alternative in {@link StlcParser#t}.
	 * @param ctx the parse tree
	 */
	void enterConstant_false(StlcParser.Constant_falseContext ctx);
	/**
	 * Exit a parse tree produced by the {@code constant_false}
	 * labeled alternative in {@link StlcParser#t}.
	 * @param ctx the parse tree
	 */
	void exitConstant_false(StlcParser.Constant_falseContext ctx);
	/**
	 * Enter a parse tree produced by {@link StlcParser#x}.
	 * @param ctx the parse tree
	 */
	void enterX(StlcParser.XContext ctx);
	/**
	 * Exit a parse tree produced by {@link StlcParser#x}.
	 * @param ctx the parse tree
	 */
	void exitX(StlcParser.XContext ctx);
	/**
	 * Enter a parse tree produced by the {@code flat_type}
	 * labeled alternative in {@link StlcParser#type}.
	 * @param ctx the parse tree
	 */
	void enterFlat_type(StlcParser.Flat_typeContext ctx);
	/**
	 * Exit a parse tree produced by the {@code flat_type}
	 * labeled alternative in {@link StlcParser#type}.
	 * @param ctx the parse tree
	 */
	void exitFlat_type(StlcParser.Flat_typeContext ctx);
	/**
	 * Enter a parse tree produced by the {@code abstraction_type}
	 * labeled alternative in {@link StlcParser#type}.
	 * @param ctx the parse tree
	 */
	void enterAbstraction_type(StlcParser.Abstraction_typeContext ctx);
	/**
	 * Exit a parse tree produced by the {@code abstraction_type}
	 * labeled alternative in {@link StlcParser#type}.
	 * @param ctx the parse tree
	 */
	void exitAbstraction_type(StlcParser.Abstraction_typeContext ctx);
}