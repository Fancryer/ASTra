// Generated from A:/Downloads/KAST/src/main/resources/astrap.g4 by ANTLR 4.13.2
package astra;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link astrap}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface astrapVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link astrap#program}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProgram(astrap.ProgramContext ctx);
	/**
	 * Visit a parse tree produced by {@link astrap#package}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPackage(astrap.PackageContext ctx);
	/**
	 * Visit a parse tree produced by {@link astrap#import_}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitImport_(astrap.Import_Context ctx);
	/**
	 * Visit a parse tree produced by {@link astrap#rule_}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRule_(astrap.Rule_Context ctx);
	/**
	 * Visit a parse tree produced by {@link astrap#mapper_rule}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMapper_rule(astrap.Mapper_ruleContext ctx);
	/**
	 * Visit a parse tree produced by {@link astrap#default}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDefault(astrap.DefaultContext ctx);
	/**
	 * Visit a parse tree produced by {@link astrap#result}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitResult(astrap.ResultContext ctx);
	/**
	 * Visit a parse tree produced by {@link astrap#block}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBlock(astrap.BlockContext ctx);
	/**
	 * Visit a parse tree produced by {@link astrap#match_clause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMatch_clause(astrap.Match_clauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link astrap#stat}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStat(astrap.StatContext ctx);
	/**
	 * Visit a parse tree produced by {@link astrap#decl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDecl(astrap.DeclContext ctx);
	/**
	 * Visit a parse tree produced by the {@code simple_type}
	 * labeled alternative in {@link astrap#type}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSimple_type(astrap.Simple_typeContext ctx);
	/**
	 * Visit a parse tree produced by the {@code function_type}
	 * labeled alternative in {@link astrap#type}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFunction_type(astrap.Function_typeContext ctx);
	/**
	 * Visit a parse tree produced by the {@code array_type}
	 * labeled alternative in {@link astrap#type}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArray_type(astrap.Array_typeContext ctx);
	/**
	 * Visit a parse tree produced by the {@code paren_type}
	 * labeled alternative in {@link astrap#type}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitParen_type(astrap.Paren_typeContext ctx);
	/**
	 * Visit a parse tree produced by the {@code access}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAccess(astrap.AccessContext ctx);
	/**
	 * Visit a parse tree produced by the {@code string}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitString(astrap.StringContext ctx);
	/**
	 * Visit a parse tree produced by the {@code match_to}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMatch_to(astrap.Match_toContext ctx);
	/**
	 * Visit a parse tree produced by the {@code false}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFalse(astrap.FalseContext ctx);
	/**
	 * Visit a parse tree produced by the {@code float}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFloat(astrap.FloatContext ctx);
	/**
	 * Visit a parse tree produced by the {@code int}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInt(astrap.IntContext ctx);
	/**
	 * Visit a parse tree produced by the {@code call}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCall(astrap.CallContext ctx);
	/**
	 * Visit a parse tree produced by the {@code infix_call}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInfix_call(astrap.Infix_callContext ctx);
	/**
	 * Visit a parse tree produced by the {@code paren}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitParen(astrap.ParenContext ctx);
	/**
	 * Visit a parse tree produced by the {@code lambda}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLambda(astrap.LambdaContext ctx);
	/**
	 * Visit a parse tree produced by the {@code null}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNull(astrap.NullContext ctx);
	/**
	 * Visit a parse tree produced by the {@code safe_access}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSafe_access(astrap.Safe_accessContext ctx);
	/**
	 * Visit a parse tree produced by the {@code variable}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitVariable(astrap.VariableContext ctx);
	/**
	 * Visit a parse tree produced by the {@code true}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTrue(astrap.TrueContext ctx);
	/**
	 * Visit a parse tree produced by the {@code elvis}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitElvis(astrap.ElvisContext ctx);
	/**
	 * Visit a parse tree produced by the {@code if_else}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIf_else(astrap.If_elseContext ctx);
	/**
	 * Visit a parse tree produced by the {@code lookup_call}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLookup_call(astrap.Lookup_callContext ctx);
	/**
	 * Visit a parse tree produced by the {@code safe_call}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSafe_call(astrap.Safe_callContext ctx);
	/**
	 * Visit a parse tree produced by {@link astrap#lookup}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLookup(astrap.LookupContext ctx);
	/**
	 * Visit a parse tree produced by {@link astrap#type_args}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitType_args(astrap.Type_argsContext ctx);
	/**
	 * Visit a parse tree produced by the {@code float_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFloat_pattern(astrap.Float_patternContext ctx);
	/**
	 * Visit a parse tree produced by the {@code constructor_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitConstructor_pattern(astrap.Constructor_patternContext ctx);
	/**
	 * Visit a parse tree produced by the {@code null_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNull_pattern(astrap.Null_patternContext ctx);
	/**
	 * Visit a parse tree produced by the {@code int_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInt_pattern(astrap.Int_patternContext ctx);
	/**
	 * Visit a parse tree produced by the {@code array_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArray_pattern(astrap.Array_patternContext ctx);
	/**
	 * Visit a parse tree produced by the {@code wildcard}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitWildcard(astrap.WildcardContext ctx);
	/**
	 * Visit a parse tree produced by the {@code raw_str_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRaw_str_pattern(astrap.Raw_str_patternContext ctx);
	/**
	 * Visit a parse tree produced by the {@code cast_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCast_pattern(astrap.Cast_patternContext ctx);
	/**
	 * Visit a parse tree produced by the {@code name_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitName_pattern(astrap.Name_patternContext ctx);
	/**
	 * Visit a parse tree produced by the {@code val_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitVal_pattern(astrap.Val_patternContext ctx);
	/**
	 * Visit a parse tree produced by the {@code tuple_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTuple_pattern(astrap.Tuple_patternContext ctx);
	/**
	 * Visit a parse tree produced by the {@code str_starts_with_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStr_starts_with_pattern(astrap.Str_starts_with_patternContext ctx);
	/**
	 * Visit a parse tree produced by the {@code logic_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLogic_pattern(astrap.Logic_patternContext ctx);
	/**
	 * Visit a parse tree produced by the {@code paren_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitParen_pattern(astrap.Paren_patternContext ctx);
	/**
	 * Visit a parse tree produced by the {@code alias_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAlias_pattern(astrap.Alias_patternContext ctx);
	/**
	 * Visit a parse tree produced by {@link astrap#default_clause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDefault_clause(astrap.Default_clauseContext ctx);
	/**
	 * Visit a parse tree produced by the {@code singleton_constructor}
	 * labeled alternative in {@link astrap#con_pattern}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSingleton_constructor(astrap.Singleton_constructorContext ctx);
	/**
	 * Visit a parse tree produced by the {@code empty_constructor}
	 * labeled alternative in {@link astrap#con_pattern}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEmpty_constructor(astrap.Empty_constructorContext ctx);
	/**
	 * Visit a parse tree produced by the {@code non_empty_constructor}
	 * labeled alternative in {@link astrap#con_pattern}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNon_empty_constructor(astrap.Non_empty_constructorContext ctx);
	/**
	 * Visit a parse tree produced by the {@code empty_array}
	 * labeled alternative in {@link astrap#arr_pattern}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEmpty_array(astrap.Empty_arrayContext ctx);
	/**
	 * Visit a parse tree produced by the {@code non_empty_array}
	 * labeled alternative in {@link astrap#arr_pattern}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNon_empty_array(astrap.Non_empty_arrayContext ctx);
	/**
	 * Visit a parse tree produced by {@link astrap#name}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitName(astrap.NameContext ctx);
}