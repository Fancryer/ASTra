// Generated from A:/Downloads/KAST/src/main/resources/astrap.g4 by ANTLR 4.13.2
package astra;
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link astrap}.
 */
public interface astrapListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link astrap#program}.
	 * @param ctx the parse tree
	 */
	void enterProgram(astrap.ProgramContext ctx);
	/**
	 * Exit a parse tree produced by {@link astrap#program}.
	 * @param ctx the parse tree
	 */
	void exitProgram(astrap.ProgramContext ctx);
	/**
	 * Enter a parse tree produced by {@link astrap#package}.
	 * @param ctx the parse tree
	 */
	void enterPackage(astrap.PackageContext ctx);
	/**
	 * Exit a parse tree produced by {@link astrap#package}.
	 * @param ctx the parse tree
	 */
	void exitPackage(astrap.PackageContext ctx);
	/**
	 * Enter a parse tree produced by {@link astrap#import_}.
	 * @param ctx the parse tree
	 */
	void enterImport_(astrap.Import_Context ctx);
	/**
	 * Exit a parse tree produced by {@link astrap#import_}.
	 * @param ctx the parse tree
	 */
	void exitImport_(astrap.Import_Context ctx);
	/**
	 * Enter a parse tree produced by {@link astrap#rule_}.
	 * @param ctx the parse tree
	 */
	void enterRule_(astrap.Rule_Context ctx);
	/**
	 * Exit a parse tree produced by {@link astrap#rule_}.
	 * @param ctx the parse tree
	 */
	void exitRule_(astrap.Rule_Context ctx);
	/**
	 * Enter a parse tree produced by {@link astrap#mapper_rule}.
	 * @param ctx the parse tree
	 */
	void enterMapper_rule(astrap.Mapper_ruleContext ctx);
	/**
	 * Exit a parse tree produced by {@link astrap#mapper_rule}.
	 * @param ctx the parse tree
	 */
	void exitMapper_rule(astrap.Mapper_ruleContext ctx);
	/**
	 * Enter a parse tree produced by {@link astrap#default}.
	 * @param ctx the parse tree
	 */
	void enterDefault(astrap.DefaultContext ctx);
	/**
	 * Exit a parse tree produced by {@link astrap#default}.
	 * @param ctx the parse tree
	 */
	void exitDefault(astrap.DefaultContext ctx);
	/**
	 * Enter a parse tree produced by {@link astrap#result}.
	 * @param ctx the parse tree
	 */
	void enterResult(astrap.ResultContext ctx);
	/**
	 * Exit a parse tree produced by {@link astrap#result}.
	 * @param ctx the parse tree
	 */
	void exitResult(astrap.ResultContext ctx);
	/**
	 * Enter a parse tree produced by {@link astrap#block}.
	 * @param ctx the parse tree
	 */
	void enterBlock(astrap.BlockContext ctx);
	/**
	 * Exit a parse tree produced by {@link astrap#block}.
	 * @param ctx the parse tree
	 */
	void exitBlock(astrap.BlockContext ctx);
	/**
	 * Enter a parse tree produced by {@link astrap#match_clause}.
	 * @param ctx the parse tree
	 */
	void enterMatch_clause(astrap.Match_clauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link astrap#match_clause}.
	 * @param ctx the parse tree
	 */
	void exitMatch_clause(astrap.Match_clauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link astrap#stat}.
	 * @param ctx the parse tree
	 */
	void enterStat(astrap.StatContext ctx);
	/**
	 * Exit a parse tree produced by {@link astrap#stat}.
	 * @param ctx the parse tree
	 */
	void exitStat(astrap.StatContext ctx);
	/**
	 * Enter a parse tree produced by {@link astrap#decl}.
	 * @param ctx the parse tree
	 */
	void enterDecl(astrap.DeclContext ctx);
	/**
	 * Exit a parse tree produced by {@link astrap#decl}.
	 * @param ctx the parse tree
	 */
	void exitDecl(astrap.DeclContext ctx);
	/**
	 * Enter a parse tree produced by the {@code simple_type}
	 * labeled alternative in {@link astrap#type}.
	 * @param ctx the parse tree
	 */
	void enterSimple_type(astrap.Simple_typeContext ctx);
	/**
	 * Exit a parse tree produced by the {@code simple_type}
	 * labeled alternative in {@link astrap#type}.
	 * @param ctx the parse tree
	 */
	void exitSimple_type(astrap.Simple_typeContext ctx);
	/**
	 * Enter a parse tree produced by the {@code function_type}
	 * labeled alternative in {@link astrap#type}.
	 * @param ctx the parse tree
	 */
	void enterFunction_type(astrap.Function_typeContext ctx);
	/**
	 * Exit a parse tree produced by the {@code function_type}
	 * labeled alternative in {@link astrap#type}.
	 * @param ctx the parse tree
	 */
	void exitFunction_type(astrap.Function_typeContext ctx);
	/**
	 * Enter a parse tree produced by the {@code array_type}
	 * labeled alternative in {@link astrap#type}.
	 * @param ctx the parse tree
	 */
	void enterArray_type(astrap.Array_typeContext ctx);
	/**
	 * Exit a parse tree produced by the {@code array_type}
	 * labeled alternative in {@link astrap#type}.
	 * @param ctx the parse tree
	 */
	void exitArray_type(astrap.Array_typeContext ctx);
	/**
	 * Enter a parse tree produced by the {@code paren_type}
	 * labeled alternative in {@link astrap#type}.
	 * @param ctx the parse tree
	 */
	void enterParen_type(astrap.Paren_typeContext ctx);
	/**
	 * Exit a parse tree produced by the {@code paren_type}
	 * labeled alternative in {@link astrap#type}.
	 * @param ctx the parse tree
	 */
	void exitParen_type(astrap.Paren_typeContext ctx);
	/**
	 * Enter a parse tree produced by the {@code access}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void enterAccess(astrap.AccessContext ctx);
	/**
	 * Exit a parse tree produced by the {@code access}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void exitAccess(astrap.AccessContext ctx);
	/**
	 * Enter a parse tree produced by the {@code string}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void enterString(astrap.StringContext ctx);
	/**
	 * Exit a parse tree produced by the {@code string}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void exitString(astrap.StringContext ctx);
	/**
	 * Enter a parse tree produced by the {@code match_to}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void enterMatch_to(astrap.Match_toContext ctx);
	/**
	 * Exit a parse tree produced by the {@code match_to}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void exitMatch_to(astrap.Match_toContext ctx);
	/**
	 * Enter a parse tree produced by the {@code false}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void enterFalse(astrap.FalseContext ctx);
	/**
	 * Exit a parse tree produced by the {@code false}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void exitFalse(astrap.FalseContext ctx);
	/**
	 * Enter a parse tree produced by the {@code float}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void enterFloat(astrap.FloatContext ctx);
	/**
	 * Exit a parse tree produced by the {@code float}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void exitFloat(astrap.FloatContext ctx);
	/**
	 * Enter a parse tree produced by the {@code int}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void enterInt(astrap.IntContext ctx);
	/**
	 * Exit a parse tree produced by the {@code int}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void exitInt(astrap.IntContext ctx);
	/**
	 * Enter a parse tree produced by the {@code call}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void enterCall(astrap.CallContext ctx);
	/**
	 * Exit a parse tree produced by the {@code call}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void exitCall(astrap.CallContext ctx);
	/**
	 * Enter a parse tree produced by the {@code infix_call}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void enterInfix_call(astrap.Infix_callContext ctx);
	/**
	 * Exit a parse tree produced by the {@code infix_call}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void exitInfix_call(astrap.Infix_callContext ctx);
	/**
	 * Enter a parse tree produced by the {@code paren}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void enterParen(astrap.ParenContext ctx);
	/**
	 * Exit a parse tree produced by the {@code paren}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void exitParen(astrap.ParenContext ctx);
	/**
	 * Enter a parse tree produced by the {@code lambda}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void enterLambda(astrap.LambdaContext ctx);
	/**
	 * Exit a parse tree produced by the {@code lambda}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void exitLambda(astrap.LambdaContext ctx);
	/**
	 * Enter a parse tree produced by the {@code null}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void enterNull(astrap.NullContext ctx);
	/**
	 * Exit a parse tree produced by the {@code null}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void exitNull(astrap.NullContext ctx);
	/**
	 * Enter a parse tree produced by the {@code safe_access}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void enterSafe_access(astrap.Safe_accessContext ctx);
	/**
	 * Exit a parse tree produced by the {@code safe_access}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void exitSafe_access(astrap.Safe_accessContext ctx);
	/**
	 * Enter a parse tree produced by the {@code variable}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void enterVariable(astrap.VariableContext ctx);
	/**
	 * Exit a parse tree produced by the {@code variable}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void exitVariable(astrap.VariableContext ctx);
	/**
	 * Enter a parse tree produced by the {@code true}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void enterTrue(astrap.TrueContext ctx);
	/**
	 * Exit a parse tree produced by the {@code true}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void exitTrue(astrap.TrueContext ctx);
	/**
	 * Enter a parse tree produced by the {@code elvis}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void enterElvis(astrap.ElvisContext ctx);
	/**
	 * Exit a parse tree produced by the {@code elvis}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void exitElvis(astrap.ElvisContext ctx);
	/**
	 * Enter a parse tree produced by the {@code if_else}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void enterIf_else(astrap.If_elseContext ctx);
	/**
	 * Exit a parse tree produced by the {@code if_else}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void exitIf_else(astrap.If_elseContext ctx);
	/**
	 * Enter a parse tree produced by the {@code lookup_call}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void enterLookup_call(astrap.Lookup_callContext ctx);
	/**
	 * Exit a parse tree produced by the {@code lookup_call}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void exitLookup_call(astrap.Lookup_callContext ctx);
	/**
	 * Enter a parse tree produced by the {@code safe_call}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void enterSafe_call(astrap.Safe_callContext ctx);
	/**
	 * Exit a parse tree produced by the {@code safe_call}
	 * labeled alternative in {@link astrap#expr}.
	 * @param ctx the parse tree
	 */
	void exitSafe_call(astrap.Safe_callContext ctx);
	/**
	 * Enter a parse tree produced by {@link astrap#lookup}.
	 * @param ctx the parse tree
	 */
	void enterLookup(astrap.LookupContext ctx);
	/**
	 * Exit a parse tree produced by {@link astrap#lookup}.
	 * @param ctx the parse tree
	 */
	void exitLookup(astrap.LookupContext ctx);
	/**
	 * Enter a parse tree produced by {@link astrap#type_args}.
	 * @param ctx the parse tree
	 */
	void enterType_args(astrap.Type_argsContext ctx);
	/**
	 * Exit a parse tree produced by {@link astrap#type_args}.
	 * @param ctx the parse tree
	 */
	void exitType_args(astrap.Type_argsContext ctx);
	/**
	 * Enter a parse tree produced by the {@code float_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 */
	void enterFloat_pattern(astrap.Float_patternContext ctx);
	/**
	 * Exit a parse tree produced by the {@code float_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 */
	void exitFloat_pattern(astrap.Float_patternContext ctx);
	/**
	 * Enter a parse tree produced by the {@code constructor_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 */
	void enterConstructor_pattern(astrap.Constructor_patternContext ctx);
	/**
	 * Exit a parse tree produced by the {@code constructor_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 */
	void exitConstructor_pattern(astrap.Constructor_patternContext ctx);
	/**
	 * Enter a parse tree produced by the {@code null_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 */
	void enterNull_pattern(astrap.Null_patternContext ctx);
	/**
	 * Exit a parse tree produced by the {@code null_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 */
	void exitNull_pattern(astrap.Null_patternContext ctx);
	/**
	 * Enter a parse tree produced by the {@code int_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 */
	void enterInt_pattern(astrap.Int_patternContext ctx);
	/**
	 * Exit a parse tree produced by the {@code int_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 */
	void exitInt_pattern(astrap.Int_patternContext ctx);
	/**
	 * Enter a parse tree produced by the {@code array_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 */
	void enterArray_pattern(astrap.Array_patternContext ctx);
	/**
	 * Exit a parse tree produced by the {@code array_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 */
	void exitArray_pattern(astrap.Array_patternContext ctx);
	/**
	 * Enter a parse tree produced by the {@code wildcard}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 */
	void enterWildcard(astrap.WildcardContext ctx);
	/**
	 * Exit a parse tree produced by the {@code wildcard}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 */
	void exitWildcard(astrap.WildcardContext ctx);
	/**
	 * Enter a parse tree produced by the {@code raw_str_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 */
	void enterRaw_str_pattern(astrap.Raw_str_patternContext ctx);
	/**
	 * Exit a parse tree produced by the {@code raw_str_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 */
	void exitRaw_str_pattern(astrap.Raw_str_patternContext ctx);
	/**
	 * Enter a parse tree produced by the {@code cast_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 */
	void enterCast_pattern(astrap.Cast_patternContext ctx);
	/**
	 * Exit a parse tree produced by the {@code cast_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 */
	void exitCast_pattern(astrap.Cast_patternContext ctx);
	/**
	 * Enter a parse tree produced by the {@code name_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 */
	void enterName_pattern(astrap.Name_patternContext ctx);
	/**
	 * Exit a parse tree produced by the {@code name_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 */
	void exitName_pattern(astrap.Name_patternContext ctx);
	/**
	 * Enter a parse tree produced by the {@code val_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 */
	void enterVal_pattern(astrap.Val_patternContext ctx);
	/**
	 * Exit a parse tree produced by the {@code val_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 */
	void exitVal_pattern(astrap.Val_patternContext ctx);
	/**
	 * Enter a parse tree produced by the {@code tuple_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 */
	void enterTuple_pattern(astrap.Tuple_patternContext ctx);
	/**
	 * Exit a parse tree produced by the {@code tuple_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 */
	void exitTuple_pattern(astrap.Tuple_patternContext ctx);
	/**
	 * Enter a parse tree produced by the {@code str_starts_with_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 */
	void enterStr_starts_with_pattern(astrap.Str_starts_with_patternContext ctx);
	/**
	 * Exit a parse tree produced by the {@code str_starts_with_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 */
	void exitStr_starts_with_pattern(astrap.Str_starts_with_patternContext ctx);
	/**
	 * Enter a parse tree produced by the {@code logic_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 */
	void enterLogic_pattern(astrap.Logic_patternContext ctx);
	/**
	 * Exit a parse tree produced by the {@code logic_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 */
	void exitLogic_pattern(astrap.Logic_patternContext ctx);
	/**
	 * Enter a parse tree produced by the {@code paren_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 */
	void enterParen_pattern(astrap.Paren_patternContext ctx);
	/**
	 * Exit a parse tree produced by the {@code paren_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 */
	void exitParen_pattern(astrap.Paren_patternContext ctx);
	/**
	 * Enter a parse tree produced by the {@code alias_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 */
	void enterAlias_pattern(astrap.Alias_patternContext ctx);
	/**
	 * Exit a parse tree produced by the {@code alias_pattern}
	 * labeled alternative in {@link astrap#pattern}.
	 * @param ctx the parse tree
	 */
	void exitAlias_pattern(astrap.Alias_patternContext ctx);
	/**
	 * Enter a parse tree produced by {@link astrap#default_clause}.
	 * @param ctx the parse tree
	 */
	void enterDefault_clause(astrap.Default_clauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link astrap#default_clause}.
	 * @param ctx the parse tree
	 */
	void exitDefault_clause(astrap.Default_clauseContext ctx);
	/**
	 * Enter a parse tree produced by the {@code singleton_constructor}
	 * labeled alternative in {@link astrap#con_pattern}.
	 * @param ctx the parse tree
	 */
	void enterSingleton_constructor(astrap.Singleton_constructorContext ctx);
	/**
	 * Exit a parse tree produced by the {@code singleton_constructor}
	 * labeled alternative in {@link astrap#con_pattern}.
	 * @param ctx the parse tree
	 */
	void exitSingleton_constructor(astrap.Singleton_constructorContext ctx);
	/**
	 * Enter a parse tree produced by the {@code empty_constructor}
	 * labeled alternative in {@link astrap#con_pattern}.
	 * @param ctx the parse tree
	 */
	void enterEmpty_constructor(astrap.Empty_constructorContext ctx);
	/**
	 * Exit a parse tree produced by the {@code empty_constructor}
	 * labeled alternative in {@link astrap#con_pattern}.
	 * @param ctx the parse tree
	 */
	void exitEmpty_constructor(astrap.Empty_constructorContext ctx);
	/**
	 * Enter a parse tree produced by the {@code non_empty_constructor}
	 * labeled alternative in {@link astrap#con_pattern}.
	 * @param ctx the parse tree
	 */
	void enterNon_empty_constructor(astrap.Non_empty_constructorContext ctx);
	/**
	 * Exit a parse tree produced by the {@code non_empty_constructor}
	 * labeled alternative in {@link astrap#con_pattern}.
	 * @param ctx the parse tree
	 */
	void exitNon_empty_constructor(astrap.Non_empty_constructorContext ctx);
	/**
	 * Enter a parse tree produced by the {@code empty_array}
	 * labeled alternative in {@link astrap#arr_pattern}.
	 * @param ctx the parse tree
	 */
	void enterEmpty_array(astrap.Empty_arrayContext ctx);
	/**
	 * Exit a parse tree produced by the {@code empty_array}
	 * labeled alternative in {@link astrap#arr_pattern}.
	 * @param ctx the parse tree
	 */
	void exitEmpty_array(astrap.Empty_arrayContext ctx);
	/**
	 * Enter a parse tree produced by the {@code non_empty_array}
	 * labeled alternative in {@link astrap#arr_pattern}.
	 * @param ctx the parse tree
	 */
	void enterNon_empty_array(astrap.Non_empty_arrayContext ctx);
	/**
	 * Exit a parse tree produced by the {@code non_empty_array}
	 * labeled alternative in {@link astrap#arr_pattern}.
	 * @param ctx the parse tree
	 */
	void exitNon_empty_array(astrap.Non_empty_arrayContext ctx);
	/**
	 * Enter a parse tree produced by {@link astrap#name}.
	 * @param ctx the parse tree
	 */
	void enterName(astrap.NameContext ctx);
	/**
	 * Exit a parse tree produced by {@link astrap#name}.
	 * @param ctx the parse tree
	 */
	void exitName(astrap.NameContext ctx);
}