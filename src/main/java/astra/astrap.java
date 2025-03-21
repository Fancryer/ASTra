// Generated from A:/Downloads/KAST/src/main/resources/astrap.g4 by ANTLR 4.13.2
package astra;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast", "CheckReturnValue", "this-escape"})
public class astrap extends Parser {
	static { RuntimeMetaData.checkVersion("4.13.2", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		As=1, Grammar=2, Default=3, Rule=4, True=5, False=6, Lookup=7, Null=8, 
		Match=9, To=10, When=11, Import=12, If=13, Else=14, Package=15, BacktickName=16, 
		Caret=17, Underscore=18, Elvis=19, Arrow=20, Dot=21, SafeDot=22, Colon=23, 
		Semicolon=24, Equals=25, Bind=26, RecBind=27, BracketStart=28, BracketEnd=29, 
		BraceStart=30, BraceEnd=31, ParenStart=32, ParenEnd=33, LookupStart=34, 
		LookupEnd=35, Comma=36, Less=37, Greater=38, Question=39, Sharp=40, Forward=41, 
		Pipe=42, DoubleQuestion=43, Star=44, BlockStart=45, BlockEnd=46, KtBlockStart=47, 
		KtBlockEnd=48, Val=49, Var=50, Name=51, InfixOperator=52, Int=53, Float=54, 
		STRING=55, WS=56, COMMENT=57;
	public static final int
		RULE_program = 0, RULE_package = 1, RULE_import_ = 2, RULE_rule_ = 3, 
		RULE_mapper_rule = 4, RULE_default = 5, RULE_result = 6, RULE_block = 7, 
		RULE_match_clause = 8, RULE_stat = 9, RULE_decl = 10, RULE_type = 11, 
		RULE_expr = 12, RULE_lookup = 13, RULE_type_args = 14, RULE_pattern = 15, 
		RULE_default_clause = 16, RULE_con_pattern = 17, RULE_arr_pattern = 18, 
		RULE_name = 19;
	private static String[] makeRuleNames() {
		return new String[] {
			"program", "package", "import_", "rule_", "mapper_rule", "default", "result", 
			"block", "match_clause", "stat", "decl", "type", "expr", "lookup", "type_args", 
			"pattern", "default_clause", "con_pattern", "arr_pattern", "name"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'as'", "'grammar'", "'default'", "'rule'", "'true'", "'false'", 
			"'lookup'", "'null'", "'match'", "'to'", "'when'", "'import'", "'if'", 
			"'else'", "'package'", null, "'^'", "'_'", "'?:'", "'->'", "'.'", "'?.'", 
			"':'", "';'", "'='", "'::='", "'<|>'", "'['", "']'", "'{'", "'}'", "'('", 
			"')'", "'|?'", "'?|'", "','", "'<'", "'>'", "'?'", "'#'", "'|->'", "'|'", 
			"'??'", "'*'", "'{#'", "'#}'", "'{|'", "'|}'", "'val'", "'var'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, "As", "Grammar", "Default", "Rule", "True", "False", "Lookup", 
			"Null", "Match", "To", "When", "Import", "If", "Else", "Package", "BacktickName", 
			"Caret", "Underscore", "Elvis", "Arrow", "Dot", "SafeDot", "Colon", "Semicolon", 
			"Equals", "Bind", "RecBind", "BracketStart", "BracketEnd", "BraceStart", 
			"BraceEnd", "ParenStart", "ParenEnd", "LookupStart", "LookupEnd", "Comma", 
			"Less", "Greater", "Question", "Sharp", "Forward", "Pipe", "DoubleQuestion", 
			"Star", "BlockStart", "BlockEnd", "KtBlockStart", "KtBlockEnd", "Val", 
			"Var", "Name", "InfixOperator", "Int", "Float", "STRING", "WS", "COMMENT"
		};
	}
	private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() { return "astrap.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public astrap(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ProgramContext extends ParserRuleContext {
		public TerminalNode Grammar() { return getToken(astrap.Grammar, 0); }
		public TerminalNode Name() { return getToken(astrap.Name, 0); }
		public TerminalNode EOF() { return getToken(astrap.EOF, 0); }
		public PackageContext package_() {
			return getRuleContext(PackageContext.class,0);
		}
		public List<Import_Context> import_() {
			return getRuleContexts(Import_Context.class);
		}
		public Import_Context import_(int i) {
			return getRuleContext(Import_Context.class,i);
		}
		public List<Rule_Context> rule_() {
			return getRuleContexts(Rule_Context.class);
		}
		public Rule_Context rule_(int i) {
			return getRuleContext(Rule_Context.class,i);
		}
		public ProgramContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_program; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterProgram(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitProgram(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitProgram(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ProgramContext program() throws RecognitionException {
		ProgramContext _localctx = new ProgramContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_program);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(40);
			match(Grammar);
			setState(41);
			match(Name);
			setState(43);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==Package) {
				{
				setState(42);
				package_();
				}
			}

			setState(48);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==Import) {
				{
				{
				setState(45);
				import_();
				}
				}
				setState(50);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(54);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & 2251799813750792L) != 0)) {
				{
				{
				setState(51);
				rule_();
				}
				}
				setState(56);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(57);
			match(EOF);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class PackageContext extends ParserRuleContext {
		public TerminalNode Package() { return getToken(astrap.Package, 0); }
		public List<TerminalNode> Name() { return getTokens(astrap.Name); }
		public TerminalNode Name(int i) {
			return getToken(astrap.Name, i);
		}
		public List<TerminalNode> Dot() { return getTokens(astrap.Dot); }
		public TerminalNode Dot(int i) {
			return getToken(astrap.Dot, i);
		}
		public TerminalNode Semicolon() { return getToken(astrap.Semicolon, 0); }
		public PackageContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_package; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterPackage(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitPackage(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitPackage(this);
			else return visitor.visitChildren(this);
		}
	}

	public final PackageContext package_() throws RecognitionException {
		PackageContext _localctx = new PackageContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_package);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(59);
			match(Package);
			setState(60);
			match(Name);
			setState(65);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==Dot) {
				{
				{
				setState(61);
				match(Dot);
				setState(62);
				match(Name);
				}
				}
				setState(67);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(69);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==Semicolon) {
				{
				setState(68);
				match(Semicolon);
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class Import_Context extends ParserRuleContext {
		public TerminalNode Import() { return getToken(astrap.Import, 0); }
		public List<TerminalNode> Name() { return getTokens(astrap.Name); }
		public TerminalNode Name(int i) {
			return getToken(astrap.Name, i);
		}
		public List<TerminalNode> Dot() { return getTokens(astrap.Dot); }
		public TerminalNode Dot(int i) {
			return getToken(astrap.Dot, i);
		}
		public TerminalNode Star() { return getToken(astrap.Star, 0); }
		public TerminalNode Semicolon() { return getToken(astrap.Semicolon, 0); }
		public Import_Context(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_import_; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterImport_(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitImport_(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitImport_(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Import_Context import_() throws RecognitionException {
		Import_Context _localctx = new Import_Context(_ctx, getState());
		enterRule(_localctx, 4, RULE_import_);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(71);
			match(Import);
			setState(72);
			match(Name);
			setState(77);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,5,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(73);
					match(Dot);
					setState(74);
					match(Name);
					}
					} 
				}
				setState(79);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,5,_ctx);
			}
			setState(82);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==Dot) {
				{
				setState(80);
				match(Dot);
				setState(81);
				match(Star);
				}
			}

			setState(85);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==Semicolon) {
				{
				setState(84);
				match(Semicolon);
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class Rule_Context extends ParserRuleContext {
		public Mapper_ruleContext mapper_rule() {
			return getRuleContext(Mapper_ruleContext.class,0);
		}
		public DefaultContext default_() {
			return getRuleContext(DefaultContext.class,0);
		}
		public Rule_Context(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_rule_; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterRule_(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitRule_(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitRule_(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Rule_Context rule_() throws RecognitionException {
		Rule_Context _localctx = new Rule_Context(_ctx, getState());
		enterRule(_localctx, 6, RULE_rule_);
		try {
			setState(89);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case BacktickName:
			case Name:
				enterOuterAlt(_localctx, 1);
				{
				setState(87);
				mapper_rule();
				}
				break;
			case Default:
				enterOuterAlt(_localctx, 2);
				{
				setState(88);
				default_();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class Mapper_ruleContext extends ParserRuleContext {
		public NameContext rule_name;
		public NameContext alias;
		public NameContext from;
		public NameContext to;
		public NameContext ctx_name;
		public NameContext fix_name;
		public TerminalNode Comma() { return getToken(astrap.Comma, 0); }
		public List<NameContext> name() {
			return getRuleContexts(NameContext.class);
		}
		public NameContext name(int i) {
			return getRuleContext(NameContext.class,i);
		}
		public TerminalNode Underscore() { return getToken(astrap.Underscore, 0); }
		public TerminalNode Bind() { return getToken(astrap.Bind, 0); }
		public TerminalNode RecBind() { return getToken(astrap.RecBind, 0); }
		public BlockContext block() {
			return getRuleContext(BlockContext.class,0);
		}
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public Mapper_ruleContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_mapper_rule; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterMapper_rule(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitMapper_rule(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitMapper_rule(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Mapper_ruleContext mapper_rule() throws RecognitionException {
		Mapper_ruleContext _localctx = new Mapper_ruleContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_mapper_rule);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(91);
			((Mapper_ruleContext)_localctx).rule_name = name();
			setState(93);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,9,_ctx) ) {
			case 1:
				{
				setState(92);
				((Mapper_ruleContext)_localctx).alias = name();
				}
				break;
			}
			setState(95);
			((Mapper_ruleContext)_localctx).from = name();
			setState(96);
			match(Comma);
			setState(97);
			((Mapper_ruleContext)_localctx).to = name();
			setState(100);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case BacktickName:
			case Name:
				{
				setState(98);
				((Mapper_ruleContext)_localctx).ctx_name = name();
				}
				break;
			case Underscore:
				{
				setState(99);
				match(Underscore);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(106);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case Bind:
				{
				setState(102);
				match(Bind);
				}
				break;
			case BacktickName:
			case Name:
				{
				setState(103);
				((Mapper_ruleContext)_localctx).fix_name = name();
				setState(104);
				match(RecBind);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(110);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,12,_ctx) ) {
			case 1:
				{
				setState(108);
				block();
				}
				break;
			case 2:
				{
				setState(109);
				expr(0);
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class DefaultContext extends ParserRuleContext {
		public TerminalNode Default() { return getToken(astrap.Default, 0); }
		public ResultContext result() {
			return getRuleContext(ResultContext.class,0);
		}
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public DefaultContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_default; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterDefault(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitDefault(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitDefault(this);
			else return visitor.visitChildren(this);
		}
	}

	public final DefaultContext default_() throws RecognitionException {
		DefaultContext _localctx = new DefaultContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_default);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(112);
			match(Default);
			setState(114);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,13,_ctx) ) {
			case 1:
				{
				setState(113);
				name();
				}
				break;
			}
			setState(116);
			result();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ResultContext extends ParserRuleContext {
		public BlockContext block() {
			return getRuleContext(BlockContext.class,0);
		}
		public ResultContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_result; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterResult(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitResult(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitResult(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ResultContext result() throws RecognitionException {
		ResultContext _localctx = new ResultContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_result);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(118);
			block();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class BlockContext extends ParserRuleContext {
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public List<StatContext> stat() {
			return getRuleContexts(StatContext.class);
		}
		public StatContext stat(int i) {
			return getRuleContext(StatContext.class,i);
		}
		public List<TerminalNode> Semicolon() { return getTokens(astrap.Semicolon); }
		public TerminalNode Semicolon(int i) {
			return getToken(astrap.Semicolon, i);
		}
		public BlockContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_block; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterBlock(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitBlock(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitBlock(this);
			else return visitor.visitChildren(this);
		}
	}

	public final BlockContext block() throws RecognitionException {
		BlockContext _localctx = new BlockContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_block);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(125);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,14,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(120);
					stat();
					setState(121);
					match(Semicolon);
					}
					} 
				}
				setState(127);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,14,_ctx);
			}
			setState(128);
			expr(0);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class Match_clauseContext extends ParserRuleContext {
		public TerminalNode Pipe() { return getToken(astrap.Pipe, 0); }
		public TerminalNode Arrow() { return getToken(astrap.Arrow, 0); }
		public TerminalNode Colon() { return getToken(astrap.Colon, 0); }
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public PatternContext pattern() {
			return getRuleContext(PatternContext.class,0);
		}
		public BlockContext block() {
			return getRuleContext(BlockContext.class,0);
		}
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public TerminalNode When() { return getToken(astrap.When, 0); }
		public Match_clauseContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_match_clause; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterMatch_clause(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitMatch_clause(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitMatch_clause(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Match_clauseContext match_clause() throws RecognitionException {
		Match_clauseContext _localctx = new Match_clauseContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_match_clause);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(130);
			match(Pipe);
			setState(134);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case Colon:
				{
				setState(131);
				match(Colon);
				setState(132);
				name();
				}
				break;
			case True:
			case False:
			case Null:
			case BacktickName:
			case Underscore:
			case BracketStart:
			case ParenStart:
			case Question:
			case Sharp:
			case Val:
			case Name:
			case Int:
			case Float:
			case STRING:
				{
				setState(133);
				pattern(0);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(138);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==When) {
				{
				setState(136);
				match(When);
				setState(137);
				expr(0);
				}
			}

			setState(140);
			match(Arrow);
			setState(143);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,17,_ctx) ) {
			case 1:
				{
				setState(141);
				block();
				}
				break;
			case 2:
				{
				setState(142);
				expr(0);
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class StatContext extends ParserRuleContext {
		public DeclContext decl() {
			return getRuleContext(DeclContext.class,0);
		}
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public StatContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_stat; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterStat(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitStat(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitStat(this);
			else return visitor.visitChildren(this);
		}
	}

	public final StatContext stat() throws RecognitionException {
		StatContext _localctx = new StatContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_stat);
		try {
			setState(147);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case Val:
			case Var:
				enterOuterAlt(_localctx, 1);
				{
				setState(145);
				decl();
				}
				break;
			case True:
			case False:
			case Null:
			case Match:
			case If:
			case BacktickName:
			case BraceStart:
			case ParenStart:
			case LookupStart:
			case Name:
			case Int:
			case Float:
			case STRING:
				enterOuterAlt(_localctx, 2);
				{
				setState(146);
				expr(0);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class DeclContext extends ParserRuleContext {
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public TerminalNode Equals() { return getToken(astrap.Equals, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode Val() { return getToken(astrap.Val, 0); }
		public TerminalNode Var() { return getToken(astrap.Var, 0); }
		public TerminalNode Colon() { return getToken(astrap.Colon, 0); }
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public DeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_decl; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterDecl(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitDecl(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitDecl(this);
			else return visitor.visitChildren(this);
		}
	}

	public final DeclContext decl() throws RecognitionException {
		DeclContext _localctx = new DeclContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_decl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(149);
			_la = _input.LA(1);
			if ( !(_la==Val || _la==Var) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			setState(150);
			name();
			setState(153);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==Colon) {
				{
				setState(151);
				match(Colon);
				setState(152);
				type(0);
				}
			}

			setState(155);
			match(Equals);
			setState(156);
			expr(0);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TypeContext extends ParserRuleContext {
		public TypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_type; }
	 
		public TypeContext() { }
		public void copyFrom(TypeContext ctx) {
			super.copyFrom(ctx);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Simple_typeContext extends TypeContext {
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public Simple_typeContext(TypeContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterSimple_type(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitSimple_type(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitSimple_type(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Function_typeContext extends TypeContext {
		public List<TypeContext> type() {
			return getRuleContexts(TypeContext.class);
		}
		public TypeContext type(int i) {
			return getRuleContext(TypeContext.class,i);
		}
		public TerminalNode Arrow() { return getToken(astrap.Arrow, 0); }
		public Function_typeContext(TypeContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterFunction_type(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitFunction_type(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitFunction_type(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Array_typeContext extends TypeContext {
		public TerminalNode BracketStart() { return getToken(astrap.BracketStart, 0); }
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public TerminalNode BracketEnd() { return getToken(astrap.BracketEnd, 0); }
		public Array_typeContext(TypeContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterArray_type(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitArray_type(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitArray_type(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Paren_typeContext extends TypeContext {
		public TerminalNode ParenStart() { return getToken(astrap.ParenStart, 0); }
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public TerminalNode ParenEnd() { return getToken(astrap.ParenEnd, 0); }
		public Paren_typeContext(TypeContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterParen_type(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitParen_type(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitParen_type(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TypeContext type() throws RecognitionException {
		return type(0);
	}

	private TypeContext type(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		TypeContext _localctx = new TypeContext(_ctx, _parentState);
		TypeContext _prevctx = _localctx;
		int _startState = 22;
		enterRecursionRule(_localctx, 22, RULE_type, _p);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(168);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case BacktickName:
			case Name:
				{
				_localctx = new Simple_typeContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;

				setState(159);
				name();
				}
				break;
			case BracketStart:
				{
				_localctx = new Array_typeContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(160);
				match(BracketStart);
				setState(161);
				type(0);
				setState(162);
				match(BracketEnd);
				}
				break;
			case ParenStart:
				{
				_localctx = new Paren_typeContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(164);
				match(ParenStart);
				setState(165);
				type(0);
				setState(166);
				match(ParenEnd);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			_ctx.stop = _input.LT(-1);
			setState(175);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,21,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new Function_typeContext(new TypeContext(_parentctx, _parentState));
					pushNewRecursionContext(_localctx, _startState, RULE_type);
					setState(170);
					if (!(precpred(_ctx, 3))) throw new FailedPredicateException(this, "precpred(_ctx, 3)");
					setState(171);
					match(Arrow);
					setState(172);
					type(4);
					}
					} 
				}
				setState(177);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,21,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ExprContext extends ParserRuleContext {
		public ExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_expr; }
	 
		public ExprContext() { }
		public void copyFrom(ExprContext ctx) {
			super.copyFrom(ctx);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class AccessContext extends ExprContext {
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode Dot() { return getToken(astrap.Dot, 0); }
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public AccessContext(ExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterAccess(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitAccess(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitAccess(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class StringContext extends ExprContext {
		public TerminalNode STRING() { return getToken(astrap.STRING, 0); }
		public StringContext(ExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterString(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitString(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitString(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Match_toContext extends ExprContext {
		public TerminalNode Match() { return getToken(astrap.Match, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode To() { return getToken(astrap.To, 0); }
		public List<Match_clauseContext> match_clause() {
			return getRuleContexts(Match_clauseContext.class);
		}
		public Match_clauseContext match_clause(int i) {
			return getRuleContext(Match_clauseContext.class,i);
		}
		public Default_clauseContext default_clause() {
			return getRuleContext(Default_clauseContext.class,0);
		}
		public Match_toContext(ExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterMatch_to(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitMatch_to(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitMatch_to(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class FalseContext extends ExprContext {
		public TerminalNode False() { return getToken(astrap.False, 0); }
		public FalseContext(ExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterFalse(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitFalse(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitFalse(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class FloatContext extends ExprContext {
		public TerminalNode Float() { return getToken(astrap.Float, 0); }
		public FloatContext(ExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterFloat(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitFloat(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitFloat(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class IntContext extends ExprContext {
		public TerminalNode Int() { return getToken(astrap.Int, 0); }
		public IntContext(ExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterInt(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitInt(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitInt(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class CallContext extends ExprContext {
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public TerminalNode ParenStart() { return getToken(astrap.ParenStart, 0); }
		public TerminalNode ParenEnd() { return getToken(astrap.ParenEnd, 0); }
		public Type_argsContext type_args() {
			return getRuleContext(Type_argsContext.class,0);
		}
		public List<TerminalNode> Comma() { return getTokens(astrap.Comma); }
		public TerminalNode Comma(int i) {
			return getToken(astrap.Comma, i);
		}
		public CallContext(ExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterCall(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitCall(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitCall(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Infix_callContext extends ExprContext {
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public TerminalNode InfixOperator() { return getToken(astrap.InfixOperator, 0); }
		public Infix_callContext(ExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterInfix_call(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitInfix_call(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitInfix_call(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class ParenContext extends ExprContext {
		public TerminalNode ParenStart() { return getToken(astrap.ParenStart, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode ParenEnd() { return getToken(astrap.ParenEnd, 0); }
		public ParenContext(ExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterParen(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitParen(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitParen(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class LambdaContext extends ExprContext {
		public TerminalNode BraceStart() { return getToken(astrap.BraceStart, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode BraceEnd() { return getToken(astrap.BraceEnd, 0); }
		public List<NameContext> name() {
			return getRuleContexts(NameContext.class);
		}
		public NameContext name(int i) {
			return getRuleContext(NameContext.class,i);
		}
		public List<TerminalNode> Colon() { return getTokens(astrap.Colon); }
		public TerminalNode Colon(int i) {
			return getToken(astrap.Colon, i);
		}
		public List<TypeContext> type() {
			return getRuleContexts(TypeContext.class);
		}
		public TypeContext type(int i) {
			return getRuleContext(TypeContext.class,i);
		}
		public TerminalNode Arrow() { return getToken(astrap.Arrow, 0); }
		public List<TerminalNode> Comma() { return getTokens(astrap.Comma); }
		public TerminalNode Comma(int i) {
			return getToken(astrap.Comma, i);
		}
		public LambdaContext(ExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterLambda(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitLambda(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitLambda(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class NullContext extends ExprContext {
		public TerminalNode Null() { return getToken(astrap.Null, 0); }
		public NullContext(ExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterNull(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitNull(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitNull(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Safe_accessContext extends ExprContext {
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode SafeDot() { return getToken(astrap.SafeDot, 0); }
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public Safe_accessContext(ExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterSafe_access(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitSafe_access(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitSafe_access(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class VariableContext extends ExprContext {
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public VariableContext(ExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterVariable(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitVariable(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitVariable(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class TrueContext extends ExprContext {
		public TerminalNode True() { return getToken(astrap.True, 0); }
		public TrueContext(ExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterTrue(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitTrue(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitTrue(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class ElvisContext extends ExprContext {
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public TerminalNode Elvis() { return getToken(astrap.Elvis, 0); }
		public ElvisContext(ExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterElvis(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitElvis(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitElvis(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class If_elseContext extends ExprContext {
		public TerminalNode If() { return getToken(astrap.If, 0); }
		public TerminalNode ParenStart() { return getToken(astrap.ParenStart, 0); }
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public TerminalNode ParenEnd() { return getToken(astrap.ParenEnd, 0); }
		public TerminalNode Else() { return getToken(astrap.Else, 0); }
		public If_elseContext(ExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterIf_else(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitIf_else(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitIf_else(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Lookup_callContext extends ExprContext {
		public LookupContext lookup() {
			return getRuleContext(LookupContext.class,0);
		}
		public Lookup_callContext(ExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterLookup_call(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitLookup_call(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitLookup_call(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Safe_callContext extends ExprContext {
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public TerminalNode DoubleQuestion() { return getToken(astrap.DoubleQuestion, 0); }
		public TerminalNode ParenStart() { return getToken(astrap.ParenStart, 0); }
		public TerminalNode ParenEnd() { return getToken(astrap.ParenEnd, 0); }
		public Type_argsContext type_args() {
			return getRuleContext(Type_argsContext.class,0);
		}
		public List<TerminalNode> Comma() { return getTokens(astrap.Comma); }
		public TerminalNode Comma(int i) {
			return getToken(astrap.Comma, i);
		}
		public Safe_callContext(ExprContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterSafe_call(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitSafe_call(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitSafe_call(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ExprContext expr() throws RecognitionException {
		return expr(0);
	}

	private ExprContext expr(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		ExprContext _localctx = new ExprContext(_ctx, _parentState);
		ExprContext _prevctx = _localctx;
		int _startState = 24;
		enterRecursionRule(_localctx, 24, RULE_expr, _p);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(232);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case Int:
				{
				_localctx = new IntContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;

				setState(179);
				match(Int);
				}
				break;
			case Null:
				{
				_localctx = new NullContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(180);
				match(Null);
				}
				break;
			case Float:
				{
				_localctx = new FloatContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(181);
				match(Float);
				}
				break;
			case STRING:
				{
				_localctx = new StringContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(182);
				match(STRING);
				}
				break;
			case BacktickName:
			case Name:
				{
				_localctx = new VariableContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(183);
				name();
				}
				break;
			case True:
				{
				_localctx = new TrueContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(184);
				match(True);
				}
				break;
			case False:
				{
				_localctx = new FalseContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(185);
				match(False);
				}
				break;
			case If:
				{
				_localctx = new If_elseContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(186);
				match(If);
				setState(187);
				match(ParenStart);
				setState(188);
				expr(0);
				setState(189);
				match(ParenEnd);
				setState(190);
				expr(0);
				setState(193);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,22,_ctx) ) {
				case 1:
					{
					setState(191);
					match(Else);
					setState(192);
					expr(0);
					}
					break;
				}
				}
				break;
			case LookupStart:
				{
				_localctx = new Lookup_callContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(195);
				lookup();
				}
				break;
			case ParenStart:
				{
				_localctx = new ParenContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(196);
				match(ParenStart);
				setState(197);
				expr(0);
				setState(198);
				match(ParenEnd);
				}
				break;
			case BraceStart:
				{
				_localctx = new LambdaContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(200);
				match(BraceStart);
				setState(216);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,24,_ctx) ) {
				case 1:
					{
					setState(201);
					name();
					setState(202);
					match(Colon);
					setState(203);
					type(0);
					setState(211);
					_errHandler.sync(this);
					_la = _input.LA(1);
					while (_la==Comma) {
						{
						{
						setState(204);
						match(Comma);
						setState(205);
						name();
						setState(206);
						match(Colon);
						setState(207);
						type(0);
						}
						}
						setState(213);
						_errHandler.sync(this);
						_la = _input.LA(1);
					}
					setState(214);
					match(Arrow);
					}
					break;
				}
				setState(218);
				expr(0);
				setState(219);
				match(BraceEnd);
				}
				break;
			case Match:
				{
				_localctx = new Match_toContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(221);
				match(Match);
				setState(222);
				expr(0);
				setState(223);
				match(To);
				setState(225); 
				_errHandler.sync(this);
				_alt = 1;
				do {
					switch (_alt) {
					case 1:
						{
						{
						setState(224);
						match_clause();
						}
						}
						break;
					default:
						throw new NoViableAltException(this);
					}
					setState(227); 
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,25,_ctx);
				} while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER );
				setState(230);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,26,_ctx) ) {
				case 1:
					{
					setState(229);
					default_clause();
					}
					break;
				}
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			_ctx.stop = _input.LT(-1);
			setState(284);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,36,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					setState(282);
					_errHandler.sync(this);
					switch ( getInterpreter().adaptivePredict(_input,35,_ctx) ) {
					case 1:
						{
						_localctx = new Infix_callContext(new ExprContext(_parentctx, _parentState));
						pushNewRecursionContext(_localctx, _startState, RULE_expr);
						setState(234);
						if (!(precpred(_ctx, 9))) throw new FailedPredicateException(this, "precpred(_ctx, 9)");
						setState(235);
						match(InfixOperator);
						setState(236);
						expr(10);
						}
						break;
					case 2:
						{
						_localctx = new ElvisContext(new ExprContext(_parentctx, _parentState));
						pushNewRecursionContext(_localctx, _startState, RULE_expr);
						setState(237);
						if (!(precpred(_ctx, 5))) throw new FailedPredicateException(this, "precpred(_ctx, 5)");
						setState(238);
						match(Elvis);
						setState(239);
						expr(6);
						}
						break;
					case 3:
						{
						_localctx = new CallContext(new ExprContext(_parentctx, _parentState));
						pushNewRecursionContext(_localctx, _startState, RULE_expr);
						setState(240);
						if (!(precpred(_ctx, 10))) throw new FailedPredicateException(this, "precpred(_ctx, 10)");
						setState(242);
						_errHandler.sync(this);
						_la = _input.LA(1);
						if (_la==Less) {
							{
							setState(241);
							type_args();
							}
						}

						setState(244);
						match(ParenStart);
						setState(253);
						_errHandler.sync(this);
						_la = _input.LA(1);
						if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 65302217145525088L) != 0)) {
							{
							setState(245);
							expr(0);
							setState(250);
							_errHandler.sync(this);
							_la = _input.LA(1);
							while (_la==Comma) {
								{
								{
								setState(246);
								match(Comma);
								setState(247);
								expr(0);
								}
								}
								setState(252);
								_errHandler.sync(this);
								_la = _input.LA(1);
							}
							}
						}

						setState(255);
						match(ParenEnd);
						}
						break;
					case 4:
						{
						_localctx = new AccessContext(new ExprContext(_parentctx, _parentState));
						pushNewRecursionContext(_localctx, _startState, RULE_expr);
						setState(256);
						if (!(precpred(_ctx, 8))) throw new FailedPredicateException(this, "precpred(_ctx, 8)");
						setState(257);
						match(Dot);
						setState(258);
						name();
						}
						break;
					case 5:
						{
						_localctx = new Safe_accessContext(new ExprContext(_parentctx, _parentState));
						pushNewRecursionContext(_localctx, _startState, RULE_expr);
						setState(259);
						if (!(precpred(_ctx, 7))) throw new FailedPredicateException(this, "precpred(_ctx, 7)");
						setState(260);
						match(SafeDot);
						setState(261);
						name();
						}
						break;
					case 6:
						{
						_localctx = new Safe_callContext(new ExprContext(_parentctx, _parentState));
						pushNewRecursionContext(_localctx, _startState, RULE_expr);
						setState(262);
						if (!(precpred(_ctx, 6))) throw new FailedPredicateException(this, "precpred(_ctx, 6)");
						setState(264);
						_errHandler.sync(this);
						_la = _input.LA(1);
						if (_la==Less) {
							{
							setState(263);
							type_args();
							}
						}

						setState(266);
						match(DoubleQuestion);
						setState(280);
						_errHandler.sync(this);
						switch ( getInterpreter().adaptivePredict(_input,34,_ctx) ) {
						case 1:
							{
							setState(267);
							expr(0);
							}
							break;
						case 2:
							{
							setState(268);
							match(ParenStart);
							setState(277);
							_errHandler.sync(this);
							_la = _input.LA(1);
							if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 65302217145525088L) != 0)) {
								{
								setState(269);
								expr(0);
								setState(274);
								_errHandler.sync(this);
								_la = _input.LA(1);
								while (_la==Comma) {
									{
									{
									setState(270);
									match(Comma);
									setState(271);
									expr(0);
									}
									}
									setState(276);
									_errHandler.sync(this);
									_la = _input.LA(1);
								}
								}
							}

							setState(279);
							match(ParenEnd);
							}
							break;
						}
						}
						break;
					}
					} 
				}
				setState(286);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,36,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class LookupContext extends ParserRuleContext {
		public TerminalNode LookupStart() { return getToken(astrap.LookupStart, 0); }
		public List<NameContext> name() {
			return getRuleContexts(NameContext.class);
		}
		public NameContext name(int i) {
			return getRuleContext(NameContext.class,i);
		}
		public TerminalNode LookupEnd() { return getToken(astrap.LookupEnd, 0); }
		public LookupContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_lookup; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterLookup(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitLookup(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitLookup(this);
			else return visitor.visitChildren(this);
		}
	}

	public final LookupContext lookup() throws RecognitionException {
		LookupContext _localctx = new LookupContext(_ctx, getState());
		enterRule(_localctx, 26, RULE_lookup);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(287);
			match(LookupStart);
			setState(288);
			name();
			setState(289);
			name();
			setState(290);
			match(LookupEnd);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class Type_argsContext extends ParserRuleContext {
		public TerminalNode Less() { return getToken(astrap.Less, 0); }
		public List<TypeContext> type() {
			return getRuleContexts(TypeContext.class);
		}
		public TypeContext type(int i) {
			return getRuleContext(TypeContext.class,i);
		}
		public TerminalNode Greater() { return getToken(astrap.Greater, 0); }
		public List<TerminalNode> Comma() { return getTokens(astrap.Comma); }
		public TerminalNode Comma(int i) {
			return getToken(astrap.Comma, i);
		}
		public Type_argsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_type_args; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterType_args(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitType_args(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitType_args(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Type_argsContext type_args() throws RecognitionException {
		Type_argsContext _localctx = new Type_argsContext(_ctx, getState());
		enterRule(_localctx, 28, RULE_type_args);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(292);
			match(Less);
			setState(293);
			type(0);
			setState(298);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==Comma) {
				{
				{
				setState(294);
				match(Comma);
				setState(295);
				type(0);
				}
				}
				setState(300);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(301);
			match(Greater);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class PatternContext extends ParserRuleContext {
		public PatternContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_pattern; }
	 
		public PatternContext() { }
		public void copyFrom(PatternContext ctx) {
			super.copyFrom(ctx);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Float_patternContext extends PatternContext {
		public TerminalNode Float() { return getToken(astrap.Float, 0); }
		public Float_patternContext(PatternContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterFloat_pattern(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitFloat_pattern(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitFloat_pattern(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Constructor_patternContext extends PatternContext {
		public TerminalNode Sharp() { return getToken(astrap.Sharp, 0); }
		public Con_patternContext con_pattern() {
			return getRuleContext(Con_patternContext.class,0);
		}
		public Constructor_patternContext(PatternContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterConstructor_pattern(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitConstructor_pattern(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitConstructor_pattern(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Null_patternContext extends PatternContext {
		public TerminalNode Null() { return getToken(astrap.Null, 0); }
		public TerminalNode Question() { return getToken(astrap.Question, 0); }
		public Null_patternContext(PatternContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterNull_pattern(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitNull_pattern(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitNull_pattern(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Int_patternContext extends PatternContext {
		public TerminalNode Int() { return getToken(astrap.Int, 0); }
		public Int_patternContext(PatternContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterInt_pattern(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitInt_pattern(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitInt_pattern(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Array_patternContext extends PatternContext {
		public Arr_patternContext arr_pattern() {
			return getRuleContext(Arr_patternContext.class,0);
		}
		public Array_patternContext(PatternContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterArray_pattern(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitArray_pattern(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitArray_pattern(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class WildcardContext extends PatternContext {
		public TerminalNode Underscore() { return getToken(astrap.Underscore, 0); }
		public WildcardContext(PatternContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterWildcard(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitWildcard(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitWildcard(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Raw_str_patternContext extends PatternContext {
		public TerminalNode STRING() { return getToken(astrap.STRING, 0); }
		public Raw_str_patternContext(PatternContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterRaw_str_pattern(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitRaw_str_pattern(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitRaw_str_pattern(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Cast_patternContext extends PatternContext {
		public PatternContext pattern() {
			return getRuleContext(PatternContext.class,0);
		}
		public TerminalNode Colon() { return getToken(astrap.Colon, 0); }
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public Cast_patternContext(PatternContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterCast_pattern(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitCast_pattern(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitCast_pattern(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Name_patternContext extends PatternContext {
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public Name_patternContext(PatternContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterName_pattern(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitName_pattern(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitName_pattern(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Val_patternContext extends PatternContext {
		public TerminalNode Val() { return getToken(astrap.Val, 0); }
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public Val_patternContext(PatternContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterVal_pattern(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitVal_pattern(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitVal_pattern(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Tuple_patternContext extends PatternContext {
		public List<PatternContext> pattern() {
			return getRuleContexts(PatternContext.class);
		}
		public PatternContext pattern(int i) {
			return getRuleContext(PatternContext.class,i);
		}
		public TerminalNode Comma() { return getToken(astrap.Comma, 0); }
		public Tuple_patternContext(PatternContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterTuple_pattern(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitTuple_pattern(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitTuple_pattern(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Str_starts_with_patternContext extends PatternContext {
		public List<PatternContext> pattern() {
			return getRuleContexts(PatternContext.class);
		}
		public PatternContext pattern(int i) {
			return getRuleContext(PatternContext.class,i);
		}
		public TerminalNode Forward() { return getToken(astrap.Forward, 0); }
		public Str_starts_with_patternContext(PatternContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterStr_starts_with_pattern(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitStr_starts_with_pattern(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitStr_starts_with_pattern(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Logic_patternContext extends PatternContext {
		public TerminalNode True() { return getToken(astrap.True, 0); }
		public TerminalNode False() { return getToken(astrap.False, 0); }
		public Logic_patternContext(PatternContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterLogic_pattern(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitLogic_pattern(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitLogic_pattern(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Paren_patternContext extends PatternContext {
		public TerminalNode ParenStart() { return getToken(astrap.ParenStart, 0); }
		public PatternContext pattern() {
			return getRuleContext(PatternContext.class,0);
		}
		public TerminalNode ParenEnd() { return getToken(astrap.ParenEnd, 0); }
		public Paren_patternContext(PatternContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterParen_pattern(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitParen_pattern(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitParen_pattern(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Alias_patternContext extends PatternContext {
		public PatternContext pattern() {
			return getRuleContext(PatternContext.class,0);
		}
		public TerminalNode As() { return getToken(astrap.As, 0); }
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public Alias_patternContext(PatternContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterAlias_pattern(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitAlias_pattern(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitAlias_pattern(this);
			else return visitor.visitChildren(this);
		}
	}

	public final PatternContext pattern() throws RecognitionException {
		return pattern(0);
	}

	private PatternContext pattern(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		PatternContext _localctx = new PatternContext(_ctx, _parentState);
		PatternContext _prevctx = _localctx;
		int _startState = 30;
		enterRecursionRule(_localctx, 30, RULE_pattern, _p);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(320);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case Int:
				{
				_localctx = new Int_patternContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;

				setState(304);
				match(Int);
				}
				break;
			case Float:
				{
				_localctx = new Float_patternContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(305);
				match(Float);
				}
				break;
			case STRING:
				{
				_localctx = new Raw_str_patternContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(306);
				match(STRING);
				}
				break;
			case Null:
			case Question:
				{
				_localctx = new Null_patternContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(307);
				_la = _input.LA(1);
				if ( !(_la==Null || _la==Question) ) {
				_errHandler.recoverInline(this);
				}
				else {
					if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
					_errHandler.reportMatch(this);
					consume();
				}
				}
				break;
			case True:
			case False:
				{
				_localctx = new Logic_patternContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(308);
				_la = _input.LA(1);
				if ( !(_la==True || _la==False) ) {
				_errHandler.recoverInline(this);
				}
				else {
					if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
					_errHandler.reportMatch(this);
					consume();
				}
				}
				break;
			case BacktickName:
			case Name:
				{
				_localctx = new Name_patternContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(309);
				name();
				}
				break;
			case Val:
				{
				_localctx = new Val_patternContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(310);
				match(Val);
				setState(311);
				name();
				}
				break;
			case BracketStart:
				{
				_localctx = new Array_patternContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(312);
				arr_pattern();
				}
				break;
			case Sharp:
				{
				_localctx = new Constructor_patternContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(313);
				match(Sharp);
				setState(314);
				con_pattern();
				}
				break;
			case Underscore:
				{
				_localctx = new WildcardContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(315);
				match(Underscore);
				}
				break;
			case ParenStart:
				{
				_localctx = new Paren_patternContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(316);
				match(ParenStart);
				setState(317);
				pattern(0);
				setState(318);
				match(ParenEnd);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			_ctx.stop = _input.LT(-1);
			setState(336);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,40,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					setState(334);
					_errHandler.sync(this);
					switch ( getInterpreter().adaptivePredict(_input,39,_ctx) ) {
					case 1:
						{
						_localctx = new Str_starts_with_patternContext(new PatternContext(_parentctx, _parentState));
						pushNewRecursionContext(_localctx, _startState, RULE_pattern);
						setState(322);
						if (!(precpred(_ctx, 12))) throw new FailedPredicateException(this, "precpred(_ctx, 12)");
						setState(323);
						match(Forward);
						setState(324);
						pattern(13);
						}
						break;
					case 2:
						{
						_localctx = new Tuple_patternContext(new PatternContext(_parentctx, _parentState));
						pushNewRecursionContext(_localctx, _startState, RULE_pattern);
						setState(325);
						if (!(precpred(_ctx, 6))) throw new FailedPredicateException(this, "precpred(_ctx, 6)");
						setState(326);
						match(Comma);
						setState(327);
						pattern(6);
						}
						break;
					case 3:
						{
						_localctx = new Alias_patternContext(new PatternContext(_parentctx, _parentState));
						pushNewRecursionContext(_localctx, _startState, RULE_pattern);
						setState(328);
						if (!(precpred(_ctx, 4))) throw new FailedPredicateException(this, "precpred(_ctx, 4)");
						setState(329);
						match(As);
						setState(330);
						name();
						}
						break;
					case 4:
						{
						_localctx = new Cast_patternContext(new PatternContext(_parentctx, _parentState));
						pushNewRecursionContext(_localctx, _startState, RULE_pattern);
						setState(331);
						if (!(precpred(_ctx, 3))) throw new FailedPredicateException(this, "precpred(_ctx, 3)");
						setState(332);
						match(Colon);
						setState(333);
						type(0);
						}
						break;
					}
					} 
				}
				setState(338);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,40,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class Default_clauseContext extends ParserRuleContext {
		public TerminalNode Pipe() { return getToken(astrap.Pipe, 0); }
		public TerminalNode Underscore() { return getToken(astrap.Underscore, 0); }
		public TerminalNode Arrow() { return getToken(astrap.Arrow, 0); }
		public BlockContext block() {
			return getRuleContext(BlockContext.class,0);
		}
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public Default_clauseContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_default_clause; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterDefault_clause(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitDefault_clause(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitDefault_clause(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Default_clauseContext default_clause() throws RecognitionException {
		Default_clauseContext _localctx = new Default_clauseContext(_ctx, getState());
		enterRule(_localctx, 32, RULE_default_clause);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(339);
			match(Pipe);
			setState(340);
			match(Underscore);
			setState(341);
			match(Arrow);
			setState(344);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,41,_ctx) ) {
			case 1:
				{
				setState(342);
				block();
				}
				break;
			case 2:
				{
				setState(343);
				expr(0);
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class Con_patternContext extends ParserRuleContext {
		public Con_patternContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_con_pattern; }
	 
		public Con_patternContext() { }
		public void copyFrom(Con_patternContext ctx) {
			super.copyFrom(ctx);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Singleton_constructorContext extends Con_patternContext {
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public Singleton_constructorContext(Con_patternContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterSingleton_constructor(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitSingleton_constructor(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitSingleton_constructor(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Empty_constructorContext extends Con_patternContext {
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public TerminalNode Null() { return getToken(astrap.Null, 0); }
		public Empty_constructorContext(Con_patternContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterEmpty_constructor(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitEmpty_constructor(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitEmpty_constructor(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Non_empty_constructorContext extends Con_patternContext {
		public NameContext name() {
			return getRuleContext(NameContext.class,0);
		}
		public List<PatternContext> pattern() {
			return getRuleContexts(PatternContext.class);
		}
		public PatternContext pattern(int i) {
			return getRuleContext(PatternContext.class,i);
		}
		public Non_empty_constructorContext(Con_patternContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterNon_empty_constructor(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitNon_empty_constructor(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitNon_empty_constructor(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Con_patternContext con_pattern() throws RecognitionException {
		Con_patternContext _localctx = new Con_patternContext(_ctx, getState());
		enterRule(_localctx, 34, RULE_con_pattern);
		try {
			int _alt;
			setState(356);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,43,_ctx) ) {
			case 1:
				_localctx = new Singleton_constructorContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(346);
				name();
				}
				break;
			case 2:
				_localctx = new Empty_constructorContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(347);
				name();
				setState(348);
				match(Null);
				}
				break;
			case 3:
				_localctx = new Non_empty_constructorContext(_localctx);
				enterOuterAlt(_localctx, 3);
				{
				setState(350);
				name();
				setState(352); 
				_errHandler.sync(this);
				_alt = 1;
				do {
					switch (_alt) {
					case 1:
						{
						{
						setState(351);
						pattern(0);
						}
						}
						break;
					default:
						throw new NoViableAltException(this);
					}
					setState(354); 
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,42,_ctx);
				} while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER );
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class Arr_patternContext extends ParserRuleContext {
		public Arr_patternContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_arr_pattern; }
	 
		public Arr_patternContext() { }
		public void copyFrom(Arr_patternContext ctx) {
			super.copyFrom(ctx);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Empty_arrayContext extends Arr_patternContext {
		public TerminalNode BracketStart() { return getToken(astrap.BracketStart, 0); }
		public TerminalNode BracketEnd() { return getToken(astrap.BracketEnd, 0); }
		public Empty_arrayContext(Arr_patternContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterEmpty_array(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitEmpty_array(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitEmpty_array(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Non_empty_arrayContext extends Arr_patternContext {
		public PatternContext head;
		public PatternContext body;
		public PatternContext tail;
		public TerminalNode BracketStart() { return getToken(astrap.BracketStart, 0); }
		public TerminalNode BracketEnd() { return getToken(astrap.BracketEnd, 0); }
		public List<PatternContext> pattern() {
			return getRuleContexts(PatternContext.class);
		}
		public PatternContext pattern(int i) {
			return getRuleContext(PatternContext.class,i);
		}
		public List<TerminalNode> Comma() { return getTokens(astrap.Comma); }
		public TerminalNode Comma(int i) {
			return getToken(astrap.Comma, i);
		}
		public TerminalNode Semicolon() { return getToken(astrap.Semicolon, 0); }
		public Non_empty_arrayContext(Arr_patternContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterNon_empty_array(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitNon_empty_array(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitNon_empty_array(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Arr_patternContext arr_pattern() throws RecognitionException {
		Arr_patternContext _localctx = new Arr_patternContext(_ctx, getState());
		enterRule(_localctx, 36, RULE_arr_pattern);
		int _la;
		try {
			setState(375);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,46,_ctx) ) {
			case 1:
				_localctx = new Empty_arrayContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(358);
				match(BracketStart);
				setState(359);
				match(BracketEnd);
				}
				break;
			case 2:
				_localctx = new Non_empty_arrayContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(360);
				match(BracketStart);
				setState(361);
				((Non_empty_arrayContext)_localctx).head = pattern(0);
				setState(366);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==Comma) {
					{
					{
					setState(362);
					match(Comma);
					setState(363);
					((Non_empty_arrayContext)_localctx).body = pattern(0);
					}
					}
					setState(368);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(371);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==Semicolon) {
					{
					setState(369);
					match(Semicolon);
					setState(370);
					((Non_empty_arrayContext)_localctx).tail = pattern(0);
					}
				}

				setState(373);
				match(BracketEnd);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class NameContext extends ParserRuleContext {
		public TerminalNode Name() { return getToken(astrap.Name, 0); }
		public TerminalNode BacktickName() { return getToken(astrap.BacktickName, 0); }
		public NameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_name; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).enterName(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof astrapListener ) ((astrapListener)listener).exitName(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof astrapVisitor ) return ((astrapVisitor<? extends T>)visitor).visitName(this);
			else return visitor.visitChildren(this);
		}
	}

	public final NameContext name() throws RecognitionException {
		NameContext _localctx = new NameContext(_ctx, getState());
		enterRule(_localctx, 38, RULE_name);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(377);
			_la = _input.LA(1);
			if ( !(_la==BacktickName || _la==Name) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public boolean sempred(RuleContext _localctx, int ruleIndex, int predIndex) {
		switch (ruleIndex) {
		case 11:
			return type_sempred((TypeContext)_localctx, predIndex);
		case 12:
			return expr_sempred((ExprContext)_localctx, predIndex);
		case 15:
			return pattern_sempred((PatternContext)_localctx, predIndex);
		}
		return true;
	}
	private boolean type_sempred(TypeContext _localctx, int predIndex) {
		switch (predIndex) {
		case 0:
			return precpred(_ctx, 3);
		}
		return true;
	}
	private boolean expr_sempred(ExprContext _localctx, int predIndex) {
		switch (predIndex) {
		case 1:
			return precpred(_ctx, 9);
		case 2:
			return precpred(_ctx, 5);
		case 3:
			return precpred(_ctx, 10);
		case 4:
			return precpred(_ctx, 8);
		case 5:
			return precpred(_ctx, 7);
		case 6:
			return precpred(_ctx, 6);
		}
		return true;
	}
	private boolean pattern_sempred(PatternContext _localctx, int predIndex) {
		switch (predIndex) {
		case 7:
			return precpred(_ctx, 12);
		case 8:
			return precpred(_ctx, 6);
		case 9:
			return precpred(_ctx, 4);
		case 10:
			return precpred(_ctx, 3);
		}
		return true;
	}

	public static final String _serializedATN =
		"\u0004\u00019\u017c\u0002\u0000\u0007\u0000\u0002\u0001\u0007\u0001\u0002"+
		"\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002\u0004\u0007\u0004\u0002"+
		"\u0005\u0007\u0005\u0002\u0006\u0007\u0006\u0002\u0007\u0007\u0007\u0002"+
		"\b\u0007\b\u0002\t\u0007\t\u0002\n\u0007\n\u0002\u000b\u0007\u000b\u0002"+
		"\f\u0007\f\u0002\r\u0007\r\u0002\u000e\u0007\u000e\u0002\u000f\u0007\u000f"+
		"\u0002\u0010\u0007\u0010\u0002\u0011\u0007\u0011\u0002\u0012\u0007\u0012"+
		"\u0002\u0013\u0007\u0013\u0001\u0000\u0001\u0000\u0001\u0000\u0003\u0000"+
		",\b\u0000\u0001\u0000\u0005\u0000/\b\u0000\n\u0000\f\u00002\t\u0000\u0001"+
		"\u0000\u0005\u00005\b\u0000\n\u0000\f\u00008\t\u0000\u0001\u0000\u0001"+
		"\u0000\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0005\u0001@\b"+
		"\u0001\n\u0001\f\u0001C\t\u0001\u0001\u0001\u0003\u0001F\b\u0001\u0001"+
		"\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0005\u0002L\b\u0002\n\u0002"+
		"\f\u0002O\t\u0002\u0001\u0002\u0001\u0002\u0003\u0002S\b\u0002\u0001\u0002"+
		"\u0003\u0002V\b\u0002\u0001\u0003\u0001\u0003\u0003\u0003Z\b\u0003\u0001"+
		"\u0004\u0001\u0004\u0003\u0004^\b\u0004\u0001\u0004\u0001\u0004\u0001"+
		"\u0004\u0001\u0004\u0001\u0004\u0003\u0004e\b\u0004\u0001\u0004\u0001"+
		"\u0004\u0001\u0004\u0001\u0004\u0003\u0004k\b\u0004\u0001\u0004\u0001"+
		"\u0004\u0003\u0004o\b\u0004\u0001\u0005\u0001\u0005\u0003\u0005s\b\u0005"+
		"\u0001\u0005\u0001\u0005\u0001\u0006\u0001\u0006\u0001\u0007\u0001\u0007"+
		"\u0001\u0007\u0005\u0007|\b\u0007\n\u0007\f\u0007\u007f\t\u0007\u0001"+
		"\u0007\u0001\u0007\u0001\b\u0001\b\u0001\b\u0001\b\u0003\b\u0087\b\b\u0001"+
		"\b\u0001\b\u0003\b\u008b\b\b\u0001\b\u0001\b\u0001\b\u0003\b\u0090\b\b"+
		"\u0001\t\u0001\t\u0003\t\u0094\b\t\u0001\n\u0001\n\u0001\n\u0001\n\u0003"+
		"\n\u009a\b\n\u0001\n\u0001\n\u0001\n\u0001\u000b\u0001\u000b\u0001\u000b"+
		"\u0001\u000b\u0001\u000b\u0001\u000b\u0001\u000b\u0001\u000b\u0001\u000b"+
		"\u0001\u000b\u0003\u000b\u00a9\b\u000b\u0001\u000b\u0001\u000b\u0001\u000b"+
		"\u0005\u000b\u00ae\b\u000b\n\u000b\f\u000b\u00b1\t\u000b\u0001\f\u0001"+
		"\f\u0001\f\u0001\f\u0001\f\u0001\f\u0001\f\u0001\f\u0001\f\u0001\f\u0001"+
		"\f\u0001\f\u0001\f\u0001\f\u0001\f\u0003\f\u00c2\b\f\u0001\f\u0001\f\u0001"+
		"\f\u0001\f\u0001\f\u0001\f\u0001\f\u0001\f\u0001\f\u0001\f\u0001\f\u0001"+
		"\f\u0001\f\u0001\f\u0005\f\u00d2\b\f\n\f\f\f\u00d5\t\f\u0001\f\u0001\f"+
		"\u0003\f\u00d9\b\f\u0001\f\u0001\f\u0001\f\u0001\f\u0001\f\u0001\f\u0001"+
		"\f\u0004\f\u00e2\b\f\u000b\f\f\f\u00e3\u0001\f\u0003\f\u00e7\b\f\u0003"+
		"\f\u00e9\b\f\u0001\f\u0001\f\u0001\f\u0001\f\u0001\f\u0001\f\u0001\f\u0001"+
		"\f\u0003\f\u00f3\b\f\u0001\f\u0001\f\u0001\f\u0001\f\u0005\f\u00f9\b\f"+
		"\n\f\f\f\u00fc\t\f\u0003\f\u00fe\b\f\u0001\f\u0001\f\u0001\f\u0001\f\u0001"+
		"\f\u0001\f\u0001\f\u0001\f\u0001\f\u0003\f\u0109\b\f\u0001\f\u0001\f\u0001"+
		"\f\u0001\f\u0001\f\u0001\f\u0005\f\u0111\b\f\n\f\f\f\u0114\t\f\u0003\f"+
		"\u0116\b\f\u0001\f\u0003\f\u0119\b\f\u0005\f\u011b\b\f\n\f\f\f\u011e\t"+
		"\f\u0001\r\u0001\r\u0001\r\u0001\r\u0001\r\u0001\u000e\u0001\u000e\u0001"+
		"\u000e\u0001\u000e\u0005\u000e\u0129\b\u000e\n\u000e\f\u000e\u012c\t\u000e"+
		"\u0001\u000e\u0001\u000e\u0001\u000f\u0001\u000f\u0001\u000f\u0001\u000f"+
		"\u0001\u000f\u0001\u000f\u0001\u000f\u0001\u000f\u0001\u000f\u0001\u000f"+
		"\u0001\u000f\u0001\u000f\u0001\u000f\u0001\u000f\u0001\u000f\u0001\u000f"+
		"\u0001\u000f\u0003\u000f\u0141\b\u000f\u0001\u000f\u0001\u000f\u0001\u000f"+
		"\u0001\u000f\u0001\u000f\u0001\u000f\u0001\u000f\u0001\u000f\u0001\u000f"+
		"\u0001\u000f\u0001\u000f\u0001\u000f\u0005\u000f\u014f\b\u000f\n\u000f"+
		"\f\u000f\u0152\t\u000f\u0001\u0010\u0001\u0010\u0001\u0010\u0001\u0010"+
		"\u0001\u0010\u0003\u0010\u0159\b\u0010\u0001\u0011\u0001\u0011\u0001\u0011"+
		"\u0001\u0011\u0001\u0011\u0001\u0011\u0004\u0011\u0161\b\u0011\u000b\u0011"+
		"\f\u0011\u0162\u0003\u0011\u0165\b\u0011\u0001\u0012\u0001\u0012\u0001"+
		"\u0012\u0001\u0012\u0001\u0012\u0001\u0012\u0005\u0012\u016d\b\u0012\n"+
		"\u0012\f\u0012\u0170\t\u0012\u0001\u0012\u0001\u0012\u0003\u0012\u0174"+
		"\b\u0012\u0001\u0012\u0001\u0012\u0003\u0012\u0178\b\u0012\u0001\u0013"+
		"\u0001\u0013\u0001\u0013\u0000\u0003\u0016\u0018\u001e\u0014\u0000\u0002"+
		"\u0004\u0006\b\n\f\u000e\u0010\u0012\u0014\u0016\u0018\u001a\u001c\u001e"+
		" \"$&\u0000\u0004\u0001\u000012\u0002\u0000\b\b\'\'\u0001\u0000\u0005"+
		"\u0006\u0002\u0000\u0010\u001033\u01b1\u0000(\u0001\u0000\u0000\u0000"+
		"\u0002;\u0001\u0000\u0000\u0000\u0004G\u0001\u0000\u0000\u0000\u0006Y"+
		"\u0001\u0000\u0000\u0000\b[\u0001\u0000\u0000\u0000\np\u0001\u0000\u0000"+
		"\u0000\fv\u0001\u0000\u0000\u0000\u000e}\u0001\u0000\u0000\u0000\u0010"+
		"\u0082\u0001\u0000\u0000\u0000\u0012\u0093\u0001\u0000\u0000\u0000\u0014"+
		"\u0095\u0001\u0000\u0000\u0000\u0016\u00a8\u0001\u0000\u0000\u0000\u0018"+
		"\u00e8\u0001\u0000\u0000\u0000\u001a\u011f\u0001\u0000\u0000\u0000\u001c"+
		"\u0124\u0001\u0000\u0000\u0000\u001e\u0140\u0001\u0000\u0000\u0000 \u0153"+
		"\u0001\u0000\u0000\u0000\"\u0164\u0001\u0000\u0000\u0000$\u0177\u0001"+
		"\u0000\u0000\u0000&\u0179\u0001\u0000\u0000\u0000()\u0005\u0002\u0000"+
		"\u0000)+\u00053\u0000\u0000*,\u0003\u0002\u0001\u0000+*\u0001\u0000\u0000"+
		"\u0000+,\u0001\u0000\u0000\u0000,0\u0001\u0000\u0000\u0000-/\u0003\u0004"+
		"\u0002\u0000.-\u0001\u0000\u0000\u0000/2\u0001\u0000\u0000\u00000.\u0001"+
		"\u0000\u0000\u000001\u0001\u0000\u0000\u000016\u0001\u0000\u0000\u0000"+
		"20\u0001\u0000\u0000\u000035\u0003\u0006\u0003\u000043\u0001\u0000\u0000"+
		"\u000058\u0001\u0000\u0000\u000064\u0001\u0000\u0000\u000067\u0001\u0000"+
		"\u0000\u000079\u0001\u0000\u0000\u000086\u0001\u0000\u0000\u00009:\u0005"+
		"\u0000\u0000\u0001:\u0001\u0001\u0000\u0000\u0000;<\u0005\u000f\u0000"+
		"\u0000<A\u00053\u0000\u0000=>\u0005\u0015\u0000\u0000>@\u00053\u0000\u0000"+
		"?=\u0001\u0000\u0000\u0000@C\u0001\u0000\u0000\u0000A?\u0001\u0000\u0000"+
		"\u0000AB\u0001\u0000\u0000\u0000BE\u0001\u0000\u0000\u0000CA\u0001\u0000"+
		"\u0000\u0000DF\u0005\u0018\u0000\u0000ED\u0001\u0000\u0000\u0000EF\u0001"+
		"\u0000\u0000\u0000F\u0003\u0001\u0000\u0000\u0000GH\u0005\f\u0000\u0000"+
		"HM\u00053\u0000\u0000IJ\u0005\u0015\u0000\u0000JL\u00053\u0000\u0000K"+
		"I\u0001\u0000\u0000\u0000LO\u0001\u0000\u0000\u0000MK\u0001\u0000\u0000"+
		"\u0000MN\u0001\u0000\u0000\u0000NR\u0001\u0000\u0000\u0000OM\u0001\u0000"+
		"\u0000\u0000PQ\u0005\u0015\u0000\u0000QS\u0005,\u0000\u0000RP\u0001\u0000"+
		"\u0000\u0000RS\u0001\u0000\u0000\u0000SU\u0001\u0000\u0000\u0000TV\u0005"+
		"\u0018\u0000\u0000UT\u0001\u0000\u0000\u0000UV\u0001\u0000\u0000\u0000"+
		"V\u0005\u0001\u0000\u0000\u0000WZ\u0003\b\u0004\u0000XZ\u0003\n\u0005"+
		"\u0000YW\u0001\u0000\u0000\u0000YX\u0001\u0000\u0000\u0000Z\u0007\u0001"+
		"\u0000\u0000\u0000[]\u0003&\u0013\u0000\\^\u0003&\u0013\u0000]\\\u0001"+
		"\u0000\u0000\u0000]^\u0001\u0000\u0000\u0000^_\u0001\u0000\u0000\u0000"+
		"_`\u0003&\u0013\u0000`a\u0005$\u0000\u0000ad\u0003&\u0013\u0000be\u0003"+
		"&\u0013\u0000ce\u0005\u0012\u0000\u0000db\u0001\u0000\u0000\u0000dc\u0001"+
		"\u0000\u0000\u0000ej\u0001\u0000\u0000\u0000fk\u0005\u001a\u0000\u0000"+
		"gh\u0003&\u0013\u0000hi\u0005\u001b\u0000\u0000ik\u0001\u0000\u0000\u0000"+
		"jf\u0001\u0000\u0000\u0000jg\u0001\u0000\u0000\u0000kn\u0001\u0000\u0000"+
		"\u0000lo\u0003\u000e\u0007\u0000mo\u0003\u0018\f\u0000nl\u0001\u0000\u0000"+
		"\u0000nm\u0001\u0000\u0000\u0000o\t\u0001\u0000\u0000\u0000pr\u0005\u0003"+
		"\u0000\u0000qs\u0003&\u0013\u0000rq\u0001\u0000\u0000\u0000rs\u0001\u0000"+
		"\u0000\u0000st\u0001\u0000\u0000\u0000tu\u0003\f\u0006\u0000u\u000b\u0001"+
		"\u0000\u0000\u0000vw\u0003\u000e\u0007\u0000w\r\u0001\u0000\u0000\u0000"+
		"xy\u0003\u0012\t\u0000yz\u0005\u0018\u0000\u0000z|\u0001\u0000\u0000\u0000"+
		"{x\u0001\u0000\u0000\u0000|\u007f\u0001\u0000\u0000\u0000}{\u0001\u0000"+
		"\u0000\u0000}~\u0001\u0000\u0000\u0000~\u0080\u0001\u0000\u0000\u0000"+
		"\u007f}\u0001\u0000\u0000\u0000\u0080\u0081\u0003\u0018\f\u0000\u0081"+
		"\u000f\u0001\u0000\u0000\u0000\u0082\u0086\u0005*\u0000\u0000\u0083\u0084"+
		"\u0005\u0017\u0000\u0000\u0084\u0087\u0003&\u0013\u0000\u0085\u0087\u0003"+
		"\u001e\u000f\u0000\u0086\u0083\u0001\u0000\u0000\u0000\u0086\u0085\u0001"+
		"\u0000\u0000\u0000\u0087\u008a\u0001\u0000\u0000\u0000\u0088\u0089\u0005"+
		"\u000b\u0000\u0000\u0089\u008b\u0003\u0018\f\u0000\u008a\u0088\u0001\u0000"+
		"\u0000\u0000\u008a\u008b\u0001\u0000\u0000\u0000\u008b\u008c\u0001\u0000"+
		"\u0000\u0000\u008c\u008f\u0005\u0014\u0000\u0000\u008d\u0090\u0003\u000e"+
		"\u0007\u0000\u008e\u0090\u0003\u0018\f\u0000\u008f\u008d\u0001\u0000\u0000"+
		"\u0000\u008f\u008e\u0001\u0000\u0000\u0000\u0090\u0011\u0001\u0000\u0000"+
		"\u0000\u0091\u0094\u0003\u0014\n\u0000\u0092\u0094\u0003\u0018\f\u0000"+
		"\u0093\u0091\u0001\u0000\u0000\u0000\u0093\u0092\u0001\u0000\u0000\u0000"+
		"\u0094\u0013\u0001\u0000\u0000\u0000\u0095\u0096\u0007\u0000\u0000\u0000"+
		"\u0096\u0099\u0003&\u0013\u0000\u0097\u0098\u0005\u0017\u0000\u0000\u0098"+
		"\u009a\u0003\u0016\u000b\u0000\u0099\u0097\u0001\u0000\u0000\u0000\u0099"+
		"\u009a\u0001\u0000\u0000\u0000\u009a\u009b\u0001\u0000\u0000\u0000\u009b"+
		"\u009c\u0005\u0019\u0000\u0000\u009c\u009d\u0003\u0018\f\u0000\u009d\u0015"+
		"\u0001\u0000\u0000\u0000\u009e\u009f\u0006\u000b\uffff\uffff\u0000\u009f"+
		"\u00a9\u0003&\u0013\u0000\u00a0\u00a1\u0005\u001c\u0000\u0000\u00a1\u00a2"+
		"\u0003\u0016\u000b\u0000\u00a2\u00a3\u0005\u001d\u0000\u0000\u00a3\u00a9"+
		"\u0001\u0000\u0000\u0000\u00a4\u00a5\u0005 \u0000\u0000\u00a5\u00a6\u0003"+
		"\u0016\u000b\u0000\u00a6\u00a7\u0005!\u0000\u0000\u00a7\u00a9\u0001\u0000"+
		"\u0000\u0000\u00a8\u009e\u0001\u0000\u0000\u0000\u00a8\u00a0\u0001\u0000"+
		"\u0000\u0000\u00a8\u00a4\u0001\u0000\u0000\u0000\u00a9\u00af\u0001\u0000"+
		"\u0000\u0000\u00aa\u00ab\n\u0003\u0000\u0000\u00ab\u00ac\u0005\u0014\u0000"+
		"\u0000\u00ac\u00ae\u0003\u0016\u000b\u0004\u00ad\u00aa\u0001\u0000\u0000"+
		"\u0000\u00ae\u00b1\u0001\u0000\u0000\u0000\u00af\u00ad\u0001\u0000\u0000"+
		"\u0000\u00af\u00b0\u0001\u0000\u0000\u0000\u00b0\u0017\u0001\u0000\u0000"+
		"\u0000\u00b1\u00af\u0001\u0000\u0000\u0000\u00b2\u00b3\u0006\f\uffff\uffff"+
		"\u0000\u00b3\u00e9\u00055\u0000\u0000\u00b4\u00e9\u0005\b\u0000\u0000"+
		"\u00b5\u00e9\u00056\u0000\u0000\u00b6\u00e9\u00057\u0000\u0000\u00b7\u00e9"+
		"\u0003&\u0013\u0000\u00b8\u00e9\u0005\u0005\u0000\u0000\u00b9\u00e9\u0005"+
		"\u0006\u0000\u0000\u00ba\u00bb\u0005\r\u0000\u0000\u00bb\u00bc\u0005 "+
		"\u0000\u0000\u00bc\u00bd\u0003\u0018\f\u0000\u00bd\u00be\u0005!\u0000"+
		"\u0000\u00be\u00c1\u0003\u0018\f\u0000\u00bf\u00c0\u0005\u000e\u0000\u0000"+
		"\u00c0\u00c2\u0003\u0018\f\u0000\u00c1\u00bf\u0001\u0000\u0000\u0000\u00c1"+
		"\u00c2\u0001\u0000\u0000\u0000\u00c2\u00e9\u0001\u0000\u0000\u0000\u00c3"+
		"\u00e9\u0003\u001a\r\u0000\u00c4\u00c5\u0005 \u0000\u0000\u00c5\u00c6"+
		"\u0003\u0018\f\u0000\u00c6\u00c7\u0005!\u0000\u0000\u00c7\u00e9\u0001"+
		"\u0000\u0000\u0000\u00c8\u00d8\u0005\u001e\u0000\u0000\u00c9\u00ca\u0003"+
		"&\u0013\u0000\u00ca\u00cb\u0005\u0017\u0000\u0000\u00cb\u00d3\u0003\u0016"+
		"\u000b\u0000\u00cc\u00cd\u0005$\u0000\u0000\u00cd\u00ce\u0003&\u0013\u0000"+
		"\u00ce\u00cf\u0005\u0017\u0000\u0000\u00cf\u00d0\u0003\u0016\u000b\u0000"+
		"\u00d0\u00d2\u0001\u0000\u0000\u0000\u00d1\u00cc\u0001\u0000\u0000\u0000"+
		"\u00d2\u00d5\u0001\u0000\u0000\u0000\u00d3\u00d1\u0001\u0000\u0000\u0000"+
		"\u00d3\u00d4\u0001\u0000\u0000\u0000\u00d4\u00d6\u0001\u0000\u0000\u0000"+
		"\u00d5\u00d3\u0001\u0000\u0000\u0000\u00d6\u00d7\u0005\u0014\u0000\u0000"+
		"\u00d7\u00d9\u0001\u0000\u0000\u0000\u00d8\u00c9\u0001\u0000\u0000\u0000"+
		"\u00d8\u00d9\u0001\u0000\u0000\u0000\u00d9\u00da\u0001\u0000\u0000\u0000"+
		"\u00da\u00db\u0003\u0018\f\u0000\u00db\u00dc\u0005\u001f\u0000\u0000\u00dc"+
		"\u00e9\u0001\u0000\u0000\u0000\u00dd\u00de\u0005\t\u0000\u0000\u00de\u00df"+
		"\u0003\u0018\f\u0000\u00df\u00e1\u0005\n\u0000\u0000\u00e0\u00e2\u0003"+
		"\u0010\b\u0000\u00e1\u00e0\u0001\u0000\u0000\u0000\u00e2\u00e3\u0001\u0000"+
		"\u0000\u0000\u00e3\u00e1\u0001\u0000\u0000\u0000\u00e3\u00e4\u0001\u0000"+
		"\u0000\u0000\u00e4\u00e6\u0001\u0000\u0000\u0000\u00e5\u00e7\u0003 \u0010"+
		"\u0000\u00e6\u00e5\u0001\u0000\u0000\u0000\u00e6\u00e7\u0001\u0000\u0000"+
		"\u0000\u00e7\u00e9\u0001\u0000\u0000\u0000\u00e8\u00b2\u0001\u0000\u0000"+
		"\u0000\u00e8\u00b4\u0001\u0000\u0000\u0000\u00e8\u00b5\u0001\u0000\u0000"+
		"\u0000\u00e8\u00b6\u0001\u0000\u0000\u0000\u00e8\u00b7\u0001\u0000\u0000"+
		"\u0000\u00e8\u00b8\u0001\u0000\u0000\u0000\u00e8\u00b9\u0001\u0000\u0000"+
		"\u0000\u00e8\u00ba\u0001\u0000\u0000\u0000\u00e8\u00c3\u0001\u0000\u0000"+
		"\u0000\u00e8\u00c4\u0001\u0000\u0000\u0000\u00e8\u00c8\u0001\u0000\u0000"+
		"\u0000\u00e8\u00dd\u0001\u0000\u0000\u0000\u00e9\u011c\u0001\u0000\u0000"+
		"\u0000\u00ea\u00eb\n\t\u0000\u0000\u00eb\u00ec\u00054\u0000\u0000\u00ec"+
		"\u011b\u0003\u0018\f\n\u00ed\u00ee\n\u0005\u0000\u0000\u00ee\u00ef\u0005"+
		"\u0013\u0000\u0000\u00ef\u011b\u0003\u0018\f\u0006\u00f0\u00f2\n\n\u0000"+
		"\u0000\u00f1\u00f3\u0003\u001c\u000e\u0000\u00f2\u00f1\u0001\u0000\u0000"+
		"\u0000\u00f2\u00f3\u0001\u0000\u0000\u0000\u00f3\u00f4\u0001\u0000\u0000"+
		"\u0000\u00f4\u00fd\u0005 \u0000\u0000\u00f5\u00fa\u0003\u0018\f\u0000"+
		"\u00f6\u00f7\u0005$\u0000\u0000\u00f7\u00f9\u0003\u0018\f\u0000\u00f8"+
		"\u00f6\u0001\u0000\u0000\u0000\u00f9\u00fc\u0001\u0000\u0000\u0000\u00fa"+
		"\u00f8\u0001\u0000\u0000\u0000\u00fa\u00fb\u0001\u0000\u0000\u0000\u00fb"+
		"\u00fe\u0001\u0000\u0000\u0000\u00fc\u00fa\u0001\u0000\u0000\u0000\u00fd"+
		"\u00f5\u0001\u0000\u0000\u0000\u00fd\u00fe\u0001\u0000\u0000\u0000\u00fe"+
		"\u00ff\u0001\u0000\u0000\u0000\u00ff\u011b\u0005!\u0000\u0000\u0100\u0101"+
		"\n\b\u0000\u0000\u0101\u0102\u0005\u0015\u0000\u0000\u0102\u011b\u0003"+
		"&\u0013\u0000\u0103\u0104\n\u0007\u0000\u0000\u0104\u0105\u0005\u0016"+
		"\u0000\u0000\u0105\u011b\u0003&\u0013\u0000\u0106\u0108\n\u0006\u0000"+
		"\u0000\u0107\u0109\u0003\u001c\u000e\u0000\u0108\u0107\u0001\u0000\u0000"+
		"\u0000\u0108\u0109\u0001\u0000\u0000\u0000\u0109\u010a\u0001\u0000\u0000"+
		"\u0000\u010a\u0118\u0005+\u0000\u0000\u010b\u0119\u0003\u0018\f\u0000"+
		"\u010c\u0115\u0005 \u0000\u0000\u010d\u0112\u0003\u0018\f\u0000\u010e"+
		"\u010f\u0005$\u0000\u0000\u010f\u0111\u0003\u0018\f\u0000\u0110\u010e"+
		"\u0001\u0000\u0000\u0000\u0111\u0114\u0001\u0000\u0000\u0000\u0112\u0110"+
		"\u0001\u0000\u0000\u0000\u0112\u0113\u0001\u0000\u0000\u0000\u0113\u0116"+
		"\u0001\u0000\u0000\u0000\u0114\u0112\u0001\u0000\u0000\u0000\u0115\u010d"+
		"\u0001\u0000\u0000\u0000\u0115\u0116\u0001\u0000\u0000\u0000\u0116\u0117"+
		"\u0001\u0000\u0000\u0000\u0117\u0119\u0005!\u0000\u0000\u0118\u010b\u0001"+
		"\u0000\u0000\u0000\u0118\u010c\u0001\u0000\u0000\u0000\u0119\u011b\u0001"+
		"\u0000\u0000\u0000\u011a\u00ea\u0001\u0000\u0000\u0000\u011a\u00ed\u0001"+
		"\u0000\u0000\u0000\u011a\u00f0\u0001\u0000\u0000\u0000\u011a\u0100\u0001"+
		"\u0000\u0000\u0000\u011a\u0103\u0001\u0000\u0000\u0000\u011a\u0106\u0001"+
		"\u0000\u0000\u0000\u011b\u011e\u0001\u0000\u0000\u0000\u011c\u011a\u0001"+
		"\u0000\u0000\u0000\u011c\u011d\u0001\u0000\u0000\u0000\u011d\u0019\u0001"+
		"\u0000\u0000\u0000\u011e\u011c\u0001\u0000\u0000\u0000\u011f\u0120\u0005"+
		"\"\u0000\u0000\u0120\u0121\u0003&\u0013\u0000\u0121\u0122\u0003&\u0013"+
		"\u0000\u0122\u0123\u0005#\u0000\u0000\u0123\u001b\u0001\u0000\u0000\u0000"+
		"\u0124\u0125\u0005%\u0000\u0000\u0125\u012a\u0003\u0016\u000b\u0000\u0126"+
		"\u0127\u0005$\u0000\u0000\u0127\u0129\u0003\u0016\u000b\u0000\u0128\u0126"+
		"\u0001\u0000\u0000\u0000\u0129\u012c\u0001\u0000\u0000\u0000\u012a\u0128"+
		"\u0001\u0000\u0000\u0000\u012a\u012b\u0001\u0000\u0000\u0000\u012b\u012d"+
		"\u0001\u0000\u0000\u0000\u012c\u012a\u0001\u0000\u0000\u0000\u012d\u012e"+
		"\u0005&\u0000\u0000\u012e\u001d\u0001\u0000\u0000\u0000\u012f\u0130\u0006"+
		"\u000f\uffff\uffff\u0000\u0130\u0141\u00055\u0000\u0000\u0131\u0141\u0005"+
		"6\u0000\u0000\u0132\u0141\u00057\u0000\u0000\u0133\u0141\u0007\u0001\u0000"+
		"\u0000\u0134\u0141\u0007\u0002\u0000\u0000\u0135\u0141\u0003&\u0013\u0000"+
		"\u0136\u0137\u00051\u0000\u0000\u0137\u0141\u0003&\u0013\u0000\u0138\u0141"+
		"\u0003$\u0012\u0000\u0139\u013a\u0005(\u0000\u0000\u013a\u0141\u0003\""+
		"\u0011\u0000\u013b\u0141\u0005\u0012\u0000\u0000\u013c\u013d\u0005 \u0000"+
		"\u0000\u013d\u013e\u0003\u001e\u000f\u0000\u013e\u013f\u0005!\u0000\u0000"+
		"\u013f\u0141\u0001\u0000\u0000\u0000\u0140\u012f\u0001\u0000\u0000\u0000"+
		"\u0140\u0131\u0001\u0000\u0000\u0000\u0140\u0132\u0001\u0000\u0000\u0000"+
		"\u0140\u0133\u0001\u0000\u0000\u0000\u0140\u0134\u0001\u0000\u0000\u0000"+
		"\u0140\u0135\u0001\u0000\u0000\u0000\u0140\u0136\u0001\u0000\u0000\u0000"+
		"\u0140\u0138\u0001\u0000\u0000\u0000\u0140\u0139\u0001\u0000\u0000\u0000"+
		"\u0140\u013b\u0001\u0000\u0000\u0000\u0140\u013c\u0001\u0000\u0000\u0000"+
		"\u0141\u0150\u0001\u0000\u0000\u0000\u0142\u0143\n\f\u0000\u0000\u0143"+
		"\u0144\u0005)\u0000\u0000\u0144\u014f\u0003\u001e\u000f\r\u0145\u0146"+
		"\n\u0006\u0000\u0000\u0146\u0147\u0005$\u0000\u0000\u0147\u014f\u0003"+
		"\u001e\u000f\u0006\u0148\u0149\n\u0004\u0000\u0000\u0149\u014a\u0005\u0001"+
		"\u0000\u0000\u014a\u014f\u0003&\u0013\u0000\u014b\u014c\n\u0003\u0000"+
		"\u0000\u014c\u014d\u0005\u0017\u0000\u0000\u014d\u014f\u0003\u0016\u000b"+
		"\u0000\u014e\u0142\u0001\u0000\u0000\u0000\u014e\u0145\u0001\u0000\u0000"+
		"\u0000\u014e\u0148\u0001\u0000\u0000\u0000\u014e\u014b\u0001\u0000\u0000"+
		"\u0000\u014f\u0152\u0001\u0000\u0000\u0000\u0150\u014e\u0001\u0000\u0000"+
		"\u0000\u0150\u0151\u0001\u0000\u0000\u0000\u0151\u001f\u0001\u0000\u0000"+
		"\u0000\u0152\u0150\u0001\u0000\u0000\u0000\u0153\u0154\u0005*\u0000\u0000"+
		"\u0154\u0155\u0005\u0012\u0000\u0000\u0155\u0158\u0005\u0014\u0000\u0000"+
		"\u0156\u0159\u0003\u000e\u0007\u0000\u0157\u0159\u0003\u0018\f\u0000\u0158"+
		"\u0156\u0001\u0000\u0000\u0000\u0158\u0157\u0001\u0000\u0000\u0000\u0159"+
		"!\u0001\u0000\u0000\u0000\u015a\u0165\u0003&\u0013\u0000\u015b\u015c\u0003"+
		"&\u0013\u0000\u015c\u015d\u0005\b\u0000\u0000\u015d\u0165\u0001\u0000"+
		"\u0000\u0000\u015e\u0160\u0003&\u0013\u0000\u015f\u0161\u0003\u001e\u000f"+
		"\u0000\u0160\u015f\u0001\u0000\u0000\u0000\u0161\u0162\u0001\u0000\u0000"+
		"\u0000\u0162\u0160\u0001\u0000\u0000\u0000\u0162\u0163\u0001\u0000\u0000"+
		"\u0000\u0163\u0165\u0001\u0000\u0000\u0000\u0164\u015a\u0001\u0000\u0000"+
		"\u0000\u0164\u015b\u0001\u0000\u0000\u0000\u0164\u015e\u0001\u0000\u0000"+
		"\u0000\u0165#\u0001\u0000\u0000\u0000\u0166\u0167\u0005\u001c\u0000\u0000"+
		"\u0167\u0178\u0005\u001d\u0000\u0000\u0168\u0169\u0005\u001c\u0000\u0000"+
		"\u0169\u016e\u0003\u001e\u000f\u0000\u016a\u016b\u0005$\u0000\u0000\u016b"+
		"\u016d\u0003\u001e\u000f\u0000\u016c\u016a\u0001\u0000\u0000\u0000\u016d"+
		"\u0170\u0001\u0000\u0000\u0000\u016e\u016c\u0001\u0000\u0000\u0000\u016e"+
		"\u016f\u0001\u0000\u0000\u0000\u016f\u0173\u0001\u0000\u0000\u0000\u0170"+
		"\u016e\u0001\u0000\u0000\u0000\u0171\u0172\u0005\u0018\u0000\u0000\u0172"+
		"\u0174\u0003\u001e\u000f\u0000\u0173\u0171\u0001\u0000\u0000\u0000\u0173"+
		"\u0174\u0001\u0000\u0000\u0000\u0174\u0175\u0001\u0000\u0000\u0000\u0175"+
		"\u0176\u0005\u001d\u0000\u0000\u0176\u0178\u0001\u0000\u0000\u0000\u0177"+
		"\u0166\u0001\u0000\u0000\u0000\u0177\u0168\u0001\u0000\u0000\u0000\u0178"+
		"%\u0001\u0000\u0000\u0000\u0179\u017a\u0007\u0003\u0000\u0000\u017a\'"+
		"\u0001\u0000\u0000\u0000/+06AEMRUY]djnr}\u0086\u008a\u008f\u0093\u0099"+
		"\u00a8\u00af\u00c1\u00d3\u00d8\u00e3\u00e6\u00e8\u00f2\u00fa\u00fd\u0108"+
		"\u0112\u0115\u0118\u011a\u011c\u012a\u0140\u014e\u0150\u0158\u0162\u0164"+
		"\u016e\u0173\u0177";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}