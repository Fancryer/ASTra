// Generated from A:/Downloads/KAST/src/main/resources/MiniJavaGrammar.g4 by ANTLR 4.13.1
package gen;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast", "CheckReturnValue"})
public class MiniJavaGrammarParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.13.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, T__3=4, T__4=5, T__5=6, INT=7, NEW=8, ELSE=9, 
		IF=10, WHILE=11, BOOLEAN=12, PUBLIC=13, THIS=14, RETURN=15, EXTENDS=16, 
		VOID=17, CLASS=18, STATIC=19, LPAREN=20, RPAREN=21, LBRACE=22, RBRACE=23, 
		LBRACK=24, RBRACK=25, SEMI=26, COMMA=27, DOT=28, ASSIGN=29, AND=30, LT=31, 
		BANG=32, ADD=33, SUB=34, MUL=35, WS=36, COMMENT=37, LINE_COMMENT=38, IDENTIFIER=39, 
		INTEGER_LITERAL=40;
	public static final int
		RULE_program = 0, RULE_mainClass = 1, RULE_classDeclaration = 2, RULE_methodDeclaration = 3, 
		RULE_varDeclaration = 4, RULE_type = 5, RULE_statement = 6, RULE_expression = 7, 
		RULE_identifier = 8;
	private static String[] makeRuleNames() {
		return new String[] {
			"program", "mainClass", "classDeclaration", "methodDeclaration", "varDeclaration", 
			"type", "statement", "expression", "identifier"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'main'", "'String'", "'System.out.println'", "'length'", "'true'", 
			"'false'", "'int'", "'new'", "'else'", "'if'", "'while'", "'boolean'", 
			"'public'", "'this'", "'return'", "'extends'", "'void'", "'class'", "'static'", 
			"'('", "')'", "'{'", "'}'", "'['", "']'", "';'", "','", "'.'", "'='", 
			"'&&'", "'<'", "'!'", "'+'", "'-'", "'*'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, null, null, null, null, null, null, "INT", "NEW", "ELSE", "IF", 
			"WHILE", "BOOLEAN", "PUBLIC", "THIS", "RETURN", "EXTENDS", "VOID", "CLASS", 
			"STATIC", "LPAREN", "RPAREN", "LBRACE", "RBRACE", "LBRACK", "RBRACK", 
			"SEMI", "COMMA", "DOT", "ASSIGN", "AND", "LT", "BANG", "ADD", "SUB", 
			"MUL", "WS", "COMMENT", "LINE_COMMENT", "IDENTIFIER", "INTEGER_LITERAL"
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
	public String getGrammarFileName() { return "MiniJavaGrammar.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public MiniJavaGrammarParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ProgramContext extends ParserRuleContext {
		public MainClassContext mainClass() {
			return getRuleContext(MainClassContext.class,0);
		}
		public TerminalNode EOF() { return getToken(MiniJavaGrammarParser.EOF, 0); }
		public List<ClassDeclarationContext> classDeclaration() {
			return getRuleContexts(ClassDeclarationContext.class);
		}
		public ClassDeclarationContext classDeclaration(int i) {
			return getRuleContext(ClassDeclarationContext.class,i);
		}
		public ProgramContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_program; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MiniJavaGrammarListener ) ((MiniJavaGrammarListener)listener).enterProgram(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MiniJavaGrammarListener ) ((MiniJavaGrammarListener)listener).exitProgram(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof MiniJavaGrammarVisitor ) return ((MiniJavaGrammarVisitor<? extends T>)visitor).visitProgram(this);
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
			setState(18);
			mainClass();
			setState(22);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==CLASS) {
				{
				{
				setState(19);
				classDeclaration();
				}
				}
				setState(24);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(25);
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
	public static class MainClassContext extends ParserRuleContext {
		public TerminalNode CLASS() { return getToken(MiniJavaGrammarParser.CLASS, 0); }
		public List<IdentifierContext> identifier() {
			return getRuleContexts(IdentifierContext.class);
		}
		public IdentifierContext identifier(int i) {
			return getRuleContext(IdentifierContext.class,i);
		}
		public List<TerminalNode> LBRACE() { return getTokens(MiniJavaGrammarParser.LBRACE); }
		public TerminalNode LBRACE(int i) {
			return getToken(MiniJavaGrammarParser.LBRACE, i);
		}
		public TerminalNode PUBLIC() { return getToken(MiniJavaGrammarParser.PUBLIC, 0); }
		public TerminalNode STATIC() { return getToken(MiniJavaGrammarParser.STATIC, 0); }
		public TerminalNode VOID() { return getToken(MiniJavaGrammarParser.VOID, 0); }
		public TerminalNode LPAREN() { return getToken(MiniJavaGrammarParser.LPAREN, 0); }
		public TerminalNode LBRACK() { return getToken(MiniJavaGrammarParser.LBRACK, 0); }
		public TerminalNode RBRACK() { return getToken(MiniJavaGrammarParser.RBRACK, 0); }
		public TerminalNode RPAREN() { return getToken(MiniJavaGrammarParser.RPAREN, 0); }
		public StatementContext statement() {
			return getRuleContext(StatementContext.class,0);
		}
		public List<TerminalNode> RBRACE() { return getTokens(MiniJavaGrammarParser.RBRACE); }
		public TerminalNode RBRACE(int i) {
			return getToken(MiniJavaGrammarParser.RBRACE, i);
		}
		public MainClassContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_mainClass; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MiniJavaGrammarListener ) ((MiniJavaGrammarListener)listener).enterMainClass(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MiniJavaGrammarListener ) ((MiniJavaGrammarListener)listener).exitMainClass(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof MiniJavaGrammarVisitor ) return ((MiniJavaGrammarVisitor<? extends T>)visitor).visitMainClass(this);
			else return visitor.visitChildren(this);
		}
	}

	public final MainClassContext mainClass() throws RecognitionException {
		MainClassContext _localctx = new MainClassContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_mainClass);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(27);
			match(CLASS);
			setState(28);
			identifier();
			setState(29);
			match(LBRACE);
			setState(30);
			match(PUBLIC);
			setState(31);
			match(STATIC);
			setState(32);
			match(VOID);
			setState(33);
			match(T__0);
			setState(34);
			match(LPAREN);
			setState(35);
			match(T__1);
			setState(36);
			match(LBRACK);
			setState(37);
			match(RBRACK);
			setState(38);
			identifier();
			setState(39);
			match(RPAREN);
			setState(40);
			match(LBRACE);
			setState(41);
			statement();
			setState(42);
			match(RBRACE);
			setState(43);
			match(RBRACE);
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
	public static class ClassDeclarationContext extends ParserRuleContext {
		public TerminalNode CLASS() { return getToken(MiniJavaGrammarParser.CLASS, 0); }
		public List<IdentifierContext> identifier() {
			return getRuleContexts(IdentifierContext.class);
		}
		public IdentifierContext identifier(int i) {
			return getRuleContext(IdentifierContext.class,i);
		}
		public TerminalNode LBRACE() { return getToken(MiniJavaGrammarParser.LBRACE, 0); }
		public TerminalNode RBRACE() { return getToken(MiniJavaGrammarParser.RBRACE, 0); }
		public TerminalNode EXTENDS() { return getToken(MiniJavaGrammarParser.EXTENDS, 0); }
		public List<VarDeclarationContext> varDeclaration() {
			return getRuleContexts(VarDeclarationContext.class);
		}
		public VarDeclarationContext varDeclaration(int i) {
			return getRuleContext(VarDeclarationContext.class,i);
		}
		public List<MethodDeclarationContext> methodDeclaration() {
			return getRuleContexts(MethodDeclarationContext.class);
		}
		public MethodDeclarationContext methodDeclaration(int i) {
			return getRuleContext(MethodDeclarationContext.class,i);
		}
		public ClassDeclarationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_classDeclaration; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MiniJavaGrammarListener ) ((MiniJavaGrammarListener)listener).enterClassDeclaration(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MiniJavaGrammarListener ) ((MiniJavaGrammarListener)listener).exitClassDeclaration(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof MiniJavaGrammarVisitor ) return ((MiniJavaGrammarVisitor<? extends T>)visitor).visitClassDeclaration(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ClassDeclarationContext classDeclaration() throws RecognitionException {
		ClassDeclarationContext _localctx = new ClassDeclarationContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_classDeclaration);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(45);
			match(CLASS);
			setState(46);
			identifier();
			setState(49);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==EXTENDS) {
				{
				setState(47);
				match(EXTENDS);
				setState(48);
				identifier();
				}
			}

			setState(51);
			match(LBRACE);
			setState(55);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==INT || _la==BOOLEAN) {
				{
				{
				setState(52);
				varDeclaration();
				}
				}
				setState(57);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(61);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==PUBLIC) {
				{
				{
				setState(58);
				methodDeclaration();
				}
				}
				setState(63);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(64);
			match(RBRACE);
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
	public static class MethodDeclarationContext extends ParserRuleContext {
		public TerminalNode PUBLIC() { return getToken(MiniJavaGrammarParser.PUBLIC, 0); }
		public List<TypeContext> type() {
			return getRuleContexts(TypeContext.class);
		}
		public TypeContext type(int i) {
			return getRuleContext(TypeContext.class,i);
		}
		public List<IdentifierContext> identifier() {
			return getRuleContexts(IdentifierContext.class);
		}
		public IdentifierContext identifier(int i) {
			return getRuleContext(IdentifierContext.class,i);
		}
		public TerminalNode LPAREN() { return getToken(MiniJavaGrammarParser.LPAREN, 0); }
		public TerminalNode RPAREN() { return getToken(MiniJavaGrammarParser.RPAREN, 0); }
		public TerminalNode LBRACE() { return getToken(MiniJavaGrammarParser.LBRACE, 0); }
		public TerminalNode RETURN() { return getToken(MiniJavaGrammarParser.RETURN, 0); }
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public TerminalNode SEMI() { return getToken(MiniJavaGrammarParser.SEMI, 0); }
		public TerminalNode RBRACE() { return getToken(MiniJavaGrammarParser.RBRACE, 0); }
		public List<VarDeclarationContext> varDeclaration() {
			return getRuleContexts(VarDeclarationContext.class);
		}
		public VarDeclarationContext varDeclaration(int i) {
			return getRuleContext(VarDeclarationContext.class,i);
		}
		public List<StatementContext> statement() {
			return getRuleContexts(StatementContext.class);
		}
		public StatementContext statement(int i) {
			return getRuleContext(StatementContext.class,i);
		}
		public List<TerminalNode> COMMA() { return getTokens(MiniJavaGrammarParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(MiniJavaGrammarParser.COMMA, i);
		}
		public MethodDeclarationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_methodDeclaration; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MiniJavaGrammarListener ) ((MiniJavaGrammarListener)listener).enterMethodDeclaration(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MiniJavaGrammarListener ) ((MiniJavaGrammarListener)listener).exitMethodDeclaration(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof MiniJavaGrammarVisitor ) return ((MiniJavaGrammarVisitor<? extends T>)visitor).visitMethodDeclaration(this);
			else return visitor.visitChildren(this);
		}
	}

	public final MethodDeclarationContext methodDeclaration() throws RecognitionException {
		MethodDeclarationContext _localctx = new MethodDeclarationContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_methodDeclaration);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(66);
			match(PUBLIC);
			setState(67);
			type();
			setState(68);
			identifier();
			setState(69);
			match(LPAREN);
			setState(81);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==INT || _la==BOOLEAN) {
				{
				setState(70);
				type();
				setState(71);
				identifier();
				setState(78);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==COMMA) {
					{
					{
					setState(72);
					match(COMMA);
					setState(73);
					type();
					setState(74);
					identifier();
					}
					}
					setState(80);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				}
			}

			setState(83);
			match(RPAREN);
			setState(84);
			match(LBRACE);
			setState(88);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==INT || _la==BOOLEAN) {
				{
				{
				setState(85);
				varDeclaration();
				}
				}
				setState(90);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(94);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & 549760011272L) != 0)) {
				{
				{
				setState(91);
				statement();
				}
				}
				setState(96);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(97);
			match(RETURN);
			setState(98);
			expression(0);
			setState(99);
			match(SEMI);
			setState(100);
			match(RBRACE);
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
	public static class VarDeclarationContext extends ParserRuleContext {
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public IdentifierContext identifier() {
			return getRuleContext(IdentifierContext.class,0);
		}
		public TerminalNode SEMI() { return getToken(MiniJavaGrammarParser.SEMI, 0); }
		public VarDeclarationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_varDeclaration; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MiniJavaGrammarListener ) ((MiniJavaGrammarListener)listener).enterVarDeclaration(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MiniJavaGrammarListener ) ((MiniJavaGrammarListener)listener).exitVarDeclaration(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof MiniJavaGrammarVisitor ) return ((MiniJavaGrammarVisitor<? extends T>)visitor).visitVarDeclaration(this);
			else return visitor.visitChildren(this);
		}
	}

	public final VarDeclarationContext varDeclaration() throws RecognitionException {
		VarDeclarationContext _localctx = new VarDeclarationContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_varDeclaration);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(102);
			type();
			setState(103);
			identifier();
			setState(104);
			match(SEMI);
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
		public TerminalNode INT() { return getToken(MiniJavaGrammarParser.INT, 0); }
		public TerminalNode LBRACK() { return getToken(MiniJavaGrammarParser.LBRACK, 0); }
		public TerminalNode RBRACK() { return getToken(MiniJavaGrammarParser.RBRACK, 0); }
		public TerminalNode BOOLEAN() { return getToken(MiniJavaGrammarParser.BOOLEAN, 0); }
		public TypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_type; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MiniJavaGrammarListener ) ((MiniJavaGrammarListener)listener).enterType(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MiniJavaGrammarListener ) ((MiniJavaGrammarListener)listener).exitType(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof MiniJavaGrammarVisitor ) return ((MiniJavaGrammarVisitor<? extends T>)visitor).visitType(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TypeContext type() throws RecognitionException {
		TypeContext _localctx = new TypeContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_type);
		try {
			setState(111);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,8,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(106);
				match(INT);
				setState(107);
				match(LBRACK);
				setState(108);
				match(RBRACK);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(109);
				match(BOOLEAN);
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(110);
				match(INT);
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
	public static class StatementContext extends ParserRuleContext {
		public TerminalNode LBRACE() { return getToken(MiniJavaGrammarParser.LBRACE, 0); }
		public TerminalNode RBRACE() { return getToken(MiniJavaGrammarParser.RBRACE, 0); }
		public List<StatementContext> statement() {
			return getRuleContexts(StatementContext.class);
		}
		public StatementContext statement(int i) {
			return getRuleContext(StatementContext.class,i);
		}
		public TerminalNode IF() { return getToken(MiniJavaGrammarParser.IF, 0); }
		public TerminalNode LPAREN() { return getToken(MiniJavaGrammarParser.LPAREN, 0); }
		public List<ExpressionContext> expression() {
			return getRuleContexts(ExpressionContext.class);
		}
		public ExpressionContext expression(int i) {
			return getRuleContext(ExpressionContext.class,i);
		}
		public TerminalNode RPAREN() { return getToken(MiniJavaGrammarParser.RPAREN, 0); }
		public TerminalNode ELSE() { return getToken(MiniJavaGrammarParser.ELSE, 0); }
		public TerminalNode WHILE() { return getToken(MiniJavaGrammarParser.WHILE, 0); }
		public TerminalNode SEMI() { return getToken(MiniJavaGrammarParser.SEMI, 0); }
		public IdentifierContext identifier() {
			return getRuleContext(IdentifierContext.class,0);
		}
		public TerminalNode ASSIGN() { return getToken(MiniJavaGrammarParser.ASSIGN, 0); }
		public TerminalNode LBRACK() { return getToken(MiniJavaGrammarParser.LBRACK, 0); }
		public TerminalNode RBRACK() { return getToken(MiniJavaGrammarParser.RBRACK, 0); }
		public StatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_statement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MiniJavaGrammarListener ) ((MiniJavaGrammarListener)listener).enterStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MiniJavaGrammarListener ) ((MiniJavaGrammarListener)listener).exitStatement(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof MiniJavaGrammarVisitor ) return ((MiniJavaGrammarVisitor<? extends T>)visitor).visitStatement(this);
			else return visitor.visitChildren(this);
		}
	}

	public final StatementContext statement() throws RecognitionException {
		StatementContext _localctx = new StatementContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_statement);
		int _la;
		try {
			setState(154);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,10,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(113);
				match(LBRACE);
				setState(117);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while ((((_la) & ~0x3f) == 0 && ((1L << _la) & 549760011272L) != 0)) {
					{
					{
					setState(114);
					statement();
					}
					}
					setState(119);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(120);
				match(RBRACE);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(121);
				match(IF);
				setState(122);
				match(LPAREN);
				setState(123);
				expression(0);
				setState(124);
				match(RPAREN);
				setState(125);
				statement();
				setState(126);
				match(ELSE);
				setState(127);
				statement();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(129);
				match(WHILE);
				setState(130);
				match(LPAREN);
				setState(131);
				expression(0);
				setState(132);
				match(RPAREN);
				setState(133);
				statement();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(135);
				match(T__2);
				setState(136);
				match(LPAREN);
				setState(137);
				expression(0);
				setState(138);
				match(RPAREN);
				setState(139);
				match(SEMI);
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(141);
				identifier();
				setState(142);
				match(ASSIGN);
				setState(143);
				expression(0);
				setState(144);
				match(SEMI);
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(146);
				identifier();
				setState(147);
				match(LBRACK);
				setState(148);
				expression(0);
				setState(149);
				match(RBRACK);
				setState(150);
				match(ASSIGN);
				setState(151);
				expression(0);
				setState(152);
				match(SEMI);
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
	public static class ExpressionContext extends ParserRuleContext {
		public TerminalNode INTEGER_LITERAL() { return getToken(MiniJavaGrammarParser.INTEGER_LITERAL, 0); }
		public IdentifierContext identifier() {
			return getRuleContext(IdentifierContext.class,0);
		}
		public TerminalNode THIS() { return getToken(MiniJavaGrammarParser.THIS, 0); }
		public TerminalNode NEW() { return getToken(MiniJavaGrammarParser.NEW, 0); }
		public TerminalNode INT() { return getToken(MiniJavaGrammarParser.INT, 0); }
		public TerminalNode LBRACK() { return getToken(MiniJavaGrammarParser.LBRACK, 0); }
		public List<ExpressionContext> expression() {
			return getRuleContexts(ExpressionContext.class);
		}
		public ExpressionContext expression(int i) {
			return getRuleContext(ExpressionContext.class,i);
		}
		public TerminalNode RBRACK() { return getToken(MiniJavaGrammarParser.RBRACK, 0); }
		public TerminalNode LPAREN() { return getToken(MiniJavaGrammarParser.LPAREN, 0); }
		public TerminalNode RPAREN() { return getToken(MiniJavaGrammarParser.RPAREN, 0); }
		public TerminalNode BANG() { return getToken(MiniJavaGrammarParser.BANG, 0); }
		public TerminalNode AND() { return getToken(MiniJavaGrammarParser.AND, 0); }
		public TerminalNode LT() { return getToken(MiniJavaGrammarParser.LT, 0); }
		public TerminalNode ADD() { return getToken(MiniJavaGrammarParser.ADD, 0); }
		public TerminalNode SUB() { return getToken(MiniJavaGrammarParser.SUB, 0); }
		public TerminalNode MUL() { return getToken(MiniJavaGrammarParser.MUL, 0); }
		public TerminalNode DOT() { return getToken(MiniJavaGrammarParser.DOT, 0); }
		public List<TerminalNode> COMMA() { return getTokens(MiniJavaGrammarParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(MiniJavaGrammarParser.COMMA, i);
		}
		public ExpressionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_expression; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MiniJavaGrammarListener ) ((MiniJavaGrammarListener)listener).enterExpression(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MiniJavaGrammarListener ) ((MiniJavaGrammarListener)listener).exitExpression(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof MiniJavaGrammarVisitor ) return ((MiniJavaGrammarVisitor<? extends T>)visitor).visitExpression(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ExpressionContext expression() throws RecognitionException {
		return expression(0);
	}

	private ExpressionContext expression(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		ExpressionContext _localctx = new ExpressionContext(_ctx, _parentState);
		ExpressionContext _prevctx = _localctx;
		int _startState = 14;
		enterRecursionRule(_localctx, 14, RULE_expression, _p);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(179);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,11,_ctx) ) {
			case 1:
				{
				setState(157);
				match(INTEGER_LITERAL);
				}
				break;
			case 2:
				{
				setState(158);
				match(T__4);
				}
				break;
			case 3:
				{
				setState(159);
				match(T__5);
				}
				break;
			case 4:
				{
				setState(160);
				identifier();
				}
				break;
			case 5:
				{
				setState(161);
				match(THIS);
				}
				break;
			case 6:
				{
				setState(162);
				match(NEW);
				setState(163);
				match(INT);
				setState(164);
				match(LBRACK);
				setState(165);
				expression(0);
				setState(166);
				match(RBRACK);
				}
				break;
			case 7:
				{
				setState(168);
				match(NEW);
				setState(169);
				identifier();
				setState(170);
				match(LPAREN);
				setState(171);
				match(RPAREN);
				}
				break;
			case 8:
				{
				setState(173);
				match(BANG);
				setState(174);
				expression(2);
				}
				break;
			case 9:
				{
				setState(175);
				match(LPAREN);
				setState(176);
				expression(0);
				setState(177);
				match(RPAREN);
				}
				break;
			}
			_ctx.stop = _input.LT(-1);
			setState(210);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,15,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					setState(208);
					_errHandler.sync(this);
					switch ( getInterpreter().adaptivePredict(_input,14,_ctx) ) {
					case 1:
						{
						_localctx = new ExpressionContext(_parentctx, _parentState);
						pushNewRecursionContext(_localctx, _startState, RULE_expression);
						setState(181);
						if (!(precpred(_ctx, 13))) throw new FailedPredicateException(this, "precpred(_ctx, 13)");
						setState(182);
						_la = _input.LA(1);
						if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 63350767616L) != 0)) ) {
						_errHandler.recoverInline(this);
						}
						else {
							if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
							_errHandler.reportMatch(this);
							consume();
						}
						setState(183);
						expression(14);
						}
						break;
					case 2:
						{
						_localctx = new ExpressionContext(_parentctx, _parentState);
						pushNewRecursionContext(_localctx, _startState, RULE_expression);
						setState(184);
						if (!(precpred(_ctx, 12))) throw new FailedPredicateException(this, "precpred(_ctx, 12)");
						setState(185);
						match(LBRACK);
						setState(186);
						expression(0);
						setState(187);
						match(RBRACK);
						}
						break;
					case 3:
						{
						_localctx = new ExpressionContext(_parentctx, _parentState);
						pushNewRecursionContext(_localctx, _startState, RULE_expression);
						setState(189);
						if (!(precpred(_ctx, 11))) throw new FailedPredicateException(this, "precpred(_ctx, 11)");
						setState(190);
						match(DOT);
						setState(191);
						match(T__3);
						}
						break;
					case 4:
						{
						_localctx = new ExpressionContext(_parentctx, _parentState);
						pushNewRecursionContext(_localctx, _startState, RULE_expression);
						setState(192);
						if (!(precpred(_ctx, 10))) throw new FailedPredicateException(this, "precpred(_ctx, 10)");
						setState(193);
						match(DOT);
						setState(194);
						identifier();
						setState(195);
						match(LPAREN);
						setState(204);
						_errHandler.sync(this);
						_la = _input.LA(1);
						if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 1653563474272L) != 0)) {
							{
							setState(196);
							expression(0);
							setState(201);
							_errHandler.sync(this);
							_la = _input.LA(1);
							while (_la==COMMA) {
								{
								{
								setState(197);
								match(COMMA);
								setState(198);
								expression(0);
								}
								}
								setState(203);
								_errHandler.sync(this);
								_la = _input.LA(1);
							}
							}
						}

						setState(206);
						match(RPAREN);
						}
						break;
					}
					} 
				}
				setState(212);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,15,_ctx);
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
	public static class IdentifierContext extends ParserRuleContext {
		public TerminalNode IDENTIFIER() { return getToken(MiniJavaGrammarParser.IDENTIFIER, 0); }
		public IdentifierContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_identifier; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MiniJavaGrammarListener ) ((MiniJavaGrammarListener)listener).enterIdentifier(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MiniJavaGrammarListener ) ((MiniJavaGrammarListener)listener).exitIdentifier(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof MiniJavaGrammarVisitor ) return ((MiniJavaGrammarVisitor<? extends T>)visitor).visitIdentifier(this);
			else return visitor.visitChildren(this);
		}
	}

	public final IdentifierContext identifier() throws RecognitionException {
		IdentifierContext _localctx = new IdentifierContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_identifier);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(213);
			match(IDENTIFIER);
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
		case 7:
			return expression_sempred((ExpressionContext)_localctx, predIndex);
		}
		return true;
	}
	private boolean expression_sempred(ExpressionContext _localctx, int predIndex) {
		switch (predIndex) {
		case 0:
			return precpred(_ctx, 13);
		case 1:
			return precpred(_ctx, 12);
		case 2:
			return precpred(_ctx, 11);
		case 3:
			return precpred(_ctx, 10);
		}
		return true;
	}

	public static final String _serializedATN =
		"\u0004\u0001(\u00d8\u0002\u0000\u0007\u0000\u0002\u0001\u0007\u0001\u0002"+
		"\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002\u0004\u0007\u0004\u0002"+
		"\u0005\u0007\u0005\u0002\u0006\u0007\u0006\u0002\u0007\u0007\u0007\u0002"+
		"\b\u0007\b\u0001\u0000\u0001\u0000\u0005\u0000\u0015\b\u0000\n\u0000\f"+
		"\u0000\u0018\t\u0000\u0001\u0000\u0001\u0000\u0001\u0001\u0001\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0002\u0001\u0002\u0001"+
		"\u0002\u0001\u0002\u0003\u00022\b\u0002\u0001\u0002\u0001\u0002\u0005"+
		"\u00026\b\u0002\n\u0002\f\u00029\t\u0002\u0001\u0002\u0005\u0002<\b\u0002"+
		"\n\u0002\f\u0002?\t\u0002\u0001\u0002\u0001\u0002\u0001\u0003\u0001\u0003"+
		"\u0001\u0003\u0001\u0003\u0001\u0003\u0001\u0003\u0001\u0003\u0001\u0003"+
		"\u0001\u0003\u0001\u0003\u0005\u0003M\b\u0003\n\u0003\f\u0003P\t\u0003"+
		"\u0003\u0003R\b\u0003\u0001\u0003\u0001\u0003\u0001\u0003\u0005\u0003"+
		"W\b\u0003\n\u0003\f\u0003Z\t\u0003\u0001\u0003\u0005\u0003]\b\u0003\n"+
		"\u0003\f\u0003`\t\u0003\u0001\u0003\u0001\u0003\u0001\u0003\u0001\u0003"+
		"\u0001\u0003\u0001\u0004\u0001\u0004\u0001\u0004\u0001\u0004\u0001\u0005"+
		"\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0003\u0005p\b\u0005"+
		"\u0001\u0006\u0001\u0006\u0005\u0006t\b\u0006\n\u0006\f\u0006w\t\u0006"+
		"\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006"+
		"\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006"+
		"\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006"+
		"\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006"+
		"\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006"+
		"\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0003\u0006\u009b\b\u0006"+
		"\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007"+
		"\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007"+
		"\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007"+
		"\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0003\u0007"+
		"\u00b4\b\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007"+
		"\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007"+
		"\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007"+
		"\u0001\u0007\u0005\u0007\u00c8\b\u0007\n\u0007\f\u0007\u00cb\t\u0007\u0003"+
		"\u0007\u00cd\b\u0007\u0001\u0007\u0001\u0007\u0005\u0007\u00d1\b\u0007"+
		"\n\u0007\f\u0007\u00d4\t\u0007\u0001\b\u0001\b\u0001\b\u0000\u0001\u000e"+
		"\t\u0000\u0002\u0004\u0006\b\n\f\u000e\u0010\u0000\u0001\u0002\u0000\u001e"+
		"\u001f!#\u00ec\u0000\u0012\u0001\u0000\u0000\u0000\u0002\u001b\u0001\u0000"+
		"\u0000\u0000\u0004-\u0001\u0000\u0000\u0000\u0006B\u0001\u0000\u0000\u0000"+
		"\bf\u0001\u0000\u0000\u0000\no\u0001\u0000\u0000\u0000\f\u009a\u0001\u0000"+
		"\u0000\u0000\u000e\u00b3\u0001\u0000\u0000\u0000\u0010\u00d5\u0001\u0000"+
		"\u0000\u0000\u0012\u0016\u0003\u0002\u0001\u0000\u0013\u0015\u0003\u0004"+
		"\u0002\u0000\u0014\u0013\u0001\u0000\u0000\u0000\u0015\u0018\u0001\u0000"+
		"\u0000\u0000\u0016\u0014\u0001\u0000\u0000\u0000\u0016\u0017\u0001\u0000"+
		"\u0000\u0000\u0017\u0019\u0001\u0000\u0000\u0000\u0018\u0016\u0001\u0000"+
		"\u0000\u0000\u0019\u001a\u0005\u0000\u0000\u0001\u001a\u0001\u0001\u0000"+
		"\u0000\u0000\u001b\u001c\u0005\u0012\u0000\u0000\u001c\u001d\u0003\u0010"+
		"\b\u0000\u001d\u001e\u0005\u0016\u0000\u0000\u001e\u001f\u0005\r\u0000"+
		"\u0000\u001f \u0005\u0013\u0000\u0000 !\u0005\u0011\u0000\u0000!\"\u0005"+
		"\u0001\u0000\u0000\"#\u0005\u0014\u0000\u0000#$\u0005\u0002\u0000\u0000"+
		"$%\u0005\u0018\u0000\u0000%&\u0005\u0019\u0000\u0000&\'\u0003\u0010\b"+
		"\u0000\'(\u0005\u0015\u0000\u0000()\u0005\u0016\u0000\u0000)*\u0003\f"+
		"\u0006\u0000*+\u0005\u0017\u0000\u0000+,\u0005\u0017\u0000\u0000,\u0003"+
		"\u0001\u0000\u0000\u0000-.\u0005\u0012\u0000\u0000.1\u0003\u0010\b\u0000"+
		"/0\u0005\u0010\u0000\u000002\u0003\u0010\b\u00001/\u0001\u0000\u0000\u0000"+
		"12\u0001\u0000\u0000\u000023\u0001\u0000\u0000\u000037\u0005\u0016\u0000"+
		"\u000046\u0003\b\u0004\u000054\u0001\u0000\u0000\u000069\u0001\u0000\u0000"+
		"\u000075\u0001\u0000\u0000\u000078\u0001\u0000\u0000\u00008=\u0001\u0000"+
		"\u0000\u000097\u0001\u0000\u0000\u0000:<\u0003\u0006\u0003\u0000;:\u0001"+
		"\u0000\u0000\u0000<?\u0001\u0000\u0000\u0000=;\u0001\u0000\u0000\u0000"+
		"=>\u0001\u0000\u0000\u0000>@\u0001\u0000\u0000\u0000?=\u0001\u0000\u0000"+
		"\u0000@A\u0005\u0017\u0000\u0000A\u0005\u0001\u0000\u0000\u0000BC\u0005"+
		"\r\u0000\u0000CD\u0003\n\u0005\u0000DE\u0003\u0010\b\u0000EQ\u0005\u0014"+
		"\u0000\u0000FG\u0003\n\u0005\u0000GN\u0003\u0010\b\u0000HI\u0005\u001b"+
		"\u0000\u0000IJ\u0003\n\u0005\u0000JK\u0003\u0010\b\u0000KM\u0001\u0000"+
		"\u0000\u0000LH\u0001\u0000\u0000\u0000MP\u0001\u0000\u0000\u0000NL\u0001"+
		"\u0000\u0000\u0000NO\u0001\u0000\u0000\u0000OR\u0001\u0000\u0000\u0000"+
		"PN\u0001\u0000\u0000\u0000QF\u0001\u0000\u0000\u0000QR\u0001\u0000\u0000"+
		"\u0000RS\u0001\u0000\u0000\u0000ST\u0005\u0015\u0000\u0000TX\u0005\u0016"+
		"\u0000\u0000UW\u0003\b\u0004\u0000VU\u0001\u0000\u0000\u0000WZ\u0001\u0000"+
		"\u0000\u0000XV\u0001\u0000\u0000\u0000XY\u0001\u0000\u0000\u0000Y^\u0001"+
		"\u0000\u0000\u0000ZX\u0001\u0000\u0000\u0000[]\u0003\f\u0006\u0000\\["+
		"\u0001\u0000\u0000\u0000]`\u0001\u0000\u0000\u0000^\\\u0001\u0000\u0000"+
		"\u0000^_\u0001\u0000\u0000\u0000_a\u0001\u0000\u0000\u0000`^\u0001\u0000"+
		"\u0000\u0000ab\u0005\u000f\u0000\u0000bc\u0003\u000e\u0007\u0000cd\u0005"+
		"\u001a\u0000\u0000de\u0005\u0017\u0000\u0000e\u0007\u0001\u0000\u0000"+
		"\u0000fg\u0003\n\u0005\u0000gh\u0003\u0010\b\u0000hi\u0005\u001a\u0000"+
		"\u0000i\t\u0001\u0000\u0000\u0000jk\u0005\u0007\u0000\u0000kl\u0005\u0018"+
		"\u0000\u0000lp\u0005\u0019\u0000\u0000mp\u0005\f\u0000\u0000np\u0005\u0007"+
		"\u0000\u0000oj\u0001\u0000\u0000\u0000om\u0001\u0000\u0000\u0000on\u0001"+
		"\u0000\u0000\u0000p\u000b\u0001\u0000\u0000\u0000qu\u0005\u0016\u0000"+
		"\u0000rt\u0003\f\u0006\u0000sr\u0001\u0000\u0000\u0000tw\u0001\u0000\u0000"+
		"\u0000us\u0001\u0000\u0000\u0000uv\u0001\u0000\u0000\u0000vx\u0001\u0000"+
		"\u0000\u0000wu\u0001\u0000\u0000\u0000x\u009b\u0005\u0017\u0000\u0000"+
		"yz\u0005\n\u0000\u0000z{\u0005\u0014\u0000\u0000{|\u0003\u000e\u0007\u0000"+
		"|}\u0005\u0015\u0000\u0000}~\u0003\f\u0006\u0000~\u007f\u0005\t\u0000"+
		"\u0000\u007f\u0080\u0003\f\u0006\u0000\u0080\u009b\u0001\u0000\u0000\u0000"+
		"\u0081\u0082\u0005\u000b\u0000\u0000\u0082\u0083\u0005\u0014\u0000\u0000"+
		"\u0083\u0084\u0003\u000e\u0007\u0000\u0084\u0085\u0005\u0015\u0000\u0000"+
		"\u0085\u0086\u0003\f\u0006\u0000\u0086\u009b\u0001\u0000\u0000\u0000\u0087"+
		"\u0088\u0005\u0003\u0000\u0000\u0088\u0089\u0005\u0014\u0000\u0000\u0089"+
		"\u008a\u0003\u000e\u0007\u0000\u008a\u008b\u0005\u0015\u0000\u0000\u008b"+
		"\u008c\u0005\u001a\u0000\u0000\u008c\u009b\u0001\u0000\u0000\u0000\u008d"+
		"\u008e\u0003\u0010\b\u0000\u008e\u008f\u0005\u001d\u0000\u0000\u008f\u0090"+
		"\u0003\u000e\u0007\u0000\u0090\u0091\u0005\u001a\u0000\u0000\u0091\u009b"+
		"\u0001\u0000\u0000\u0000\u0092\u0093\u0003\u0010\b\u0000\u0093\u0094\u0005"+
		"\u0018\u0000\u0000\u0094\u0095\u0003\u000e\u0007\u0000\u0095\u0096\u0005"+
		"\u0019\u0000\u0000\u0096\u0097\u0005\u001d\u0000\u0000\u0097\u0098\u0003"+
		"\u000e\u0007\u0000\u0098\u0099\u0005\u001a\u0000\u0000\u0099\u009b\u0001"+
		"\u0000\u0000\u0000\u009aq\u0001\u0000\u0000\u0000\u009ay\u0001\u0000\u0000"+
		"\u0000\u009a\u0081\u0001\u0000\u0000\u0000\u009a\u0087\u0001\u0000\u0000"+
		"\u0000\u009a\u008d\u0001\u0000\u0000\u0000\u009a\u0092\u0001\u0000\u0000"+
		"\u0000\u009b\r\u0001\u0000\u0000\u0000\u009c\u009d\u0006\u0007\uffff\uffff"+
		"\u0000\u009d\u00b4\u0005(\u0000\u0000\u009e\u00b4\u0005\u0005\u0000\u0000"+
		"\u009f\u00b4\u0005\u0006\u0000\u0000\u00a0\u00b4\u0003\u0010\b\u0000\u00a1"+
		"\u00b4\u0005\u000e\u0000\u0000\u00a2\u00a3\u0005\b\u0000\u0000\u00a3\u00a4"+
		"\u0005\u0007\u0000\u0000\u00a4\u00a5\u0005\u0018\u0000\u0000\u00a5\u00a6"+
		"\u0003\u000e\u0007\u0000\u00a6\u00a7\u0005\u0019\u0000\u0000\u00a7\u00b4"+
		"\u0001\u0000\u0000\u0000\u00a8\u00a9\u0005\b\u0000\u0000\u00a9\u00aa\u0003"+
		"\u0010\b\u0000\u00aa\u00ab\u0005\u0014\u0000\u0000\u00ab\u00ac\u0005\u0015"+
		"\u0000\u0000\u00ac\u00b4\u0001\u0000\u0000\u0000\u00ad\u00ae\u0005 \u0000"+
		"\u0000\u00ae\u00b4\u0003\u000e\u0007\u0002\u00af\u00b0\u0005\u0014\u0000"+
		"\u0000\u00b0\u00b1\u0003\u000e\u0007\u0000\u00b1\u00b2\u0005\u0015\u0000"+
		"\u0000\u00b2\u00b4\u0001\u0000\u0000\u0000\u00b3\u009c\u0001\u0000\u0000"+
		"\u0000\u00b3\u009e\u0001\u0000\u0000\u0000\u00b3\u009f\u0001\u0000\u0000"+
		"\u0000\u00b3\u00a0\u0001\u0000\u0000\u0000\u00b3\u00a1\u0001\u0000\u0000"+
		"\u0000\u00b3\u00a2\u0001\u0000\u0000\u0000\u00b3\u00a8\u0001\u0000\u0000"+
		"\u0000\u00b3\u00ad\u0001\u0000\u0000\u0000\u00b3\u00af\u0001\u0000\u0000"+
		"\u0000\u00b4\u00d2\u0001\u0000\u0000\u0000\u00b5\u00b6\n\r\u0000\u0000"+
		"\u00b6\u00b7\u0007\u0000\u0000\u0000\u00b7\u00d1\u0003\u000e\u0007\u000e"+
		"\u00b8\u00b9\n\f\u0000\u0000\u00b9\u00ba\u0005\u0018\u0000\u0000\u00ba"+
		"\u00bb\u0003\u000e\u0007\u0000\u00bb\u00bc\u0005\u0019\u0000\u0000\u00bc"+
		"\u00d1\u0001\u0000\u0000\u0000\u00bd\u00be\n\u000b\u0000\u0000\u00be\u00bf"+
		"\u0005\u001c\u0000\u0000\u00bf\u00d1\u0005\u0004\u0000\u0000\u00c0\u00c1"+
		"\n\n\u0000\u0000\u00c1\u00c2\u0005\u001c\u0000\u0000\u00c2\u00c3\u0003"+
		"\u0010\b\u0000\u00c3\u00cc\u0005\u0014\u0000\u0000\u00c4\u00c9\u0003\u000e"+
		"\u0007\u0000\u00c5\u00c6\u0005\u001b\u0000\u0000\u00c6\u00c8\u0003\u000e"+
		"\u0007\u0000\u00c7\u00c5\u0001\u0000\u0000\u0000\u00c8\u00cb\u0001\u0000"+
		"\u0000\u0000\u00c9\u00c7\u0001\u0000\u0000\u0000\u00c9\u00ca\u0001\u0000"+
		"\u0000\u0000\u00ca\u00cd\u0001\u0000\u0000\u0000\u00cb\u00c9\u0001\u0000"+
		"\u0000\u0000\u00cc\u00c4\u0001\u0000\u0000\u0000\u00cc\u00cd\u0001\u0000"+
		"\u0000\u0000\u00cd\u00ce\u0001\u0000\u0000\u0000\u00ce\u00cf\u0005\u0015"+
		"\u0000\u0000\u00cf\u00d1\u0001\u0000\u0000\u0000\u00d0\u00b5\u0001\u0000"+
		"\u0000\u0000\u00d0\u00b8\u0001\u0000\u0000\u0000\u00d0\u00bd\u0001\u0000"+
		"\u0000\u0000\u00d0\u00c0\u0001\u0000\u0000\u0000\u00d1\u00d4\u0001\u0000"+
		"\u0000\u0000\u00d2\u00d0\u0001\u0000\u0000\u0000\u00d2\u00d3\u0001\u0000"+
		"\u0000\u0000\u00d3\u000f\u0001\u0000\u0000\u0000\u00d4\u00d2\u0001\u0000"+
		"\u0000\u0000\u00d5\u00d6\u0005\'\u0000\u0000\u00d6\u0011\u0001\u0000\u0000"+
		"\u0000\u0010\u001617=NQX^ou\u009a\u00b3\u00c9\u00cc\u00d0\u00d2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}