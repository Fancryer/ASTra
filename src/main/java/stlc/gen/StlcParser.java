// Generated from A:/Downloads/KAST/src/main/resources/Stlc.g4 by ANTLR 4.13.1
package stlc.gen;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast", "CheckReturnValue"})
public class StlcParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.13.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, T__3=4, T__4=5, T__5=6, T__6=7, T__7=8, T__8=9, 
		T__9=10, T__10=11, ID=12, WS=13;
	public static final int
		RULE_t = 0, RULE_x = 1, RULE_type = 2;
	private static String[] makeRuleNames() {
		return new String[] {
			"t", "x", "type"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'\\'", "':'", "','", "'true'", "'false'", "'if'", "'then'", "'else'", 
			"'('", "')'", "'->'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, null, null, null, null, null, null, null, null, null, null, null, 
			"ID", "WS"
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
	public String getGrammarFileName() { return "Stlc.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public StlcParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TContext extends ParserRuleContext {
		public TContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_t; }
	 
		public TContext() { }
		public void copyFrom(TContext ctx) {
			super.copyFrom(ctx);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Constant_trueContext extends TContext {
		public Constant_trueContext(TContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof StlcListener ) ((StlcListener)listener).enterConstant_true(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof StlcListener ) ((StlcListener)listener).exitConstant_true(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof StlcVisitor ) return ((StlcVisitor<? extends T>)visitor).visitConstant_true(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class ConditionalContext extends TContext {
		public TContext pred;
		public TContext if_true;
		public TContext if_false;
		public List<TContext> t() {
			return getRuleContexts(TContext.class);
		}
		public TContext t(int i) {
			return getRuleContext(TContext.class,i);
		}
		public ConditionalContext(TContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof StlcListener ) ((StlcListener)listener).enterConditional(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof StlcListener ) ((StlcListener)listener).exitConditional(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof StlcVisitor ) return ((StlcVisitor<? extends T>)visitor).visitConditional(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class ApplicationContext extends TContext {
		public List<TContext> t() {
			return getRuleContexts(TContext.class);
		}
		public TContext t(int i) {
			return getRuleContext(TContext.class,i);
		}
		public ApplicationContext(TContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof StlcListener ) ((StlcListener)listener).enterApplication(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof StlcListener ) ((StlcListener)listener).exitApplication(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof StlcVisitor ) return ((StlcVisitor<? extends T>)visitor).visitApplication(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class AbstractionContext extends TContext {
		public XContext x() {
			return getRuleContext(XContext.class,0);
		}
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public TContext t() {
			return getRuleContext(TContext.class,0);
		}
		public AbstractionContext(TContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof StlcListener ) ((StlcListener)listener).enterAbstraction(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof StlcListener ) ((StlcListener)listener).exitAbstraction(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof StlcVisitor ) return ((StlcVisitor<? extends T>)visitor).visitAbstraction(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class VariableContext extends TContext {
		public XContext x() {
			return getRuleContext(XContext.class,0);
		}
		public VariableContext(TContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof StlcListener ) ((StlcListener)listener).enterVariable(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof StlcListener ) ((StlcListener)listener).exitVariable(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof StlcVisitor ) return ((StlcVisitor<? extends T>)visitor).visitVariable(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class ParenthesisContext extends TContext {
		public TContext t() {
			return getRuleContext(TContext.class,0);
		}
		public ParenthesisContext(TContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof StlcListener ) ((StlcListener)listener).enterParenthesis(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof StlcListener ) ((StlcListener)listener).exitParenthesis(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof StlcVisitor ) return ((StlcVisitor<? extends T>)visitor).visitParenthesis(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Constant_falseContext extends TContext {
		public Constant_falseContext(TContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof StlcListener ) ((StlcListener)listener).enterConstant_false(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof StlcListener ) ((StlcListener)listener).exitConstant_false(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof StlcVisitor ) return ((StlcVisitor<? extends T>)visitor).visitConstant_false(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TContext t() throws RecognitionException {
		return t(0);
	}

	private TContext t(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		TContext _localctx = new TContext(_ctx, _parentState);
		TContext _prevctx = _localctx;
		int _startState = 0;
		enterRecursionRule(_localctx, 0, RULE_t, _p);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(28);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case ID:
				{
				_localctx = new VariableContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;

				setState(7);
				x();
				}
				break;
			case T__0:
				{
				_localctx = new AbstractionContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(8);
				match(T__0);
				setState(9);
				x();
				setState(10);
				match(T__1);
				setState(11);
				type();
				setState(12);
				match(T__2);
				setState(13);
				t(5);
				}
				break;
			case T__3:
				{
				_localctx = new Constant_trueContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(15);
				match(T__3);
				}
				break;
			case T__4:
				{
				_localctx = new Constant_falseContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(16);
				match(T__4);
				}
				break;
			case T__5:
				{
				_localctx = new ConditionalContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(17);
				match(T__5);
				setState(18);
				((ConditionalContext)_localctx).pred = t(0);
				setState(19);
				match(T__6);
				setState(20);
				((ConditionalContext)_localctx).if_true = t(0);
				setState(21);
				match(T__7);
				setState(22);
				((ConditionalContext)_localctx).if_false = t(2);
				}
				break;
			case T__8:
				{
				_localctx = new ParenthesisContext(_localctx);
				_ctx = _localctx;
				_prevctx = _localctx;
				setState(24);
				match(T__8);
				setState(25);
				t(0);
				setState(26);
				match(T__9);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			_ctx.stop = _input.LT(-1);
			setState(34);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,1,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new ApplicationContext(new TContext(_parentctx, _parentState));
					pushNewRecursionContext(_localctx, _startState, RULE_t);
					setState(30);
					if (!(precpred(_ctx, 6))) throw new FailedPredicateException(this, "precpred(_ctx, 6)");
					setState(31);
					t(7);
					}
					} 
				}
				setState(36);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,1,_ctx);
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
	public static class XContext extends ParserRuleContext {
		public TerminalNode ID() { return getToken(StlcParser.ID, 0); }
		public XContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_x; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof StlcListener ) ((StlcListener)listener).enterX(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof StlcListener ) ((StlcListener)listener).exitX(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof StlcVisitor ) return ((StlcVisitor<? extends T>)visitor).visitX(this);
			else return visitor.visitChildren(this);
		}
	}

	public final XContext x() throws RecognitionException {
		XContext _localctx = new XContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_x);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(37);
			match(ID);
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
	public static class Flat_typeContext extends TypeContext {
		public TerminalNode ID() { return getToken(StlcParser.ID, 0); }
		public Flat_typeContext(TypeContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof StlcListener ) ((StlcListener)listener).enterFlat_type(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof StlcListener ) ((StlcListener)listener).exitFlat_type(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof StlcVisitor ) return ((StlcVisitor<? extends T>)visitor).visitFlat_type(this);
			else return visitor.visitChildren(this);
		}
	}
	@SuppressWarnings("CheckReturnValue")
	public static class Abstraction_typeContext extends TypeContext {
		public TerminalNode ID() { return getToken(StlcParser.ID, 0); }
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public Abstraction_typeContext(TypeContext ctx) { copyFrom(ctx); }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof StlcListener ) ((StlcListener)listener).enterAbstraction_type(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof StlcListener ) ((StlcListener)listener).exitAbstraction_type(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof StlcVisitor ) return ((StlcVisitor<? extends T>)visitor).visitAbstraction_type(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TypeContext type() throws RecognitionException {
		TypeContext _localctx = new TypeContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_type);
		try {
			setState(43);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,2,_ctx) ) {
			case 1:
				_localctx = new Flat_typeContext(_localctx);
				enterOuterAlt(_localctx, 1);
				{
				setState(39);
				match(ID);
				}
				break;
			case 2:
				_localctx = new Abstraction_typeContext(_localctx);
				enterOuterAlt(_localctx, 2);
				{
				setState(40);
				match(ID);
				setState(41);
				match(T__10);
				setState(42);
				type();
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

	public boolean sempred(RuleContext _localctx, int ruleIndex, int predIndex) {
		switch (ruleIndex) {
		case 0:
			return t_sempred((TContext)_localctx, predIndex);
		}
		return true;
	}
	private boolean t_sempred(TContext _localctx, int predIndex) {
		switch (predIndex) {
		case 0:
			return precpred(_ctx, 6);
		}
		return true;
	}

	public static final String _serializedATN =
		"\u0004\u0001\r.\u0002\u0000\u0007\u0000\u0002\u0001\u0007\u0001\u0002"+
		"\u0002\u0007\u0002\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0001"+
		"\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0001"+
		"\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0001"+
		"\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0003"+
		"\u0000\u001d\b\u0000\u0001\u0000\u0001\u0000\u0005\u0000!\b\u0000\n\u0000"+
		"\f\u0000$\t\u0000\u0001\u0001\u0001\u0001\u0001\u0002\u0001\u0002\u0001"+
		"\u0002\u0001\u0002\u0003\u0002,\b\u0002\u0001\u0002\u0000\u0001\u0000"+
		"\u0003\u0000\u0002\u0004\u0000\u00001\u0000\u001c\u0001\u0000\u0000\u0000"+
		"\u0002%\u0001\u0000\u0000\u0000\u0004+\u0001\u0000\u0000\u0000\u0006\u0007"+
		"\u0006\u0000\uffff\uffff\u0000\u0007\u001d\u0003\u0002\u0001\u0000\b\t"+
		"\u0005\u0001\u0000\u0000\t\n\u0003\u0002\u0001\u0000\n\u000b\u0005\u0002"+
		"\u0000\u0000\u000b\f\u0003\u0004\u0002\u0000\f\r\u0005\u0003\u0000\u0000"+
		"\r\u000e\u0003\u0000\u0000\u0005\u000e\u001d\u0001\u0000\u0000\u0000\u000f"+
		"\u001d\u0005\u0004\u0000\u0000\u0010\u001d\u0005\u0005\u0000\u0000\u0011"+
		"\u0012\u0005\u0006\u0000\u0000\u0012\u0013\u0003\u0000\u0000\u0000\u0013"+
		"\u0014\u0005\u0007\u0000\u0000\u0014\u0015\u0003\u0000\u0000\u0000\u0015"+
		"\u0016\u0005\b\u0000\u0000\u0016\u0017\u0003\u0000\u0000\u0002\u0017\u001d"+
		"\u0001\u0000\u0000\u0000\u0018\u0019\u0005\t\u0000\u0000\u0019\u001a\u0003"+
		"\u0000\u0000\u0000\u001a\u001b\u0005\n\u0000\u0000\u001b\u001d\u0001\u0000"+
		"\u0000\u0000\u001c\u0006\u0001\u0000\u0000\u0000\u001c\b\u0001\u0000\u0000"+
		"\u0000\u001c\u000f\u0001\u0000\u0000\u0000\u001c\u0010\u0001\u0000\u0000"+
		"\u0000\u001c\u0011\u0001\u0000\u0000\u0000\u001c\u0018\u0001\u0000\u0000"+
		"\u0000\u001d\"\u0001\u0000\u0000\u0000\u001e\u001f\n\u0006\u0000\u0000"+
		"\u001f!\u0003\u0000\u0000\u0007 \u001e\u0001\u0000\u0000\u0000!$\u0001"+
		"\u0000\u0000\u0000\" \u0001\u0000\u0000\u0000\"#\u0001\u0000\u0000\u0000"+
		"#\u0001\u0001\u0000\u0000\u0000$\"\u0001\u0000\u0000\u0000%&\u0005\f\u0000"+
		"\u0000&\u0003\u0001\u0000\u0000\u0000\',\u0005\f\u0000\u0000()\u0005\f"+
		"\u0000\u0000)*\u0005\u000b\u0000\u0000*,\u0003\u0004\u0002\u0000+\'\u0001"+
		"\u0000\u0000\u0000+(\u0001\u0000\u0000\u0000,\u0005\u0001\u0000\u0000"+
		"\u0000\u0003\u001c\"+";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}