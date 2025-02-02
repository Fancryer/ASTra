// Generated from A:/Downloads/KAST/src/main/resources/Lisp.g4 by ANTLR 4.13.2
package gen;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast", "CheckReturnValue", "this-escape"})
public class LispLexer extends Lexer {
	static { RuntimeMetaData.checkVersion("4.13.2", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, Int=4, Id=5, WS=6, Comment=7, LineComment=8;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	private static String[] makeRuleNames() {
		return new String[] {
			"T__0", "T__1", "T__2", "Int", "Id", "ATOMIC", "LETTER", "DIGIT", "OP", 
			"WS", "Comment", "LineComment"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'('", "'.'", "')'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, null, null, null, "Int", "Id", "WS", "Comment", "LineComment"
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


	public LispLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "Lisp.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getChannelNames() { return channelNames; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public static final String _serializedATN =
		"\u0004\u0000\bR\u0006\uffff\uffff\u0002\u0000\u0007\u0000\u0002\u0001"+
		"\u0007\u0001\u0002\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002\u0004"+
		"\u0007\u0004\u0002\u0005\u0007\u0005\u0002\u0006\u0007\u0006\u0002\u0007"+
		"\u0007\u0007\u0002\b\u0007\b\u0002\t\u0007\t\u0002\n\u0007\n\u0002\u000b"+
		"\u0007\u000b\u0001\u0000\u0001\u0000\u0001\u0001\u0001\u0001\u0001\u0002"+
		"\u0001\u0002\u0001\u0003\u0004\u0003!\b\u0003\u000b\u0003\f\u0003\"\u0001"+
		"\u0004\u0004\u0004&\b\u0004\u000b\u0004\f\u0004\'\u0001\u0005\u0001\u0005"+
		"\u0001\u0005\u0003\u0005-\b\u0005\u0001\u0006\u0001\u0006\u0001\u0007"+
		"\u0001\u0007\u0001\b\u0001\b\u0001\t\u0004\t6\b\t\u000b\t\f\t7\u0001\t"+
		"\u0001\t\u0001\n\u0001\n\u0001\n\u0001\n\u0005\n@\b\n\n\n\f\nC\t\n\u0001"+
		"\n\u0001\n\u0001\n\u0001\n\u0001\n\u0001\u000b\u0001\u000b\u0005\u000b"+
		"L\b\u000b\n\u000b\f\u000bO\t\u000b\u0001\u000b\u0001\u000b\u0001A\u0000"+
		"\f\u0001\u0001\u0003\u0002\u0005\u0003\u0007\u0004\t\u0005\u000b\u0000"+
		"\r\u0000\u000f\u0000\u0011\u0000\u0013\u0006\u0015\u0007\u0017\b\u0001"+
		"\u0000\u0005\u0001\u0000az\u0001\u000009\u0005\u0000%%*+--//<>\u0003\u0000"+
		"\t\n\r\r  \u0002\u0000\n\n\r\rT\u0000\u0001\u0001\u0000\u0000\u0000\u0000"+
		"\u0003\u0001\u0000\u0000\u0000\u0000\u0005\u0001\u0000\u0000\u0000\u0000"+
		"\u0007\u0001\u0000\u0000\u0000\u0000\t\u0001\u0000\u0000\u0000\u0000\u0013"+
		"\u0001\u0000\u0000\u0000\u0000\u0015\u0001\u0000\u0000\u0000\u0000\u0017"+
		"\u0001\u0000\u0000\u0000\u0001\u0019\u0001\u0000\u0000\u0000\u0003\u001b"+
		"\u0001\u0000\u0000\u0000\u0005\u001d\u0001\u0000\u0000\u0000\u0007 \u0001"+
		"\u0000\u0000\u0000\t%\u0001\u0000\u0000\u0000\u000b,\u0001\u0000\u0000"+
		"\u0000\r.\u0001\u0000\u0000\u0000\u000f0\u0001\u0000\u0000\u0000\u0011"+
		"2\u0001\u0000\u0000\u0000\u00135\u0001\u0000\u0000\u0000\u0015;\u0001"+
		"\u0000\u0000\u0000\u0017I\u0001\u0000\u0000\u0000\u0019\u001a\u0005(\u0000"+
		"\u0000\u001a\u0002\u0001\u0000\u0000\u0000\u001b\u001c\u0005.\u0000\u0000"+
		"\u001c\u0004\u0001\u0000\u0000\u0000\u001d\u001e\u0005)\u0000\u0000\u001e"+
		"\u0006\u0001\u0000\u0000\u0000\u001f!\u0003\u000f\u0007\u0000 \u001f\u0001"+
		"\u0000\u0000\u0000!\"\u0001\u0000\u0000\u0000\" \u0001\u0000\u0000\u0000"+
		"\"#\u0001\u0000\u0000\u0000#\b\u0001\u0000\u0000\u0000$&\u0003\u000b\u0005"+
		"\u0000%$\u0001\u0000\u0000\u0000&\'\u0001\u0000\u0000\u0000\'%\u0001\u0000"+
		"\u0000\u0000\'(\u0001\u0000\u0000\u0000(\n\u0001\u0000\u0000\u0000)-\u0003"+
		"\r\u0006\u0000*-\u0003\u000f\u0007\u0000+-\u0003\u0011\b\u0000,)\u0001"+
		"\u0000\u0000\u0000,*\u0001\u0000\u0000\u0000,+\u0001\u0000\u0000\u0000"+
		"-\f\u0001\u0000\u0000\u0000./\u0007\u0000\u0000\u0000/\u000e\u0001\u0000"+
		"\u0000\u000001\u0007\u0001\u0000\u00001\u0010\u0001\u0000\u0000\u0000"+
		"23\u0007\u0002\u0000\u00003\u0012\u0001\u0000\u0000\u000046\u0007\u0003"+
		"\u0000\u000054\u0001\u0000\u0000\u000067\u0001\u0000\u0000\u000075\u0001"+
		"\u0000\u0000\u000078\u0001\u0000\u0000\u000089\u0001\u0000\u0000\u0000"+
		"9:\u0006\t\u0000\u0000:\u0014\u0001\u0000\u0000\u0000;<\u0005;\u0000\u0000"+
		"<=\u0005*\u0000\u0000=A\u0001\u0000\u0000\u0000>@\t\u0000\u0000\u0000"+
		"?>\u0001\u0000\u0000\u0000@C\u0001\u0000\u0000\u0000AB\u0001\u0000\u0000"+
		"\u0000A?\u0001\u0000\u0000\u0000BD\u0001\u0000\u0000\u0000CA\u0001\u0000"+
		"\u0000\u0000DE\u0005*\u0000\u0000EF\u0005;\u0000\u0000FG\u0001\u0000\u0000"+
		"\u0000GH\u0006\n\u0000\u0000H\u0016\u0001\u0000\u0000\u0000IM\u0005;\u0000"+
		"\u0000JL\b\u0004\u0000\u0000KJ\u0001\u0000\u0000\u0000LO\u0001\u0000\u0000"+
		"\u0000MK\u0001\u0000\u0000\u0000MN\u0001\u0000\u0000\u0000NP\u0001\u0000"+
		"\u0000\u0000OM\u0001\u0000\u0000\u0000PQ\u0006\u000b\u0000\u0000Q\u0018"+
		"\u0001\u0000\u0000\u0000\u0007\u0000\"\',7AM\u0001\u0006\u0000\u0000";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}