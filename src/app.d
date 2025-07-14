import std.stdio;
import std.variant;
import std.ascii;
import std.file;
import std.conv;

/*
	LEXER
*/

enum TokenType
{
	// KEYWORDS
	IF,
	THEN,
	ELSE,
	PRINT, // Diminui o numero de funções, otimizando o interpretador

	// NUMERICOS
	INT, // 3
	FLOAT, // 3.1415

	// IDENTIFICADOR
	ID, // abcdefghiklmnopqrstuvwxyz

	// SIMBOLOS
	EQUALS, // =
	LPAREN, // (
	RPAREN, // )
	MINUS, // -
	PLUS, // +
	ASTERISK, // *
	SLASH, // /
	SEMICOLON, // ;

	EOF, // EndOfFile
}

// Vou usar classes pra tudo para evitar copias

class Loc
{
protected:
	ulong line;
	ulong start;
	ulong end;
public:
	this(ulong line, ulong start, ulong end)
	{
		this.line = line;
		this.start = start;
		this.end = end;
	}
}

class Token
{
protected:
	TokenType kind;
	Variant value;
	Loc location;
public:
	this(
		TokenType kind,
		Variant value,
		Loc location
	)
	{
		this.kind = kind;
		this.value = value;
		this.location = location;
	}

	void print()
	{
		writeln("Kind: ", this.kind);
		writeln("Value: ", this.value);
		writeln("Location");
		writeln(" ⊦ Line number: ", this.location.line);
		writeln(" ⊦ Start: ", this.location.start);
		writeln(" ⌞ End: ", this.location.end, "\n");
	}
}

class Lexer
{
protected:
	string source;
	ulong offset = 0;
	ulong line = 1;
	ulong lineOffset = 0; // Relativo a linha
	static immutable TokenType[string] keywords = initKeywords();
	static immutable TokenType[string] singleChars = initSingleChars();
	Token[] tokens = [];
private:
	static TokenType[string] initKeywords()
	{
		TokenType[string] map = null;
		map["if"] = TokenType.IF;
		map["then"] = TokenType.THEN;
		map["else"] = TokenType.ELSE;
		map["print"] = TokenType.PRINT;
		return map;
	}

	static TokenType[string] initSingleChars()
	{
		TokenType[string] map = null;
		map["-"] = TokenType.MINUS;
		map["+"] = TokenType.PLUS;
		map["*"] = TokenType.ASTERISK;
		map["="] = TokenType.EQUALS;
		map["/"] = TokenType.SLASH;
		map[")"] = TokenType.RPAREN;
		map["("] = TokenType.LPAREN;
		return map;
	}

	bool isAtEnd()
	{
		return this.offset >= this.source.length;
	}

	char next(bool lineOffset = true)
	{
		if (this.isAtEnd())
			return '\0';
		if (lineOffset)
			this.lineOffset++;
		return this.source[this.offset++];
	}

	char peek()
	{
		if (this.isAtEnd())
			return '\0';
		return this.source[this.offset];
	}

	Token createToken(TokenType kind, Variant value, ulong start)
	{
		Token token = new Token(kind, value, new Loc(this.line, start, this.lineOffset));
		this.tokens ~= token;
		return token;
	}

	void lexAlpha()
	{
		ulong start = this.lineOffset;
		string buffer;
		while (!this.isAtEnd() && isAlpha(this.peek()))
		{
			buffer ~= this.next();
		}
		Variant val = Variant(buffer);
		if (buffer in this.keywords)
		{
			this.createToken(this.keywords[buffer], val, start);
		}
		else
		{
			this.createToken(TokenType.ID, val, start);
		}
	}

	void lexDigit()
	{
		ulong start = this.lineOffset;
		string buffer;
		while (!this.isAtEnd() && isDigit(this.peek()))
		{
			buffer ~= this.next();
		}

		Variant val = Variant(buffer);
		this.createToken(TokenType.INT, val, start);
	}

public:
	this(string source = "")
	{
		this.source = source;
	}

	Token[] tokenize()
	{
		try
		{
			while (!this.isAtEnd())
			{
				char ch = this.peek();

				if (ch == '\n')
				{
					this.line++;
					this.next(false);
					this.lineOffset = 0;
					continue;
				}

				if (isWhite(ch))
				{
					this.next();
					continue;
				}

				string single = to!string(ch);
				if (single in this.singleChars)
				{
					Variant val = Variant(single);
					this.createToken(this.singleChars[single], val, this.lineOffset);
					this.next();
					continue;
				}

				if (isAlpha(ch))
				{
					this.lexAlpha();
					continue;
				}

				if (isDigit(ch))
				{
					this.lexDigit();
					continue;
				}

				throw new Exception("Char desconhecido:", to!string(ch));
			}
		}
		catch (Exception e)
		{
			writeln("Lexer error:", e);
		}
		this.createToken(TokenType.EOF, Variant("\0"), this.lineOffset);
		return this.tokens;
	}
}

/*
	PARSER
*/

enum NodeType
{
	Program, // entry
	IntLiteral,
	FloatLiteral,
	BinaryExpr,
	VarAssignment,
	Identifier,
	UnaryExpr,
}

class Node
{
	NodeType kind;
	Variant value;
	string type;
	Loc loc;

	// BinaryExpr
	Node left;
	Node right;
	string op;

	// Método print base com indentação
	void print(int indent = 0)
	{
		string indentStr = "";
		for (int i = 0; i < indent; i++)
		{
			indentStr ~= "  ";
		}

		writeln(indentStr ~ "Node {");
		writeln(indentStr ~ "  kind: " ~ to!string(this.kind));
		if (!this.value.peek!(typeof(null)))
		{
			writeln(indentStr ~ "  value: " ~ to!string(this.value));
		}
		if (this.type != "")
		{
			writeln(indentStr ~ "  type: " ~ this.type);
		}
		if (this.loc !is null)
		{
			writeln(indentStr ~ "  location: line " ~ to!string(
					this.loc.line) ~
					", start " ~ to!string(
						this.loc.start) ~
					", end " ~ to!string(this.loc.end));
		}
		writeln(indentStr ~ "}");
	}
}

class Program : Node
{
	Node[] body;

	this(Node[] body)
	{
		this.kind = NodeType.Program;
		this.body = body;
	}

	override void print(int indent = 0)
	{
		string indentStr = "";
		for (int i = 0; i < indent; i++)
		{
			indentStr ~= "  ";
		}

		writeln(indentStr ~ "Program {");
		writeln(indentStr ~ "  kind: " ~ to!string(this.kind));
		writeln(indentStr ~ "  body: [");

		foreach (size_t i, Node node; this.body)
		{
			if (node !is null)
			{
				writeln(indentStr ~ "    [" ~ to!string(i) ~ "]:");
				node.print(indent + 3);
			}
			else
			{
				writeln(indentStr ~ "    [" ~ to!string(i) ~ "]: null");
			}
		}

		writeln(indentStr ~ "  ]");
		writeln(indentStr ~ "}");
	}
}

class BinaryExpr : Node
{
	this(Node left, Node right, string op)
	{
		this.kind = NodeType.BinaryExpr;
		this.left = left;
		this.right = right;
		this.op = op;
	}

	override void print(int indent = 0)
	{
		string indentStr = "";
		for (int i = 0; i < indent; i++)
		{
			indentStr ~= "  ";
		}

		writeln(indentStr ~ "BinaryExpr {");
		writeln(indentStr ~ "  kind: " ~ to!string(this.kind));
		writeln(indentStr ~ "  operator: \"" ~ this.op ~ "\"");

		writeln(indentStr ~ "  left:");
		if (this.left !is null)
		{
			this.left.print(indent + 2);
		}
		else
		{
			writeln(indentStr ~ "    null");
		}

		writeln(indentStr ~ "  right:");
		if (this.right !is null)
		{
			this.right.print(indent + 2);
		}
		else
		{
			writeln(indentStr ~ "    null");
		}

		writeln(indentStr ~ "}");
	}
}

class UnaryExpr : Node
{
private:
	string operator;
	Node operand;
public:
	this(string operator, Node operand)
	{
		this.kind = NodeType.UnaryExpr;
		this.operator = operator;
		this.operand = operand;
		this.op = op;
		this.loc = operand.loc;
	}

	override void print(int indent = 0)
	{
		string indentStr = "";
		for (int i = 0; i < indent; i++)
		{
			indentStr ~= "  ";
		}

		writeln(indentStr ~ "UnaryExpr {");
		writeln(indentStr ~ "  kind: " ~ to!string(this.kind));
		writeln(indentStr ~ "  operator: \"" ~ this.op ~ "\"");

		writeln(indentStr ~ "  operand:");
		if (this.operand !is null)
		{
			this.operand.print(indent + 2);
		}
		else
		{
			writeln(indentStr ~ "    null");
		}

		writeln(indentStr ~ "}");
	}
}

class IntLiteral : Node
{
	this(int value, Loc loc)
	{
		this.kind = NodeType.IntLiteral;
		this.type = "int";
		this.value = value;
		this.loc = loc;
	}

	override void print(int indent = 0)
	{
		string indentStr = "";
		for (int i = 0; i < indent; i++)
		{
			indentStr ~= "  ";
		}

		writeln(indentStr ~ "IntLiteral {");
		writeln(indentStr ~ "  kind: " ~ to!string(this.kind));
		writeln(indentStr ~ "  type: " ~ this.type);
		writeln(indentStr ~ "  value: " ~ to!string(this.value));

		if (this.loc !is null)
		{
			writeln(indentStr ~ "  location: {");
			writeln(indentStr ~ "    line: " ~ to!string(this.loc.line));
			writeln(indentStr ~ "    start: " ~ to!string(this.loc.start));
			writeln(indentStr ~ "    end: " ~ to!string(this.loc.end));
			writeln(indentStr ~ "  }");
		}

		writeln(indentStr ~ "}");
	}
}

class Identifier : Node
{
	this(string value, Loc loc)
	{
		this.kind = NodeType.Identifier;
		this.type = "id";
		this.value = value;
		this.loc = loc;
	}

	override void print(int indent = 0)
	{
		string indentStr = "";
		for (int i = 0; i < indent; i++)
		{
			indentStr ~= "  ";
		}

		writeln(indentStr ~ "Identifier {");
		writeln(indentStr ~ "  kind: " ~ to!string(this.kind));
		writeln(indentStr ~ "  type: " ~ this.type);
		writeln(indentStr ~ "  value: \"" ~ to!string(this.value) ~ "\"");

		if (this.loc !is null)
		{
			writeln(indentStr ~ "  location: {");
			writeln(indentStr ~ "    line: " ~ to!string(this.loc.line));
			writeln(indentStr ~ "    start: " ~ to!string(this.loc.start));
			writeln(indentStr ~ "    end: " ~ to!string(this.loc.end));
			writeln(indentStr ~ "  }");
		}

		writeln(indentStr ~ "}");
	}
}

class VarAssignment : Node
{
	this(Node value, string type)
	{
		this.kind = NodeType.VarAssignment;
		this.type = type;
		this.value = value;
		this.loc = value.loc;
	}

	override void print(int indent = 0)
	{
		string indentStr = "";
		for (int i = 0; i < indent; i++)
		{
			indentStr ~= "  ";
		}

		writeln(indentStr ~ "VarAssignment {");
		writeln(indentStr ~ "  kind: " ~ to!string(this.kind));
		writeln(indentStr ~ "  type: " ~ this.type);
		writeln(indentStr ~ "  value: \"" ~ to!string(this.value) ~ "\"");

		if (this.loc !is null)
		{
			writeln(indentStr ~ "  location: {");
			writeln(indentStr ~ "    line: " ~ to!string(this.loc.line));
			writeln(indentStr ~ "    start: " ~ to!string(this.loc.start));
			writeln(indentStr ~ "    end: " ~ to!string(this.loc.end));
			writeln(indentStr ~ "  }");
		}

		writeln(indentStr ~ "}");
	}
}

enum Precedence
{
	LOWEST = 1,
	ASSIGN = 2, // =
	SUM = 3, // + -
	PRODUCT = 4, // * / %
	PREFIX = 5, // -x
	CALL = 6, // myFunction(x)
}

class Parser
{
private:
	Token[] tokens;
	size_t pos = 0; // offset

	Node parsePrefix()
	{
		Token token = this.advance();

		switch (token.kind)
		{
		case TokenType.INT:
			return new IntLiteral(to!int(token.value.get!string), token.location);
		case TokenType.ID:
			if (this.tokens[this.pos + 1].kind == TokenType.EQUALS)
			{
				this.consume(TokenType.EQUALS, "Expected '=' after ID.");
				Node expr = this.parseExpression(Precedence.ASSIGN);
				return new VarAssignment(expr, "int");
			}
			return new Identifier(token.value.get!string, token.location);
		case TokenType.LPAREN:
			Node expr = this.parseExpression(Precedence.LOWEST);
			this.consume(TokenType.RPAREN, "Expected ')' after '(' 'EXPR'.");
			return expr;
		case TokenType.MINUS:
			Node expr = this.parseExpression(Precedence.LOWEST);
			return new UnaryExpr("-", expr);
		case TokenType.PLUS:
			Node expr = this.parseExpression(Precedence.LOWEST);
			return new UnaryExpr("+", expr);
		default:
			throw new Exception("No prefix parse function for " ~ to!string(token.value));
		}
	}

	Node parseBinaryInfix(Node left)
	{
		this.advance();
		Token operatorToken = this.previous();

		Precedence precedence = this.getPrecedence(operatorToken.kind);
		Node right = this.parseExpression(precedence);
		string type = this.inferType(left, right);

		BinaryExpr node = new BinaryExpr(left, right, operatorToken.value.get!string);
		node.type = type;
		node.loc = this.makeLoc(left.loc, right.loc);
		return node;
	}

	void infix(ref Node leftOld)
	{
		switch (this.peek().kind)
		{
		case TokenType.PLUS:
		case TokenType.MINUS:
		case TokenType.SLASH:
		case TokenType.ASTERISK:
			leftOld = this.parseBinaryInfix(leftOld);
			return;
		default:
			return;
		}
	}

	Node parseExpression(Precedence precedence)
	{
		Node left = this.parsePrefix();

		while (!this.isAtEnd() && precedence < this.peekPrecedence())
		{
			this.infix(left);
		}

		return left;
	}

	Node parseNode()
	{
		Node Node = this.parseExpression(Precedence.LOWEST);
		this.match([TokenType.SEMICOLON]);
		return Node;
	}

public:
	this(Token[] tokens = [])
	{
		this.tokens = tokens;
	}

	Program parse()
	{
		Program program = new Program([]);
		program.type = "null";
		program.value = null;

		try
		{
			while (!this.isAtEnd())
			{
				program.body ~= this.parseNode();
			}

			if (this.tokens.length == 0)
			{
				return program;
			}
		}
		catch (Exception e)
		{

			writeln("Erro:", e.msg);
			throw e;
		}

		return program;
	}

	// Helpers
private:
	bool isAtEnd()
	{
		return this.peek().kind == TokenType.EOF;
	}

	Variant next()
	{
		if (this.isAtEnd())
			return Variant(false);
		return Variant(this.tokens[this.pos + 1]);
	}

	Token peek()
	{
		return this.tokens[this.pos];
	}

	Token previous(size_t i = 1)
	{
		return this.tokens[this.pos - i];
	}

	Token advance()
	{
		if (!this.isAtEnd())
			this.pos++;
		return this.previous();
	}

	bool match(TokenType[] kinds)
	{
		foreach (kind; kinds)
		{
			if (this.check(kind))
			{
				this.advance();
				return true;
			}
		}
		return false;
	}

	bool check(TokenType kind)
	{
		if (this.isAtEnd())
			return false;
		return this.peek().kind == kind;
	}

	Token consume(TokenType expected, string message)
	{
		if (this.check(expected))
			return this.advance();
		const token = this.peek();
		throw new Exception("Erro de parsing: " ~ message);
	}

	Precedence getPrecedence(TokenType kind)
	{
		switch (kind)
		{
		case TokenType.EQUALS:
			return Precedence.ASSIGN;
		case TokenType.PLUS:
		case TokenType.MINUS:
			return Precedence.SUM;
		case TokenType.SLASH:
		case TokenType.ASTERISK:
			return Precedence.PRODUCT;
		case TokenType.LPAREN:
			return Precedence.CALL;
		default:
			return Precedence.LOWEST;
		}
	}

	Precedence peekPrecedence()
	{
		return this.getPrecedence(this.peek().kind);
	}

	string inferType(Node left, Node right)
	{
		if (left.type == "float" || right.type == "float")
		{
			return "float";
		}
		return "int";
	}

	Loc makeLoc(ref Loc start, ref Loc end)
	{
		return new Loc(start.line, start.start, end.end);
	}
}

/*
	CONTEXT
*/

class Context
{

}

/*
	RUNTIME
*/

class RuntimeValue
{
	string type;
	Variant value;
	Loc location;
}

class IntValue : RuntimeValue
{
	this(int value, Loc loc)
	{
		this.value = Variant(value);
		this.location = loc;
	}
}

class Runtime
{
private:
	RuntimeValue evaluateProgram(Program program)
	{
		RuntimeValue lastEval;
		foreach (node; program.body)
		{
			lastEval = this.evaluate(node);
		}

		writeln(lastEval.value);
		return lastEval;
	}

	RuntimeValue evaluateBinaryExpr(BinaryExpr binaryExpr)
	{
		RuntimeValue left = this.evaluate(binaryExpr.left);
		RuntimeValue right = this.evaluate(binaryExpr.right);

		switch (binaryExpr.op)
		{
		case "+":
			int result = left.value.get!int + right.value.get!int;
			return new IntValue(result, binaryExpr.loc);
		case "-":
			int result = left.value.get!int - right.value.get!int;
			return new IntValue(result, binaryExpr.loc);
		case "*":
			int result = left.value.get!int * right.value.get!int;
			return new IntValue(result, binaryExpr.loc);
		case "/":
			int result = left.value.get!int / right.value.get!int;
			return new IntValue(result, binaryExpr.loc);
		default:
			break;
		}

		return new IntValue(-1, binaryExpr.loc);
	}

	RuntimeValue evaluateUnaryExpr(UnaryExpr unaryExpr)
	{
		switch (unaryExpr.operator)
		{
		case "-":
			auto value = this.evaluate(unaryExpr.operand);
			value.value = Variant(-value.value.get!int);
			return value;
		case "+":
			auto value = this.evaluate(unaryExpr.operand);
			value.value = Variant(+value.value.get!int);
			return value;
		default:
			break;
		}
		return new IntValue(-3, unaryExpr.loc);
	}

public:
	RuntimeValue evaluate(Node node)
	{
		switch (node.kind)
		{
		case NodeType.Program:
			return this.evaluateProgram(cast(Program) node);
		case NodeType.IntLiteral:
			return new IntValue(node.value.get!int, node.loc);
		case NodeType.BinaryExpr:
			return this.evaluateBinaryExpr(cast(BinaryExpr) node);
		case NodeType.UnaryExpr:
			return this.evaluateUnaryExpr(cast(UnaryExpr) node);
		default:
			break;
		}
		return new IntValue(-2, node.loc);
	}
}

/*
	REPL-CLI
*/

/*
	BOOTSTRAP
*/

void main(string[] args)
{
	if (args.length < 2)
	{
		writeln("Error: você deve fornecer um arquivo '.calc' como argumento.");
		return;
	}

	string fileName = args[1];

	if (!exists(fileName))
	{
		writeln("Error: o arquivo não existe.");
		return;
	}

	if (!isFile(fileName))
	{
		writeln("Error: você deve fornecer um arquivo válido.");
		return;
	}

	string content = readText(fileName);
	Lexer lexer = new Lexer(content);
	Token[] tokens = lexer.tokenize();

	// foreach (Token token; tokens)
	// {
	// 	token.print();
	// }

	Parser parser = new Parser(tokens);
	Program program = parser.parse();

	// program.print();

	Runtime runtime = new Runtime();
	runtime.evaluateProgram(program);
}
