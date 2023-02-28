from stringWithArrows import *

''''
    CONSTANTS
'''

DIGITS = '0123456789'

''''
    ERRORS
'''

class Error:
    def __init__(self, posStart, posEnd, errorName, details):
        self.PosStart = posStart
        self.PosEnd = posEnd
        self.ErrorName = errorName
        self.Details = details

    def AsString(self):
        result = f'{self.ErrorName}: {self.Details}\n'
        result += f'File {self.PosStart.FileName}, line {self.PosStart.Line + 1}'
        result += '\n\n' + StringWithArrows(self.PosStart.FileTxt, self.PosStart, self.PosEnd)
        return result

class IllegalCharError(Error):
    def __init__(self, posStart, posEnd, details):
        super().__init__(posStart, posEnd, 'Illegal Character', details)

class InvalidSyntaxError(Error):
    def __init__(self, posStart, posEnd, details=''):
        super().__init__(posStart, posEnd, 'Invalid Syntax', details)

'''
    POSITION
'''

class Position:
    def __init__(self, index, line, column, fileName, fileTxt):
        self.Index = index
        self.Line = line
        self.Column = column
        self.FileName = fileName
        self.FileTxt = fileTxt

    def Advance(self, currentChar=None):
        self.Index += 1
        self.Column += 1

        if currentChar == '\n':
            self.Line += 1
            self.Column = 0
        
        return self
    
    def Copy(self):
        return Position(self.Index, self.Line, self.Column, self.FileName, self.FileTxt)

''''
    TOKENS
'''

TT_INT = 'INT'
TT_FLOAT = 'FLOAT'
TT_PLUS = 'PLUS'
TT_MINUS = 'MINUS'
TT_MUL = 'MUL'
TT_DIV = 'DIV'
TT_LPAREN = 'LPAREN'
TT_RPAREN = 'RPAREN'
TT_EOF = 'EOF'

class Token:
    def __init__(self, type, value=None, posStart=None, posEnd=None):
        self.Type = type
        self.Value = value

        if posStart:
            self.PosStart = posStart.Copy()
            self.PosEnd = posStart.Copy()
            self.PosEnd.Advance()

        if posEnd: self.PosEnd = posEnd

    def __repr__(self):
        if self.Value: return f'{self.Type}:{self.Value}'
        return f'{self.Type}'

''''
    LEXER
'''
 
class Lexer:

    def __init__ (self, fileName ,text):
        self.FileName = fileName
        self.Text = text
        self.Position = Position(-1, 0, -1, fileName, text)
        self.CurrentChar = None
        self.Advance()

    def Advance(self):
        self.Position.Advance(self.CurrentChar)
        self.CurrentChar = self.Text[self.Position.Index] if self.Position.Index < len(self.Text) else None

    def MakeTokens(self):
        tokens = []
        
        while self.CurrentChar != None:
            if self.CurrentChar in ' \t': 
                self.Advance()

            elif self.CurrentChar in DIGITS:
                tokens.append(self.MakeNumber())

            elif self.CurrentChar == '+':
                tokens.append(Token(TT_PLUS, posStart=self.Position))
                self.Advance()

            elif self.CurrentChar == '-':
                tokens.append(Token(TT_MINUS, posStart=self.Position))
                self.Advance()

            elif self.CurrentChar == '*':
                tokens.append(Token(TT_MUL, posStart=self.Position))
                self.Advance()

            elif self.CurrentChar == '/':
                tokens.append(Token(TT_DIV, posStart=self.Position))
                self.Advance()

            elif self.CurrentChar == '(':
                tokens.append(Token(TT_LPAREN, posStart=self.Position))
                self.Advance()

            elif self.CurrentChar == ')':
                tokens.append(Token(TT_RPAREN, posStart=self.Position))
                self.Advance()
            
            else: 
                posStart = self.Position.Copy()
                char = self.CurrentChar
                self.Advance()
                return [], IllegalCharError(posStart, self.Position, "'" + char + "'")

        tokens.append(Token(TT_EOF, posStart=self.Position))
        return tokens, None

    def MakeNumber(self):
        NumStr = ''
        dotCount = 0
        posStart = self.Position.Copy()

        while self.CurrentChar != None and self.CurrentChar in DIGITS + '.':
            if self.CurrentChar == '.':
                if dotCount == 1: break
                dotCount += 1
                NumStr += '.'
            else: NumStr += self.CurrentChar
            self.Advance()

        if dotCount == 0: return Token(TT_INT, int(NumStr), posStart, self.Position)

        else: return Token(TT_FLOAT, float(NumStr), posStart, self.Position)

'''
    NODES
'''

class NumberNode:

    def __init__(self, token):
        self.Token = token

    def __repr__(self):
        return f'{self.Token}'

class BinOperatorNode:

    def __init__(self, leftNode, operatorToken, rightNode):
        self.LeftNode = leftNode
        self.OperatorToken = operatorToken
        self.RightNode = rightNode

    def __repr__(self):
        return f'({self.LeftNode}, {self.OperatorToken}, {self.RightNode})'

class UnaryOperatorNode:

    def __init__(self, operatorToken, node):
        self.OperatorToken = operatorToken
        self.Node = node

    def __repr__(self):
        return f'({self.OperatorToken}, {self.Node})'

'''
    PARSE RESULT
'''

class ParseResult:

    def __init__(self):
        self.Error = None
        self.Node = None

    def Register(self, result):
        if isinstance(result, ParseResult):
            if result.Error: self.Error = result.Error
            return result.Node
        return result

    def Success(self, node):
        self.Node = node
        return self

    def Failure(self, error):
        self.Error = error
        return self

'''
    PARSER
'''

class Parser:

    def __init__(self, tokens):
        self.Tokens = tokens
        self.TokenIndex = -1
        self.Advance()

    def Advance(self, ):
        self.TokenIndex += 1
        if self.TokenIndex < len(self.Tokens):
            self.CurrentToken = self.Tokens[self.TokenIndex]
        return self.CurrentToken
    
    def Parse(self):
        result = self.Expr()
        if not result.Error and self.CurrentToken.Type != TT_EOF:
            return result.Failure(InvalidSyntaxError(
                self.CurrentToken.PosStart, self.CurrentToken.PosEnd, "Expected '+', '-', '*' or '/'"
            ))
        return result

    def Factor(self):
        result = ParseResult()
        token = self.CurrentToken

        if token.Type in (TT_PLUS, TT_MINUS):
            result.Register(self.Advance())
            factor = result.Register(self.Factor())
            if result.Error: return result
            return result.Success(UnaryOperatorNode(token, factor))
		
        elif token.Type in (TT_INT, TT_FLOAT):
            result.Register(self.Advance())
            return result.Success(NumberNode(token))

        elif token.Type == TT_LPAREN:
            result.Register(self.Advance())
            expr = result.Register(self.Expr())
            if result.Error: return result
            if self.CurrentToken.Type == TT_RPAREN:
                result.Register(self.Advance())
                return result.Success(expr)
            else:
                return result.Failure(InvalidSyntaxError(
                    self.CurrentToken.PosStart, self.CurrentToken.PosEnd,
                    "Expected ')'"
                ))

        return result.Failure(InvalidSyntaxError(
            token.PosStart, token.PosEnd,
            "Expected int or float"
        ))

    def Term(self):
        return self.BinOperator(self.Factor, (TT_MUL, TT_DIV))

    def Expr(self):
        return self.BinOperator(self.Term, (TT_PLUS, TT_MINUS))

    def BinOperator(self, function, ops):
        result = ParseResult()
        left = result.Register(function())

        if result.Error: return result

        while self.CurrentToken.Type in ops:
            operatorToken = self.CurrentToken
            result.Register(self.Advance())
            right = result.Register(function())

            if result.Error: return result

            left = BinOperatorNode(left, operatorToken, right)

        return result.Success(left)

'''
    RUN
'''

def Run(fileName, text):
    lexer = Lexer(fileName, text)
    tokens, error = lexer.MakeTokens()
    if error: return None, error

    #generate AST
    parser= Parser(tokens)
    ast = parser.Parse()

    return ast.Node, ast.Error

