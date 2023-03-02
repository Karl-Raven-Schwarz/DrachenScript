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

class RunTimeError(Error):
    def __init__(self, posStart, posEnd, details, context):
        super().__init__(posStart, posEnd, 'Run Time Error', details)
        self.Context = context

    def AsString(self):
        result = self.GenerateTraceback()
        result += f'File {self.ErrorName}, {self.Details}'
        result += '\n\n' + StringWithArrows(self.PosStart.FileTxt, self.PosStart, self.PosEnd)
        return result

    def GenerateTraceback(self):
        result = ''
        position = self.PosStart
        context = self.Context

        while context:
            result = f' File {position.FileName}, line {str(position.Line + 1)}, in {context.DisplayName} \n' + result
            position = context.ParentEntryPosition
            context = context.Parent
        
        return 'Traceback (most recent call last):\n' + result

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
TT_POW = 'POW'
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

            elif self.CurrentChar == '^':
                tokens.append(Token(TT_POW, posStart=self.Position))
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

        self.PosStart = self.Token.PosStart
        self.PosEnd = self.Token.PosEnd

    def __repr__(self):
        return f'{self.Token}'

class BinOperatorNode:

    def __init__(self, leftNode, operatorToken, rightNode):
        self.LeftNode = leftNode
        self.OperatorToken = operatorToken
        self.RightNode = rightNode

        self.PosStart = self.LeftNode.PosStart
        self.PosEnd = self.RightNode.PosEnd

    def __repr__(self):
        return f'({self.LeftNode}, {self.OperatorToken}, {self.RightNode})'

class UnaryOperatorNode:

    def __init__(self, operatorToken, node):
        self.OperatorToken = operatorToken
        self.Node = node

        self.PosStart = self.OperatorToken.PosStart
        self.PosEnd = node.PosEnd

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

    def Atom(self):
        result = ParseResult()
        token = self.CurrentToken

        if token.Type in (TT_INT, TT_FLOAT):
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
            "Expected int or float, '+', '-', or '('"
        ))

    def Power(self):
        return self.BinOperator(self.Atom, (TT_POW, ), self.Factor)

    def Factor(self):
        result = ParseResult()
        token = self.CurrentToken

        if token.Type in (TT_PLUS, TT_MINUS):
            result.Register(self.Advance())
            factor = result.Register(self.Factor())
            if result.Error: return result
            return result.Success(UnaryOperatorNode(token, factor))

        return self.Power()

    def Term(self):
        return self.BinOperator(self.Factor, (TT_MUL, TT_DIV))

    def Expr(self):
        return self.BinOperator(self.Term, (TT_PLUS, TT_MINUS))

    def BinOperator(self, functionOne, ops, functionTwo=None):
        if functionTwo == None: functionTwo = functionOne
        result = ParseResult()
        left = result.Register(functionOne())

        if result.Error: return result

        while self.CurrentToken.Type in ops:
            operatorToken = self.CurrentToken
            result.Register(self.Advance())
            right = result.Register(functionTwo())

            if result.Error: return result

            left = BinOperatorNode(left, operatorToken, right)

        return result.Success(left)

'''
    RUN TIME RESULT
'''

class RunTimeResult:
    
    def __init__(self):
        self.Value = None
        self.Error = None
    
    def Register(self, result):
        if result.Error: self.Error = result.Error
        return result.Value
    
    def Success(self, value):
        self.Value = value
        return self
    
    def Failure(self, error):
        self.Error = error
        return self

'''
    VALUES
'''

class Number:

    def __init__(self, value):
        self.Value = value
        self.SetPosition()

    def SetPosition(self, posStart=None, posEnd=None):
        self.PosStart = posStart
        self.PosEnd = posEnd
        return self
    
    def SetContext(self, context=None):
        self.Context = context
        return self

    def AddedTo(self, other): 
        if isinstance(other, Number):
            return Number(self.Value + other.Value).SetContext(self.Context), None
        
    def SubbedBy(self, other):
        if isinstance(other, Number):
            return Number(self.Value - other.Value).SetContext(self.Context), None
        
    def MultedBy(self, other):
        if isinstance(other, Number):
            return Number(self.Value * other.Value).SetContext(self.Context), None
        
    def DivedBy(self, other):
        if isinstance(other, Number):
            if other.Value == 0: return None, RunTimeError(
                    other.PosStart, other.PosEnd, "Division by zero '0'", self.Context
            )
            return Number(self.Value / other.Value).SetContext(self.Context), None
        
    def PowedBy(self, other):
        if isinstance(other, Number): return Number(self.Value**other.Value).SetContext(self.Context), None
        
    def __repr__(self):
        return str(self.Value)

'''
    CONTEXT
'''

class Context:

    def __init__(self, displayName, parent=None, parentEntryPosition=None):
        self.DisplayName = displayName
        self.Parent = parent
        self.ParentEntryPosition = parentEntryPosition

'''
    INTERPRETER
'''

class Interpreter:
    
    def Visit(self, node, context):
        methodName = f'Visit{type(node).__name__}'
        method = getattr(self, methodName, self.NoVisitMethod)
        return method(node, context)
    
    def NoVisitMethod(self, node, context):
        return Exception(f'No Visit{type(node).__name__} method define')
        
    def VisitNumberNode(self, node, context):
        return RunTimeResult().Success(Number(node.Token.Value).SetContext(context).SetPosition(node.PosStart, node.PosEnd))

    def VisitBinOperatorNode(self, node, context): 
        runTimeResult = RunTimeResult()
        left = runTimeResult.Register(self.Visit(node.LeftNode, context))
        if runTimeResult.Error: return runTimeResult
        right = runTimeResult.Register(self.Visit(node.RightNode, context))
        if runTimeResult.Error: return runTimeResult

        if node.OperatorToken.Type == TT_PLUS: result, error = left.AddedTo(right)
        elif node.OperatorToken.Type == TT_MINUS: result, error = left.SubbedBy(right)
        elif node.OperatorToken.Type == TT_MUL: result, error = left.MultedBy(right)
        elif node.OperatorToken.Type == TT_DIV: result, error = left.DivedBy(right)
        elif node.OperatorToken.Type == TT_POW: result, error = left.PowedBy(right)

        if error: return runTimeResult.Failure(error)
        else: return runTimeResult.Success(result.SetPosition(node.PosStart, node.PosEnd))
        
    def VisitUnaryOperatorNode(self, node, context):
        runTimeError = RunTimeError()
        number = runTimeError.Register(self.Visit(node.Node, context))
        if runTimeError.Error: return runTimeError

        error = None

        if node.OperatorToken == TT_MINUS: number = number.MultedBy(Number(-1))

        if error: return runTimeError.Failure(error)
        else: return runTimeError.Success(number.SetPosition(node.PosStart, node.PosEnd))


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
    if ast.Error: return None, ast.Error

    #run program
    interpreter = Interpreter()
    context = Context('<program>')
    result = interpreter.Visit(ast.Node, context)

    return result.Value, result.Error

