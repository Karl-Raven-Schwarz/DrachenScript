from StringWithArrows import *
from Position import *
from Error import *
from Token import *

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

            elif self.CurrentChar in LETTERS + GRAMMAR_SIGNS:
                tokens.append(self.MakeIdentifier())

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

            elif self.CurrentChar == '!':
                token, error = self.MakeNotEquals()
                if error: return [], error
                tokens.append(token)

            elif self.CurrentChar == '=':
                tokens.append(self.MakeEqualsOrArrow())

            elif self.CurrentChar == '<':
                tokens.append(self.MakeLessThan())

            elif self.CurrentChar == '>':
                tokens.append(self.MakeGreaterThan())

            elif self.CurrentChar == ',':
                tokens.append(Token(TT_COMMA, posStart=self.Position))
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
            NumStr += self.CurrentChar
            self.Advance()

        if dotCount == 0: return Token(TT_INT, int(NumStr), posStart, self.Position)

        else: return Token(TT_FLOAT, float(NumStr), posStart, self.Position)

    def MakeIdentifier(self):
        idStr = ''
        posStart = self.Position.Copy()

        while self.CurrentChar != None and self.CurrentChar in LETTERS_DIGITS + GRAMMAR_SIGNS:
            idStr += self.CurrentChar
            self.Advance()
        
        tokenType = TT_KEYWORD if idStr in KEYWORDS else TT_IDENTIFIER
        return Token(tokenType, idStr, posStart, self.Position)

    def MakeNotEquals(self):
        posStart = self.Position.Copy()
        self.Advance()

        if self.CurrentChar == '=':
            self.Advance()
            return Token(TT_NE, posStart=posStart, posEnd=self.Position), None
        
        self.Advance()
        return None, ExpectedCharError(posStart, self.Position, "'=' (after '!')")
    
    def MakeEqualsOrArrow(self):
        tokenType = TT_EQUAL
        posStart = self.Position.Copy()
        self.Advance()

        if self.CurrentChar == '=':
            self.Advance()
            tokenType = TT_EE
        elif self.CurrentChar == '>':
            self.Advance()
            tokenType = TT_ARROW

        return Token(tokenType, posStart=posStart, posEnd=self.Position)
    
    def MakeLessThan(self):
        tokenType = TT_LT
        posStart = self.Position.Copy()
        self.Advance()

        if self.CurrentChar == '=':
            self.Advance()
            tokenType = TT_LTE

        return Token(tokenType, posStart=posStart, posEnd=self.Position)
    
    def MakeGreaterThan(self):
        tokenType = TT_GT
        posStart = self.Position.Copy()
        self.Advance()

        if self.CurrentChar == '=':
            self.Advance()
            tokenType = TT_GTE

        return Token(tokenType, posStart=posStart, posEnd=self.Position)

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

class VarAccessNode:
    
    def __init__(self, varNameToken):
        self.VarNameToken = varNameToken
        self.PosStart = self.VarNameToken.PosStart
        self.PosEnd = self.VarNameToken.PosEnd

class VarAssingNode:

    def __init__(self, varNameToken, valueNode):
        self.VarNameToken = varNameToken
        self.ValueNode = valueNode
        self.PosStart = self.VarNameToken.PosStart
        self.PosEnd = self.VarNameToken.PosEnd

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

class IfNode:

    def __init__(self, cases, elseCase):
        self.Cases = cases
        self.ElseCase = elseCase
        self.PosStart = self.Cases[0][0].PosStart
        self.PosEnd = (self.ElseCase or self.Cases[len(self.Cases) - 1][0]).PosEnd

class ForNode:

    def __init__(self, varNameToken, startValueNode, endValueNode, stepValueNode, bodyNode):
        self.VarNameToken = varNameToken
        self.StartValueNode = startValueNode
        self.EndValueNode = endValueNode
        self.StepValueNode = stepValueNode
        self.BodyNode = bodyNode

        self.PosStart = self.VarNameToken.PosStart
        self.PosEnd = self.BodyNode.PosEnd

class WhileNode:

    def __init__(self, conditionNode, bodyNode):
        self.ConditionNode = conditionNode
        self.BodyNode = bodyNode

        self.PosStart = self.ConditionNode.PosStart
        self.PosEnd = self.BodyNode.PosEnd

class FunctionDefinitionNode:

    def __init__(self, varNameToken, argNameTokens, bodyNode):
        self.VarNameToken = varNameToken
        self.ArgNameTokens = argNameTokens
        self.BodyNode = bodyNode

        if self.VarNameToken:
            self.PosStart = self.VarNameToken.PosStart
        elif len(self.ArgNameTokens) > 0:
            self.PosStart = self.ArgNameTokens[0].PosStart
        else:
            self.PosStart = self.BodyNode.PosStart

        self.PosEnd = self.BodyNode.PosEnd

class CallNode:

    def __init__(self, nodeToCall, argNodes):
        self.NodeToCall = nodeToCall
        self.ArgNodes = argNodes
        self.PosStart = self.NodeToCall.PosStart

        if len(self.ArgNodes) > 0:
            self.PosEnd = self.ArgNodes[len(self.ArgNodes) -1].PosEnd
        else:
            self.PosEnd = self.NodeToCall.PosEnd

'''
    PARSE RESULT
'''

class ParseResult:

    def __init__(self):
        self.Error = None
        self.Node = None
        self.LastRegisteredAdvanceCount = 0
        self.AdvanceCount = 0

    def RegisterAdvancement(self):
        self.LastRegisteredAdvanceCount = 1
        self.AdvanceCount += 1

    def Register(self, result):
        self.LastRegisteredAdvanceCount = result.AdvanceCount
        self.AdvanceCount += result.AdvanceCount
        if result.Error: self.Error = result.Error
        return result.Node

    def Success(self, node):
        self.Node = node
        return self

    def Failure(self, error):
        if not self.Error or self.LastRegisteredAdvanceCount == 0:
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
                self.CurrentToken.PosStart, self.CurrentToken.PosEnd, "Expected '+', '-', '*', '/', '^', '==', '!=', '<', '>', <=', '>=', 'AND' or 'OR'"
            ))
        return result

###################################################

    def IfExpr(self):
        parseResult = ParseResult()
        cases = []
        elseCase = None

        if not self.CurrentToken.Matches(TT_KEYWORD, 'if'):
            return parseResult.Failure(InvalidSyntaxError(
                self.CurrentToken.PosStart, self.CurrentToken.PosEnd, f"Expected 'if'"
            ))
        
        parseResult.RegisterAdvancement()
        self.Advance()

        condition = parseResult.Register(self.Expr())

        if parseResult.Error: return parseResult

        if not self.CurrentToken.Matches(TT_KEYWORD, '{'):
            return parseResult.Failure(InvalidSyntaxError(
                self.CurrentToken.PosStart, self.CurrentToken.PosEnd, f"Expected {'{'} (then)"
            ))
        
        parseResult.RegisterAdvancement()
        self.Advance()

        expr = parseResult.Register(self.Expr())

        if parseResult.Error: return parseResult

        cases.append((condition, expr))

        while self.CurrentToken.Matches(TT_KEYWORD, 'elif'):
            parseResult.RegisterAdvancement()
            self.Advance()
            condition = parseResult.Register(self.Expr())

            if parseResult.Error: return parseResult

            if not self.CurrentToken.Matches(TT_KEYWORD, '{'):
                return parseResult.Failure(InvalidSyntaxError(
                    self.CurrentToken.PosStart, self.CurrentToken.PosEnd, f"Expected {'{'}"
                ))
            
            parseResult.RegisterAdvancement()
            self.Advance()
            expr = parseResult.Register(self.Expr())

            if parseResult.Error: return parseResult

            cases.append((condition, expr))
        
        if self.CurrentToken.Matches(TT_KEYWORD, 'else'):
            parseResult.RegisterAdvancement()
            self.Advance()
            expr = parseResult.Register(self.Expr())

            if parseResult.Error: return parseResult

            elseCase = expr
        
        return parseResult.Success(IfNode(cases, elseCase))

    def ForExpr(self):
        parseResult = ParseResult()

        if not self.CurrentToken.Matches(TT_KEYWORD, 'for'):
            return parseResult.Failure(InvalidSyntaxError(
                    self.CurrentToken.PosStart, self.CurrentToken.PosEnd, f"Expected 'for'"
                ))
        
        parseResult.RegisterAdvancement()
        self.Advance()

        if parseResult.Error: return parseResult

        if self.CurrentToken.Type != TT_IDENTIFIER:
            return parseResult.Failure(InvalidSyntaxError(
                self.CurrentToken.PosStart, self.CurrentToken.PosEnd, f"Expected identifier"
            ))
        
        varName = self.CurrentToken
        parseResult.RegisterAdvancement()
        self.Advance()

        if self.CurrentToken.Type != TT_EQUAL:
            return parseResult.Failure(InvalidSyntaxError(
                self.CurrentToken.PosStart, self.CurrentToken.PosEnd, f"Expected '='"
            ))
        
        parseResult.RegisterAdvancement()
        self.Advance()

        startValue = parseResult.Register(self.Expr())

        if parseResult.Error: return parseResult

        if not self.CurrentToken.Matches(TT_KEYWORD, 'to'):
            return parseResult.Failure(InvalidSyntaxError(
                self.CurrentToken.PosStart, self.CurrentToken.PosEnd, f"Expected 'to'"
            ))
        
        parseResult.RegisterAdvancement()
        self.Advance()

        endValue = parseResult.Register(self.Expr())
        
        if parseResult.Error: return parseResult

        if self.CurrentToken.Matches(TT_KEYWORD, 'step'):
            parseResult.RegisterAdvancement()
            self.Advance()
            stepValue = parseResult.Register(self.Expr())

            if parseResult.Error: return parseResult

        else: stepValue = None

        if not self.CurrentToken.Matches(TT_KEYWORD, '{'):
            return parseResult.Failure(InvalidSyntaxError(
                self.CurrentToken.PosStart, self.CurrentToken.PosEnd, f"Expected {'{'}"
            ))
        
        parseResult.RegisterAdvancement()
        self.Advance()
        body = parseResult.Register(self.Expr())
        
        if parseResult.Error: return parseResult

        return parseResult.Success(ForNode(varName, startValue, endValue, stepValue, body))
        
    def WhileExpr(self):
        parseResult = ParseResult()

        if not self.CurrentToken.Matches(TT_KEYWORD, 'while'):
            return parseResult.Failure(InvalidSyntaxError(
                self.CurrentToken.PosStart, self.CurrentToken.PosEnd, f"Expected 'while'"
            ))
        
        parseResult.RegisterAdvancement()
        self.Advance()
        condition = parseResult.Register(self.Expr())

        if parseResult.Error: return parseResult

        if not self.CurrentToken.Matches(TT_KEYWORD, '{'):
            return parseResult.Failure(InvalidSyntaxError(
                self.CurrentToken.PosStart, self.CurrentToken.PosEnd, f"Expected {'{'}"
            ))
        
        parseResult.RegisterAdvancement()
        self.Advance()
        body = parseResult.Register(self.Expr())

        if parseResult.Error: return parseResult

        return parseResult.Success(WhileNode(condition, body))

    def FunctionDefinition(self):
        parseResult = ParseResult()
        
        if not self.CurrentToken.Matches(TT_KEYWORD, 'func'):
            return parseResult.Failure(InvalidSyntaxError(
                self.CurrentToken.PosStart, self.CurrentToken.PosEnd, f"Expected 'func'"
            ))
        
        parseResult.RegisterAdvancement()
        self.Advance()

        if self.CurrentToken.Type == TT_IDENTIFIER:
            varNameToken = self.CurrentToken
            parseResult.RegisterAdvancement()
            self.Advance()

            if self.CurrentToken.Type != TT_LPAREN:
                return parseResult.Failure(InvalidSyntaxError(
                    self.CurrentToken.PosStart, self.CurrentToken.PosEnd, f"Expected '('"
                ))
            
        else: 
            varNameToken = None

            if self.CurrentToken.Type != TT_LPAREN:
                return parseResult.Failure(InvalidSyntaxError(
                    self.CurrentToken.PosStart, self.CurrentToken.PosEnd, f"Expected Identifier or '('"
                ))
                
            parseResult.RegisterAdvancement()
            self.Advance()
            argNameTokens = []

            if self.CurrentToken.Type == TT_IDENTIFIER:
                argNameTokens.append(self.CurrentToken)
                parseResult.RegisterAdvancement()
                self.Advance()

                while self.CurrentToken.Type == TT_COMMA:
                    parseResult.RegisterAdvancement()
                    self.Advance()

                    if self.CurrentToken.Type != TT_IDENTIFIER:
                        return parseResult.Failure(InvalidSyntaxError(
                            self.CurrentToken.PosStart, self.CurrentToken.PosEnd, f"Expected Identifier"
                        ))
                    
                    argNameTokens.append(self.CurrentToken)
                    parseResult.RegisterAdvancement()
                    self.Advance()

                if self.CurrentToken.Type != TT_RPAREN:
                    return parseResult.Failure(InvalidSyntaxError(
                            self.CurrentToken.PosStart, self.CurrentToken.PosEnd, f"Expected ',' or ')'"
                        ))
                
            else:
                if self.CurrentToken.Type != TT_RPAREN:
                    return parseResult.Failure(InvalidSyntaxError(
                            self.CurrentToken.PosStart, self.CurrentToken.PosEnd, f"Expected Identifier or ')'"
                        ))
                
            parseResult.RegisterAdvancement()
            self.Advance()

            if self.CurrentToken.Type != TT_ARROW:
                return parseResult.Failure(InvalidSyntaxError(
                        self.CurrentToken.PosStart, self.CurrentToken.PosEnd, f"Expected '=>'"
                    ))
            
            parseResult.RegisterAdvancement()
            self.Advance()
            nodeToReturn = parseResult.Register(self.Expr())

            if parseResult.Error: return parseResult

            return parseResult.Success(FunctionDefinitionNode(varNameToken, argNameTokens, nodeToReturn))

    def Atom(self):
        parseResult = ParseResult()
        token = self.CurrentToken

        if token.Type in (TT_INT, TT_FLOAT):
            parseResult.RegisterAdvancement()
            self.Advance()
            return parseResult.Success(NumberNode(token))
        
        elif token.Type == TT_IDENTIFIER:
            parseResult.RegisterAdvancement()
            self.Advance()
            return parseResult.Success(VarAccessNode(token))

        elif token.Type == TT_LPAREN:
            parseResult.RegisterAdvancement()
            self.Advance()
            expr = parseResult.Register(self.Expr())
            
            if parseResult.Error: return parseResult
            
            if self.CurrentToken.Type == TT_RPAREN:
                parseResult.RegisterAdvancement()
                self.Advance()
                return parseResult.Success(expr)
            else:
                return parseResult.Failure(InvalidSyntaxError(
                    self.CurrentToken.PosStart, self.CurrentToken.PosEnd, "Expected ')'"
                ))
        
        elif token.Matches(TT_KEYWORD, 'if'):
            ifExpr = parseResult.Register(self.IfExpr())
            
            if parseResult.Error: return parseResult

            return parseResult.Success(ifExpr)
        
        elif token.Matches(TT_KEYWORD, 'for'):
            forExpr = parseResult.Register(self.ForExpr())
            
            if parseResult.Error: return parseResult

            return parseResult.Success(forExpr)
        
        elif token.Matches(TT_KEYWORD, 'while'):
            whileExpr = parseResult.Register(self.WhileExpr())
            
            if parseResult.Error: return parseResult

            return parseResult.Success(whileExpr)
        
        elif token.Matches(TT_KEYWORD, 'func'):
            functionDefinition = parseResult.Register(self.FunctionDefinition())
            
            if parseResult.Error: return parseResult

            return parseResult.Success(functionDefinition)

        return parseResult.Failure(InvalidSyntaxError(
            token.PosStart, token.PosEnd, "Expected int, float, identifier, '+', '-', '(', 'if', 'for', 'while', 'func'"
        ))

    def Power(self):
        return self.BinOperator(self.Call, (TT_POW, ), self.Factor)
        #return self.BinOperator(self.Atom, (TT_POW, ), self.Factor)

    def Call(self):
        parseResult = ParseResult()
        atom = parseResult.Register(self.Atom())
        if parseResult.Error: return parseResult

        if self.CurrentToken.Type == TT_LPAREN:
            parseResult.RegisterAdvancement()
            self.Advance()
            argNodes = []

            if self.CurrentToken.Type == TT_RPAREN:
                parseResult.RegisterAdvancement()
                self.Advance()
            
            else:
                argNodes.append(parseResult.Register(self.Expr()))

                if parseResult.Error:
                    return parseResult.Failure(InvalidSyntaxError(
                        self.CurrentToken.PosStart, self.CurrentToken.PosEnd, f"Expected ')', 'var', 'if', 'for', 'while', int, float, Identifier, '+', '-', '(' or 'not'"
                    ))
                
                while self.CurrentToken.Type == TT_COMMA:
                    parseResult.RegisterAdvancement()
                    self.Advance()
                    argNodes.append(parseResult.Register(self.Expr()))

                    if parseResult.Error: return parseResult

                if self.CurrentToken.Type != TT_RPAREN:
                    return parseResult.Failure(InvalidSyntaxError(
                        self.CurrentToken.PosStart, self.CurrentToken.PosEnd, f"Expected ',' or ')'"
                    ))
                
                parseResult.RegisterAdvancement()
                self.Advance()
            return parseResult.Success(CallNode(atom, argNodes))
            
        return parseResult.Success(atom)

    def Factor(self):
        result = ParseResult()
        token = self.CurrentToken

        if token.Type in (TT_PLUS, TT_MINUS):
            result.RegisterAdvancement()
            self.Advance()
            factor = result.Register(self.Factor())
            if result.Error: return result
            return result.Success(UnaryOperatorNode(token, factor))

        return self.Power()

    def Term(self):
        return self.BinOperator(self.Factor, (TT_MUL, TT_DIV))

    def ArithExpr(self):
        return self.BinOperator(self.Term, (TT_PLUS, TT_MINUS))

    def ComparisonExpr(self):
        result = ParseResult()

        if self.CurrentToken.Matches(TT_KEYWORD, 'not'):
            operatorToken = self.CurrentToken
            result.RegisterAdvancement()
            self.Advance()
            node = result.Register(self.ComparisonExpr())

            if result.Error: return result

            return result.Success(UnaryOperatorNode(operatorToken, node))
        
        node = result.Register(self.BinOperator(self.ArithExpr, (TT_EE, TT_NE, TT_LT, TT_GT, TT_LTE, TT_GTE)))

        if result.Error: return result.Failure(InvalidSyntaxError(
                self.CurrentToken.PosStart, self.CurrentToken.PosEnd, "Expected int, float, identifier, '+', '-', '(', 'not')"
        ))

        return result.Success(node)

    def Expr(self):
        result = ParseResult()

        if self.CurrentToken.Matches(TT_KEYWORD, 'var'): 
            result.RegisterAdvancement()
            self.Advance()

            if self.CurrentToken.Type != TT_IDENTIFIER:
                return result.Failure(InvalidSyntaxError(
                    self.CurrentToken.PosStart, self.CurrentToken.PosEnd, 'Expected Identifier'
                ))
            
            varName = self.CurrentToken
            result.RegisterAdvancement()
            self.Advance()

            if self.CurrentToken.Type != TT_EQUAL:
                return result.Failure(InvalidSyntaxError(
                    self.CurrentToken.PosStart, self.CurrentToken.PosEnd, "Expected '='"
                ))
            
            result.RegisterAdvancement()
            self.Advance()
            expr = result.Register(self.Expr())

            if result.Error : return result

            return result.Success(VarAssingNode(varName, expr))

        node = result.Register(self.BinOperator(self.ComparisonExpr, ((TT_KEYWORD, 'and'), (TT_KEYWORD, 'or'))))

        if result.Error: return result.Failure(InvalidSyntaxError(
                self.CurrentToken.PosStart, self.CurrentToken.PosEnd,"Expected 'var', int, float, identifier, '+', '-', '(' or 'not'"
                ))
        
        return result.Success(node)

    def BinOperator(self, functionOne, ops, functionTwo=None):
        if functionTwo == None: functionTwo = functionOne
        result = ParseResult()
        left = result.Register(functionOne())

        if result.Error: return result

        while self.CurrentToken.Type in ops or (self.CurrentToken.Type, self.CurrentToken.Value) in ops:
            operatorToken = self.CurrentToken
            result.RegisterAdvancement()
            self.Advance()
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

class Value:

    def __init__(self):
        self.SetPosition()
        self.SetContext()
    
    def SetPosition(self, posStart=None, posEnd=None):
        self.PosStart = posStart
        self.PosEnd = posEnd
        return self
    
    def SetContext(self, context=None):
        self.Context = context
        return self

    def AddedTo(self, other): 
        return None, self.IllegalOperation(other)
        
    def SubbedBy(self, other):
        return None, self.IllegalOperation(other)
        
    def MultedBy(self, other):
        return None, self.IllegalOperation(other)
        
    def DivedBy(self, other):
        return None, self.IllegalOperation(other)
        
    def PowedBy(self, other):
        return None, self.IllegalOperation(other)

    def GetComparisonEq(self, other):
        return None, self.IllegalOperation(other)

    def GetComparisonNE(self, other):
        return None, self.IllegalOperation(other)

    def GetComparisonLT(self, other):
        return None, self.IllegalOperation(other)

    def GetComparisonGT(self, other):
        return None, self.IllegalOperation(other)

    def GetComparisonLTE(self, other):
        return None, self.IllegalOperation(other)

    def GetComparisonGTE(self, other):
        return None, self.IllegalOperation(other)

    def AndedBy(self, other):
        return None, self.IllegalOperation(other)
    
    def OredBy(self, other):
        return None, self.IllegalOperation(other)

    def Notted(self, other):
        return None, self.IllegalOperation(other)

    def Execute(self, args):
        return RunTimeResult().Failure(self.IllegalOperation())

    def Copy(self):
        raise Exception('No copy method defined')

    def IsTrue(self):
        return False

    def IllegalOperation(self, other=None):
        if not other: other = self
        return RunTimeError( self.PosStart, other.PosEnd, 'Illegal operation', self.Context)

class Number(Value):

    def __init__(self, value):
        super().__init__()
        self.Value = value

    def AddedTo(self, other): 
        if isinstance(other, Number):
            return Number(self.Value + other.Value).SetContext(self.Context), None
        else:
            return None, Value.IllegalOperation(self, other)
        
    def SubbedBy(self, other):
        if isinstance(other, Number):
            return Number(self.Value - other.Value).SetContext(self.Context), None
        else:
            return None, Value.IllegalOperation(self, other)
        
    def MultedBy(self, other):
        if isinstance(other, Number):
            return Number(self.Value * other.Value).SetContext(self.Context), None
        else:
            return None, Value.IllegalOperation(self, other)
        
    def DivedBy(self, other):
        if isinstance(other, Number):
            if other.Value == 0: return None, RunTimeError(
                    other.PosStart, other.PosEnd, "Division by zero '0'", self.Context
            )
            return Number(self.Value / other.Value).SetContext(self.Context), None
        else:
            return None, Value.IllegalOperation(self, other)
        
    def PowedBy(self, other):
        if isinstance(other, Number): 
            return Number(self.Value**other.Value).SetContext(self.Context), None
        else:
            return None, Value.IllegalOperation(self, other)

    def GetComparisonEq(self, other):
        if isinstance(other, Number): 
            return Number(int(self.Value == other.Value)).SetContext(self.Context), None
        else:
            return None, Value.IllegalOperation(self, other)

    def GetComparisonNE(self, other):
        if isinstance(other, Number): 
            return Number(int(self.Value != other.Value)).SetContext(self.Context), None
        else:
            return None, Value.IllegalOperation(self, other)

    def GetComparisonLT(self, other):
        if isinstance(other, Number): 
            return Number(int(self.Value < other.Value)).SetContext(self.Context), None
        else:
            return None, Value.IllegalOperation(self, other)

    def GetComparisonGT(self, other):
        if isinstance(other, Number): 
            return Number(int(self.Value > other.Value)).SetContext(self.Context), None
        else:
            return None, Value.IllegalOperation(self, other)

    def GetComparisonLTE(self, other):
        if isinstance(other, Number): 
            return Number(int(self.Value <= other.Value)).SetContext(self.Context), None
        else:
            return None, Value.IllegalOperation(self, other)

    def GetComparisonGTE(self, other):
        if isinstance(other, Number): 
            return Number(int(self.Value >= other.Value)).SetContext(self.Context), None
        else:
            return None, Value.IllegalOperation(self, other)

    def AndedBy(self, other):
        if isinstance(other, Number): 
            return Number(int(self.Value and other.Value)).SetContext(self.Context), None
        else:
            return None, Value.IllegalOperation(self, other)
    
    def OredBy(self, other):
        if isinstance(other, Number): 
            return Number(int(self.Value or other.Value)).SetContext(self.Context), None
        else:
            return None, Value.IllegalOperation(self, other)

    def Notted(self):
        return Number(1 if self.Value == 0 else 0).SetContext(self.Context), None

    def Copy(self):
        copy = Number(self.Value)
        copy.SetPosition(self.PosStart, self.PosEnd)
        copy.SetContext(self.Context)
        return copy

    def IsTrue(self):
        return self.Value != 0

    def __repr__(self):
        return str(self.Value)

class Function(Value):           
        
	def __init__(self, name, bodyNode, argNames):
		super().__init__()
		self.Name = name or "<anonymous>"
		self.BodyNode = bodyNode
		self.ArgNames = argNames

	def Execute(self, args):
		rTResult = RunTimeResult()
		interpreter = Interpreter()
		newContext = Context(self.Name, self.Context, self.PosStart)
		newContext.SymbolTable = SymbolTable(newContext.Parent.SymbolTable)

		if len(args) > len(self.ArgNames):
			return rTResult.Failure(RunTimeError(
				self.pos_start, self.PosEnd, f"{len(args) - len(self.ArgNames)} too many args passed into '{self.Name}'", self.Context
			))
		
		if len(args) < len(self.ArgNames):
			return rTResult.Failure(RunTimeError(self.PosStart, self.PosEnd, f"{len(self.ArgNames) - len(args)} too few args passed into '{self.Name}'"
                , self.Context))
                
		for i in range(len(args)):
			argName = self.ArgNames[i]
			argValue = args[i]
			argValue.SetContext(newContext)
			newContext.SymbolTable.Set(argName, argValue)

		value = rTResult.Register(interpreter.Visit(self.BodyNode, newContext))
                
		if rTResult.Error: return rTResult
                
		return rTResult.Success(value)

	def Copy(self):
		copy = Function(self.Name, self.BodyNode, self.ArgNames)
		copy.SetContext(self.Context)
		copy.SetPosition(self.PosStart, self.PosEnd)
		return copy

	def __repr__(self):
		return f"<function {self.Name}>"

'''
    CONTEXT
'''

class Context:

    def __init__(self, displayName, parent=None, parentEntryPosition=None):
        self.DisplayName = displayName
        self.Parent = parent
        self.ParentEntryPosition = parentEntryPosition
        self.SymbolTable = None

'''
    SYMBOL TABLE
'''

class SymbolTable:

    def __init__(self, parent=None):
        self.Symbols = {}
        self.Parent = parent

    def Get(self, name):
        value = self.Symbols.get(name, None)

        if value == None and self.Parent: return self.Parent.Get(name)

        return value
    
    def Set(self, name, value):
        self.Symbols[name] = value

    def Remove(self, name):
        del self.Symbols[name]

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

    def VisitVarAccessNode(self, node, context):
        result = RunTimeResult()
        varName = node.VarNameToken.Value
        value = context.SymbolTable.Get(varName)

        if not value: return result.Failure(RunTimeError(                
                node.PosStart, node.PosEnd, f"'{varName}' is not defined", context       
        ))
        value = value.Copy().SetPosition(node.PosStart, node.PosEnd)
        return result.Success(value)
    
    def VisitVarAssingNode(self, node, context):
        result = RunTimeResult()
        varName = node.VarNameToken.Value
        value = result.Register(self.Visit(node.ValueNode, context))
        
        if result.Error: return result

        context.SymbolTable.Set(varName, value)

        return result.Success(value)

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
        #   comparison
        elif node.OperatorToken.Type == TT_EE: result, error = left.GetComparisonEq(right)
        elif node.OperatorToken.Type == TT_NE: result, error = left.GetComparisonNE(right)
        elif node.OperatorToken.Type == TT_LT: result, error = left.GetComparisonLT(right)
        elif node.OperatorToken.Type == TT_GT: result, error = left.GetComparisonGT(right)
        elif node.OperatorToken.Type == TT_LTE: result, error = left.GetComparisonLTE(right)
        elif node.OperatorToken.Type == TT_GTE: result, error = left.GetComparisonGTE(right)
        elif node.OperatorToken.Matches(TT_KEYWORD, 'and'): result, error = left.AndedBy(right)
        elif node.OperatorToken.Matches(TT_KEYWORD, 'or'): result, error = left.OredBy(right)

        if error: return runTimeResult.Failure(error)
        else: return runTimeResult.Success(result.SetPosition(node.PosStart, node.PosEnd))
        
    def VisitUnaryOperatorNode(self, node, context):
        rTResult = RunTimeResult()
        number = rTResult.Register(self.Visit(node.Node, context))
        if rTResult.Error: return rTResult

        error = None

        if node.OperatorToken.Type == TT_MINUS: 
            number, error = number.MultedBy(Number(-1))
        elif node.OperatorToken.Matches(TT_KEYWORD, 'not'): 
            number, error = number.Notted()

        if error: return rTResult.Failure(error)
        else: return rTResult.Success(number.SetPosition(node.PosStart, node.PosEnd))

    def VisitIfNode(self, node, context):
        rTResult = RunTimeResult()
        
        for condition, expr in node.Cases:
            conditionValue = rTResult.Register(self.Visit(condition, context))

            if rTResult.Error: return rTResult

            if conditionValue.IsTrue():
                exprValue = rTResult.Register(self.Visit(expr, context))

                if rTResult.Error: return rTResult

                return rTResult.Success(exprValue)
        
        if node.ElseCase:
            elseValue = rTResult.Register(self.Visit(node.ElseCase, context))

            if rTResult.Error: return rTResult

            return rTResult.Success(elseValue)
        
        return rTResult.Success(None)

    def VisitForNode(self, node, context):
        rTResult = RunTimeResult()
        startValue = rTResult.Register(self.Visit(node.StartValueNode, context))

        if rTResult.Error: return rTResult

        endValue = rTResult.Register(self.Visit(node.EndValueNode, context))
        
        if rTResult.Error: return rTResult

        if node.StepValueNode:
            stepValue = rTResult.Register(self.Visit(node.StepValueNode, context))

            if rTResult.Error: return rTResult

        else: stepValue = Number(1)

        i = startValue.Value

        if stepValue.Value >= 0: condition = lambda: i < endValue.Value
        else: condition = lambda: i > endValue.Value

        while condition():
            context.SymbolTable.Set(node.VarNameToken.Value, Number(i))
            i += stepValue.Value
            rTResult.Register(self.Visit(node.BodyNode, context))

            if rTResult.Error: return rTResult

        return rTResult.Success(None)

    def VisitWhileNode(self, node, context):
        rTResult = RunTimeResult()

        while True:
            condition = rTResult.Register(self.Visit(node.ConditionNode, context))

            if rTResult.Error: return rTResult

            if not condition.IsTrue(): break

            rTResult.Register(self.Visit(node.BodyNode, context))

            if rTResult.Error: return rTResult
        
        return rTResult.Success(None)

    def VisitFunctionDefinitionNode(self, node, context):
        rTResult = RunTimeResult()
        functionName = node.VarNameToken.Value if node.VarNameToken else None
        bodyNode = node.BodyNode
        argNames = [argName.Value for argName in node.ArgNameTokens]
        functionValue = Function(functionName, bodyNode, argNames).SetContext(context).SetPosition(node.PosStart, node.PosEnd)

        if node.VarNameToken:
            context.SumbolTable.Set(functionName, functionValue)

        return rTResult.Success(functionValue)
    
    def VisitCallNode(self, node, context):
        rTResult = RunTimeResult()
        args = []
        valueToCall = rTResult.Register(self.Visit(node.NodeToCall, context))

        if rTResult.Error: return rTResult

        valueToCall = valueToCall.Copy().SetPosition(node.PosStart, node.PosEnd)

        for argNode in node.ArgNodes:
            args.append(rTResult.Register(self.Visit(argNode, context)))

            if rTResult.Error: return rTResult
        
        returnValue = rTResult.Register(valueToCall.Execute(args))

        if rTResult.Error: return rTResult

        return rTResult.Success(returnValue)

'''
    RUN
'''

GlobalSymbolTable = SymbolTable()
GlobalSymbolTable.Set('null', Number(0))
GlobalSymbolTable.Set('true', Number(1))
GlobalSymbolTable.Set('false', Number(0))

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
    context.SymbolTable = GlobalSymbolTable
    result = interpreter.Visit(ast.Node, context)

    return result.Value, result.Error


print("----------------------------")