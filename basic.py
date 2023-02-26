''''
    CONSTANTS
'''

DIGITS = '0123456789'

''''
    ERRORS
'''

class Error:
    def __init__(self, errorName, details):
        self.ErrorName = errorName
        self.Details = details

    def AsString(self):
        result = f'{self.ErrorName}: {self.Details}'
        return result

class IllegalCharError(Error):
    def __init__(self, details):
        super().__init__('Illegal Character', details)

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

class Token:
    def __init__(self, type, value = None):
        self.Type = type
        self.Value = value

    def __repr__(self):
        if self.Value: return f'{self.Type}:{self.Value}'
        return f'{self.Type}'

''''
    LEXER
'''
 
class Lexer:
    def __init__ (self, text):
        self.Text = text
        self.Pos = -1
        self.CurrentChar = None
        self.Advance()

    def Advance(self):
        self.Pos += 1
        self.CurrentChar = self.Text[self.Pos] if self.Pos < len(self.Text) else None

    def MakeTokens(self):
        tokens = []
        
        while self.CurrentChar != None:
            if self.CurrentChar in ' \t': self.Advance()

            elif self.CurrentChar in DIGITS:
                tokens.append(self.MakeNumber())
                self.Advance()

            elif self.CurrentChar == '+':
                tokens.append(Token(TT_PLUS))
                self.Advance()

            elif self.CurrentChar == '-':
                tokens.append(Token(TT_MINUS))
                self.Advance()

            elif self.CurrentChar == '*':
                tokens.append(Token(TT_MUL))
                self.Advance()

            elif self.CurrentChar == '/':
                tokens.append(Token(TT_DIV))
                self.Advance()

            elif self.CurrentChar == '()':
                tokens.append(Token(TT_LPAREN))
                self.Advance()

            elif self.CurrentChar == ')':
                tokens.append(Token(TT_RPAREN))
                self.Advance()
            
            else: 
                char = self.CurrentChar
                self.Advance()
                return [], IllegalCharError("'" + char + "'")

        return tokens, None

    def MakeNumber(self):
        NumStr = ''
        DotCount = 0

        while self.CurrentChar != None and self.CurrentChar in DIGITS + '-':
            if self.CurrentChar == '.':
                if DotCount == 1: break
                DotCount += 1
                NumStr += '.'
            else: NumStr += self.CurrentChar
            self.Advance()

        if DotCount == 0: return Token(TT_INT, int(NumStr))

        else: return Token(TT_FLOAT, float(NumStr))

'''
    RUN
'''

def Run(text):
    lexer = Lexer(text)
    tokens, error = lexer.MakeTokens()

    return tokens, error

