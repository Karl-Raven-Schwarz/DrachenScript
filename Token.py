import string

''''
    CONSTANTS
'''

DIGITS = '0123456789'
LETTERS = string.ascii_letters
LETTERS_DIGITS = LETTERS + DIGITS
GRAMMAR_SIGNS = f"_:{'{'}"
print(GRAMMAR_SIGNS)

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
TT_EOF = 'EOF' #    end or final
TT_IDENTIFIER = 'IDENTIFIER'
TT_KEYWORD = 'KEYWORD'
TT_EQUAL = 'EQUAL'
TT_COMMA = 'COMMA'
TT_ARROW = 'ARROW'

#   logical operators
TT_EE = 'EE'
TT_NE = 'NE'
TT_LT = 'LT'
TT_GT = 'GT'
TT_LTE = 'LTE'
TT_GTE = 'GTE'

KEYWORDS = [
    'var', 'and', 'or', 'not', 'if', '{', 'elif', 'else', 'for', 'to', 'step', 'while', 'func'
]

class Token:

    def __init__(self, type, value=None, posStart=None, posEnd=None):
        self.Type = type
        self.Value = value

        if posStart:
            self.PosStart = posStart.Copy()
            self.PosEnd = posStart.Copy()
            self.PosEnd.Advance()

        if posEnd: self.PosEnd = posEnd.Copy()

    def Matches(self, type, value):
        return self.Type == type and self.Value == value

    def __repr__(self):
        if self.Value: return f'{self.Type}:{self.Value}'
        return f'{self.Type}'