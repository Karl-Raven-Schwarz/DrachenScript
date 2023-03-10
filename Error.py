from StringWithArrows import *
#from Position import *

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

class ExpectedCharError(Error):
    
    def __init__(self, posStart, posEnd, details):
        super().__init__(posStart, posEnd, 'Expected Character', details)

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
