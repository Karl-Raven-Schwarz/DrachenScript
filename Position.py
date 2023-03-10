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