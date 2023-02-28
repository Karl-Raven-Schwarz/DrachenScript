def StringWithArrows(text, posStart, posEnd):
    result = ''

    # Calculate indices
    indexStart = max(text.rfind('\n', 0, posStart.Index), 0)
    indexEnd = text.find('\n', indexStart + 1)
    if indexEnd < 0: indexEnd = len(text)
    
    # Generate each line
    lineCount = posEnd.Line - posStart.Line + 1
    for i in range(lineCount):
        # Calculate line columns
        line = text[indexStart:indexEnd]
        colStart = posStart.Column if i == 0 else 0
        colEnd = posEnd.Column if i == lineCount - 1 else len(line) - 1

        # Append to result
        result += line + '\n'
        result += ' ' * colStart + '^' * (colEnd - colStart)

        # Re-calculate indices
        indexStart = indexEnd
        indexEnd = text.find('\n', indexStart + 1)
        if indexEnd < 0: indexEnd = len(text)

    return result.replace('\t', '')