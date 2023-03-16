import drachenScript

while True:
    text = input("Drachen Script > ")
    result, error = drachenScript.Run("<stdin>", text)
    
    if error: print(error.AsString())
    elif result: print(result)
    
    