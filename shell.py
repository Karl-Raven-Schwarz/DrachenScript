import basic

while True:
    text = input("basic > ")
    result, error = basic.Run("<stdin>", text)
    
    if error: print(error.AsString())
    else: print(result)