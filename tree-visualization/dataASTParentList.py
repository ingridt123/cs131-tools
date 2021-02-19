testList = ["(Num (10))", "(10)", "(Times (Num (3)) (Plus (Num (1)) (Num (11))))", "(Plus (Num (1)) (Num (11)))"]
for test in testList:
    test = test[1:-1]

    if ' ' in test:
        parentList = [test[:test.index(' ')]]
        test = test[test.index(' ')+1:]
        while len(test) > 0:
            paranCount = 1
            index = 1
            while paranCount > 0:
                if test[index] == '(':
                    paranCount += 1
                elif test[index] == ')':
                    paranCount -= 1
                index += 1
            parentList.append(test[:index])
            test = test[index+1:]
    else:
        parentList = [test]
    print(parentList)