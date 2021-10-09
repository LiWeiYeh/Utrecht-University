somelist = []

class Test():
    def __init__(self):
        self.willDie = False


for i in range(0,5):
    test = Test()
    somelist.append(test)



print(list(filter(lambda x: x.willDie == False, somelist)))

