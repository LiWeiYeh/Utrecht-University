import random

class Main():
    def __init__(self):
        self.regulars_list = []
        self.greenbeards_list = []

    def create_initial_population(self, size):
        self.regulars_list = []
        for i in range(0, size):
            reg = Regular()
            self.regulars_list.append(reg)
            # gb = Greenbeard(0.5)
            # self.greenbeards_list.append(gb)

    def run_gen(self, runs):
        for i in range(0, runs):
            for regular in self.regulars_list:
                regular.roll()

            # filters out the ones who die, the ones who live will stay
            self.regulars_list = list(filter(lambda reg: reg.will_die == False, self.regulars_list))
            self.create_next_gen()

    def create_next_gen(self):
        for i in range(len(self.regulars_list)):
            reg = Regular()
            self.regulars_list.append(reg)

    def simulate(self, runs=50):
        self.create_initial_population(10000)

        self.run_gen(50)

        self.results()

    def results(self):
        print('amount of regulars left: {0}'.format(len(self.regulars_list)))

class Regular():
    def __init__(self):
        self.chance = 0.5
        self.will_die = False

    def roll(self):
        rnd = random.random()
        if (rnd <= self.chance):
            self.will_die = True

class Greenbeard():
    def __init__(self):
        self.chance = 0.5

if __name__ == "__main__":
    main = Main()
    main.simulate()

