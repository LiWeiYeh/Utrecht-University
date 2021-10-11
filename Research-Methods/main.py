import random
# import pandas as pd

class Main():
    def __init__(self):
        self.regulars_list = []
        self.greenbeards_list = []

    def create_initial_population(self, size):
        self.regulars_list = []
        self.greenbeards_list = []

        for i in range(0, size):
            reg = Regular()
            self.regulars_list.append(reg)
            gb = Greenbeard()
            self.greenbeards_list.append(gb)

    def run_gens(self, runs):
        for i in range(0, runs):
            for reg in self.regulars_list:
                reg.roll()
            # filters out the ones who die, the ones who live will stay
            self.regulars_list = list(filter(lambda reg: reg.will_die == False, self.regulars_list))

            for gb in self.greenbeards_list:
                gb.roll()
            
            self.greenbeards_list = list(filter(lambda gb: gb.will_die == False, self.greenbeards_list))
            saved_gb = 0
            for gb in self.greenbeards_list:
                gb.save_other()
                saved_gb += 1
            self.greenbeards_list = list(filter(lambda gb: gb.will_die == False, self.greenbeards_list))
            for i in range(0, saved_gb):
                gb = Greenbeard()
                self.greenbeards_list.append(gb)

            self.create_next_gen()
            self.results()

    def create_next_gen(self):
        for i in range(len(self.regulars_list)):
            reg = Regular()
            self.regulars_list.append(reg)

        for i in range(len(self.greenbeards_list)):
            gb = Greenbeard()
            self.greenbeards_list.append(gb)

    def simulate(self, runs=50, population_size=10000):
        self.create_initial_population(population_size)

        self.run_gens(runs)

    def results(self):
        print('amount of regulars left: {0}'.format(len(self.regulars_list)))
        print('amount of greenbeards left: {0}'.format(len(self.greenbeards_list)))
        self.total = len(self.regulars_list + self.greenbeards_list)
        print(self.total)


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
        self.chance = 0.75
        self.deathchance_saving = 0.01
        self.will_die = False

    def roll(self):
        rnd = random.random()
        if (rnd <= self.chance):
            self.will_die = True

    def save_other(self):
        rnd = random.random()
        if (rnd <= self.deathchance_saving):
            self.will_die = True



if __name__ == "__main__":
    main = Main()
    main.simulate(runs=100, population_size=10000)
