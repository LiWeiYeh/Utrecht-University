import random
import matplotlib.pyplot as plt
import numpy as np
# import pandas as pd

class Main():
    def __init__(self):
        self.population = []
        self.reg_pop_size = []
        self.gb_pop_size = []

    def create_initial_population(self, reg_pop_size, gb_pop_size):
        self.population = []

        for i in range(0, reg_pop_size):
            reg = Regular()
            self.population.append(reg)
        
        for i in range(0, gb_pop_size):
            gb = Greenbeard()
            self.population.append(gb)

        random.shuffle(self.population)

    def run_gens(self, runs):
        for i in range(0, runs):
            food = len(self.population)
            self.run_gen(food)
            self.add_altriusm()
            self.kill_pop_no_food()
            list_of_regulars = list(filter(lambda blob: isinstance(blob, Regular), self.population))
            list_of_greenbeards = list(filter(lambda blob: isinstance(blob, Greenbeard), self.population))
            self.reg_pop_size.append(len(list_of_regulars))
            self.gb_pop_size.append(len(list_of_greenbeards))

    def run_gen(self, food):
        for j in range(0, food):
            index = random.randrange(len(self.population))
            self.population[index].food += 1            

    def make_children(self):
        for i in range(0, len(list(filter(lambda blob: isinstance(blob, Regular), self.population)))):
            reg = Regular()
            self.population.append(reg)

        for i in range(0, len(list(filter(lambda blob: isinstance(blob, Greenbeard), self.population)))):
            gb = Greenbeard()
            self.population.append(gb)
        random.shuffle(self.population)


    def kill_pop_no_food(self):
        self.population = list(filter(lambda blob: blob.food != 0, self.population))
        for blob in self.population:
            blob.food -= 1
            # blob can keep at max 1 food
            # if blob.food > 0:
            #     blob.food = 1
            # if you want food per generation, not stacked food,
            # blob.food = 0

    def add_altriusm(self):
        list_of_regulars = list(filter(lambda blob: isinstance(blob, Regular), self.population))
        list_of_greenbeards = list(filter(lambda blob: isinstance(blob, Greenbeard), self.population))

        # give food to other greenbeards
        remaining_food = 0
        for gb in list_of_greenbeards:
            if gb.food > 1:
                while gb.food > 1:
                    gb.food -= 1
                    remaining_food += 1

        for gb in list_of_greenbeards:
            if (gb.food == 0) and (remaining_food > 0):
                gb.food += 1
                remaining_food -= 1

        if remaining_food > 0:
            list_of_greenbeards[0].food += remaining_food

        self.population = list_of_regulars + list_of_greenbeards

    def simulate(self, runs=50, reg_pop_size=5000, gb_pop_size=5000):
        self.create_initial_population(reg_pop_size, gb_pop_size)
        list_of_regulars = list(filter(lambda blob: isinstance(blob, Regular), self.population))
        list_of_greenbeards = list(filter(lambda blob: isinstance(blob, Greenbeard), self.population))
        self.reg_pop_size.append(len(list_of_regulars))
        self.gb_pop_size.append(len(list_of_greenbeards))

        self.run_gens(runs)

    def results(self):
        list_of_regulars = list(filter(lambda blob: isinstance(blob, Regular), self.population))
        list_of_greenbeards = list(filter(lambda blob: isinstance(blob, Greenbeard), self.population))
        print('amount of regulars left: {0}'.format(len(list_of_regulars)))
        print('amount of greenbeards left: {0}'.format(len(list_of_greenbeards)))
        total = len(self.population)
        print('total population size: {0}'.format(total))
        # print(self.reg_pop_size)

class Regular():
    def __init__(self):
        self.food = 0

class Greenbeard():
    def __init__(self):
        self.food = 0

if __name__ == "__main__":
    gb_win = 0
    reg_win = 0

    simulations = 500
    runs = 150

    list_size_reg = []
    list_size_gb = []

    for i in range(0,simulations):
        main = Main()

        gb_y = []
        reg_y = []

        main.simulate(runs=runs, reg_pop_size=1000, gb_pop_size=1000)

        for i in range(0, len(main.reg_pop_size)):
            gen = i
            size_reg = main.reg_pop_size[gen]
            size_gb = main.gb_pop_size[gen]

            gb_y.append(size_gb/(size_reg + size_gb))
            reg_y.append(size_reg/(size_reg + size_gb))

        if gb_y[-1] > reg_y[-1]:
            gb_win += 1
        elif gb_y[-1] < reg_y[-1]:
            reg_win += 1

        list_size_reg.append(reg_y)
        list_size_gb.append(gb_y)

    reg_y = []
    gb_y = []
    for i in range(0, runs):
        count_reg = 0
        count_gb = 0
        for j in range(0, simulations):
            count_reg += list_size_reg[j][i]
            count_gb += list_size_gb[j][i]
        reg_y.append(count_reg/simulations)
        gb_y.append(count_gb/simulations)
    reg_x = np.arange(runs)
    gb_x = np.arange(runs)

    plt.plot(gb_x, gb_y, label="Green-beard", linestyle='--', color='black')
    plt.plot(reg_x, reg_y, label="Regular", linestyle='-', color='black')
    plt.ylim(0, 1)
    plt.xlabel('Generations')
    plt.ylabel('Population size')
    # plt.title('Simulating Altruism')
    plt.legend()
    plt.show()

    print("The greenbeard has won {0} out of {1} times".format(gb_win, gb_win+reg_win))
