def main():
    # sorted = [1.4, 2.0, 3.1, 3.6, 5.4, 7.3, 8.9, 9.7, 12.1, 13.0]
    images = [3.6, 1.4, 2.0, 5.4, 13.0, 12.1, 8.9, 3.1, 7.3, 9.7]
    blackboxes = [(2.4, 3.7), (5.2, 8.4), (8.7, 8.9), (10.1, 16.7), (22.1, 25.3), (29.8, 30)]

    alg = Algorithm(images, blackboxes)
    print(alg.blackbox_times())

class Algorithm:
    def __init__(self, images: list[float], blackboxes: list[tuple[int, int]]) -> None:
        self.images = images.sort()
        self.blackboxes = blackboxes

        print("images:         ", images)
        print("blackboxes:     ", blackboxes)
        print("available_times:", self.available_times())

    def optimal_solution(self):
        # first blackbox, first element of tuple
        start_first_blackbox = self.blackboxes[0][0]

        pass

    def blackbox_times(self):
        blackbox_times = []

        for (start, finish) in self.blackboxes:
            blackbox_times.append(round(finish - start, 3))
        
        return blackbox_times
        

    def available_timeslots(self) -> list:
        available_timeslots: list = []
        last_time: float = 0

        # (2.4, 3.7), (5.2, 8.4)
        for (start, finish) in self.blackboxes:
            available_timeslots.append((last_time, start))
            last_time = finish

        available_timeslots.append((last_time, "inf"))

        return available_timeslots

    def available_times(self) -> list:
        available_times = []

        available_timeslots = self.available_timeslots()
        for (start, finish) in available_timeslots[:-1]:
            available_times.append(round((finish - start), 3))

        available_times.append("inf")

        return available_times


if __name__ == "__main__":
    main()