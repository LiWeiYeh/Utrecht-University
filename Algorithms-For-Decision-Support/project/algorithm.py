

def main():
    # sorted = [1.4, 2.0, 3.1, 3.6, 5.4, 7.3, 8.9, 9.7, 12.1, 13.0]
    images = [3.6, 1.4, 2.0, 5.4, 13.0, 12.1, 8.9, 3.1, 7.3, 9.7]
    blackboxes = [(2.4, 3.7), (5.2, 8.4), (8.7, 8.9), (10.1, 16.7), (22.1, 25.3), (29.8, 30)]

    alg = algorithm(images, blackboxes)
    print(alg.blackbox_times())

def algorithm(images, blackboxes):
    pass