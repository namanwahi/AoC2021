import numpy as np

def bin_to_dec(binary):
    return int(sum(val*(2**idx) for idx, val in enumerate(reversed(binary))))

def enhance(image, enhance_alg, fill):
    # pad with 2 layers of the filling to represent infinite space around the image
    image = np.pad(image, ((2, 2), (2, 2)), 'constant', constant_values=fill)

    width, height = image.shape
    new_image = np.zeros_like(image) - 1

    # fill new_image in for all points but outermost layer
    for row in range(1, height - 1):
        for col in range(1, width - 1):
            binary = image[row-1: row+2, col-1: col+2].flatten().tolist()
            assert len(binary) == 9
            decimal = bin_to_dec(binary)
            new_image[row][col] = enhance_alg[decimal]

    return new_image[1:-1, 1:-1]


with open("input.txt", "r") as f:
    split_lines = f.read().split("\n")
    split_lines.remove("")

enhance_alg = (np.array(list(split_lines[0])) == '#').astype(int)
image = (np.array(list(map(list, split_lines[1:]))) == '#').astype(int)

for i in range(50):
    if enhance_alg[0] == 1:
        # infinite flash case
        fill = i % 2
    else:
        fill = 0

    image = enhance(image, enhance_alg, fill)
print(image.sum())
