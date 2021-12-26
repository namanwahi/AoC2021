
from itertools import product, zip_longest
from typing import Dict

def get_instructions():
    with open("input.txt", "r") as f:
        return list(map(lambda x: x.split() , f.read().splitlines()))

def grouper(iterable, n, fillvalue=None):
    "Collect data into non-overlapping fixed-length chunks or blocks"
    args = [iter(iterable)] * n
    return list(zip_longest(*args, fillvalue=fillvalue))

def eval_arg(arg: str, state: Dict[str, int]):
    if arg in state:
        return state[arg]
    return int(arg)

NUM_GROUPS = 14

def naive_monad(model_num):
    assert len(model_num) == 14

    model_num = model_num.copy()

    state = {'w': 0, 'x': 0, 'y': 0, 'z': 0}
    for i, instruction in enumerate(get_instructions()):
        operation = instruction[0]
        args = tuple(instruction[1:])
        if operation == 'inp':
            (reg,) = args
            state[reg] = model_num.pop(0)
        elif operation == 'add':
            (reg, a) = args
            state[reg] += eval_arg(a, state)
        elif operation == 'mul':
            (reg, a) = args
            state[reg] *= eval_arg(a, state)
        elif operation == 'div':
            (reg, a) = args
            state[reg] //= eval_arg(a, state)
        elif operation == 'mod':
            (reg, a) = args
            state[reg] %= eval_arg(a, state)
        elif operation == "eql":
            (reg, a) = args
            state[reg] = int(eval_arg(a, state) == state[reg])
        else:
            assert False, operation
    return state['z']

ns = [13, 11, 15, -11, 14, 0, 12, 12, 14, -6, -10, -12, -3, -5]
ms = [1,  1,  1,   26, 1, 26, 1,  1,  1,  26, 26,  26,  26, 26]
os = [13, 10, 5, 14, 5, 15, 4, 11, 1, 15, 12, 8, 14, 9]
# when ms==1, there is no value of 1<=w<=9. Therefore it's not needed.
def run_group(z, w, group_idx):
    if ((z % 26) + ns[group_idx]) == w:
        # z needs to decrease
        assert ms[group_idx] == 26
        newZ = (z // ms[group_idx])
    else:
        # z needs to increase
        assert ms[group_idx] == 1
        newZ = ((z // ms[group_idx]) * 26) + w + os[group_idx]

    return newZ


def fast_monad(model_num):
    assert len(model_num) == 14

    z = 0

    for group_idx in range(NUM_GROUPS):
        w = model_num[group_idx]
        z = run_group(z, w, group_idx)
    return z

def valid(increase_digits):
    z_needs_to_increase = [m == 1 for m in ms]
    model_num = [-1] * 14
    digits_index = 0

    z = 0
    for i in range(14):
        if z_needs_to_increase[i]:
            w = increase_digits[digits_index]
            digits_index += 1
        else:
            w = (z % 26) + ns[i]
            if not (1 <= w <=9):
                return None

        model_num[i] = w
        z = run_group(z, w, i)

    if z == 0:
        return model_num

    return None



if __name__ == "__main__":
    instructions = get_instructions()
    group_size = 18
    assert len(instructions) % group_size == 0

    # inspect each instruction group. They follow a pattern
    grouped_instructions = grouper(instructions, 18)

    # part 1
    for increase_digits in product(range(9, 0, -1), repeat=7):
        model_num = valid(increase_digits)
        if model_num:
            print("".join(map(str,model_num)))
            break

    # part 2
    for increase_digits in product(range(1, 10), repeat=7):
        model_num = valid(increase_digits)
        if model_num:
            print("".join(map(str,model_num)))
            break

    z = fast_monad(model_num)
    print(z)