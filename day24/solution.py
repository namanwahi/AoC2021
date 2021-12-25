
from itertools import permutations, zip_longest
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
    model_num = list(str(model_num))

    state = {'w': 0, 'x': 0, 'y': 0, 'z': 0}
    for i, instruction in enumerate(get_instructions()[:NUM_GROUPS * 18]):
        operation = instruction[0]
        args = tuple(instruction[1:])
        if operation == 'inp':
            (reg,) = args
            state[reg] = int(model_num.pop(0))
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
        if (NUM_GROUPS - 1) * 18 <= i <= (NUM_GROUPS + 1) * 18:
            print(instruction, state)
    return state['z']

ns = [13, 11, 15, -11, 14, 0, 12, 12, 14, -6, -10, -12, -3, -5]
ms = [1, 1, 1, 26, 1, 26, 1, 1, 1, 26, 26, 26, 26, 26]
os = [13, 10, 5, 14, 5, 15, 4, 11, 1, 15, 12, 8, 14, 9]

def run_group(z, w, group_idx):
    x = ((z % 26) + ns[group_idx]) != w

    if x:
        z = ((z // ms[group_idx]) * 26) + w + os[group_idx]
    else:
        z = (z // ms[group_idx])

    return z

def fast_monad(model_num):
    assert len(model_num) == 14
    model_num = list(str(model_num))

    z = 0

    for group_idx in range(NUM_GROUPS):
        w = int(model_num.pop(0))

        z = run_group(z, w, group_idx)

    return z


if __name__ == "__main__":
    instructions = get_instructions()
    group_size = 18
    assert len(instructions) % group_size == 0

    # inspect each instruction group. They follow a pattern
    grouped_instructions = grouper(instructions, 18)
    for i in range(group_size):
        print("Group at index ", i)
        for group in grouped_instructions:
            print(group[i])
        print("-----------")

    print(naive_monad("13579246899999"))
    print(fast_monad("13579246899999"))


