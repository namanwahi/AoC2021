from typing import Iterator, List, Tuple
import itertools
import numpy as np
from numpy.linalg import norm
import random

AxisTranslation = Tuple[bool, bool, bool, str, str, str]

def parse_data() -> List[np.ndarray]:
    scanners = []
    with open("input.txt") as f:
        current_scanner = []
        lines = f.readlines()
        for line in lines:
            if line.startswith("--- scanner"):
                continue

            if line == "\n":
                scanners.append(current_scanner)
                current_scanner = []
                continue

            current_scanner.append([int(num) for num in line.split(",")])
    scanners.append(current_scanner)
    return scanners

def get_all_axes_transforms() -> List[Tuple[bool, bool, bool, str, str, str]]:
    """ x,y,z flips and mapping from x->x', y->y', z->z' """
    res = []
    for xFlip in [True, False]:
        for yFlip in [True, False]:
            for zFlip in [True, False]:
                for (x, y, z) in itertools.permutations("xyz", 3):
                    res.append((xFlip, yFlip, zFlip, x, y, z))
    random.shuffle(res)
    return res

def apply_axes_transform(scanner: np.ndarray, xFlip, yFlip, zFlip, xMap, yMap, zMap):
    axes_index = lambda a: "xyz".index(a)

    new_scanner = np.zeros_like(scanner)
    new_scanner[:, axes_index(xMap)] = scanner[:, 0]
    new_scanner[:, axes_index(yMap)] = scanner[:, 1]
    new_scanner[:, axes_index(zMap)] = scanner[:, 2]

    if xFlip:
        new_scanner[:, 0] = -new_scanner[:,0]
    if yFlip:
        new_scanner[:, 1] = -new_scanner[:, 1]
    if zFlip:
        new_scanner[:, 2] = -new_scanner[:, 2]

    return new_scanner


def do_scanners_overlap(reference_scanner, other_scanner):
    for ref_beacon in reference_scanner:
        for other_beacon in other_scanner:
            scanner_translation = (ref_beacon - other_beacon)
            translated_other_scanner = other_scanner + scanner_translation
            all_beacons = np.vstack([reference_scanner, translated_other_scanner])
            unique_beacons, counts = np.unique(all_beacons, axis=0, return_counts=True)
            overlapping_beacons = unique_beacons[counts > 1]
            if overlapping_beacons.shape[0] >= 12:
                return translated_other_scanner, scanner_translation
    return None

def find_overlap(reference_scanners, all_scanners, tried_pairs, scanners_offsets):
    for ref_id, ref_scanner in reference_scanners.items():
        for other_id, other_scanner in enumerate(all_scanners):
            if other_id in reference_scanners:
                continue

            if ((ref_id, other_id) in tried_pairs) or ((other_id, ref_id) in tried_pairs):
                continue
            else:
                tried_pairs.add((ref_id, other_id))

            for axis_transform in get_all_axes_transforms():
                overlap_test = do_scanners_overlap(ref_scanner, apply_axes_transform(other_scanner, *axis_transform))
                if overlap_test is not None:
                    (translated_other_scanner, scanner_translation) = overlap_test
                    reference_scanners[other_id] = translated_other_scanner
                    scanners_offsets[other_id] = scanner_translation
                    return


if __name__ == "__main__":
    scanners = list(map(np.array, parse_data()))
    # scanners rotated and translated to align with scanner 0
    reference_scanners = {
        0: scanners[0]
    }

    # scanner offsets
    scanners_offsets = {
        0: np.array([0, 0, 0])
    }

    tried_pairs = set()
    i = 0
    while len(reference_scanners) < len(scanners):
        print("iter: ", i)
        i += 1
        find_overlap(reference_scanners, scanners, tried_pairs, scanners_offsets)

    stacked_values = np.vstack(list(reference_scanners.values()))

    unique = np.unique(stacked_values, axis=0)

    print(unique.shape[0])

    max_dist = 0
    for v1 in scanners_offsets.values():
        for v2 in scanners_offsets.values():
            max_dist = max(max_dist, np.linalg.norm(v1 - v2, ord=1))
    print(max_dist)