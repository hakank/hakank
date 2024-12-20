# https://open.kattis.com/problems/tripodometer
# 1s
# 2.0 Easy

# Generated by GPT4

"""
# Version 1: Incorrect output for the first sample. Correct on #2..4
def main():
    # Read the input
    N = int(input().strip())
    distances = list(map(int, input().strip().split()))

    # Find the total distance and the min and max distances
    total_distance = sum(distances)
    min_distance = min(distances)
    max_distance = max(distances)

    # Create a set of possible distances
    possible_distances = set()

    # Iterate through the distances
    for distance in distances:
        # Calculate the remaining distance
        remaining_distance = total_distance - distance

        # Check if the remaining distance is valid
        if remaining_distance - min_distance >= max_distance:
            possible_distances.add(remaining_distance)

    # Output the results
    print(len(possible_distances))
    print(' '.join(map(str, sorted(possible_distances))))

if __name__ == "__main__":
    main()
"""

# Version 2: Wrong on #1..3
def main():
    # Read the input
    N = int(input().strip())
    distances = list(map(int, input().strip().split()))

    # Calculate the total distance and sort the distances
    total_distance = sum(distances)
    distances.sort()

    # Calculate the sum of distances excluding the minimum and maximum
    sum_without_min = total_distance - distances[0]
    sum_without_max = total_distance - distances[-1]

    # Create a set of possible distances
    possible_distances = set()

    # Iterate through the distances
    for distance in distances:
        # Calculate the remaining distance
        remaining_distance = total_distance - distance

        # Check if the remaining distance is valid
        if remaining_distance >= sum_without_min or remaining_distance <= sum_without_max:
            possible_distances.add(remaining_distance)

    # Output the results
    print(len(possible_distances))
    print(' '.join(map(str, sorted(possible_distances))))

if __name__ == "__main__":
    main()
