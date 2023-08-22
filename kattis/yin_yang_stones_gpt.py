# https://open.kattis.com/problems/yinyangstones
# 1s
# 1.7 Easy


"""
GPT4

This time I only gave the input and the output and asked to generate Python3 code,

Input: WWBWBB  Output: 1
Input: WWWWBBW Output: 0
Input: WBBBBBWWBW Output: 0

"""

"""
1st first attempt

Based on the provided input-output pairs, it seems that the objective is to count the number of occurrences of the pattern "WBB" in the input string. Here's a Python function that generates the output based on the input:

hakank: The output is
Output for input1: 1
Output for input2: 1
Output for input3: 1

which is clearly wrong (as is its interpretation of the problem)
"""
"""
def count_wbb_pattern(input_string):
    return input_string.count("WBB")

input1 = "WWBWBB"
output1 = count_wbb_pattern(input1)
print(f"Output for input1: {output1}")

input2 = "WWWWBBW"
output2 = count_wbb_pattern(input2)
print(f"Output for input2: {output2}")

input3 = "WBBBBBWWBW"
output3 = count_wbb_pattern(input3)
print(f"Output for input3: {output3}")
"""

"""
Then I gave some extra examples:
Input: WB Output:1
Input WWB Output: 0

GPT4 then interpreted this as

Based on the new input-output pairs, it seems the objective is to count the number of "W" characters before the first "B" character appears in the input string.

Which is not correct either. I added the following examples:

Input: BW Output: 1
Input: BWB Output: 0

It then answered:
Based on the updated input-output pairs, it seems the objective is to check whether the number of "W" characters before the first "B" character is odd or even. If odd, the output is 1, otherwise, the output is 0. And the following Python code:

def odd_w_before_first_b(input_string):
    w_count = 0
    for char in input_string:
        if char == 'W':
            w_count += 1
        elif char == 'B':
            break
    return 1 if w_count % 2 != 0 else 0

input1 = "WWBWBB"
output1 = odd_w_before_first_b(input1)
print(f"Output for input1: {output1}")

input2 = "WWWWBBW"
output2 = odd_w_before_first_b(input2)
print(f"Output for input2: {output2}")

input3 = "WBBBBBWWBW"
output3 = odd_w_before_first_b(input3)
print(f"Output for input3: {output3}")

input4 = "WB"
output4 = odd_w_before_first_b(input4)
print(f"Output for input4: {output4}")

input5 = "WWB"
output5 = odd_w_before_first_b(input5)
print(f"Output for input5: {output5}")

input6 = "BW"
output6 = odd_w_before_first_b(input6)
print(f"Output for input6: {output6}")

input7 = "BWB"
output7 = odd_w_before_first_b(input7)
print(f"Output for input7: {output7}")

which is not correct.

I then summarized the input/output pairs and asked it to generate
an explation again (but no Python code):
    
 Based on the provided input-output pairs, it seems that the objective is to determine whether there are an odd number of "W" characters between each pair of consecutive "B" characters in the input string. If there are an odd number of "W" characters between all consecutive "B" characters, return 1. If there are an even number of "W" characters between any consecutive "B" characters, return 0.

After I gave a hint (that it has something to do with counting the numbers of W and B it
gave another explanation:

Determine whether the difference between the number of "W" characters and the
number of "B" characters in the input string is an odd number.
If the difference is odd, return 1. If the difference is even or zero, return 0.

Interesting.

"""


def odd_difference(input_string):
    print()
    w_count = input_string.count('W')
    b_count = input_string.count('B')
    difference = abs(w_count - b_count)
    print(input_string,"difference:",difference)
    return 1 if difference % 2 != 0 else 0


input1 = "WWBWBB"     # 0: shuld be 1 
input2 = "WWWWBBW"    # 1: should be 0  ok
input3 = "WBBBBBWWBW" # 0: should be 0  ok 
input4 = "WB"         # 0: should be 1
input5 = "WWB"        # 1: should be 0
input6 = "BW"         # 0: should be 1
input7 = "BWB"        # 1: should be 0
print(odd_difference(input1))
print(odd_difference(input2))
print(odd_difference(input3))
print(odd_difference(input4))
print(odd_difference(input5))
print(odd_difference(input6))
print(odd_difference(input7))
