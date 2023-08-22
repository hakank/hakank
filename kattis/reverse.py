# https://open.kattis.com/problems/ofugsnuid
# 1s
# 1.3-1.5 Easy

# 58 chars Not short enough for Top 10 (16..35 chars)
import sys
print("".join(sys.stdin.readlines()[1:][::-1]))

# From GPT4: 61 chars
# print(*reversed([int(input()) for _ in range(int(input()))]))
