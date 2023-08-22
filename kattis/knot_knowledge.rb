# https://open.kattis.com/problems/knotknowledge
# 1s
# 1.4 Easy

# 71 chars
s=$<.read.split.map(&:to_i)
l,k=s[1..].each_slice(s[0]).to_a
p (l-k)[0]

# 65 chars: Not short enough for Top 10 (29..62)
# s=$<.read.split.map(&:to_i)
# l=s[1..s[0]];k=s[s[0]+1..]
# p (l-k)[0]
