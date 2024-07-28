#
# Read test in Elixir.
#
# The traditional read test, i.e. reading a wordlist and search
# for words that match the regular expressions
#   a.*b.*c.*d.*e,
#   b.*c.*d.*e.*f,
#   ...
#
# This Elixir program was created by Hakan Kjellerstrand, hakank@gmail.com
# See also my Elixir page: http://www.hakank.org/elixir/
#

import Enum

defmodule ReadTest do
  
  def test_regex(s,words) do
    IO.puts("\nTesting #{s}")    
    rx = Regex.compile!(s)
    ws = words
         # |> filter(fn w -> Regex.match?(rx,w) end)
         |> filter(fn w -> w =~ rx end)
    len = length(ws)
    if len > 0 do IO.inspect([ws,length(ws)]) end
    {s,length(ws)}
  end

  # For English words (415 835 words)
  #
  # Testing .*a.*b.*c.*d.*e.*
  # [
  #   ["abecedaire", "abecedaries", "abjectedness", "aborticide", "absconded",
  #    "abscondedly", "abscondence", "absconder", "absconders", "abstractedness",
  #    "amblycephalidae", "ambuscade", "ambuscaded", "ambuscader", "ambuscades",
  #    "ambuscadoed", "amebicide", "amoebicide", "bambocciade", "bambochade",
  #    "carbacidometer", "cerambycidae", "nonabstractedness", "oxylabracidae",
  #    "scabicide", "unabstractedness"],
  #   26
  # ]
  # ...
  #  Regex with some matches: [
  #   {".*a.*b.*c.*d.*e.*", 26},
  #   {".*d.*e.*f.*g.*h.*", 1},
  #   {".*e.*f.*g.*h.*i.*", 6},
  #   {".*k.*l.*m.*n.*o.*", 3},
  #   {".*l.*m.*n.*o.*p.*", 23},
  #   {".*r.*s.*t.*u.*v.*", 23}
  # ]
  # elixir read_test.exs  14,12s user 0,94s system 212% cpu 7,089 total
  #
  # There's no match for 6 chars.
  def main(num_chars \\ 5) do
    words = File.read!("words_lower.txt") |> String.split("\n")
    IO.puts("Number of words: #{length(words)}")
    for t <- chunk_every(?a..?z,num_chars,1,:discard) do
      to_string(t)
      |> String.split("")
      |> Enum.intersperse(".*")
      |> join("")
      |> test_regex(words)
    end
    |> filter(fn {_s,len} -> len > 0 end)
    |> IO.inspect(label: "\nRegex with some matches")
  end

  # For Swedish words (391 086 words)
  # Testing .*a.*b.*c.*d.*e.*
  # [
  #   ["abc-stridsmedel", "abc-stridsmedels", "abc-stridsmedlen",
  #    "abc-stridsmedlens", "abc-stridsmedlet", "abc-stridsmedlets", "abducerade",
  #    "abscessbildningen", "abscessbildningens", "arbetscylinder",
  #    "avblockningskondensator", "bakåtblickande", "fabricerade", "fabricerades",
  #    "framåtblickande", "handelsblockaden", "handelsblockadens",
  #    "handelsblockader", "handelsblockaderna", "handelsblockadernas",
  #    "handelsblockaders", "kalibercylinder", "kalibreringscylinder",
  #    "parabolarbetscylinder", "prefabricerade", "snabbtryckande",
  #    "tillbakablickande"],
  #   27
  # ]
  # ...
  # Regex with some matches: [
  #   {".*a.*b.*c.*d.*e.*", 27},
  #   {".*c.*d.*e.*f.*g.*", 22},
  #   {".*d.*e.*f.*g.*h.*", 8},
  #   {".*e.*f.*g.*h.*i.*", 6},
  #   {".*f.*g.*h.*i.*j.*", 1},
  #   {".*h.*i.*j.*k.*l.*", 4},
  #   {".*i.*j.*k.*l.*m.*", 4},
  #   {".*j.*k.*l.*m.*n.*", 14},
  #   {".*k.*l.*m.*n.*o.*", 70},
  #   {".*l.*m.*n.*o.*p.*", 26},
  #   {".*r.*s.*t.*u.*v.*", 76}
  # ]
  #
  # Time:
  # elixir read_test.exs  14,36s user 1,10s system 205% cpu 7,526 total
  #
  #
  # For 6 chars, the only matches are for .*k.*l.*m.*n.*o.*p.*:
  # [
  #   ["alkoholmonopol", "kaliumtetracyanokuprat", "kaliumtetracyanoplatinat",
  #    "komplementoperation", "kulminationspunkt", "vinkelmätningsmikroskop"],
  #   6
  # ]
  #
  def main_swe(num_chars \\ 5) do
    words = File.read!("sv_spelling_org_utf8.txt") |> String.split("\n")
    IO.puts("Number of words: #{length(words)}")
    for t <- chunk_every((?a..?z |> to_list) ++ [?å,?ä,?ö],num_chars,1,:discard) do
      to_string(t)
      |> String.split("")
      |> Enum.intersperse(".*")
      |> join("")
      |> test_regex(words)
    end
    |> filter(fn {_s,len} -> len > 0 end)
    |> IO.inspect(label: "\nRegex with some matches")
  end
  
end

# English
ReadTest.main() # English words, 5 chars
# ReadTest.main(6) # English words, 6 chars. No matches
# ReadTest.main(7) # English words, 7 chars. No matches

# Swedish
# ReadTest.main_swe() # Swedish words, 5 chars
# ReadTest.main_swe(6) # Swedish words, 6 chars
# ReadTest.main_swe(7) # Swedish words, 7 chars. No matches
