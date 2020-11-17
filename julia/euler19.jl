#=
  Euler #19 in Julia.

  Problem 19:
  """
  You are given the following information, but you may prefer
  to do some research for yourself.

  * 1 Jan 1900 was a Monday.
  * Thirty days has September,
    April, June and November.
    All the rest have thirty-one,
    Saving February alone,
    Which has twenty-eight, rain or shine.
    And on leap years, twenty-nine.
  * A leap year occurs on any year evenly divisible by 4, but not
    on a century unless it is divisible by 400.

  How many Sundays fell on the first of the month during the
  twentieth century (1 Jan 1901 to 31 Dec 2000)?
  """


  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")

using Dates

# 0.00001321s
function euler19a()
    count = 0;
    for year in 1901:2000
        for month in 1:12
            if Dates.dayofweek(Date(year, month, 1)) == 7
                count += 1
            end
        end
    end

    return count
end

run_euler(euler19a)
