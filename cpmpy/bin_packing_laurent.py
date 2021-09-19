"""
Bin packing problem in cpmpy.

This is a fairly faithful port of Laurent Perron's OR-tools CP-SAT model
from https://groups.google.com/g/or-tools-discuss/c/-LfLqZezJ78/m/joeTpWEvAgAJ

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/

"""
from cpmpy import *
import numpy as np
from cpmpy_hakank import *


class BinPacking:
    BOX_COEFFICIENT = 10000000

    def __init__(self):
        self.items = {
            1: {'volume': 33, 'weight': 50, 'quantity': 1},
            2: {'volume': 45, 'weight': 30, 'quantity': 1},
            3: {'volume': 10, 'weight': 20, 'quantity': 1},
            4: {'volume': 12, 'weight': 25, 'quantity': 1},
            5: {'volume': 1, 'weight': 2, 'quantity': 1},
            6: {'volume': 22, 'weight': 28, 'quantity': 1},
            7: {'volume': 50, 'weight': 68, 'quantity': 1},
        }

        self.boxes = {
            1: {'volume': 40, 'weight': 600},
            2: {'volume': 100, 'weight': 1000},
            3: {'volume': 5, 'weight': 1000},
            4: {'volume': 150, 'weight': 500},
            5: {'volume': 120, 'weight': 1000},
        }

    def main(self):

        all_items = range(1, len(self.items) + 1)
        all_boxes = range(1, len(self.boxes) + 1)

        x = {}
        for i in all_items:
            for j in all_boxes:
                x[(i, j)] = intvar(0, self.items[i]['quantity'], name='x_%i_%i' % (i, j))

        y = {}
        for j in all_boxes:
            y[j] = boolvar(name='y[%i]' % j)

        model = Model(minimize= sum(y[j] * (self.BOX_COEFFICIENT + self.boxes[j]['volume']) for j in all_boxes))
        
        for i in all_items:
            model += (sum(x[i, j] for j in self.boxes) == self.items[i]['quantity'])

        for j in all_boxes:
            model += (sum(x[(i, j)] * self.items[i]['volume'] for i in all_items) <= y[j] * self.boxes[j]['volume'])
            model += (sum(x[(i, j)] * self.items[i]['weight'] for i in all_items) <= y[j] * self.boxes[j]['weight'])



        ss = CPM_ortools(model)
        ss.ort_solver.parameters.log_search_progress = True
        ss.ort_solver.parameters.num_search_workers = 1 # Can be 8 or more for larger problems
        ss.ort_solver.parameters.linearization_level = 2 # Useful if the number of workers is 1

        if ss.solve():
            num_bins = 0
            for j in all_boxes:
                if y[j].value():
                    bin_items = []
                    bin_volume = 0
                    bin_weight = 0
                    for i in all_items:
                        solution_value = x[i, j].value()
                        if solution_value > 0:
                            bin_items.append({i: solution_value})
                            bin_volume += solution_value * self.items[i]['volume']
                            bin_weight += solution_value * self.items[i]['weight']
                    if bin_volume > 0:
                        num_bins += 1
                        print('Box number', j)
                        print('  Items packed:', bin_items)
                        print('  Total volume:', bin_volume)
                        print('  Total weight:', bin_weight)
                        print()
            print()
            print('Number of boxes used:', num_bins)
            # print('Time = ', solver.WallTime(), ' seconds')
            print('Status = ', ss.status())            
        else:
            print('The problem does not have an optimal solution.')

bp = BinPacking()
bp.main()

