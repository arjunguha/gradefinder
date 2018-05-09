gradefinder
===========

This program finds weights for assignments and cutoffs for letter grades while
minimizing the number of students whose grades would change if they received
just one more point. The program runs forever, but every 15 seconds it prints
a new solution.

Usage:


    java -jar gradefinder.jar <grades.csv> <boundaries.csv> <noise> <numSamples>

- `grades.csv` must be the name of a CSV file grades for each assignment. The
  header row must have assignments' names and each grade must be in the range
  0--100.

- `boundaries.csv` must be the name of a CSV file with the header
  `Min,Max,Letter`. The letter grades should be listed in ascending order
  (i.e., A last).

- `noise` must be a real number in the range (0, 1). If you have ten
  assignments, setting noise to 0.006 will select weights between  0.07 and
  0.13 for each assignment.


There is an example `boundaries.csv` file in the root of this repository.

Example output:

```
-- Grade cutoffs ---
C : >= 0
C+ : >= 68
B- : >= 76
B : >= 79
B+ : >= 83
A- : >= 86
A : >= 89
--- Weights ---
Assignment 1: 0.112
Assignment 2: 0.099
Assignment 3: 0.095
Assignment 4: 0.095
Assignment 5: 0.114
Assignment 6: 0.095
Assignment 7: 0.095
Assignment 8: 0.103
Assignment 9: 0.095
Assignment 10: 0.096
-- Solution quality ---
Number of students whose letter grades will change if they receive 1 more point: 12
```