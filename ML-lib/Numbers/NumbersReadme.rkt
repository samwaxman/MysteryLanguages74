#lang racket
#|Numbers Readme:

The numbers languages are defined as follows:
Numbers1.rkt Treats all numbers as exact. 0/0 is NaN. Anything else divided by 0 is infinity or negative infinity.
Numbers2.rkt Treats all numbers as inexact. All number operations are like floating point equivalents.
Numbers3.rkt Treats integers as exact and decimals as inexact. Exact integers are represented with 64 bits.
After exceeding the max int or going beneath the min int of 64 bits, the numbers will wrap around. Normal expected
behavior for floating point contagion. If both arguments to division are exact integers, performs integer division.

All of these languages print numbers out in decimal form. Thus to tell them apart, you must (e.g.) look for NaN
or floating point errors.

The comminalities of each file are found in NumbersSetup. These are the first languages, so they setup the printer
and provide the test functions. No other languages should do this.|#
