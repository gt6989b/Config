#!/usr/bin/python

##----------------------------------------------------------------------------##
## \file  test_randNumGen.py                                                  ##
## \brief Random number generator unit test                                   ##
##                                                                            ##
## $URL: https://trstwprsvnapv01.treasury.corp.ge.com/svn/Treasury/QUANTS%20GROUP/Branches/Dima/mdm0/Utils/Python/Maths/test/test_randNumGen.py $
## $Id: test_randNumGen.py 23400 2015-05-28 20:01:34Z 212361829 $             ##
##----------------------------------------------------------------------------##

from Maths.randNumGen import RandNumGen

import unittest

class TestRandNumGen(unittest.TestCase):
    def test_uniformInt(self):
        for seed in range(1, 127, 11):
            rng = RandNumGen(seed = seed)
            for testSize in range(100, 250):
                withRepl = rng.uniformInt(low     = 0,     high = testSize,
                                          replace = False, size = testSize)
                resultSize = len(list(set(withRepl)))
                self.assertEqual(first  = resultSize,
                                 second = testSize,
                                 msg    = 'w/o repl seed %d size %d != %d'\
                                          % (seed, testSize, resultSize))

                withoutRepl = rng.uniformInt(low  = 0,
                                             high = testSize,
                                             size = testSize)
                resultSize = len(list(set(withoutRepl)))
                self.assertLess(a    = resultSize,
                                b    = testSize,
                                msg  = 'with repl seed %d size %d <= %d' \
                                       % (seed, testSize, resultSize))

if __name__ == '__main__':
    unittest.main()
