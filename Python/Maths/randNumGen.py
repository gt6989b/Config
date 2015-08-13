#!/usr/bin/python

##----------------------------------------------------------------------------##
## \file  randNumGen.py                                                       ##
## \brief Random number generator                                             ##
##                                                                            ##
## $URL: https://trstwprsvnapv01.treasury.corp.ge.com/svn/Treasury/QUANTS%20GROUP/Branches/Dima/mdm0/Utils/Python/Maths/randNumGen.py $
## $Id: randNumGen.py 23399 2015-05-28 19:58:41Z 212361829 $                  ##
##----------------------------------------------------------------------------##

from numpy import random

class RandNumGen(object):
    def __init__(self, seed = 1):
        self._seed = None
        self.seed  = seed

    @property
    def seed(self):
        """
        :return: seed
        :rtype:  int
        """
        return self._seed

    @seed.setter
    def seed(self, seed):
        """
        :param  seed: New seed
        :type   seed: unsigned long
        :return: Nothing
        :rtype:  None
        """
        self._seed = seed
        random.seed(seed = seed)

    @staticmethod
    def uniformInt(low, high = None, size = None, replace = True):
        """
        Random integers from low to high
        If high is valid, then from [low, high). If high = None, from [0, low).
        :param low:     boundary of the interval to draw from
        :type  low:     int
        :param high:    boundary of the interval to draw from
        :type  high:    int
        :param size:    Output matrix shape
        :type  size:    int or tuple of ints
        :param replace: Drawing with replacement (True) or without (False)
        :type  replace: bool
        :rtype:
        """
        if size is None:
            size = 1

        if replace or (size == 1):
            return random.randint(low = low, high = high, size = size)

        if high is None:
            data = range(low)
        else:
            data = range(low, high)
        return random.choice(a = data, size = size, replace = False)
