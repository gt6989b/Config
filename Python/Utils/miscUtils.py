## ----------------------------------------------------------------------------
# \file  miscUtils.py
# \brief Miscellaneous utilities
#
# $URL: https://trstwprsvnapv01.treasury.corp.ge.com/svn/Treasury/QUANTS%20GROUP/Branches/Dima/mdm0/Utils/Python/miscUtils.py $
# $Id: miscUtils.py 18550 2015-02-05 03:35:42Z 212361829 $
#  ------------------------------------------------------------------------------

from collections import Counter, OrderedDict, defaultdict
from itertools   import chain, repeat

def constantFactory(value):
    """
    Return the value specified forever.
    :param value: any object
    :type  value: any type
    :return: value
    :rtype:  same as value
    """
    return repeat(value).next

def chunksGen(aList, chunkSize):
    """
    Yield successive chunks of length chunkSize from aList
        usage: print list(chunksGen(aList = range(10), chunkSize = 4))
           >>> [[0, 1, 2, 3], [4, 5, 6, 7], [8, 9]]
       source: http://stackoverflow.com/a/312464/399573

    :param aList: list to break into chunks
    :type  aList: list of [type]
    :return: list of chunks of aList
    :rtype:  generator of list of [type]
    """
    for i in xrange(0, len(aList), chunkSize):
        yield aList[i:i+chunkSize]

def unfoldGen(func, initArg, numSteps = None):
    """
    Analog of the unfold in functional programming.
    Given a function f, initial argument x and number of steps N,
    create a generator to yield the list [f[x], f[f[x]], f[f[f[x]]], ...]
    of length N. If N = None, the generator is perpetual.
    :param func:     Function to call
    :type  func:     function
    :param initArg:  initial argument to use
    :type  initArg:  type
    :param numSteps: number of steps (None or <1 means forever)
    :type  numSteps: int
    :return:
    :rtype:
    """
    nextArg = initArg
    for _ in repeat(0, # object to repeat, makes no different
                    times = numSteps if numSteps and numSteps > 0 else None):
        nextArg = func(nextArg)
        yield nextArg

def factors(n):
    """
    Return the set of factors of the integer n
        usage: print factors(18)
           >>> set([1, 2, 3, 6, 9, 18])
       source: http://stackoverflow.com/a/6800214/399573
    :param n: integer to factor
    :type  n: int
    :return:  the set of factors of the integer n
    :rtype:   set of [int]
    """
    return set(reduce(list.__add__,
                      ([i, n//i] for i in range(1, int(n**0.5) + 1) \
                          if n % i == 0)))

def flatten(aList):
    """
    Flatten a list of lists
    :param aList: list of lists to flatten
    :type  aList: list of [list of [type]]
    :return: flattened list
    :rtype:  list of [type]
    """
    return list(chain.from_iterable(aList))

def isFloat(s):
    """
    Return float(s) if s is a valid float, else return None.
    Use s.isdigit() to test for positive integers.
    :param   s: item to be converted to float, likely string
    :type    s: unknown type
    :return: resulting float if converted or None if not
    :rtype:  float
    """
    try:
        return float(s)
    except ValueError:
        return None

class OrderedCounter(Counter, OrderedDict):
    """Counter that remembers the order elements are first encountered"""

    def __repr__(self):
         return '%s(%r)' % (self.__class__.__name__, OrderedDict(self))

    def __reduce__(self):
         return self.__class__, (OrderedDict(self),)

class Singleton(type):
    """Class implementing the singleton design pattern.
       Method #3 in
       http://stackoverflow.com/questions/6760685/creating-a-singleton-in-python
       Use as follows:
       class MyClass(BaseClass):
           __metaclass__ = Singleton
    """
    _instances = {}
    def __call__(cls, *args, **kwargs):
        if cls not in cls._instances:
            cls._instances[cls] = super(Singleton, cls).__call__(*args, **kwargs)
        return cls._instances[cls]

class MissingDict(defaultdict):
    """
    Dictionary with default key support. On missing key, it does not insert
    the new key into itself (as defaultdict does) and does not raise
    exceptions (as dict does), instead returning the passed-in factory result.

    Adapted from http://stackoverflow.com/a/17956989/399573
    """
    def __missing__(self, key):
        return self.default_factory()
