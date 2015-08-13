#!/usr/bin/python

##----------------------------------------------------------------------------##
## \file  blockSampler.py                                                     ##
## \brief Distribution sampler respecting correlation                         ##
##                                                                            ##
## $URL: https://trstwprsvnapv01.treasury.corp.ge.com/svn/Treasury/QUANTS%20GROUP/Branches/Dima/mdm0/Utils/Python/Maths/blockSampler.py $
## $Id: blockSampler.py 23307 2015-05-26 21:42:05Z 212361829 $                ##
##----------------------------------------------------------------------------##

from numpy import zeros

class BlockSampler(object):
    def __init__(self, rng, investmentHorizon, windowSize, historicalData):
        """
        Constructor
        :param rng:               Random number generator
        :type  rng:               Maths.randNumGen
        :param investmentHorizon:
        :type  investmentHorizon:
        :param windowSize:
        :type  windowSize:
        :param historicalData:
        :type  historicalData:
        :return:
        :rtype:
        """
        self.rng     = rng
        self.horizon = investmentHorizon
        self.window  = windowSize
        self.prices  = historicalData
        self.returns = None

    @property
    def rng(self):
        """
        :return: rng
        :rtype:  int
        """
        return self._rng

    @rng.setter
    def rng(self, newRNG):
        """
        :param  newRNG: New random number generator
        :type   newRNG: ?
        :return: Nothing
        :rtype:  None
        """
        self._rng = newRNG

    @property
    def horizon(self):
        """
        :return: rng
        :rtype:  int
        """
        return self._horizon

    @horizon.setter
    def horizon(self, investmentHorizon):
        """
        :param  investmentHorizon: New investment horizon
        :type   investmentHorizon: int
        :return: Nothing
        :rtype:  None
        """
        self._horizon = investmentHorizon

    @property
    def window(self):
        """
        :return: rng
        :rtype:  int
        """
        return self._window

    @window.setter
    def window(self, newWindowSize):
        """
        :param  newWindowSize: New random number generator
        :type   newWindowSize: int
        :return: Nothing
        :rtype:  None
        """
        self._window = newWindowSize

    def calcPaths(self, numberPaths, overlap,
                        investmentHorizon = None, windowSize = None):
        """
        Generate paths from the historically inspired distribution
        :param investmentHorizon: total path length
        :type  investmentHorizon: int
        :param windowSize:        window size
        :type  windowSize:        int
        :param numberPaths:       number of paths to generate
        :type  numberPaths:       int
        :param overlap:           non-overlap intervals or decay overlap
        :type  overlap:           bool
        :return:  Matrix of generated paths
        :rtype:   np.array
        """
        if investmentHorizon:
            self.horizon = investmentHorizon

        if windowSize:
            self.window  = windowSize

        if overlap:
            self.calcReturnsOverlap()
        else:
            self.calcReturnsNonOverlap()

        return self.recalcPaths(numberPaths = numberPaths)

    def calcReturnsOverlap(self):
        self.returns = self.prices

    def calcReturnsNonOverlap(self):
        self.returns = self.prices

    def recalcPaths(self, numberPaths):
        """
        Generate paths from the historically inspired distribution
        :param numberPaths:       number of paths to generate
        :type  numberPaths:       int
        :return:  Matrix of generated paths
        :rtype:   np.array
        """
        return zeros(numberPaths)
