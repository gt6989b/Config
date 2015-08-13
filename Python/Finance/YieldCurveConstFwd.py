#!/usr/bin/python

##----------------------------------------------------------------------------##
## \file  YieldCurveConstFwd.py                                               ##
## \brief Yield curve interpolation assuming piecewise constant forward curve ##
##                                                                            ##
## $URL: https://trstwprsvnapv01.treasury.corp.ge.com/svn/Treasury/QUANTS%20GROUP/Branches/Dima/mdm0/Utils/Python/Finance/YieldCurveConstFwd.py $
## $Id: YieldCurveConstFwd.py 20512 2015-03-17 17:10:14Z 212361829 $                  ##
##----------------------------------------------------------------------------##

from scipy.interpolate import interp1d

import numpy as np, YieldCurve as yc

class YieldCurveConstFwd(yc.YieldCurve):

    def __init__(self, times, zeros = None, discounts = None, log = None):
        """
        Yield curve constructor
        :param times:      list of times for initial curve data
        :type  times:      list of [float]
        :param zeros:      list of zero rates to initialize from, or None
        :type  zeros:      list of [float]
        :param discounts:  list of discount factors to init from, or None
        :type  discounts:  list of [float]
        :param log:        logging class
        :type  log:        logging.Logger
        :return: Nothing
        :rtype:  None
        """
        super(YieldCurveConstFwd, self).__init__(times, zeros, discounts, log)

        self._lndiscInterp\
            = interp1d(self.T, self.lndisc,  kind       = 'linear',
                       bounds_error = False, fill_value = np.nan)

        self._fwdInterp = interp1d(self.T, self.avgFwds, kind       = 'zero',
                                   bounds_error = False, fill_value = self.Fmax)

    @property
    def lndiscInterp(self):
        """
        :return: ln(disc) = -r*t interpolator object
        :rtype:  interp1d
        """
        return self._lndiscInterp

    @property
    def fwdInterp(self):
        """
        :return: forward rate interpolator object
        :rtype:  interp1d
        """
        return self._fwdInterp

    def getZeros(self, times):
        """
        Calculate zero rates at times
        :param times: list of times to compute zero rates for
        :type  times: list of [float]
        :return: Computed zero rates
        :rtype:  np.array
        """
        T = yc.convertTimes(times = times, log = self.log)
        T[T == 0.] = 1.e-16 # replace by epsilon to allow division
        zeros = -self.lndiscInterp(T) / T

        # short end should not be extrapolated here, but in the interp object
        # zeros[T < self.Tmin] = self.zeros[0] # short end - flat extrapolation
        zeros[T > self.Tmax] = self.Zmax # long end extrapolation: flat in rate

        yc.validateOutput(values = zeros,       times = T,
                          descr  = 'Zero rate', log   = self.log)
        return zeros

    def getLnDisc(self, times):
        """
        Calculate ln(discount) = r*t factor at times
        :param times: list of times to compute ln(discount factors) for
        :type  times: list of [float]
        :return: Computed ln(discount factors)
        :rtype:  np.array
        """
        T      = yc.convertTimes(times = times, log = self.log)
        lndisc = self.lndiscInterp(T)
        # short end should not be extrapolated here but in the interp object

        # long end extrapolation
        idx = (T > self.Tmax) # indices that need long end extrapolation
        if idx.size > 0:
            lndisc[idx] = self.Fmax * (self.Tmax - T[idx]) - self.zTmax

        yc.validateOutput(values = lndisc,     times = T,
                          descr  = 'Discount', log   = self.log)
        return lndisc

    def getFwds(self, times, baseTime = None):
        """
        Calculate implied forward rates at times, in the future baseTime
        :param times: list of times to compute forwards for
        :type  times: list of [float]
        :param baseTime: starting time to compute implied forward rates from
        :type  baseTime: float
        :return: Computed implied forward rates
        :rtype:  np.array
        """
        if baseTime:
            T      = yc.convertTimes(times = times+[baseTime], log = self.log)
            lndisc = self.getLnDisc(T)
            fwds   = (lndisc[:-1] - lndisc[-1])/(T[-1] - T[:-1])
        else:
            T    = yc.convertTimes(times = times, log = self.log)
            fwds = self.fwdInterp(T)

        yc.validateOutput(values = fwds,       times = T,
                          descr  = 'Fwd rate', log   = self.log)
        return fwds
