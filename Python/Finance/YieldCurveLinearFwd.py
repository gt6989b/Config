#!/usr/bin/python

##----------------------------------------------------------------------------##
## \file  YieldCurveLinearFwd.py                                              ##
## \brief Yield curve interpolation assuming piecewise linear forward curve   ##
##                                                                            ##
## $URL: https://trstwprsvnapv01.treasury.corp.ge.com/svn/Treasury/QUANTS%20GROUP/Branches/Dima/mdm0/Utils/Python/Finance/YieldCurveLinearFwd.py $
## $Id: YieldCurveLinearFwd.py 20549 2015-03-17 20:25:55Z 212361829 $                  ##
##----------------------------------------------------------------------------##

from scipy.interpolate import interp1d

import numpy as np, YieldCurve as yc

class YieldCurveLinearFwd(yc.YieldCurve):

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
        super(YieldCurveLinearFwd, self).__init__(times, zeros, discounts, log)

        # compute the instantaneous forward rates
        # avgFwds has the avg for each interval,
        # since (start+end)/2 = avg we use end = 2*avg - start
        # but prepend by first value an extra time b/c of shift from avg to inst
        self._fwds = np.append(self.avgFwds[0], 2.*self.avgFwds[:-1])
        for i in range(1, len(self.fwds)):
            self._fwds[i] -= self._fwds[i-1]

        self._fwdInterp\
            = interp1d(self.T, self.fwds,    kind       = 'linear',
                       bounds_error = False, fill_value = self.fwds[-1])

    @property
    def fwds(self):
        """
        :return: Instantaneous forward rates at break points
        :rtype:  np.array
        """
        return self._fwds

    @property
    def fwdInterp(self):
        """
        :return: forward rate interpolator object
        :rtype:  interp1d
        """
        return self._fwdInterp

    def getLnDisc(self, times):
        """
        Calculate ln(discount) = r*t factor at times
        :param times: list of times to compute ln(discount factors) for
        :type  times: list of [float]
        :return: Computed ln(discount factors)
        :rtype:  np.array
        """
        T      = yc.convertTimes(times = times, log = self.log)
        fwds   = self.getFwds(T) # instantaneous fwd rates at times T
        lowIdx = np.maximum(self.T.searchsorted(T) - 1, 0)

        # if L_t = -t*r_t = ln(discount factor) at time t, then
        # L_t = L_0 + (t - t_0)(f_t + f_0)/2
        # where _0 denotes everything at previous breaking point
        lndisc = self.lndisc[lowIdx] \
                 - 0.5*(T - self.T[lowIdx])*(fwds + self.fwds[lowIdx])

        yc.validateOutput(values = lndisc,       times = T,
                          descr  = 'LnDiscount', log   = self.log)
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
