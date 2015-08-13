#!/usr/bin/python

##----------------------------------------------------------------------------##
## \file  YieldCurve.py                                                       ##
## \brief Yield curve modeling class                                          ##
##                                                                            ##
## Describes the yield curve class, used for rate conversions                 ##
##                                                                            ##
## $URL: https://trstwprsvnapv01.treasury.corp.ge.com/svn/Treasury/QUANTS%20GROUP/Branches/Dima/mdm0/Utils/Python/Finance/YieldCurve.py $
## $Id: YieldCurve.py 20549 2015-03-17 20:25:55Z 212361829 $                  ##
##----------------------------------------------------------------------------##

from abc import ABCMeta, abstractmethod

import numpy as np

class YieldCurve(object):
    """
    This is an abstract yield curve interpolation class.
    Inherit from this and implement you own functions for each interpolation
    type.
    """
    __metaclass__ = ABCMeta

    def __init__(self, times, zeros = None, discounts = None, log = None):
        """
        Yield curve constructor
        :param times:      list of times for initial curve data
        :type  times:      list of [float]
        :param zeros:      list of zero rates to initialize from, or None
        :type  zeros:      list of [float]
        :param discounts:  list of discount factors to init from, or None
        :type  discounts:  list of [float]
        :param log:        logging object
        :type  log:        logging.Logger
        :return: Nothing
        :rtype:  None
        """
        self._log = log
        if zeros is None:
            if discounts is None:
                msg = 'Yield curve must be initialized with rates or discounts'
                if self.log:
                    self.log.error(msg)
                raise ValueError(msg)

            self._T, self._lndisc = toSortedArrays(times  = times,
                                                   values = np.log(discounts))
            if self.T[0] <= 0.: # t<0 could be small error
                if self.T.size == 1:
                    self._zeros = zeros(1)  # single pt at t=0 can be anything
                else:
                    self._zeros = -self.lndisc
                    self.zeros[1:] /= self.T[1:]
                    self.zeros[0] = self.zeros[1]  # flat extrapolation
            else:
                self._zeros = -self.lndisc / self.T
        else:
            self._T, self._zeros = toSortedArrays(times = times, values = zeros)
            self._lndisc = -self.T * self.zeros

        if self.T[0] > 0:
            self._T      = np.append(np.zeros(1), self.T)
            self._lndisc = np.append(np.zeros(1), self.lndisc)
            self._zeros  = np.append(np.zeros(1), self.zeros)

        # zero rate extrema
        self._Tmin = 0.
        self._Tmax = self.T[-1]
        self._Zmax = self.zeros[-1]
        self._zTmax = -self.lndisc[-1]

        # set up forward rate data
        self._Fmax    = self.Zmax  # extrapolate flat to the right
        rawAvgFwds    = -np.diff(self.lndisc) / np.diff(self.T)
        self._avgFwds = np.append(rawAvgFwds, rawAvgFwds[-1])

    @property
    def log(self):
        """
        :return: logging object
        :rtype:  logging.Logger
        """
        return self._log

    @log.setter
    def log(self, newLog):
        """
        :param  newLog: New logging object to set to
        :type   newLog: logging.Logger
        :return: Nothing
        :rtype:  None
        """
        self._log = newLog

    @property
    def T(self):
        """
        :return: Array of times
        :rtype:  np.array
        """
        return self._T

    @property
    def lndisc(self):
        """
        :return: Array of -r*t, logarithms of discount factor
        :rtype:  np.array
        """
        return self._lndisc

    @property
    def zeros(self):
        """
        :return: Array of zero rates
        :rtype:  np.array
        """
        return self._zeros

    @property
    def Tmin(self):
        """
        :return: Minimum time
        :rtype:  float
        """
        return self._Tmin

    @property
    def Tmax(self):
        """
        :return: Maximum time
        :rtype:  float
        """
        return self._Tmax

    @property
    def Fmax(self):
        """
        :return: Instantaneous forward rate at the maximum time
        :rtype:  float
        """
        return self._Fmax

    @property
    def Zmax(self):
        """
        :return: Zero rate at the maximum time
        :rtype:  float
        """
        return self._Zmax

    @property
    def zTmax(self):
        """
        :return: ln(disc) = -r*t at the maximum time
        :rtype:  float
        """
        return self._zTmax

    @property
    def avgFwds(self):
        """
        :return: Average forward rates at break points
        :rtype:  np.array
        """
        return self._avgFwds

    def getZeros(self, times):
        """
        Calculate zero rates at times
        :param times: list of times to compute zero rates for
        :type  times: list of [float]
        :return: Computed zero rates
        :rtype:  np.array
        """
        T = convertTimes(times = times, log = self.log)
        T[T == 0.] = 1.e-16 # replace by epsilon to allow division
        return -self.getLnDisc(times = T)/T

    def getLnDisc(self, times):
        """
        Calculate ln(discount) = r*t factor at times
        :param times: list of times to compute ln(discount factors) for
        :type  times: list of [float]
        :return: Computed ln(discount factors)
        :rtype:  np.array
        """
        return np.log(self.getDisc(times = times))

    def getDisc(self, times):
        """
        Calculate discount factor at times
        :param times: list of times to compute discount factors for
        :type  times: list of [float]
        :return: Computed discount factors
        :rtype:  np.array
        """
        return np.exp(self.getLnDisc(times = times))

    @abstractmethod
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
        pass

def yieldCurveFactory(times, zeros=None, discounts=None, log=None,
                             type = 'constFwd'):
    """
    Yield curve factory
    :param times:      list of times for initial curve data
    :type  times:      list of [float]
    :param zeros:      list of zero rates to initialize from, or None
    :type  zeros:      list of [float]
    :param discounts:  list of discount factors to init from, or None
    :type  discounts:  list of [float]
    :param log:        logging class
    :type  log:        logging.Logger
    :param type:       yield curve interpolation type: constFwd, linearFwd
    :type  type:       str
    :return:  Initialized inteprolated yield curve implementation
    :rtype:   YieldCurve
    """
    from YieldCurveConstFwd  import YieldCurveConstFwd
    from YieldCurveLinearFwd import YieldCurveLinearFwd

    if type == 'constFwd':
        cls = YieldCurveConstFwd
    else:
        cls = YieldCurveLinearFwd

    return cls(times = times, zeros = zeros, discounts = discounts, log = log)

def toSortedArrays(times, values):
    """
    Convert lists of times and values to a pair of sorted arrays
    :param times:   Times list
    :type  times:   list of [float]
    :param values:  Value list
    :type  values:  list of [float]
    :return: (times, values) arrays as a tuple
    :rtype:  (np.array, np.array)
    """
    T = np.array(times)
    if np.all(np.diff(T) > 0.):
        V = np.array(values)
    else:
        T, V = [np.array(L) for L in zip(*sorted(zip(times, values)))]
    return T, V

def convertTimes(times, log):
    T = np.array(times)
    if (T < 0.).any():
        msg = 'Invalid negative interp time in %s' % str(times)
        if log:
            log.error(msg)
        raise KeyError(msg)
    return T

def validateOutput(values, times, descr, log):
    if np.isnan(values).any():
        msg = '%s interp error at t=%f: NaN found' \
                        % (descr, times[np.isnan(values)][0])
        if log:
            log.error(msg)
        raise RuntimeError(msg)

def testYieldCurve(cls):
    className = cls.__name__
    print className + ' testing started'

    t = range(1,11)
    z = [0.05, 0.05, 0.05, 0.05, 0.05, 0.06, 0.06, 0.06, 0.06, 0.06]
    ycZ = cls(times = t, zeros = z)
    lnDisc = -ycZ.getLnDisc(times = [4.8, 5.5, 6.5, 9.9, 10.1])
    lnDiscStd = {'YieldCurveConstFwd' :[0.24, 0.305, 0.39,   0.594,  0.606],
                 'YieldCurveLinearFwd':[0.24, 0.29,  0.4175, 0.5841, 0.617]}
    if (np.abs(lnDisc - lnDiscStd[className]) > 1.e-15).any():
        print '0: Hagan-West example failed'
        return False

    # first test -- reproduce its own values going zero -> disc -> zero
    t = [0.25, 0.5, 1., 2., 5., 10.]
    z = [0.03, 0.04, 0.03, 0.03, 0.02, 0.03]
    ycZ = cls(times = t, zeros = z)
    disc = ycZ.getDisc(times = t)
    ycD = cls(times = t, discounts = disc)
    z2  = ycD.getZeros(times = t)
    if (np.abs(z-z2) > 1.e-15).any():
        print '1: zero -> disc -> zero failed'
        return False

    # second test -- reproduce fixed values in zero rates, discount and fwds
    testT = [0., 0.1, 0.5, 0.75, 1.0, 3., 9., 10., 15.]
    z2 = ycZ.getZeros(testT)
    z2std = {'YieldCurveConstFwd' : [0.03, 0.03, 0.04, 0.1/3, 0.03,
                                     0.22/9, 0.26/9, 0.03, 0.03],
             'YieldCurveLinearFwd': [0.03, 0.03, 0.04, 0.125/3, 0.03,
                                     41.44/999, 19.684/999, 0.03, 0.61/9]}
    if (np.abs(z2std[className] - z2) > 1.e-15).any():
        print '2: zero regression failed'
        return False

    f2 = ycZ.getFwds(testT)
    f2std = {'YieldCurveConstFwd' : [0.03,   0.03,  0.02,   0.02,  0.03,
                                     0.04/3, 0.04,  0.04,   0.03],
             'YieldCurveLinearFwd': [0.03,   0.03,  0.07,   0.02, -0.03,
                                     0.35/9, 0.102, 0.43/3, 0.43/3]}
    if (np.abs(f2std[className] - f2) > 1.e-15).any():
        print '2: fwd regression failed'
        return False

    lnDisc    = -ycZ.getLnDisc(testT)
    lnDiscStd = {'YieldCurveConstFwd' : [0.,     0.003,   0.02, 0.025,   0.03,
                                         0.22/3, 0.26,    0.3,  0.45],
                 'YieldCurveLinearFwd': [0.,     0.003,   0.02, 0.03125, 0.03,
                                         1.12/9, 0.532/3, 0.3,  3.05/3]}
    if (np.abs(lnDiscStd[className] - lnDisc) > 1.e-15).any():
        print '2: disc regression failed'
        return False

    print testClass.__name__ + ' testing passed'

if __name__ == "__main__":
    from YieldCurveConstFwd  import YieldCurveConstFwd
    from YieldCurveLinearFwd import YieldCurveLinearFwd

    for testClass in [YieldCurveConstFwd, YieldCurveLinearFwd]:
        testYieldCurve(cls = testClass)
