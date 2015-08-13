#!/usr/bin/python

##----------------------------------------------------------------------------##
## \file  basicCalendar.py                                                    ##
## \brief Calendar class implementation                                       ##
##                                                                            ##
## Describes the calendar class.                                              ##
##----------------------------------------------------------------------------##

from datetime               import datetime as dt, date, time, timedelta
from dateutil.relativedelta import relativedelta
from calendar               import monthrange

DAY_Act365    = 1./365.
DAY_Act360    = 1./360.
HOUR_Act365   = DAY_Act365 / 24.
HOUR_Act360   = DAY_Act360 / 24.
MINUTE_Act365 = HOUR_Act365 / 60.
MINUTE_Act360 = HOUR_Act360 / 60.
SECOND_Act365 = MINUTE_Act365 / 60.
SECOND_Act360 = MINUTE_Act360 / 60.

dFmtSQL         = '%Y-%m-%d'
dtFmtSQL        = '%Y-%m-%d %H:%M:%S.%f'
dFmtLog         = '%Y%m%d'
dtFmtLog        = '%Y%m%d.%H%M%S'
dFmtShort       = '%y%m%d'
dtFmtLongPacked = '%Y%m%d%H%M%S%f'

# TODO: consider inheriting from std calendar.Calendar
# cannot call it Calendar to avoid name conflict with standard library class
class BasicCalendar(object):
    def __init__(self):
        pass

    def DateTimesToFloats(self, dateTimes, baseDate):
        """
        Calculate year fractions of dateTimes from the base date.
        Assumes dateTimes is SORTED in INCREASING order.
        :type   dateTimes: list of [datetime.datetime] or [datetime.date]
        :type   baseDate:  datetime.datetime or datetime.date
        :rtype: list of [float]
        :param  dateTimes: INCREASING of dateTimes to compute year fractions for
        :param  baseDate:  Base date from which to compute
        :return: List of year fractions
        """
        days = [(t - baseDate).total_seconds()/86400. for t in dateTimes]
        return self.DaysToFloats(days = days, baseDate = baseDate)

    def DaysToFloats(self, days, baseDate):
        """
        Converts an ascending array of day offsets from \a baseDate to floats.
        :param days:       The list of day offsets to convert, sorted asc.
        :type  days:       list of floats
        :param baseDate:   The base date from which the offsets are counted
        :type  baseDate:   datetime.datetime
        :return:  List of floats, corresponding to \a days
        :rtype:   list of floats
        """
        prevYearDays, nextYearDays, nextYears, years = 0, 0, 0, []
        for d in days:
            if d == 0:
                years.append(0.)
                continue

            while d > nextYearDays:
                prevYearDays = nextYearDays
                nextYears   += 1
                nextYearDays = self.SplitTenor2Days(baseDate  = baseDate,
                                                    tenorSize = nextYears,
                                                    tenorUnit = 'Y')
            # now prevYearDays < d <= nextYearDays
            dayFloat = float(d - prevYearDays)/(nextYearDays - prevYearDays)
            years.append(nextYears - 1 + dayFloat)
        return years

    def AddSplitTenor(self, baseDate, tenorSize, tenorUnit):
        """
        Add a split tenor (size, unit) [like 3 'M'] to a reference date.
        This would work for both date and datetime objects, as long as the
        arithmetic with datetime.relativedelta is supported.
        :param baseDate:  reference date
        :type  baseDate:  datetime.date or datetime.datetime
        :param tenorSize: tenor size
        :type  tenorSize: int
        :param tenorUnit: tenor unit
        :type  tenorUnit: str
        :return: Resulting date
        :rtype:  datetime.date or datetime.datetime
        """
        unit = tenorUnit.upper()
        if unit == 'D':
            resultDate = baseDate + relativedelta(days = tenorSize)
        elif unit == 'W':
            resultDate = baseDate + relativedelta(weeks = tenorSize)
        elif unit == 'M':
            resultDate = baseDate + relativedelta(months = tenorSize)
        elif unit == 'Y':
            resultDate = baseDate + relativedelta(years = tenorSize)
        else:
            raise ValueError('Tenor unit %s unknown' % unit)

        return resultDate

    def AddTenor(self, baseDate, tenor):
        return self.AddSplitTenor(baseDate  = baseDate,
                                  tenorSize = int(tenor[:-1]),
                                  tenorUnit = tenor[-1])

    def AddWholeMonths(self, baseDate, numMonths):
        """
        Add a whole number of months to baseDate - if it falls on the last day
        of the month, the result will do so as well. E.g. 4/30 + 1M = 5/31.
        If baseDate is not end of the month, this just adds a month to it.
        :param baseDate:  base date from which to compute
        :type  baseDate:  datetime.date
        :param numMonths: number of months to add
        :type  numMonths: int
        :return:  resulting date
        :rtype:   datetime.date
        """
        endDate = self.AddSplitTenor(baseDate  = baseDate,
                                     tenorSize = numMonths,
                                     tenorUnit = 'M')
        tomorrow = baseDate + timedelta(days = 1)
        return endDate if baseDate.month == tomorrow.month else \
                       self.EndOfMonth(baseDate = endDate)

    def SplitTenor2Days(self, baseDate, tenorSize, tenorUnit):
        # the date difference is actually a datetime.timedelta object
        return (self.AddSplitTenor(baseDate  = baseDate,
                                   tenorSize = tenorSize,
                                   tenorUnit = tenorUnit) - baseDate).days

    def SplitTenor2Floats(self, baseDate, tenorSize, tenorUnit):
        if tenorUnit.upper() == 'Y':
            return tenorSize
        dayDiff = self.SplitTenor2Days(baseDate  = baseDate,
                                       tenorSize = tenorSize,
                                       tenorUnit = tenorUnit)
        return self.DaysToFloats(days = [dayDiff], baseDate = baseDate)[0]

    def Tenor2Days(self, baseDate, tenor):
        return self.SplitTenor2Days(baseDate  = baseDate,
                                    tenorSize = int(tenor[:-1]),
                                    tenorUnit = tenor[-1])

    def Tenor2Floats(self, baseDate, tenor):
        return self.SplitTenor2Floats(baseDate  = baseDate,
                                      tenorSize = int(tenor[:-1]),
                                      tenorUnit = tenor[-1])

    def SortTenors(self, tenorList, baseDate = dt.now()):
        pairs = sorted([(self.AddTenor(baseDate, t),t) for t in tenorList],
                       key = lambda t: t[0])
        return [x[1] for x in pairs]

    @classmethod
    def EndOfMonth(cls, baseDate):
        """
        Returns end of this month, e.g. 3/31 --> 3/31 and 3/20 --> 3/31
        :param baseDate: date from which to count end-of-month date
        :type  baseDate: date
        :return: next end of month
        :rtype:  date
        """
        return baseDate.replace(day = monthrange(year  = baseDate.year,
                                                 month = baseDate.month)[1])
    @classmethod
    def NextEndOfMonth(cls, baseDate):
        """
        Returns next end of month, e.g. 3/31 --> 4/30 and 3/20 --> 3/31
        :param baseDate: date from which to count next end-of-month date
        :type  baseDate: date
        :return: next end of month
        :rtype:  date
        """
        tomorrow = baseDate + timedelta(days = 1)
        return cls.EndOfMonth(baseDate = baseDate) \
                   if baseDate.month == tomorrow.month else \
                       cls.EndOfMonth(baseDate = tomorrow)

    @classmethod
    def DatesToSplitTenor(cls, baseDate, targetDate):
        """
        Converts an offset between two dates to a tenor
        :param baseDate:   Starting date from which to count
        :type  baseDate:   date
        :param targetDate: Target date ending the date difference interval
        :type  targetDate: date
        :return:  tenorSize, tenorUnit
        :rtype:   tuple of [int, str]
        """
        return (targetDate - baseDate).days, 'D'