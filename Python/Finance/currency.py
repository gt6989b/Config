#!/usr/bin/python

##----------------------------------------------------------------------------##
## \file  Finance/currency.py                                                 ##
## \brief Currency class implementation                                       ##
##                                                                            ##
## Describes the currency class. Uses the calendar class.                     ##
##                                                                            ##
## $URL: https://trstwprsvnapv01.treasury.corp.ge.com/svn/Treasury/QUANTS%20GROUP/Branches/Dima/mdm0/MDM/MDMUtils.py $
## $Id: currency.py 16417 2014-11-27 17:17:51Z 212361829 $                    ##
##----------------------------------------------------------------------------##

from basicCalendar import BasicCalendar
from miscUtils     import Singleton

class CurrencyNameTranslator():
    __metaclass__ = Singleton

    def __init__(self, db):
        data = db.spGetCurrencies()
        self.tkr2code = {(x.CcyTicker, x.CcyOffShore) : x.Ccy for x in data}
        self.code2tkr = {x.Ccy : (x.CcyTicker, x.CcyOffShore) for x in data}

    def ticker2code(self, ccyTicker, onShore = 0):
        return self.tkr2code[(ccyTicker, onShore)]

    def tickerPair2code(self, ccyTickerPair):
        return self.tkr2code[ccyTickerPair]

    def code2ticker(self, ccyCode):
        return self.code2tkr[ccyCode]

class Currency(object):
    @classmethod
    def LoadFromDB(cls, db):
        ccyData = db.spGetMDMCurrencies()
        defaultCalendar = BasicCalendar()
        ccyName2ccy = {x.Ccy : cls(name = x.Ccy, calendar = defaultCalendar)
                         for x in ccyData}
        return ccyName2ccy

    def __init__(self, name, calendar):
        self.name = name
        self.cal  = calendar

    @property
    def cal(self):
        """
        :return: currency calendar
        :rtype:  basicCalendar.BasicCalendar
        """
        return self._cal

    @cal.setter
    def cal(self, newCal):
        """
        :param  newCal: New calendar to set to
        :type   newCal: basicCalendar.BasicCalendar
        :return: Nothing
        :rtype:  None
        """
        self._cal = newCal

    @property
    def name(self):
        """
        :return: curve name
        :rtype:  str
        """
        return self._name

    @name.setter
    def name(self, newName):
        """
        :param newName: Name to set to
        :type  newName: str
        :return: Nothing
        :rtype:  None
        """
        self._name = newName
