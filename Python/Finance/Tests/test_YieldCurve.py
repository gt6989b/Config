#!/usr/bin/python

##----------------------------------------------------------------------------##
## \file  test_YieldCurve.py                                                  ##
## \brief Forwards calculation                                                ##
##                                                                            ##
## Tests the yield curve forwards calculations                                ##
## For complete extraction and forwards generation see                        ##
##                                       MDM/Tools/ForwardsAnalysis.py script ##
##                                                                            ##
## $URL: https://trstwprsvnapv01.treasury.corp.ge.com/svn/Treasury/QUANTS%20GROUP/Branches/Dima/mdm0/Utils/Python/Finance/tests/test_YieldCurve.py $                 ##
## $Id: test_YieldCurve.py 20975 2015-03-23 22:42:16Z 212431598 $                  ##
##----------------------------------------------------------------------------##
import logging
import xlrd
import numpy as np
import Finance.YieldCurve as FYC
from sortedcontainers   import SortedList
from miscUtils          import unfoldGen
from datetime           import datetime as dt
from basicCalendar      import BasicCalendar as Cal
from dateTimeUtils      import dtFmtLog
from xlsxwriter         import Workbook

logging.basicConfig(level=logging.DEBUG)

TENORS = [[ 1,'D'], [ 1,'M'], [ 2,'M'], [ 3,'M'], [6,'M'], [9,'M'],
          [ 1,'Y'], [ 2,'Y'], [ 3,'Y'], [ 4,'Y'], [5,'Y'], [7,'Y'],
          [10,'Y'], [15,'Y'], [20,'Y'], [30,'Y']]

ZERO_TIMES = [  1.0/365,  1.0/12,  2.0/12,  3.0/12, 6.0/12, 9.0/12,  1.0,  2.0,
                3.0,      4.0,     5.0,     7.0,    10.0,   15.0,    20.0, 30.0]

NUMBER_FWD_MONTHS = 36


def is_not_number(s):

    strNumber = str(s)

    isNumber = False

    try:
        float(strNumber)
        isNumber = True
    except ValueError:
        pass

    try:
        import unicodedata
        unicodedata.numeric(strNumber)
        isNumber = True
    except (TypeError, ValueError):
        pass

    return not isNumber

def testYieldCurve():

    # first test -- reproduce its own values going zero -> disc -> zero

    t = [0.25,  0.5,   1.,   2.,   5.,  10.]
    z = [0.03, 0.04, 0.03, 0.03, 0.02, 0.03]
    ycZ = FYC.yieldCurveFactory(times = t, zeros = z)

    # # second test -- reproduce fixed values in zero rates, discount and fwds
    testT = [  0.,  0.1,   0.5,  0.75,  1.0,     3.,     9.,  10., 15.]

    f2 = ycZ.getFwds(testT)
    f2std = [0.03, 0.03, 0.02,  0.02,  0.03, 0.04/3, 0.04, 0.04, 0.03]
    assert (np.abs(f2std-f2) > 1.e-15).any() == False,\
        '2: fwd regression failed'

def testYieldCurveFwdsGeneration():

    # ==========================================================================
    # A_Auto: USD_IRS_LIBOR_3M

    # Test based on Auto Zero rates
    # zeroRates=[0.00233531255143132,
    #            0.00233531255143297,
    #            0.00233531255143352,
    #            0.00233531255143333,
    #            0.00297455023730465,
    #            0.00319723236979883,
    #            0.00330765830399476,
    #            0.00691586090809845,
    #            0.01085698247648050,
    #            0.01394073189902450,
    #            0.01633917521905550,
    #            0.02000659612723410,
    #            0.02346660204636610,
    #            0.02685361531988290,
    #            0.02854781579990930,
    #            0.02975553109452430]

    # Test based on Manual Zero rates
    zeroRates = [
                0.00236843676021893,
                0.00236821394576394,
                0.00236797582662841,
                0.00236773777132554,
                0.00290042675432344,
                0.00308558288679761,
                0.00330592988104989,
                0.00691281016439806,
                0.01085462048320750,
                0.01393891008113780,
                0.01633775879077160,
                0.02000467532244330,
                0.02346505198474200,
                0.02685243199737300,
                0.02854687951971270,
                0.02975479160795850
    ]

    yieldCurve = FYC.yieldCurveFactory(times=ZERO_TIMES,
                                       zeros=zeroRates,
                                       type='linearFwd')

    runTime = dt.strptime('20141128.230000', dtFmtLog)
    runDate = runTime.date() # same as outDates

    # make sure the first out date is the end of the following month
    nextDate = Cal.NextEndOfMonth(runDate)
    startDate = nextDate if nextDate.month == runDate.month else runDate

    dfltFwdDates = SortedList(unfoldGen(func     = Cal.NextEndOfMonth,
                                        initArg  = startDate,
                                        numSteps = NUMBER_FWD_MONTHS))

    cal = Cal()

    fwdDates = [[cal.AddSplitTenor(baseDate  = baseDate,
                                    tenorSize = x[0],
                                    tenorUnit = x[1])
                  for x in [[0,'D']] + TENORS]
                      for baseDate in dfltFwdDates]

    fwdTimes = [cal.DateTimesToFloats(dateTimes = fwdDateList,
                                       baseDate = runDate)
                for fwdDateList in fwdDates]


    outFwds = np.empty((len(fwdTimes[0])-1,NUMBER_FWD_MONTHS))

    for idx, fwdTimesAtDate in enumerate(fwdTimes):
        outFwds[:,idx] \
            = yieldCurve.getFwds(
                    times    = fwdTimesAtDate[1:],
                    baseTime = fwdTimesAtDate[0])

    rateTable =  np.column_stack((zeroRates, outFwds))
    # outFwds = np.maximum(0., outFwds)

    workbook = Workbook('tests/test_YieldCurve.xlsx',{'constant_memory': True})

    headerFormat     = workbook.add_format({'bold'     : True,
                                            'font_name': 'Arial',
                                            'font_size': 8})

    rateFormat       = workbook.add_format({'num_format': '0.000%',
                                            'font_name' : 'Arial',
                                            'font_size' : 8,
                                            'border'    : 1})

    stringFormat     = workbook.add_format({'font_name' : 'Arial',
                                            'font_size' : 8,
                                            'border'    : 1})

    dateFormat       = workbook.add_format({'bold'      : False,
                                            'num_format': 'mm/dd/yyyy',
                                            'font_name' : 'Arial',
                                            'font_size' : 8,
                                            'border'    : 1})

    headerDateFormat = workbook.add_format({'bold'      : True,
                                            'num_format': 'mm/dd/yyyy',
                                            'font_name' : 'Arial',
                                            'font_size' : 8,
                                            'border'    : 1})

    # ==========================================================================
    wks = workbook.add_worksheet("Forwards")
    row = 1
    initCol = 2

    col = initCol
    wks.write_string(row, col, 'Maturities/Dates', headerFormat)

    col +=1
    wks.write_datetime(row, col, runDate, headerDateFormat)

    for outDate in fwdDates:
        col +=1
        wks.write_datetime(row, col, outDate[0], headerDateFormat)

    # write values
    for tenor, rateRow in zip(TENORS, rateTable):
        row += 1
        col  = initCol
        # print row, col, tenor
        wks.write_string(row, col, "%d%s"%(tenor[0],tenor[1]), headerFormat)
        col += 1
        # print rateRow
        for rate in rateRow:
            # print row, col, rate
            wks.write_number(row, col, rate,rateFormat)
            col += 1

    # ==========================================================================
    # Format desired output
    crvName = "USD_IRS_LIBOR_3M"
    scenarioName = "A_Base"
    # for baseDate in dfltFwdDates
    # for tenor in TENORS
    wks = workbook.add_worksheet("Comparision")
    initRow = 0
    initCol = 0
    headerString = ['Curve','Scenario','Base Date', 'Tenor', 'Rate']
    wks.write_row(initRow, initCol, headerString, headerFormat)
    col = initCol
    row = initRow + 1
    for tenor, rateRow in zip(TENORS, outFwds):
        for baseDate, rate in zip (dfltFwdDates, rateRow):
            col=initCol
            wks.write_string(row, col, crvName, stringFormat)
            col +=1
            wks.write_string(row, col, scenarioName, stringFormat)
            col +=1
            wks.write_datetime(row, col, baseDate, dateFormat)
            col +=1
            wks.write_string(row, col, "%d%s"%(tenor[0],tenor[1]), stringFormat)
            col +=1
            wks.write_number(row, col, rate, rateFormat)
            row +=1

    workbook.close()
