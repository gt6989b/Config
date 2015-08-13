#!/usr/bin/python

##----------------------------------------------------------------------------##
## \file  common.py                                                           ##
## \brief Common calendar items                                               ##
##                                                                            ##
## Common calendar constants.                                                 ##
##----------------------------------------------------------------------------##

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
