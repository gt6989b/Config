#!/usr/bin/python

##----------------------------------------------------------------------------##
## \file  fileUtils.py                                                        ##
## \brief Miscellaneous file utility functions                                ##
##                                                                            ##
## $URL: https://trstwprsvnapv01.treasury.corp.ge.com/svn/Treasury/QUANTS%20GROUP/Branches/Dima/mdm0/Utils/Python/fileUtils.py $
## $Id: fileUtils.py 24589 2015-07-06 16:50:19Z 212361829 $                     ##
##----------------------------------------------------------------------------##

from dateTimeUtils import dtFmtLog
from datetime      import datetime as dt

import os, errno

def validateFile(fileName, log, accessType = os.R_OK):
    """Make sure we can access \a fileName with \a accessType, raise on error.
    :type  fileName:   str
    :type  log:        logging.Logger
    :param accessType: os access type
    :return: None
    """
    if not os.access(fileName, accessType):
        msg = 'Cannot access the file %s' % fileName
        log.error(msg)
        raise RuntimeError(msg)

def stripTimeStampFromFileName(fileName):
    dateStr, timeStr = fileName.split('.')[1:3]
    try:
        fileTime = dt.strptime(dateStr + '.' + timeStr, dtFmtLog)
    except ValueError:
        fileTime = None

    return fileTime

def ensureDirExists(absPath):
    """
    :type  absPath: str
    :param absPath: absolute directory path to be validated and/or created
    :return: None

    Validate a directory path, and create it if necessary. See the discussion
    in http://stackoverflow.com/a/5032238/399573 on a possible race condition
    in the simple implementation of this problem.
    """
    try:
        os.makedirs(absPath)
    except OSError as e:
        if e.errno != errno.EEXIST:
            raise
