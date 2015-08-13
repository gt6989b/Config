#------------------------------------------------------------------------------
# \file  logUtils.py
# \brief Logging utilities
#
# $URL: https://trstwprsvnapv01.treasury.corp.ge.com/svn/Treasury/QUANTS%20GROUP/Branches/Dima/mdm0/Utils/Python/logUtils.py $
# $Id: logUtils.py 24589 2015-07-06 16:50:19Z 212361829 $
#------------------------------------------------------------------------------

import logging

def setupLog(log, fileName = None):
    """
    Set up logging object handlers
    :param log:      logging object to set up handlers on
    :type  log:      logging.Logger
    :param fileName: name of the log file to dump to, or None
    :type  fileName: str or None
    :return: Nothing
    :rtype:  None
    """
    global eventHandler
    log.setLevel(logging.INFO)

    formatter = logging.Formatter('%%(%s)s' %\
                  ')s|%('.join(['asctime',  'name', 'levelname','message']))

    try:
        eventHandler = logging.FileHandler(fileName) if fileName else \
                       logging.StreamHandler()

    except IOError as e:
        import errno
        if e.errno == errno.EACCES:
            msg = 'File %s is not accessible'
            log.exception(msg)
            raise ValueError(msg)

    eventHandler.setLevel(logging.INFO)
    eventHandler.setFormatter(formatter)
    log.addHandler(eventHandler)
