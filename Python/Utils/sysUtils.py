## ----------------------------------------------------------------------------
# \file  sysUtils.py
# \brief System utilities
#
# $URL: https://trstwprsvnapv01.treasury.corp.ge.com/svn/Treasury/QUANTS%20GROUP/Branches/Dima/mdm0/Utils/Python/sysUtils.py $
# $Id: sysUtils.py 18868 2015-02-10 20:35:47Z 212361829 $
#  ------------------------------------------------------------------------------

import os, psutil

def completeProcess(process = None, beforeMsg = '', afterMsg = '', log = None):
    """
    Monitor a process until it terminates, printing messages before and after
    :param process:   process to monitor
    :type  process:   multiprocessing.Process
    :param beforeMsg: message to log before monitoring
    :type  beforeMsg: str
    :param afterMsg:  message to log after process is over
    :type  afterMsg:  str
    :param log:       log to which to dump messages
    :type  log:       logging.Logger
    :return:   Nothing
    :rtype:    None
    """
    if process:
        if beforeMsg and log is not None:
            log.info(beforeMsg)
        process.join()
        if afterMsg and log is not None:
            log.info(afterMsg)

def currentMemoryUsage(process = None, pid = None):
    """
    return the memory usage of the active process in MB
    http://stackoverflow.com/a/21632554/399573
    :param process: process to get memory usage for
    :type  process: psutil.Process
    :param pid:     process id, if no process passed
    :type  pid:     int
    :return:    Current memory usage in MB
    :rtype:     float
    """

    if not process:
        if not pid:
            pid = os.getpid()
        process = psutil.Process(pid)

    mem = process.get_memory_info()[0] / float(2 ** 20)
    return mem
