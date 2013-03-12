"""Simple module for getting amount of memory used by a specified user's
processes on a UNIX system.

It uses UNIX ps utility to get the memory usage for a specified username and
pipe it to awk for summing up per application memory usage and return the total.
Python's Popen() from subprocess module is used for spawning ps and awk.

http://stackoverflow.com/questions/276052/how-to-get-current-cpu-and-ram-usage-in-python
"""

import subprocess

class MemoryMonitor(object):

    def __init__(self, pid):
        """Create new MemoryMonitor instance."""
        self.pid = str(pid)

    def usage(self):
        """Return int containing memory used by user's processes."""
        process = subprocess.Popen("ps -p %s -o rss -o vsz" % self.pid,
                                   shell  = True,
                                   stdout = subprocess.PIPE, )

        # will get back {'RSS', 'VSZ', rss#, vsz#}
        stdout_list = process.communicate()[0].split()
        return [int(outstr) for outstr in stdout_list[2:4]] # ignore tags
