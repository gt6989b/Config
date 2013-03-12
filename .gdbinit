set print pretty on
set print object on
set print static-members off
set print vtbl on
set print demangle on
set demangle-style gnu-v3

set history filename ~/.gdb_history
set history save on

source bin/stl-views.gdb

# Optimize time to step into a function
set env LD_BIND_NOW=on

### python
### import sys
### sys.path.insert(0, '/home/n543654/swt/gdb_pretty_print/python')
### from libstdcxx.v6.printers import register_libstdcxx_printers
### register_libstdcxx_printers (None)
### end
