import string
import random

def RandomStr(size  = 10,
              chars = string.ascii_letters + string.digits,
              rep   = True):
    return ''.join(random.choice(chars) for x in range(size)) if rep \
        else ''.join(random.sample(chars, size))
