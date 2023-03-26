def make_readable(seconds):
    (ss1, ss2) = divmod(seconds, 60)
    (hh, mm2)  = divmod(ss1, 60)
    return ":".join(map(trans, [hh, mm2, ss2]))

def trans(x):
    return "0" + str(x) if x < 10 else str(x)