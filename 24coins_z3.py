# -*- coding: utf-8 -*-
# this program is developed by Ren Yamada.
from z3 import *

def weigh(c_p,l,s,val):
    a = []
    for i in val:
        a_l = If(Distinct(i[:8]+[c_p]),0,l)
        b_l = If(Distinct(i[8:16]+[c_p]),0,l)
        a.append(2 + a_l - b_l)
    return 1000*a[0]+100*a[1]+10*a[2]+a[3]

def search_rules():
    s = Solver()
    val = [[Int("val[%d,%d]" % (i,j)) for j in range(16)] for i in range(4)]

    for i in range(4):
        for j in range(16):
            s.add(1 <= val[i][j], val[i][j] <= 24)

    for i in range(4):
        tmpList = []
        for j in range(16):
            tmpList.append(val[i][j])
        s.add(Distinct(tmpList))

    all_weigh = [weigh(i//2+1,-2*(i%2)+1,s,val) for i in range(48)]
    s.add(Distinct(all_weigh))

    r = s.check()
    print r
    if r == sat:
        m = s.model()
        for i in range(4):
            for j in range(16):
                print m[val[i][j]].as_long(),
                if j != 15 : print ",",
            print ""
        for i in range(48):
            for j in val:
                a = 0
                if i//2+1 in [m[k].as_long() for k in j[:8]]:
                    a = -2*(i%2)+1
                elif i//2+1 in [m[k].as_long() for k in j[8:16]]:
                    a = 2*(i%2)-1

                if a == 0: print "=",
                elif a == 1: print ">",
                elif a == -1: print "<",
            print ",",
        print ""

search_rules()
