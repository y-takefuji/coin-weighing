# -*- coding: utf-8 -*-


from z3 import *

def weigh(c_p,l,s,val):
    a = []
    for i in val:
        a_l = If(Distinct(i[:4]+[c_p]),0,l)
        b_l = If(Distinct(i[4:8]+[c_p]),0,l)
        a.append(2 + a_l - b_l)
    return 100*a[0]+10*a[1]+a[2]

def search_rules():
    s = Solver()
    val = [[Int("val[%d,%d]" % (i,j)) for j in range(8)] for i in range(3)]

    for i in range(3):
        for j in range(8):
            s.add(1 <= val[i][j], val[i][j] <= 12)

    for i in range(3):
        tmpList = []
        for j in range(8):
            tmpList.append(val[i][j])
        s.add(Distinct(tmpList))

    all_weigh = [weigh(i//2+1,-2*(i%2)+1,s,val) for i in range(24)]
    s.add(Distinct(all_weigh))

    r = s.check()
    #print(r,end=" ")
    if r == sat:
        m = s.model()
        for i in range(3):
            for j in range(8):
                print(m[val[i][j]].as_long(),end=" ")
                if j != 7 : print(",",end=" ")
            print("",)
        for i in range(24):
            if i==12:print(" ")
            for j in val:
                a = 0
                if i//2+1 in [m[k].as_long() for k in j[:4]]:
                    a = -2*(i%2)+1
                elif i//2+1 in [m[k].as_long() for k in j[4:8]]:
                    a = 2*(i%2)-1

                if a == 0: print("=",end=" ")
                elif a == 1: print(">",end=" ")
                elif a == -1: print("<",end=" ")
            print(",",end=" ")
        print("",)

search_rules()
