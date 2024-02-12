import numpy as np
coins = list(range(24))
H=np.zeros((24,24))
np.fill_diagonal(H,1)
L=np.zeros((24,24))
np.fill_diagonal(L,-1)
instance=np.append(H,L,axis=0)
rule=48

def loop(B):
 for i in instance:
  balance=""
  for j in B:
   if (i[j[0]]+i[j[1]]+i[j[2]]+i[j[3]]+i[j[4]]+i[j[5]]+i[j[6]]+i[j[7]])>(i[j[8]]+i[j[9]]+i[j[10]]+i[j[11]]+i[j[12]]+i[j[13]]+i[j[14]]+i[j[15]]):
    balance += '>'
   elif (i[j[0]]+i[j[1]]+i[j[2]]+i[j[3]]+i[j[4]]+i[j[5]]+i[j[6]]+i[j[7]])<(i[j[8]]+i[j[9]]+i[j[10]]+i[j[11]]+i[j[12]]+i[j[13]]+i[j[14]]+i[j[15]]):
    balance += '<'
   else: balance += '='
  rules.append(balance)
  balance=""
  if len(set(rules))==rule:
   break

from random import sample,seed
import random
random.seed(7)
for i in instance:
 print(i)
for i in range(100000):
 b1=sample(coins,16)
 b2=sample(coins,16)
 b3=sample(coins,16)
 b4=sample(coins,16)
 B=[b1,b2,b3,b4]
 rules=[]
 loop(B)
 if len(set(rules))==rule:
  for j in B:
   j=[x+1 for x in j]
   print(j)
  print(rules,i,"\n")
  break
