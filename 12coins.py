import numpy as np
coins = [0,1,2,3,4,5,6,7,8,9,10,11]
H=np.zeros((12,12))
np.fill_diagonal(H,1)
L=np.zeros((12,12))
np.fill_diagonal(L,-1)
instance=np.append(H,L,axis=0)

def checkRules(B):
 for i in instance:
  balance=""
  for j in B:
   if (i[j[0]]+i[j[1]]+i[j[2]]+i[j[3]])>(i[j[4]]+i[j[5]]+i[j[6]]+i[j[7]]):
    balance += '>'
   elif (i[j[0]]+i[j[1]]+i[j[2]]+i[j[3]])<(i[j[4]]+i[j[5]]+i[j[6]]+i[j[7]]):
    balance += '<'
   else: balance += '='
  rules.append(balance)
  balance=""
  if len(set(rules))==24:
   break

from random import sample,seed
import random
random.seed(8)
for i in instance:
 print(i)
for i in range(1000):
 b1=sample(coins,8)
 b2=sample(coins,8)
 b3=sample(coins,8)
 B=[b1,b2,b3]
 rules=[]
 checkRules(B)
 if len(set(rules))==24:
  for j in B:
   j=[x+1 for x in j]
   print(j)
  print(rules,i,"\n")
