#Introduction to Statistical Thinking (With R, Without Calculus)

#4.1
#1  What is the value of p?
# (2+3+4+5+6+1)p = 21p -> p = 1/21
#2  P(Y < 3) = ? 
# 1/21 + 2/21 + 3/21 = 6/21 = .28571429
#3  P(Y = odd) = ?
# 2/21 + 4/21 + 6/21 = 12/21 = 0.57142857
#4  P(1 ??? Y < 4) = ?
# 2/21 + 3/21 + 4/21 = 9/21 = 0.42857143
#5  P(|Y ??? 3| < 1.5) = ?
# 3/21 + 4/21 + 5/21 = 12/21 = 0.57142857
#6  E(Y ) = ?
# (0*1/21) + (1* 2/21) + (2 * 3/21) + (3 * 4/21) + (4 * 5/21) + (5* 6/21) = 70/21 = 3.33333
#7 Var(Y ) = ? (variance of y)
# ((0 -3.3333) * 1/21) + ((1 -3.3333) * 2/21) + ((2 -3.3333) * 3/21)+ ((3 -3.3333) * 4/21)+ ((4 -3.3333) * 5/21)+((5 -3.3333) * 6/21)
# = 2.2222
#8 What is the standard deviation of Y . (square root of variance)
# sq-rt(2.2222) = 1.49070453
#4.2
#1  What is the probability of winning the game?
# HHH, TTT, HTH, THT, HHT, TTH, THH, HTT
#1/8
#2  What is the probability of loosing the game?
# HHH, TTT, HTH, THT, HHT, TTH, THH, HTT
#7/8
#3  What is the expected gain for the player that plays this game? (Notice
#that the expectation can obtain a negative value.)
#Wins 8 dollars 10-2, loses 2 dollars 
# 8 (1/8) + -2(7/8) = 1 -1.75 = -0.75

#Chapter 6
#6.1
#1 
1 - pnorm(650,560,57)
#0.05717406
#2
1-pnorm(650,630,61)
#0.3715054
#3
qnorm(0.1,560,57)
#486.9516
qnorm(0.9,560,57)
#633.0484
#4
qnorm(0.1,630,61)
#551.8254
qnorm(0.9,630,61)
#708.1746
