#Use the example above to answer the following questions.

#What is the probability of reciprocating an incoming tie to a similar actor?
exp(-2+2.5+1)/(1+exp(-2+2.5+1))

#  What is the probability of moving from 3 to 4, ignoring the similarity to friends?
exp(-1*(4-3)-0.5*(4-3)^2)/(1+exp(-1*(4-3)-0.5*(4-3)^2))

#  Suppose an actor has 5 friends, 3 of whom score 4 and 2 of whom score 3.
#What is the probability of moving from 3 to 4, if we account for homophily?
exp(-1*(4-3)-0.5*(4-3)^2+2.5*.2)/(1+exp(-1*(4-3)-0.5*(4-3)^2+2.5*.2))
