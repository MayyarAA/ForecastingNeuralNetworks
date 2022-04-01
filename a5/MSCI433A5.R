library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(magrittr)
library(dplyr)
library(ROI)
library(lpSolve)

#Q1.b)

objectiveFcnQ1 = c(3,1,2.5,3,2,2)

matricQ1 = matrix(c(0,2,0,0,2,0,
                  0,0,1.5,0,1,0,
                  0,0,1.5,1,0,0,
                  0,2.5,0,0,0,2,
                  2,0,0,0,1,0), nrow =5,byrow= TRUE)

constraintSingsQ1=c("<=",">=",">=",">=",">=")

rhsQ1 = c(5,7,7,7,7)
optimalsolnQ1 = lp("min",objectiveFcnQ1,matricQ1,constraintSingsQ1,rhsQ1,compute.sens = TRUE)
print(optimalsolnQ1)
#Q1.c)




#Q2.b)



#Q2.c)
