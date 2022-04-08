library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(magrittr)
library(dplyr)
library(ROI)
library(lpSolve)


objectiveFcnQ1 = c(11125.61983,4076.033058,8519.008264,5523.966942)


matricQ1 = matrix(c(
                    #<.15
                    .85,-.15,-.15,-.15,
                    -.15,.85,-.15,-.15,
                    -.15,-.15,.85,-.15,
                    -.15,-.15,-.15,.85,
                    #<.50
                    .5,-.5,-.5,-.5,
                    -.5,.5,-.5,-.5,
                    -.5,-.5,1.5,-.5,
                    -.5,-.5,-.5,1.5,
                    #<=10mill yr1
                    (.2*27000),(.2*22000),(.2*18000),(.2*14000)
                    
                    ), nrow =9,byrow= TRUE)

constraintSingsQ1=c(">=",">=",">=",">=",    "<=","<=","<=","<="    
                    ,"<=")
#0.15
rhsQ1 = c(0,0,0,0  ,0,0,0,0   ,10000000   )
optimalsolnQ1 = lp("max",objectiveFcnQ1,matricQ1,constraintSingsQ1,rhsQ1,compute.sens = TRUE)
print(optimalsolnQ1)
