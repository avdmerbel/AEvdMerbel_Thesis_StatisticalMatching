# In this script some jointprobabilities are calculated from the populations to 
# check whether it makes sense that some estimations are completely outside the
# Fréchet bounds.

setwd("./Data/Populations")
populations = 
  readRDS("main_populations.RData")

setwd("./Functions")
file.sources = list.files(pattern="*.R$")
sapply(file.sources,source,.GlobalEnv)

set.seed(2310)

# Recalculate one sample which was outside the frechet bounds to see if the reason
# is the restriction or the fact that the calculations are wrong because of a wrong
# assumption.

# For population 2, common instrumental the 2-2 combination was outside the bounds
# in the simulation
samplepop2_ins = 
  GenerateSample(population = populations[[2]]$FullPopulation,
                 common_variable = "instrumental",
                 n1 = 2000,
                 n2 = 2000)

# get x given y from data
X_given_Y = 
  prop.table(samplepop2_ins$DistributionFirstSample, margin = 1)

# get z given y from data
Z_given_Y = 
  prop.table(samplepop2_ins$DistributionSecondSample, margin = 1)

# get p(Y) from data
p_Y =
  ( sum(samplepop2_ins$DistributionFirstSample[1,]) + sum(samplepop2_ins$DistributionSecondSample[1,]) ) / 2

# get p(X) using p(Y) for more accuracy
P_X = 
  (X_given_Y[1,1] * p_Y ) + 
  (X_given_Y[2,1] * (1 - p_Y))

X_given_Y;Z_given_Y

# Calculate Z_given X
Z_given_X = 
  c(solve(X_given_Y, Z_given_Y[,1]), solve(X_given_Y, Z_given_Y[,2]))

# get joint distribution 
c(Z_given_X[c(1,3)]*P_X, Z_given_X[c(2,4)]*(1-P_X))

# 2-2 combination is outside the bounds in one sample already so makes sense to
# say that the assumption being violated is the reason
# which is a nice example of the result reflecting the assumption and not the
# true distribution, which was the problem with the CIA

# For population 2, common outcome the 2-2 and ins2-m1 combination were outside the bounds
# in the simulation
samplepop2_out = 
  GenerateSample(population = populations[[2]]$FullPopulation,
                 common_variable = "outcome",
                 n1 = 2000,
                 n2 = 2000)

# get z given x from data
Z_given_X = 
  prop.table(samplepop2_out$DistributionSecondSample, margin = 1)

# get z given y from data
Z_given_Y = 
  prop.table(samplepop2_out$DistributionFirstSample, margin = 1)

# get p(Z) from data
p_Z =
  ( sum(samplepop2_out$DistributionFirstSample[,1]) + sum(samplepop2_out$DistributionSecondSample[,1]) ) / 2

# get p(X) using p(Y) for more accuracy
p_Y = 
  (Z_given_Y[1,1] * p_Y ) + 
  (Z_given_Y[2,1] * (1 - p_Y))

Z_given_X;Z_given_Y

# Calculate Z_given X
X_given_Y = 
  c(solve(Z_given_X, Z_given_Y[,1]), solve(Z_given_X, Z_given_Y[,2]))

# get joint distribution 
# needs restriction 
c(X_given_Y[c(1,3)]*p_Y, X_given_Y[c(2,4)]*(1-p_Y))

# 2-1 and 2-2 are outside the bounds as expected, 1-2 would be if not for the restriction
# the bound starts at 0. 

########################################## POP 3 ##########################################

# For population 3, common instrumental the 2-2 combination was outside the bounds
# in the simulation
samplepop3_ins = 
  GenerateSample(population = populations[[3]]$FullPopulation,
                 common_variable = "instrumental",
                 n1 = 2000,
                 n2 = 2000)

# get x given y from data
X_given_Y = 
  prop.table(samplepop3_ins$DistributionFirstSample, margin = 1)

# get z given y from data
Z_given_Y = 
  prop.table(samplepop3_ins$DistributionSecondSample, margin = 1)

# get p(Y) from data
p_Y =
  ( sum(samplepop3_ins$DistributionFirstSample[1,]) + sum(samplepop3_ins$DistributionSecondSample[1,]) ) / 2

# get p(X) using p(Y) for more accuracy
P_X = 
  (X_given_Y[1,1] * p_Y ) + 
  (X_given_Y[2,1] * (1 - p_Y))

X_given_Y;Z_given_Y

# Calculate Z_given X
Z_given_X = 
  c(solve(X_given_Y, Z_given_Y[,1]), solve(X_given_Y, Z_given_Y[,2]))

# get joint distribution 
c(Z_given_X[c(1,3)]*P_X, Z_given_X[c(2,4)]*(1-P_X))

# 2-2 combination is outside the bounds in one sample already so makes sense to
# say that the assumption being violated is the reason
# which is a nice example of the result reflecting the assumption and not the
# true distribution, which was the problem with the CIA

# For population 3, common outcome the 2-2 and ins2-m1 combination were outside the bounds
# in the simulation
samplepop3_out = 
  GenerateSample(population = populations[[3]]$FullPopulation,
                 common_variable = "outcome",
                 n1 = 2000,
                 n2 = 2000)

# get z given x from data
Z_given_X = 
  prop.table(samplepop3_out$DistributionSecondSample, margin = 1)

# get z given y from data
Z_given_Y = 
  prop.table(samplepop3_out$DistributionFirstSample, margin = 1)

# get p(Z) from data
p_Z =
  ( sum(samplepop3_out$DistributionFirstSample[,1]) + sum(samplepop3_out$DistributionSecondSample[,1]) ) / 2

# get p(X) using p(Y) for more accuracy
p_Y = 
  (Z_given_Y[1,1] * p_Y ) + 
  (Z_given_Y[2,1] * (1 - p_Y))

Z_given_X;Z_given_Y

# Calculate Z_given X
X_given_Y = 
  c(solve(Z_given_X, Z_given_Y[,1]), solve(Z_given_X, Z_given_Y[,2]))

# get joint distribution 
# needs restriction 
c(X_given_Y[c(1,3)]*p_Y, X_given_Y[c(2,4)]*(1-p_Y))

# 2-2 is outside the bounds as expected, 1-2 would be if not for the restriction
# the bound starts at 0. 2-1 is on the edge of the bound so it is likely that 
# in other samples it goes outside of the bounds

