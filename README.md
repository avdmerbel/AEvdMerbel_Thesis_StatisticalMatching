# Statistical Matching using the Instrumental Variable Assumption

## 1. Introduction
The purpose of this project is to evaluate the use of an instrumental variable assumption when statistically matching two data sets. Within this project the focus lies on categorical variables with two categories each, which is the premise the functions outlined below are based on. In section 2 each functions purpose is described. The input arguments and their formats are given and the output is outlined. 

## 2. Functions
### a. Generating data
#### i. GeneratePopulation_InstrumentalVariable
This function generates a population distribution under the instrumental variable assumption (conditional independence of the instrumental variable and the outcome, given the mediator). Note: this function uses the Reverse_OddsRatio function detailed in 2e: Support functions.

**REQUIRED PACKAGES**: dplyr, sampling

INPUT													| description
------------------------------|---------------------------------------------------------------------------------------------
N 								= 1000000		| Numeric argument indicating the size of the population to be generated
p_instrumental 		= runif(1)	| Numeric argument indicating the distribution of the instrumental variable in the population
p_mediator 				= runif(1)	| Numeric argument indicating the distribution of the mediating variable in the population
p_outcome 				= runif(1)	| Numeric argument indicating the distribution of the outcome variable in the population
oddsratio_ins_med = 7					| Odds Ratio between the instrumental and the mediating variable in the population
oddsratio_med_out = 4					| Odds Ratio between the mediating and the outcome variable in the population


OUTPUT												| description
------------------------------|---------------------------------------------------------------------------------------------
FullPopulation								| Data frame with the distribution of all combinations of the three variables in the population in probability and counts
DistributionMediator1					| Data frame with the distribution of the instrumental and outcome variables in the population, for level 1 of the mediator
DistributionMediator2					| Data frame with the distribution of the instrumental and outcome variables in the population, for level 2 of the mediator
AggregatedDistribution				| List of data frames with the distribution of the combination of two variables, for each statistical matching situation


#### ii. GeneratePopulation_NotInstrumentalVariable
This function generates a population distribution where the instrumental variable assumption does not hold (conditional independence of the instrumental variable and the outcome, given the mediator). Note: this function uses the Reverse_OddsRatio function detailed in 2e: Support functions.

**REQUIRED PACKAGES**: dplyr

INPUT													| description
------------------------------|---------------------------------------------------------------------------------------------
mediator_1										| Data frame with the distribution of the instrumental and outcome variables in the population, for level 1 of the mediator (output from GeneratePopulation_InstrumentalVariable)
mediator_2										| Data frame with the distribution of the instrumental and outcome variables in the population, for level 2 of the mediator (output from GeneratePopulation_InstrumentalVariable)
oddsratio_mediator_1 = 1			| Odds Ratio between the instrumental and the outcome variable for level 1 of the mediator
oddsratio_mediator_2 = 1			| Odds Ratio between the instrumental and the outcome variable for level 2 of the mediator


OUTPUT												| description
------------------------------|--------------------------------------------------------------------------------------------
FullPopulation								| Data frame with the distribution of all combinations of the three variables in the population in probability and counts
DistributionMediator1					| Data frame with the distribution of the instrumental and outcome variables in the population, for level 1 of the mediator
DistributionMediator2					| Data frame with the distribution of the instrumental and outcome variables in the population, for level 2 of the mediator
AggregatedDistribution				| List of data frames with the distribution of the combination of two variables, for each statistical matching situation


#### iii. GenerateSample
This function generates a sample from the population generated using the GeneratePopulation_... functions.

**REQUIRED PACKAGES**: dplyr, extraDistr

INPUT													| description
------------------------------|--------------------------------------------------------------------------------------------
population										| Data frame with the distribution of the instrumental and outcome variables in the population, (output from GeneratePopulation_InstrumentalVariable)
common_variable								| String argument which specifies which variable is jointly observed (instrumental, mediator, outcome). 
n1 = 2000 										| Numeric argument indicating the desired sample size for the first sample
n2 = 2000											| Numeric argument indicating the desired sample size for the second sample


OUTPUT												| description
------------------------------|---------------------------------------------------------------------------------------------
FirstSample										| Data frame with the counts and proportions of for each category combination in the first sample
N1														| Realized sample size for the first sample
DistributionFirstSample				| xtabs object representing the contingency table between the variables in the first sample
SecondSample									| Data frame with the counts and proportions of for each category combination in the second sample
N2														| Realized sample size for the second sample
DistributionSecondSample			| xtabs object representing the contingency table between the variables in the second sample
FirstSampleUnobserved					| Data frame with the counts and proportions of each category combination in the first sample including the 'unobserved' variable
SecondSampleUnobserved				| Data frame with the counts and proportions of each category combination in the second sample including the 'unobserved' variable
CommonVariable								| String indicating which variable was the common variable in generating the sample


### b. Statistical Matching
#### i. EstimateDistribution_InstrumentalVariable
This function estimates the distribution between the not commonly observed variables under the instrumental variable assumption. Under the instrumental variable assumption it holds that:

$$ P(Outcome = k | Instrumental = i) = \sum_j P(Mediator = j | Instrumental = i)P(Outcome = k | Mediator = j) $$

Each time one variable is commonly observed, since we have three variables this can be either the instrumental, the mediator, or the outcome. If the mediating variable is commonly observed, the equation is easily calculated. If any of the other variables is commonly observed, linear equations have to be solved to get to the final solution.
To calculate the joint probability of the not commonly observed variables, we multiply the conditional probability by the total probability. To get the most accurate result we use the total probability of the commonly observed variable, since we have the most data on this variable (from both samples). For example if we want the joint distribution of the outcome and the instrumental variables we would have:

$$ P(Outcome, Instrumental) = \sum_j P(Outcome | Mediator = j)P(Instrumental | Mediator = j)P(Mediator = j) $$

The output includes restricted probabilities and counts. In some situations it might occur that the estimation includes probabilities that are outside of the bounds of 0 and 1. This is obviously impossible so the probabilities are coerced to be between 0 and 1. 

**REQUIRED PACKAGES**: _none_

INPUT													| description
------------------------------|--------------------------------------------------------------------------------------------
sample_input									| List with the output from GenerateSample
instrumental = "instrumental"	| String variable which indicates which variable has the role of the instrumental (if other variable names are used)
mediator = "mediator"					| String variable which indicates which variable has the role of the mediator (if other variable names are used)
outcome = "outcome"						| String variable which indicates which variable has the role of the outcome (if other variable names are used)


OUTPUT												| description
------------------------------|---------------------------------------------------------------------------------------------
Estimation										| Data frame with the estimated (restricted) probabilities and (restricted) counts of each variable combination of the not commonly observed variables

#### ii. EstimateDistribution_ConditionalIndependence
This function estimates the distribution between the not commonly observed variables under the conditional independence assumption. Under the instrumental variable assumption it holds that:

$$ P(Z,Y) = \sum_x P(Z | X = x)P(Y | X = x)P(X = x) $$

The conditional independence assumption assumes that whatever the situation is, the not commonly observed variables are independent of each other given the commonly observed variable. In that sense there is always one situation possible and the values can be filled in in the formula.

The output includes restricted probabilities and counts. In some situations it might occur that the estimation includes probabilities that are outside of the bounds of 0 and 1. This is obviously impossible so the probabilities are coerced to be between 0 and 1. 

**REQUIRED PACKAGES**: dplyr, tools, stats, sampling

INPUT													| description
------------------------------|--------------------------------------------------------------------------------------------
sample_input									| List with the output from GenerateSample


OUTPUT												| description
------------------------------|---------------------------------------------------------------------------------------------
Estimation										| Data frame with the estimated (restricted) probabilities and (restricted) counts of each variable combination of the not commonly observed variables


### c. Bootstrap

### d. Simulation

### e. Support functions

## 4. Example
