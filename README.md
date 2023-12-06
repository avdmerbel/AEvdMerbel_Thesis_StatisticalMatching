# Statistical Matching using the Instrumental Variable Assumption

## 1. Introduction
The purpose of this project is to evaluate the use of an instrumental variable assumption when statistically matching two data sets. Within this project the focus lies on categorical variables with two categories each, which is the premise the functions outlined below are based on. In section 2 each functions purpose is described. The input arguments and their formats are given and the output is outlined. Section 3 includes coding examples of the use of the functions. 

## 2. Functions
### a. Generating data
#### i. GeneratePopulation_InstrumentalVariable
This function generates a population distribution under the instrumental variable assumption (conditional independence of the instrumental variable and the outcome, given the mediator). 

**REQUIRED PACKAGES**: dplyr, sampling

**REQUIRED FUNCTIONS**: Reverse_OddsRatio


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
This function generates a population distribution where the instrumental variable assumption does not hold (conditional independence of the instrumental variable and the outcome, given the mediator).

**REQUIRED PACKAGES**: dplyr

**REQUIRED FUNCTIONS**: Reverse_OddsRatio


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
#### i. StatisticalMatching_Bootstrap
This function takes a bootstrap sample from the input and estimates the joint distribution of not commonly observed variables. 

**REQUIRED PACKAGES**: _none_

**REQUIRED FUNCTIONS**: EstimateDistribution_InstrumentalVariable, EstimateDistribution_ConditionalIndependence

INPUT													| description
------------------------------|--------------------------------------------------------------------------------------------
sample_input									| Data frame with two samples, with one overlapping variable (output of GenerateSample)
assumption = c("IVA", "CIA")  | String variable indicating under which assumption the estimation is done; instrumental variable or conditional independence
restriction = FALSE           | Boolean indicating whether the probabilities in the estimation need to be restricted between 0 and 1


OUTPUT												| description
------------------------------|---------------------------------------------------------------------------------------------
estimated  										| Numeric vector with the bootstrapped estimation of the joint distribution of the not commonly observed variables
variable_combinations         | Data frame with the category combinations in the same order as the estimated values (for reference)

#### ii. StatisticalMatching_BootstrapParameters
This function estimates the mean, median, variance and standard deviation of the bootstrap estimations calculated using StatisticalMatching_Bootstrap. These parameters are calculated for each category combination separately. Below an example for three bootstrap samples:

variable X | variable Y | Bootstrap 1 | Bootstrap 2 | Bootstrap 3 | mean | median | variance | standard deviation
-----------|------------|-------------|-------------|-------------|------|--------|----------|--------------------
1          | 1          | .3          | .25         | .31         | .287 | .3     | .001     | .0321
1          | 2          | .03         | .04         | .04         | .037 | .04    | < .0001  | .0058
2          | 1          | .09         | .11         | .07         | .09  | .09    | < .0001  | .02
2          | 2          | .58         | .60         | .58         | .587 | .58    | .0001    | .012

**REQUIRED PACKAGES**: _none_

**REQUIRED FUNCTIONS**: GenerateSample, EstimateDistribution_InstrumentalVariable, EstimateDistribution_ConditionalIndependece 


INPUT													| description
------------------------------|--------------------------------------------------------------------------------------------
bootstraps  									| Data frame with the bootstrapped estimations of the joint distribution (output of StatisticalMatching_Bootstrap
variable_combinations         | Data frame with the variable combinations used in estimating the bootstraps (output of StatisticalMatching_Bootstrap)


OUTPUT												| description
------------------------------|---------------------------------------------------------------------------------------------
result    										| List with a data frame for each statistical matching situation with the category combinations and their mean, median, variance and standard deviation


### d. Simulation
#### i. StatisticalMatching_Simulation
This function estimates the joint distribution from a sample simulated form the population. 

**REQUIRED PACKAGES**: _none_

**REQUIRED FUNCTIONS**: GenerateSample, EstimateDistribution_InstrumentalVariable, EstimateDistribution_ConditionalIndependece 

INPUT													| description
------------------------------|--------------------------------------------------------------------------------------------
population  									| Data frame with the population you want to simulate from (output FullPopulation from GeneratePopulation_...)
assumption  = c("IVA", "CIA") | String variable indicating under which assumption to estimate the joint probability, instrumental variable or conditional independence
restriction = FALSE           | Boolean indicating whether to restrict the probabilities between 0 and 1
situations  = c("instrumental", "mediator", "outcome") | String variable indicating for which situation you want to simulate, common instrumental, mediator or outcome (all by default)
n1          = 2000            | Numeric variable indicating the desired sample size of the first sample
n2          = 2000            | Numeric variable indicating the desired sample size of the second sample


OUTPUT												| description
------------------------------|---------------------------------------------------------------------------------------------
estimated  										| List of numeric vectors for each situation containing the estimated joint probabilities for all variable combinations
variable_combinations         | List of the variable combinations in the order of the estimated probabilities (for reference)

#### ii. StatisticalMatching_TrueParameters
This function calculates from simulated estimations the estimated 'true' mean, median, variance and standard deviation. This is done in the same way as in the Bootstrap functions, for each category combination. 

**REQUIRED PACKAGES**: _none_

INPUT													| description
------------------------------|--------------------------------------------------------------------------------------------
simulations  									| Data frame with the estimated joint probabilities from a simulation (output from StatisticalMatching_Simulation)
variable_combinations         | List of the variable combinations in the order of the estimated probabilities in the simulatins (output from StatisticalMatching_Simulation)

OUTPUT												| description
------------------------------|---------------------------------------------------------------------------------------------
result    										| List of data frames for each situation containing the 'true' mean, median, variance and standard deviation for each category combination

#### iii. StatisticalMatching_EstimationError
This function calulates the difference between each simulated distribution and the population distribution. It then returns three statistics:
- The Mean Absolute Error (MAE): With $s$ the number of simulated distributions $\hat{y}, ( \hat{y_i}, i = 1, 2, ..., s)$ and $y$ the distribution in the population it is defined as

$$ MAE = \frac{1}{s} \sum^{s}_{i=1} | \hat{y_i} - y| $$
  
- The Mean Squared Error (MSE): With $s$ the number of simulated distributions $\hat{y}, ( \hat{y_i}, i = 1, 2, ..., s)$ and $y$ the distribution in the population it is defined as

$$ MSE = \frac{1}{s} \sum^{s}_{i=1} (\hat{y_i} - y)^2 $$

- The Root Mean Squared Error (RMSE):

$$ RMSE = \sqrt{MSE} $$
   
INPUT													| description
------------------------------|--------------------------------------------------------------------------------------------
simulations  									| Data frame with the estimated joint probabilities from a simulation (output from StatisticalMatching_Simulation)
variable_combinations         | List of the variable combinations in the order of the estimated probabilities in the simulatins (output from StatisticalMatching_Simulation)
population_distribution       | List of matrices representing the distribution of the not commonly observed variables in the population (output AggregatedDistribution from GeneratePopulation_...)

OUTPUT												| description
------------------------------|---------------------------------------------------------------------------------------------
result    										| List of data frames for each situation containing the MAE, MSE and RMSE for each category combination

### e. Support functions
#### i. Reverse_OddsRatio
This function calculates the joint distribution for a given odds ratio. The odds ratio is calculated as $\frac{ad}{bc}$ where:

x \ y     | 1   | 2   | totals
----------|-----|-----|-----------
1         | a   | b   | $rowtotal_1$
2         | c   | d   | $rowtotal_2$
totals    | $columntotal_1$ | $columntotal_2$ | total

With the row and column totals (marginals) known it is possible to rewrite the equation and solve for e.g. $a$:

$$Odds Ratio = \frac{a * (rowtotal_2 - (columntotal_1 - a))}{(rowtotal_1 - a) * (columntotal_1 -a)}$$

Then we can use the value we found to calculate $b$, $c$, and $d$ and the row and column totals.

**REQUIRED PACKAGES**: stats

INPUT													| description
------------------------------|--------------------------------------------------------------------------------------------
rowmarginals  					   | Numeric vector of row marginals (ordered by category, so say you have categories 0 and 1, the first element in the vector should be for 0)
columnmarginals         | Numeric vector of column marginals (ordered by category, so say you have categories 0 and 1, the first element in the vector should be for 0)
oddsratio       | Numeric argument indicating the desired odds ratio

OUTPUT												| description
------------------------------|---------------------------------------------------------------------------------------------
output    										| Matrix with the frequency table beloning to the given odds ratio (so the values for a, b, c, and d)

## 4. Example
