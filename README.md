Running and Graphing glmers
================
TJ Wukitsch
July 2023

# BACKGROUND

These data are from [Wukitsch & Cain, 2021](10.1007/s00213-021-05805-y)
and are used here as an example of how to use `ggplot2` to graph
Generalized Linear Mixed Effects Models using R. In this case, we have
count data so we will use a Poisson model. The `ggplot2` library along
with a few others, were used to produce the images in the aforementioned
publication. Below is a comprehensive guide on how you can make ones
like it for your glmer models too!

## Understanding the Data

![](README_files/Experimental%20Structure.png)

These data originate from a portion of the study that had 2 independent
(output) variables: Total Aversive Responses & Total Hedonic Responses.
These variables are counts of the number of behaviors emitted in
response to an orally infused fluid and are the behavioral results of
the Taste Reactivity Test. Prior to undergoing Taste Reactivity testing,
adult and adolescent Long-Evans rats were allowed Intermittent
(voluntary) Access to drink Ethanol (IAE) Monday, Wednesday, and Friday.
The amount and pattern of drinking over time during IAE were dependent
(predictor) variables in some of our models in addition to their Age
group. Control animals received only water during IAE and were not
exposed to ethanol until Taste Reactivity testing. During Taste
Reactivity testing, 3 substances were orally infused: Water on the first
and last day of Taste Reactivity testing, Ethanol (at 5, 20, and 40% v/v
concentrations), and Sucrose (at .01, .1, and 1 molar concentrations).
We will focus on the Ethanol data here and want to treat all variables
as they are… No categorizing our numeric continua into factors. This
yields the following set of variables for our models:

**Independent (Outcome) Variables:**

- Total Aversive Responses (count data)
- Total Hedonic Responses (count data)

**Dependent (Predictor) Variables:**

- **Age** (during IAE period)
  - Adult
  - Adolescent
- Intermittent Ethanol Access (IAE) **Condition**
  - IAE (Had access to ethanol during IAE period)
    - **Ethanol Consumption** during IAE period *(A NESTED VARIABLE)*
  - CTRL (Did not have access to ethanol prior to Taste Reactivity
    Testing)
- **Concentration** of Ethanol
  - 5%
  - 20%
  - 40%

## Modeling Approach

This nested data structure leaves us with some options. We can either 1.
use a nested model or 2. perform two separate analyses of the same data
that answer different questions. It all depends on your audience. We
decided it was better for our audience to keep it straightforward and
use the two analysis approach. The first model will answer questions
about overall differences between our IAE and our CTRL groups, Age, and
Concentration. The second will be able to answer questions about
individual differences in ethanol consumption during IAE and how those
may interact with Age and Concentration to alter taste reactivity. Thus,
the formulas for our `glmer` models are as follows:

**Overall Models**

- **Independent (AKA Predicted AKA Outcome) Variables**
  - Total Aversive Responses
  - Total Hedonic Responses
- **Fixed Effect (AKA Main Effect) Variables (Full Factorial)**
  - Concentration
  - Age
  - Condition
- **Random Effect Variables**
  - Intercept (RatID)
  - (Slope of) Concentration

This yields the following conceptual formulas for the Overall models:

- $Aversive~Responses = exp(Age * Condition * Concentration + (Concentration | RatID))$
- $Hedonic~Responses = exp(Age * Condition * Concentration + (Concentration | RatID))$

**Ethanol Consumption Models**

We haven’t yet determined which measure of ethanol consumption to use
for the Ethanol Consumption Models. We could use the sum total of all
ethanol consumed during IAE (Total Ethanol), or we could use the
intercept (Mean Alcohol Consumed; MAC) and slope of a line that best
fits each individual’s drinking during IAE across time (Rate of Change;
RoC). We used `lmers` to calculate these values, but that is beyond the
scope of the present example. A quirk of adolescent drinking during IAE
was also of interest. Adolescents had very high drinking on the first
day. This created some non-monotonicity (non-linearity) in our data, so,
we wondered if removing the first day would impact the RoC
substantially. Thus, we had an additional model that did not include the
first day of drinking when calculating RoC and MAC (RoC3 and MAC3,
respectively). Our plan was to compare the models predicting responses
to orally infused ethanol and use the model with the best (lowest)
AIC/BIC scores for interpretation.

- **Independent (AKA Predicted AKA Outcome) Variables**
  - Total Aversive Responses
  - Total Hedonic Responses
- **Fixed Effect (AKA Main Effect) Variables (Full Factorial where
  appropriate)**
  - Concentration
  - Age
  - Ethanol Consumption
    - Total Ethanol Consumed (grams ethanol/kg bodyweight; during IAE)
    - MAC & RoC (Consumption Pattern during IAE)
    - MAC3 & RoC3 (Consumption Pattern starting at day 3 \[the second
      day of drinking\])
- **Random Effect Variables**
  - Intercept (RatID)
  - (Slope of) Concentration

This yields the following conceptual formulas for the Ethanol
Consumption models:

- Aversive Responses
  - $Aversive~Responses = exp(Concentration * Age * TotalEthanolConsumed + (Concentration | RatID))$
  - $Aversive~Responses = exp(Concentration * Age * [MAC + RoC] + (Concentration | RatID))$
  - $Aversive~Responses = exp(Concentration * Age * [MAC3 + RoC3] + (Concentration | RatID))$
- Hedonic Responses
  - $Hedonic~Responses = exp(Concentration * Age * TotalEthanolConsumed + (Concentration | RatID))$
  - $Hedonic~Responses = exp(Concentration * Age * [MAC + RoC] + (Concentration | RatID))$
  - $Hedonic~Responses = exp(Concentration * Age * [MAC3 + RoC3] + (Concentration | RatID))$

*Note: the brackets ‘\[\]’ are intended to indicate that MAC and RoC
(and MAC3 and RoC3) do not interact with each other in the models above.
This will become clearer when we specify the formula in long-form within
the coding for the model in a later step.*

# Coding Approach Goals

Now that we understand our data and our modeling approach, we need to
consider our order of operations and goals for our data. We want an
organized workspace that we can come back to and modify if needs change
or if we have additional questions. We want to organize our models and
any additional analyses into smaller sets of code, so we can more easily
navigate and troubleshoot. And, finally, we want to have some pretty
graphs that get our point across and clearly represent the data well.
This gives us 4 goals…

**GOALS:**

1.  Organize and Setup Data Workspace & Structures
2.  Model Data (Generalized Linear Mixed Effects Models) & Organize
    Output Objects
3.  Perform Comparisons, Obtain Trend Info., & Organize Output objects
4.  Create Publication-Quality Nested Multi-panel Graphs with `ggplot2`
    & Organize Output objects

# 1. Workspace Setup

## Install and Load Required Packages

If you haven’t already downloaded and installed Rtools, please install
[`Rtools`](https://cran.r-project.org/bin/windows/Rtools/) and restart
R. The C++ compiler is required for some of the packages that you are
about to install.

The following code will check and remind you if you haven’t yet
installed Rtools. Then it will check to see if the package is already
downloaded and installed. If so, it will load the package. If not, it
will install the package from CRAN and then load the package once the
installation is complete. You may be prompted to allow some of the
downloads to take place so please remain at your device. I recommend
making a separate R script like this for every project. As you add
packages that you need, be sure to add them to your setup script and
save time and guess work.

``` r
# Load Packages ####

# See if rtools is installed. It has necessary C++ compiler for installs of some of the libraries that are required.
if (Sys.which("make") == "") {  # Check to see if "make" command from rtools is found in system's PATH. If an empty string is retuned, rtools isn't installed.
  message("Rtools is not found. Please install Rtools from https://cran.r-project.org/bin/windows/Rtools/ and restart R.")
} else { #rtools installed == TRUE
  message("Rtools is installed.")
  
  # Load package or install MASS and load if not present
  if (!require(MASS)) {
    install.packages("MASS")
    library(MASS)
  }
  
  # Load package or install lattice and load if not present
  if (!require(lattice)) {
    install.packages("lattice")
    library(lattice)
  }
  
  # Load package or install boot and load if not present
  if (!require(boot)) {
    install.packages("boot")
    library(boot)
  }
  
  # Load package or install car and load if not present
  if (!require(car)) {
    install.packages("car")
    library(car)
  }
  
  # Load package or install emmeans and load if not present
  if (!require(emmeans)) {
    install.packages("emmeans")
    library(emmeans)
  }
  
  
  # Load package or install lme4 and load if not present
  if (!require(lme4)) {
    install.packages("lme4")
    library(lme4)
  }
  
  # Load package or install zoo and load if not present
  if (!require(zoo)) {
    install.packages("zoo")
    library(zoo)
  }
  
  # Load package or install tidyr and load if not present
  if (!require(tidyr)) {
    install.packages("tidyr")
    library(tidyr)
  }
  
  # Load package or install ggplot2 and load if not present
  if (!require(ggplot2)) {
    install.packages("ggplot2")
    library(ggplot2)
  }
  
  # Load package or install lmerTest and load if not present
  if (!require(lmerTest)) {
    install.packages("lmerTest")
    library(lmerTest)
  }
  
  # Load package or install dplyr and load if not present
  if (!require(dplyr)) {
    install.packages("dplyr")
    library(dplyr)
  }
  
  # Load package or install multcomp and load if not present
  if (!require(multcomp)) {
    install.packages("multcomp")
    library(multcomp)
  }
  
  # Load package or install foreign and load if not present
  if (!require(foreign)) {
    install.packages("foreign")
    library(foreign)
  }
  
  # Load package or install msm and load if not present
  if (!require(msm)) {
    install.packages("msm")
    library(msm)
  }
  
  # Load package or install effects and load if not present
  if (!require(effects)) {
    install.packages("effects")
    library(effects)
  }
  
    # Load package or install effects and load if not present
  if (!require(gridExtra)) {
    install.packages("gridExtra")
    library(gridExtra)
  }

  # Load package or install patchwork and load if not present
  if (!require(patchwork)) {
    install.packages("patchwork")
    library(patchwork)
  }

} # end rtools installed == TRUE
```

## Set your working directory

Using a working directory allows for relative referencing as opposed to
absolute (aka static) referencing. Instead of changing every single
absolute reference to a folder on one computer throughout all your code,
you just need to change the working directory at the beginning and
load/save from there. For example, say I want to reference the
Experimental Structure.png from above. Instead of an absolute reference
path like “C:/Users/name/Documents/ABHV/README_files/Experimental
Structure.png”, a relative reference path would be
“README_files/Experimental Structure.png” Because our working directory
is already set to the ABHV folder that contains the README_files folder,
we dont need the extra fluff. This results in code that not only saves
time because it is shorter, but is much more flexible for a variety of
situations like using the code on another computer. Always try to use
relative referencing once you have changed your working directory to
make your life (and anyone’s life that you share you code with) much
easier.

``` r
# Set your working directory to your project folder and check that it is correct. 
setwd("C:/Users/kieri/Documents/ABHV")
getwd()
```

## Workspace Overview

To keep an organized and uncluttered workspace, I use sets of
hierarchical lists to store related objects. We have the following
object categories with most analyses:

- data
- models
- comparisons (abbv. “compars”)
- plots

In this specific set of analyses we have a number of needs that
determine the objects in each category.

### Data Storage

The full data requires various sub-setting to ensure the correct
substances are being assessed independently. For example, we want to
make sure that controls that never received access to ethanol are not
included in an analysis or mean calculation when it would be
inappropriate to do so. Thus, my data list object will look like this
(with abbreviated names for easier calling in the code itself):

- Data
  - Raw
  - Ethanol
    - With Controls
    - No Controls
  - Sucrose
    - With Controls
    - No Controls
  - water
    - With Controls
    - No Controls

To do this we generate a list called `data` and begin populating it with
subsets of the original data. In the process, it will be easier to make
some changes now because subsets of data inherit certain attributes from
their parent data sets. Therefore, during this step, we will also adjust
the contrast coding and scale of some of our variables to help us avoid
problems later. We adjust our categorical (factor) variables’ contrast
coding attribute because we want our `glmer` models’ categorical
variables to be coded like an ANOVA to ease interpretation of the
effects. In other words, we want our model’s intercept to be at the
Grand Mean. This is just a personal preference of mine because I am used
to it. This requires us to recode our categorical variables using
sum-to-zero aka contrast coding instead of dummy coding or other types.
Briefly, one group is assigned +1 and the other is assigned -1 and they
sum to zero.

``` r
# Create our main data object
ABHVdata <- list()

    # Read in data file 
    ABHVdata$raw <- read.csv("ABHV2018.csv",
                         na.strings="\"\"", #with blanks/ N/As set to blank ("\"\"")
                         stringsAsFactors = TRUE) # categorical/character data as "factor"
    ABHVdata$raw$RatID <- as.factor(ABHVdata$raw$RatID) # Convert RatID to factor type because it is not a numeric variable although it seems like one.
      
    # Change to Contrast Sum Coding (sum-to-zero). 
      # This attribute will be inherited for all data frames subset from this one,
      # so it is a good idea to perform this step now, instead of many times later.
    
      # adjust Age to contrast coding
      contrasts(ABHVdata$raw$Age)=contr.sum(2)
      contrasts(ABHVdata$raw$Age)
      # adjust Condition to contrast coding
      contrasts(ABHVdata$raw$Condition)=contr.sum(2)
      contrasts(ABHVdata$raw$Condition)
      
        
  ## Subset Ethanol Data ####
      
    # With Controls
    ABHVdata$eth$ctrl <- subset(ABHVdata$raw, Substance == "Ethanol")
        # Rescale to avoid issues with eigen values later
        ABHVdata$eth$ctrl$recoded.conc <- car::recode(ABHVdata$eth$ctrl$Concentration, "5 =.05; 20 =.20; 40 =.40")
      
    # No Controls
      # Subset rats that could drink ethanol from rats that never had the opportunity
      ABHVdata$eth$no.ctrl <- subset(ABHVdata$eth$ctrl, Condition != "CTRL")
    
    # Uncomment the 2 lines below to view your data frames in RStudio
    #View(ABHVdata$eth$ctrl)
    #View(ABHVdata$eth$no.ctrl)
    
  ## Subset Sucrose Data ####
    # With Controls
    ABHVdata$suc$ctrl <- subset(ABHVdata$raw, Substance == "Sucrose")
      # Rescale -- to avoid issues with eigen values later
      ABHVdata$suc$ctrl$molarity <- car::recode(ABHVdata$suc$ctrl$Concentration, ".34=.01; 3.4=.1; 34=1")
    
    # No Controls
      # Subset rats that could drink ethanol from rats that never had the opportunity
    ABHVdata$suc$no.ctrl <- subset(ABHVdata$suc$ctrl, Condition != "CTRL")
    
    #Uncomment the 2 lines below to view your data frames in RStudio
    #View(ABHVdata$suc$ctrl)
    #View(ABHVdata$suc$no.ctrl)
  
  ## Subset Water Data ####
    # With Controls
    ABHVdata$h2o$ctrl <- subset(ABHVdata$raw, Substance == "Water1" | Substance == "Water2")
      # Get rid of the 4 levels of Substance inherited from the original dataset
      ABHVdata$h2o$ctrl$Substance <- factor(ABHVdata$h2o$ctrl$Substance)
      # Recode this for clarity and easy use as a label in future graphs
      ABHVdata$h2o$ctrl$trial <- dplyr::recode(ABHVdata$h2o$ctrl$Substance,
                                          "Water1" = "Trial 1",
                                          "Water2" = "Trial 2")
      # Adjust contrasts to sum-to-zero now that there are 2 factors
      contrasts(ABHVdata$h2o$ctrl$Substance)=contr.sum(2)
      contrasts(ABHVdata$h2o$ctrl$Substance)

      
    # No Controls
      # Subset rats that could drink ethanol from rats that never had the opportunity
    ABHVdata$h2o$no.ctrl <- subset(ABHVdata$h2o$ctrl, Condition != "CTRL")      
    
    #Uncomment the 2 lines below to view your data frames in RStudio
    #View(ABHVdata$h2o$ctrl)
    #View(ABHVdata$h2o$no.ctrl)
```

Now we have our data list object with the following hierarchical
structure:

- `ABHVdata`
  - `ABHVdata$raw`
  - `ABHVdata$eth`
    - `ABHVdata$eth$ctrl`
    - `ABHVdata$eth$no.ctrl`
  - `ABHVdata$suc`
    - `ABHVdata$suc$ctrl`
    - `ABHVdata$suc$no.ctrl`
  - `ABHVdata$h2o`
    - `ABHVdata$h2o$ctrl`
    - `ABHVdata$h2o$no.ctrl`

And we have also performed our coding adjustments. However, we still
need to further prepare our data to avoid issues during analysis such as
problems with variance inflation. If we mean-center the continuous
variables that will be predictors in an `lmer` or `glmer` model, the
variance inflation factor tends to remain within a tolerable range and
shouldn’t affect results. Thus we mean-center the variables we need to
below and save our workspace, so we can come back to this point later if
we mess something up.

``` r
  ## Mean Centering Variables ####
    
    # Ethanol
      # center Concentration to avoid issues with variance inflation factor (VIF) tolerances
      ABHVdata$eth$ctrl$c.conc <- ABHVdata$eth$ctrl$recoded.conc - mean(ABHVdata$eth$ctrl$recoded.conc)
      # center TOTAL.ETOH.Swap.Consumed..g.kg.
      ABHVdata$eth$no.ctrl$c.totale <- ABHVdata$eth$no.ctrl$TOTAL.ETOH.Swap.Consumed..g.kg. - mean(ABHVdata$eth$no.ctrl$TOTAL.ETOH.Swap.Consumed..g.kg.)
      # center Concentration 
      ABHVdata$eth$no.ctrl$c.conc <- ABHVdata$eth$no.ctrl$recoded.conc - mean(ABHVdata$eth$no.ctrl$recoded.conc)
      
      # center MAC and ROC
      ABHVdata$eth$no.ctrl$c.MAC <- ABHVdata$eth$no.ctrl$MAC - mean(ABHVdata$eth$no.ctrl$MAC)
      ABHVdata$eth$no.ctrl$c.ROC <- ABHVdata$eth$no.ctrl$ROC - mean(ABHVdata$eth$no.ctrl$ROC)
      ABHVdata$eth$no.ctrl$c.MAC3 <- ABHVdata$eth$no.ctrl$MAC3 - mean(ABHVdata$eth$no.ctrl$MAC3)
      ABHVdata$eth$no.ctrl$c.ROC3 <- ABHVdata$eth$no.ctrl$ROC3 - mean(ABHVdata$eth$no.ctrl$ROC3)
    
    
    # Sucrose
      # Center molarity
      ABHVdata$suc$ctrl$c.molarity <- ABHVdata$suc$ctrl$molarity - mean(ABHVdata$suc$ctrl$molarity)
      ABHVdata$suc$no.ctrl$c.molarity <- ABHVdata$suc$no.ctrl$molarity - mean(ABHVdata$suc$no.ctrl$molarity)
      # center TOTAL.ETOH.Swap.Consumed..g.kg.
      ABHVdata$suc$no.ctrl$c.totale <- ABHVdata$suc$no.ctrl$TOTAL.ETOH.Swap.Consumed..g.kg. - mean(ABHVdata$suc$no.ctrl$TOTAL.ETOH.Swap.Consumed..g.kg.)
    
    
    # Water
      # center TOTAL.ETOH.Swap.Consumed..g.kg.
      ABHVdata$h2o$no.ctrl$c.totale <- ABHVdata$h2o$no.ctrl$TOTAL.ETOH.Swap.Consumed..g.kg. - mean(ABHVdata$h2o$no.ctrl$TOTAL.ETOH.Swap.Consumed..g.kg.)
    
    # Save the workspace
    save.image("ABHV_workspace.RData")
```

### Model and Comparison Storage

**Model Data**

To accommodate and store each list object returned by our models, we
need another hierarchical list object (`models`) with a similar
structure to the data above. However, there is more nuance because we
are performing analyses for hedonic and aversive responses and, for each
of those, we have analyses that involve different variables (Overall
Model, Total Ethanol, MR, and MR3). So we will need a slightly different
list structure that accommodates each formula we discussed in the
background section above. And, since we are comparing models based on
their ability to predict reactions to the substance of most interest to
us (Ethanol), we will have less analyses for Sucrose and Water. In the
end it should look something like this:

- Models
  - Ethanol
    - Aversive
      - Overall
      - Total Ethanol
      - Mean Alcohol Consumed & Rate of Change
      - Mean Alcohol Consumed & Rate of Change excluding day 3
    - Hedonic
      - Overall
      - Total Ethanol
      - Mean Alcohol Consumed & Rate of Change
      - Mean Alcohol Consumed & Rate of Change excluding day 3
  - Sucrose
    - Aversive
      - Overall
      - Total Ethanol
    - Hedonic
      - Overall
      - Total Ethanol
  - Water
    - Aversive
      - Overall
      - Total Ethanol
    - Hedonic
      - Overall
      - Total Ethanol

**Comparison Data**

Our comparison data will look a bit different. The overall structure of
our comparison list object (`compars`) will be the same as the models;
however, we won’t need to make comparisons for variables that aren’t
significant. Therefore, we will just have objects in `compars` based on
the analysis they came from and the name of the fixed effect/interaction
they are from. For example, the structure might look something like:

- Comparisons
  - Ethanol
    - Aversive
      - Overall
        - Condition
      - Total Ethanol
        - Age
        - Age\*Concentration (interaction)

We will discuss the organization of `plots` and related variables when
we reach that portion so it makes more sense. But we will need our
`models` and `compars` lists for the next steps, so lets make those now.

``` r
# List setup ####

## Models ####
models <- list()

## Comparisons ####
compars <- list()
```

# 2. & 3. Modeling & Comparisons

To quickly recap: This is count data that has some missing data due to
attrition. It also has a continuous predictor variable that represents
our repeated measure (Concentration) since each animal reacted to each
concentration of each substance. Therefore, a Poisson Generalized Linear
Mixed Effects Regression (glmer with a Poisson link function) is
appropriate for analysis because it can handle these issues/needs much
better than a lot of other models. As discussed before, there is some
nesting in the structure of the data. For the audience of the
publication, we opted to do two separate analyses on this data set
instead of properly nesting it. We are using ethanol as an illustrative
example because it also has two models that I wanted to compare and they
demonstrate different `glmer` formula configurations well.

For convenience, I will restate our variables:

**Independent (Outcome) Variables:**

- Total Aversive Responses (count data)
- Total Hedonic Responses (count data)

**Dependent (Predictor) Variables:**

- **Age** (during IAE period)
  - Adult
  - Adolescent
- Intermittent Ethanol Access (IAE) **Condition**
  - IAE (Had access to ethanol during IAE period)
    - **Ethanol Consumption** during IAE period *(A NESTED VARIABLE)*
  - CTRL (Did not have access to ethanol prior to Taste Reactivity
    Testing)
- **Concentration** of Ethanol
  - 5%
  - 20%
  - 40%

Now that we have our variables and our storage objects ready to go, we
can begin running our models. Remember that our continuous variables
will be centered variables (e.g. `c.conc` for Concentration)

## Ethanol Aversive Responses

### Overall Model

``` r
# Ethanol vs Control (Overall)

  ## Ethanol Aversives GLMER (with EtOH vs CTRL)####
    models$eth$avers$overall <- glmer(Total.Aversive ~ c.conc*Age*Condition # predictors: full factorial fixed effects of centered concentration, age, and condition
                                      + (c.conc|RatID), # and the random effects of the intercept (RatID) and the slope of concentration
                                      data = ABHVdata$eth$ctrl,
                                      family = poisson)
    # Get a summary of our output
    summary(models$eth$avers$overall)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: poisson  ( log )
    ## Formula: Total.Aversive ~ c.conc * Age * Condition + (c.conc | RatID)
    ##    Data: ABHVdata$eth$ctrl
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   1899.2   1934.7   -938.6   1877.2      175 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.4469 -1.1484 -0.2595  0.6624  7.9161 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev. Corr 
    ##  RatID  (Intercept) 0.6866   0.8286        
    ##         c.conc      9.8052   3.1313   -0.13
    ## Number of obs: 186, groups:  RatID, 63
    ## 
    ## Fixed effects:
    ##                         Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)             2.601289   0.115329  22.555  < 2e-16 ***
    ## c.conc                  2.490842   0.458706   5.430 5.63e-08 ***
    ## Age1                    0.114481   0.114782   0.997    0.319    
    ## Condition1              0.597808   0.115138   5.192 2.08e-07 ***
    ## c.conc:Age1            -0.009319   0.452014  -0.021    0.984    
    ## c.conc:Condition1       0.542285   0.456812   1.187    0.235    
    ## Age1:Condition1        -0.062439   0.114785  -0.544    0.586    
    ## c.conc:Age1:Condition1 -0.287132   0.452022  -0.635    0.525    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) c.conc Age1   Cndtn1 c.c:A1 c.c:C1 Ag1:C1
    ## c.conc      -0.146                                          
    ## Age1        -0.014  0.005                                   
    ## Condition1   0.327 -0.039  0.008                            
    ## c.conc:Age1  0.005 -0.027 -0.141  0.004                     
    ## c.cnc:Cndt1 -0.039  0.261  0.004 -0.143  0.012              
    ## Age1:Cndtn1  0.007  0.005  0.337 -0.014 -0.045  0.004       
    ## c.cnc:A1:C1  0.005  0.009 -0.045  0.004  0.292 -0.025 -0.141

Now we need to check to see if our assumption of normally distributed
residuals is correct using a simple plot.

``` r
    # plot residual density function
    plot(density(residuals(models$eth$avers$overall)), 
         main = "",
         xlab = "", 
         frame = FALSE)
    # Add normal distribution to the residual plot for comparison to check assumption of normality
    MEAN = mean(residuals(models$eth$avers$overall))
    STDEV = sqrt(var(residuals(models$eth$avers$overall)))
    curve(dnorm(x, mean = MEAN, sd = STDEV),
          col = "darkblue",
          lwd = 2,
          add = TRUE,
          yaxt = "n") # Generate normal curve
```

![](README_files/figure-gfm/Aversive%20Responses:%20Overall%20Model%20Residual%20Normality%20Check-1.png)<!-- -->

``` r
    remove(MEAN, STDEV) # Clean workspace
```

The blue line is a normal distribution. The black line is our residual
distribution. From the graph we can see the residuals are slighly
kurtotic (pointier and a bit thinner than a normal curve), but mostly
normal. I would say that the assumption of normality has been met.

Now, on to interpretation. We have our summary from before, but we need
to compare our significant variables. `emmeans` is a convenient package
for this. I can look at the means from the model and compare my two
levels of my Condition variable to see which direction the difference is
in and how large the difference between means is. When your categorical
variables have 3 or more levels, *post hoc* comparisons that control for
familywise error rate are available using the `pairs()` function from
the `emmeans` library.

``` r
  # Compare significant variables
  compars$eth$avers$overall$condition <- emmeans(models$eth$avers$overall, ~ Condition) # Perform comparison of Condition with emmeans
  
  # We dont need a comparison from c.conc because we have a direction AND a magnitude on a continuous variable! WOO!
  
  summary(compars$eth$avers$overall$condition, type = "response") # get summary in the numerical space of the original variable not the log/Poisson space
```

    ##  Condition  rate    SE  df asymp.LCL asymp.UCL
    ##  CTRL      24.51 4.601 Inf     16.97     35.41
    ##  EtOH       7.41 0.991 Inf      5.71      9.64
    ## 
    ## Results are averaged over the levels of: Age 
    ## Confidence level used: 0.95 
    ## Intervals are back-transformed from the log scale

``` r
  #Save Workspace
  save.image("ABHV_workspace.RData")
```

We document these numbers, save our workspace and then move on to the
next model.

### Total Ethanol Consumed

Next we run our Total Ethanol Consumed model. However, this model will
have some problems with convergence, so we will need to update from
where the model left off and use the optimizer controls to extend the
number of iterations the model uses to find convergence.

``` r
# predictors: full factorial fixed effects of centered concentration, age, and total ethanol consumed during drinking phase of study
models$eth$avers$total.e <- glmer(Total.Aversive ~ c.conc*Age*c.totale 
                                  + (c.conc|RatID), # and the random effects of the intercept (RatID) and the slope of concentration
                                  data = ABHVdata$eth$no.ctrl,
                                  family = poisson)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00253217 (tol = 0.002, component 1)

``` r
  # Model did not converge. Grab Theta and fixed effects data from model and put into temporary object
  ss <- getME(models$eth$avers$total.e,c("theta", "fixef"))
  # Extend the number of iterations with optimizer and update model
  models$eth$avers$total.e <- update(models$eth$avers$total.e,
                                     start = ss,
                                     control = glmerControl(optCtrl = list(maxfun = 2e6))) 
```

``` r
  # Model converged, get summary of output
  summary(models$eth$avers$total.e)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: poisson  ( log )
    ## Formula: Total.Aversive ~ c.conc * Age * c.totale + (c.conc | RatID)
    ##    Data: ABHVdata$eth$no.ctrl
    ## Control: glmerControl(optCtrl = list(maxfun = 2e+06))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   1129.8   1161.2   -553.9   1107.8      117 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.4668 -1.0539 -0.2397  0.5683  7.8749 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev. Corr 
    ##  RatID  (Intercept)  0.6764  0.8225        
    ##         c.conc      16.2682  4.0334   -0.16
    ## Number of obs: 128, groups:  RatID, 43
    ## 
    ## Fixed effects:
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.9159211  0.1458033  13.140  < 2e-16 ***
    ## c.conc                2.4540457  0.7498839   3.273  0.00107 ** 
    ## Age1                  0.4364453  0.1439052   3.033  0.00242 ** 
    ## c.totale             -0.0257438  0.0055196  -4.664  3.1e-06 ***
    ## c.conc:Age1           0.1303544  0.7348544   0.177  0.85920    
    ## c.conc:c.totale       0.0002537  0.0284031   0.009  0.99287    
    ## Age1:c.totale         0.0020847  0.0054987   0.379  0.70460    
    ## c.conc:Age1:c.totale -0.0450882  0.0283497  -1.590  0.11174    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) c.conc Age1   c.totl c.c:A1 c.cn:. Ag1:c.
    ## c.conc      -0.214                                          
    ## Age1        -0.092  0.041                                   
    ## c.totale     0.101 -0.065 -0.391                            
    ## c.conc:Age1  0.042 -0.126 -0.203  0.108                     
    ## c.cnc:c.ttl -0.065  0.139  0.107 -0.210 -0.400              
    ## Age1:c.totl -0.387  0.111  0.090 -0.080 -0.058  0.076       
    ## c.cnc:Ag1:.  0.110 -0.393 -0.058  0.078  0.127 -0.096 -0.206

``` r
  remove(ss) # Remove temporary object to declutter workspace
```

Again, before we begin interpreting, we need to check to see if our
assumption of normally distributed residuals is correct using a plot of
probability density.

![](README_files/figure-gfm/Aversive%20Responses:%20Total%20Ethanol%20Residual%20Normality%20Check-1.png)<!-- -->

Again, the blue line is a normal distribution. The black line is our
residual distribution. From the graph we can see the residuals are,
again, slightly kurtotic but mostly normal. I would say that the
assumption of normality has been met here as well.

On to interpretation. We need to look at the means from the model and
compare the two levels of our Age variable to see which direction and
how large the difference is. Copy down the data we need, save, rinse,
and repeat…

``` r
    # Compare significant variables
    compars$eth$avers$total.e <- list() # Create new comparison list for total.e model
    compars$eth$avers$total.e$age <- emmeans(models$eth$avers$total.e, ~ Age) # Perform comparison of Condition with emmeans
    summary(compars$eth$avers$total.e$age, type = "response") # get summary in the numerical space of the original variable not the log/Poisson space
```

    ##  Age         rate   SE  df asymp.LCL asymp.UCL
    ##  Adolescent 10.51 2.05 Inf      7.17     15.41
    ##  Adult       4.39 0.94 Inf      2.89      6.68
    ## 
    ## Confidence level used: 0.95 
    ## Intervals are back-transformed from the log scale

``` r
  #Save Workspace
  save.image("ABHV_workspace.RData")
```

### MAC & RoC (MR1)

``` r
  # predictors: note that these are not the full factorial fixed effects. I'm not interested in the higher level interactions here.
models$eth$avers$MR1 <- glmer(Total.Aversive ~ c.conc+Age+c.MAC+c.ROC
                              + c.conc:Age
                              + c.conc:c.MAC
                              + c.conc:c.ROC
                              + Age:c.MAC
                              + Age:c.ROC
                              + c.conc:Age:c.MAC
                              + c.conc:Age:c.ROC
                              + (c.conc|RatID),
                              data = ABHVdata$eth$no.ctrl, 
                              family = poisson)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0177798 (tol = 0.002, component 1)

``` r
# Model did not converge, used code below to extend # of iterations and start from where the previous model left off.
ss <- getME(models$eth$avers$MR1,c("theta","fixef"))
models$eth$avers$MR1 <- update(models$eth$avers$MR1,
                               start = ss,
                               control = glmerControl(optCtrl = list(maxfun = 2e6)))

summary(models$eth$avers$MR1)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: poisson  ( log )
    ## Formula: Total.Aversive ~ c.conc + Age + c.MAC + c.ROC + c.conc:Age +  
    ##     c.conc:c.MAC + c.conc:c.ROC + Age:c.MAC + Age:c.ROC + c.conc:Age:c.MAC +  
    ##     c.conc:Age:c.ROC + (c.conc | RatID)
    ##    Data: ABHVdata$eth$no.ctrl
    ## Control: glmerControl(optCtrl = list(maxfun = 2e+06))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   1132.6   1175.4   -551.3   1102.6      113 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5160 -1.0793 -0.1980  0.5798  7.8073 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev. Corr 
    ##  RatID  (Intercept)  0.6209  0.788         
    ##         c.conc      15.4047  3.925    -0.15
    ## Number of obs: 128, groups:  RatID, 43
    ## 
    ## Fixed effects:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        1.868989   0.198485   9.416  < 2e-16 ***
    ## c.conc             3.209061   1.042655   3.078  0.00209 ** 
    ## Age1               0.326850   0.196141   1.666  0.09563 .  
    ## c.MAC             -0.450534   0.105338  -4.277 1.89e-05 ***
    ## c.ROC             -3.986178   3.119113  -1.278  0.20125    
    ## c.conc:Age1        0.272922   1.025776   0.266  0.79019    
    ## c.conc:c.MAC      -0.004359   0.551635  -0.008  0.99370    
    ## c.conc:c.ROC       1.627719  16.505790   0.099  0.92144    
    ## Age1:c.MAC         0.019427   0.104897   0.185  0.85307    
    ## Age1:c.ROC        -1.586671   3.126691  -0.507  0.61183    
    ## c.conc:Age1:c.MAC -0.764444   0.550781  -1.388  0.16516    
    ## c.conc:Age1:c.ROC 20.559640  16.551260   1.242  0.21417    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) c.conc Age1   c.MAC  c.ROC  c.c:A1 c.:.MA c.:.RO A1:.MA
    ## c.conc      -0.251                                                        
    ## Age1        -0.384  0.122                                                 
    ## c.MAC        0.322 -0.151 -0.547                                          
    ## c.ROC       -0.440  0.101  0.683 -0.344                                   
    ## c.conc:Age1  0.122 -0.407 -0.234  0.166 -0.156                            
    ## c.cnc:c.MAC -0.149  0.351  0.164 -0.241  0.105 -0.553                     
    ## c.cnc:c.ROC  0.100 -0.445 -0.154  0.107 -0.168  0.689 -0.344              
    ## Age1:c.MAC  -0.544  0.167  0.314 -0.223  0.345 -0.139  0.136 -0.119       
    ## Age1:c.ROC   0.682 -0.165 -0.444  0.347 -0.589  0.101 -0.126  0.088 -0.345
    ## c.c:A1:.MAC  0.167 -0.549 -0.140  0.139 -0.121  0.344 -0.240  0.352 -0.235
    ## c.c:A1:.ROC -0.164  0.686  0.101 -0.128  0.089 -0.453  0.354 -0.604  0.105
    ##             A1:.RO c.:A1:.M
    ## c.conc                     
    ## Age1                       
    ## c.MAC                      
    ## c.ROC                      
    ## c.conc:Age1                
    ## c.cnc:c.MAC                
    ## c.cnc:c.ROC                
    ## Age1:c.MAC                 
    ## Age1:c.ROC                 
    ## c.c:A1:.MAC  0.105         
    ## c.c:A1:.ROC -0.171 -0.348

``` r
remove(ss) # Clean workspace

# Check variance inflation factor (VIF)
vif(models$eth$avers$MR1)
```

    ##           c.conc              Age            c.MAC            c.ROC 
    ##         2.429371         2.445679         1.534528         2.434163 
    ##       c.conc:Age     c.conc:c.MAC     c.conc:c.ROC        Age:c.MAC 
    ##         2.525427         1.562976         2.526051         1.241919 
    ##        Age:c.ROC c.conc:Age:c.MAC c.conc:Age:c.ROC 
    ##         1.695400         1.539071         2.501038

``` r
# Compare with the previous model
AIC(models$eth$avers$MR1, models$eth$avers$total.e)
```

    ##                          df      AIC
    ## models$eth$avers$MR1     15 1132.591
    ## models$eth$avers$total.e 11 1129.812

``` r
BIC(models$eth$avers$MR1, models$eth$avers$total.e)
```

    ##                          df      BIC
    ## models$eth$avers$MR1     15 1175.371
    ## models$eth$avers$total.e 11 1161.184

Again, check normality of residuals

![](README_files/figure-gfm/Aversive%20Responses:%20MAC%20and%20RoC%20(MR1)%20Residual%20Normality%20Check-1.png)<!-- -->

Similar to previous residuals. No planned comparisons here. This model
was not selected for interpretation (considerably higher BIC), so I
didn’t perform any. Then we repeat for the 4th model which excludes the
first day of drinking from calculations of MAC and RoC.

### MAC3 and RoC3 (MR3)

``` r
  # predictors: note that these are not the full factorial fixed effects. I'm not interested in the higher level interactions here.
models$eth$avers$MR3 <- glmer(Total.Aversive ~ c.conc+Age+c.MAC3+c.ROC3
                              + c.conc:Age
                              + c.conc:c.MAC3
                              + c.conc:c.ROC3
                              + Age:c.MAC3
                              + Age:c.ROC3
                              + c.conc:Age:c.MAC3
                              + c.conc:Age:c.ROC3
                              + (c.conc|RatID),
                              data = ABHVdata$eth$no.ctrl,
                              family = poisson)

# Model did not converge, used code below to extend # of iterations and start from where the previous model left off.
  ss <- getME(models$eth$avers$MR3, c("theta","fixef"))
  models$eth$avers$MR3 <- update(models$eth$avers$MR3,
                                 start = ss,
                                 control = glmerControl(optCtrl = list(maxfun = 2e9)))
  remove(ss) # Clean workspace

summary(models$eth$avers$MR3)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: poisson  ( log )
    ## Formula: Total.Aversive ~ c.conc + Age + c.MAC3 + c.ROC3 + c.conc:Age +  
    ##     c.conc:c.MAC3 + c.conc:c.ROC3 + Age:c.MAC3 + Age:c.ROC3 +  
    ##     c.conc:Age:c.MAC3 + c.conc:Age:c.ROC3 + (c.conc | RatID)
    ##    Data: ABHVdata$eth$no.ctrl
    ## Control: glmerControl(optCtrl = list(maxfun = 2e+09))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   1132.1   1174.9   -551.1   1102.1      113 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5026 -1.0806 -0.2188  0.5934  7.8305 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev. Corr 
    ##  RatID  (Intercept)  0.6227  0.7891        
    ##         c.conc      15.4599  3.9319   -0.18
    ## Number of obs: 128, groups:  RatID, 43
    ## 
    ## Fixed effects:
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)          1.913654   0.155267  12.325  < 2e-16 ***
    ## c.conc               2.874742   0.812341   3.539 0.000402 ***
    ## Age1                 0.387545   0.153011   2.533 0.011316 *  
    ## c.MAC3              -0.463840   0.097582  -4.753    2e-06 ***
    ## c.ROC3              -1.785224   3.030785  -0.589 0.555841    
    ## c.conc:Age1         -0.097315   0.796187  -0.122 0.902720    
    ## c.conc:c.MAC3        0.112024   0.511575   0.219 0.826667    
    ## c.conc:c.ROC3      -13.019915  16.170612  -0.805 0.420728    
    ## Age1:c.MAC3          0.007496   0.097146   0.077 0.938498    
    ## Age1:c.ROC3         -0.902356   3.035920  -0.297 0.766294    
    ## c.conc:Age1:c.MAC3  -0.803758   0.510194  -1.575 0.115165    
    ## c.conc:Age1:c.ROC3  27.199243  16.205513   1.678 0.093270 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) c.conc Age1   c.MAC3 c.ROC3 c.c:A1 c.:.MA c.:.RO A1:.MA
    ## c.conc      -0.281                                                        
    ## Age1        -0.232  0.116                                                 
    ## c.MAC3       0.211 -0.139 -0.441                                          
    ## c.ROC3      -0.335  0.131  0.445 -0.299                                   
    ## c.conc:Age1  0.117 -0.261 -0.265  0.170 -0.157                            
    ## c.cnc:.MAC3 -0.138  0.246  0.167 -0.273  0.135 -0.449                     
    ## c.cnc:.ROC3  0.130 -0.338 -0.153  0.136 -0.206  0.450 -0.300              
    ## Age1:c.MAC3 -0.436  0.170  0.200 -0.125  0.277 -0.127  0.126 -0.136       
    ## Age1:c.ROC3  0.446 -0.167 -0.337  0.280 -0.713  0.128 -0.144  0.148 -0.299
    ## c.:A1:.MAC3  0.169 -0.442 -0.126  0.128 -0.139  0.236 -0.142  0.284 -0.266
    ## c.:A1:.ROC3 -0.164  0.448  0.125 -0.144  0.149 -0.344  0.286 -0.728  0.132
    ##             A1:.RO c.:A1:.M
    ## c.conc                     
    ## Age1                       
    ## c.MAC3                     
    ## c.ROC3                     
    ## c.conc:Age1                
    ## c.cnc:.MAC3                
    ## c.cnc:.ROC3                
    ## Age1:c.MAC3                
    ## Age1:c.ROC3                
    ## c.:A1:.MAC3  0.134         
    ## c.:A1:.ROC3 -0.208 -0.302

``` r
# Variance inflation check & model comparison
  vif(models$eth$avers$MR3)
```

    ##            c.conc               Age            c.MAC3            c.ROC3 
    ##          1.469411          1.498365          1.342508          2.348989 
    ##        c.conc:Age     c.conc:c.MAC3     c.conc:c.ROC3        Age:c.MAC3 
    ##          1.530819          1.364606          2.457766          1.183591 
    ##        Age:c.ROC3 c.conc:Age:c.MAC3 c.conc:Age:c.ROC3 
    ##          2.194909          1.346142          2.459400

``` r
  AIC(models$eth$avers$MR3, models$eth$avers$MR1, models$eth$avers$total.e)
```

    ##                          df      AIC
    ## models$eth$avers$MR3     15 1132.101
    ## models$eth$avers$MR1     15 1132.591
    ## models$eth$avers$total.e 11 1129.812

``` r
  BIC(models$eth$avers$MR3, models$eth$avers$MR1, models$eth$avers$total.e)
```

    ##                          df      BIC
    ## models$eth$avers$MR3     15 1174.881
    ## models$eth$avers$MR1     15 1175.371
    ## models$eth$avers$total.e 11 1161.184

It looks like our Total Ethanol Consumed model is still doing the best
in terms of BIC so we will use it for the remaining substances instead
of MR1 or MR3.

Again, check normality of residuals

![](README_files/figure-gfm/Aversive%20Responses:%20MAC%20and%20RoC%20(MR3)%20Residual%20Normality%20Check-1.png)<!-- -->

And again, things look pretty normal. The remaining Sucrose and Water
model runs are not shown here for brevity.

# 4. Plotting/Graphing GLMERs with ggplot2
