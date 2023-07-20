Running and Graphing glmers
================
TJ Wukitsch
July 2023

# Background

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

Before we jump into graphing, the next steps seem like a lot of info
*because they are a lot of info*. It takes time with any new thing to
become comfortable and you may feel you are drinking from a fire-hose at
times. The flexibility of `ggplot2` is both its blessing and its curse.
It has a steep learning curve, but is powerful because it can do pretty
much whatever you want it to do for your graph. If you feel like you are
drinking from a fire-hose as you go through the content below, I
recommend finding a few beginners tutorials on `ggplot2`, especially if
this is your first time using it. Practice using it with some simpler
graphing first. The details we go into will start to make sense once you
are more familiar with the syntax.

## Planning Our Graphs

***How do we want our graphs to look?***

This is the question we MUST answer before we start writing our first
line of code. It is the key to our audience understanding our data. If
you are unsure which way might look best, try a few options and talk to
mentors or colleagues. Draw a small mock-up and see if the rough plan
works, then see if it best tells the story of your data. The more
perspectives you get, the better prepared you are to get a solid answer
before making the graph into something worth publishing. That initial
effort saves tons of time later.

In our case, we have obvious Y-axis variables: `Hedonic` and
`Aversive Responses`. However, our X-variable situation is less
straightforward at first glance. In our ‘Overall’ analyses, we have 2
categorical variables (`Condition` & `Age`) and one continuous repeated
measure variable (`Concentration`). Ergo, it makes sense to use
`Concentration` as our X variable and show lines of change for each
group across that axis in a single graph. Simple enough.

For the ‘Total Ethanol’ analyses, however, we have one categorical
variable (`Age`) and 2 continuous variables (`Concentration` &
`Total Ethanol Consumed`) which will require multiple graphs. In
`ggplot2` we can make a graph with one shared Y-axis and multiple graphs
that represent slices through one of the continuous variables to
effectively create two X-axes. The questions are: which variable do we
choose and where do we slice? We could go with a few graphs that slice
`Total Ethanol Consumed` into ranges for example from 0-39.9, 40-79.9,
and 80-120 g/kg of `Total Ethanol Consumed` and then for each range’s
graph show `Concentration` as the X-axis with our Y-variable averaged
across all the individuals in those ranges at each concentration tested.
However, in this case, we already have predefined slices in one of our
variables because we sampled at set concentrations along a range of
concentrations (5, 20, & 40%). It makes sense to use these predetermined
`Concentration` slices as the points that determine our graph slices and
our `Total Ethanol Consumed` variable as our X-axis. This is sensical
for at least 2 reasons: 1. It will cut the slices at a point where a
high amount of measurement occurred along the `Concentration` continuum
rendering those values very likely to be accurate, and 2. it will better
capture the rich variability that is present in our
`Total Ethanol Consumed` variable.

We will go with the latter of the options: `Concentration` as the
X-variable that is sliced to produce multiple graph panels of Responses
vs `Total Ethanol Consumed`. To ease comparison between the two
analyses, we will show the ‘Overall’ data and the ‘Total Ethanol’ data
side by side with the same Y-axis specifications. This means we need a
nested multi-panel graph. Panel A. will show the graph of the ‘Overall’
analysis, while panel B. will show the multi-panel graph of the ‘Total
Ethanol’ analysis.

## Testing and Organizing

Now that we know what we want our graphs to look like, we need to keep
the aesthetic variables associated with them easy to change and we need
an organized structure to store these variables along with the graphs
themselves. If we only needed to make one graph, there would be no real
point in assigning values to objects to use for argument values in our
ggplot code. We’d simply change them directly in the code and be sure to
have a nice organized list object for storing the plots. However, since
we are making multiple sets of these graphs (6 to be exact), we want
them to share aesthetic elements that we don’t want to have to change
manually for each graph each time we make a change. Thus, we need to
integrate our hierarchical organization of our plots into a list object
with our organization of our variables related to those plots. But how
do we know which elements of our graph code we need variables for? The
answer is iterative.

### Graph Setup

First, we need to get our model’s predictions into some form of data
that we can use for our graph. Our `xlevels` are points on the graph
that each function as a joint or pivot point for the sets of lines that
will make up our curved trend lines and error ribbons. The more points
you have, the curvier your curves look. They are made from a sequence
along your x-axis(es) that you determine in advance. For our ‘Overall’
models, we are making a sequence for `Concentration` from .05 to .40 by
0.025 increments resulting in 15 points for our curve. This should be
plenty curvy enough. We also have to mean-center these points so they
align with the data that our model used. Then we “uncenter” them so our
graphs are in our original units and show trends at our tested values.

``` r
# Create list for storage
plots <- list()

        # Mean center xlevel graphing points 
        plots$eth$x.axis$overall$xlvls <- seq(.05, .40, .025) - mean(ABHVdata$eth$ctrl$recoded.conc) # the points along the curve you want to use to get good error ribbons.
        
        # Pull the effects from model & calculate confidence intervals for graphing
        plots$eth$avers$overall$fx <- Effect(c("c.conc", "Age", "Condition"),
                                        models$eth$avers$overall,
                                        se = list(level = .68), # se is std err and the level is the confidence level. .68 = actual std err for conf int. lower and upper.
                                        xlevels = list(c.conc = plots$eth$x.axis$overall$xlvls))# the xlevels command is used to increase the number of points calculated for the total ethanol (default = 5)
        
        # Convert effect object to data frame
        plots$eth$avers$overall$fx <- as.data.frame(plots$eth$avers$overall$fx)
        # Get original Concentration values (un-center the variable)
        plots$eth$avers$overall$fx$Concentration <- (plots$eth$avers$overall$fx$c.conc + mean(ABHVdata$eth$ctrl$recoded.conc)) * 100
        # Check that everything went smoothly, look at your new data frame
        plots$eth$avers$overall$fx
```

    ##          c.conc        Age Condition       fit        se     lower     upper
    ## 1  -0.167741935 Adolescent      CTRL 16.314985  5.456286 11.699001 22.752263
    ## 2  -0.142741935 Adolescent      CTRL 17.470276  5.576873 12.718395 23.997567
    ## 3  -0.117741935 Adolescent      CTRL 18.707374  5.713027 13.807629 25.345833
    ## 4  -0.092741935 Adolescent      CTRL 20.032074  5.872096 14.966653 26.811873
    ## 5  -0.067741935 Adolescent      CTRL 21.450578  6.063284 16.194232 28.413035
    ## 6  -0.042741935 Adolescent      CTRL 22.969528  6.297742 17.487911 30.169367
    ## 7  -0.017741935 Adolescent      CTRL 24.596038  6.588517 18.844119 32.103654
    ## 8   0.007258065 Adolescent      CTRL 26.337723  6.950311 20.258469 34.241268
    ## 9   0.032258065 Adolescent      CTRL 28.202740  7.399062 21.726235 36.609865
    ## 10  0.057258065 Adolescent      CTRL 30.199822  7.951433 23.242934 39.238988
    ## 11  0.082258065 Adolescent      CTRL 32.338320  8.624303 24.804895 42.159700
    ## 12  0.107258065 Adolescent      CTRL 34.628249  9.434412 26.409698 45.404367
    ## 13  0.132258065 Adolescent      CTRL 37.080331 10.398220 28.056398 49.006683
    ## 14  0.157258065 Adolescent      CTRL 39.706050 11.532002 29.745528 53.001930
    ## 15  0.182258065 Adolescent      CTRL 42.517700 12.852128 31.478935 57.427445
    ## 16 -0.167741935      Adult      CTRL 13.310398  4.504304  9.506883 18.635622
    ## 17 -0.142741935      Adult      CTRL 14.465767  4.669382 10.493809 19.941130
    ## 18 -0.117741935      Adult      CTRL 15.721425  4.850841 11.567330 21.367351
    ## 19 -0.092741935      Adult      CTRL 17.086076  5.055562 12.730693 22.931508
    ## 20 -0.067741935      Adult      CTRL 18.569182  5.292457 13.986183 24.653939
    ## 21 -0.042741935      Adult      CTRL 20.181024  5.572714 15.334985 26.558470
    ## 22 -0.017741935      Adult      CTRL 21.932777  5.909907 16.777187 28.672668
    ## 23  0.007258065      Adult      CTRL 23.836587  6.319948 18.311964 31.027959
    ## 24  0.032258065      Adult      CTRL 25.905650  6.820842 19.937947 33.659570
    ## 25  0.057258065      Adult      CTRL 28.154313  7.432303 21.653759 36.606362
    ## 26  0.082258065      Adult      CTRL 30.598164  8.175336 23.458594 39.910646
    ## 27  0.107258065      Adult      CTRL 33.254146  9.071907 25.352722 43.618127
    ## 28  0.132258065      Adult      CTRL 36.140673 10.144805 27.337826 47.778057
    ## 29  0.157258065      Adult      CTRL 39.277756 11.417742 29.417128 52.443669
    ## 30  0.182258065      Adult      CTRL 42.687144 12.915653 31.595327 57.672841
    ## 31 -0.167741935 Adolescent      EtOH  6.091812  1.458448  4.801178  7.729389
    ## 32 -0.142741935 Adolescent      EtOH  6.440491  1.466421  5.135509  8.077082
    ## 33 -0.117741935 Adolescent      EtOH  6.809128  1.477397  5.487615  8.448886
    ## 34 -0.092741935 Adolescent      EtOH  7.198865  1.493212  5.857071  8.848050
    ## 35 -0.067741935 Adolescent      EtOH  7.610909  1.516140  6.243123  9.278360
    ## 36 -0.042741935 Adolescent      EtOH  8.046538  1.548899  6.644677  9.744156
    ## 37 -0.017741935 Adolescent      EtOH  8.507101  1.594605  7.060359 10.250294
    ## 38  0.007258065 Adolescent      EtOH  8.994025  1.656677  7.488615 10.802062
    ## 39  0.032258065 Adolescent      EtOH  9.508819  1.738677  7.927868 11.405039
    ## 40  0.057258065 Adolescent      EtOH 10.053079  1.844139  8.376711 12.064926
    ## 41  0.082258065 Adolescent      EtOH 10.628491  1.976399  8.834071 12.787403
    ## 42  0.107258065 Adolescent      EtOH 11.236838  2.138492  9.299321 13.578038
    ## 43  0.132258065 Adolescent      EtOH 11.880005  2.333129  9.772320 14.442274
    ## 44  0.157258065 Adolescent      EtOH 12.559986  2.562743 10.253372 15.385499
    ## 45  0.182258065 Adolescent      EtOH 13.278887  2.829583 10.743139 16.413157
    ## 46 -0.167741935      Adult      EtOH  4.694106  1.161280  3.670352  6.003411
    ## 47 -0.142741935      Adult      EtOH  4.894324  1.150671  3.873955  6.183450
    ## 48 -0.117741935      Adult      EtOH  5.103083  1.142575  4.084449  6.375757
    ## 49 -0.092741935      Adult      EtOH  5.320745  1.138366  4.301013  6.582247
    ## 50 -0.067741935      Adult      EtOH  5.547692  1.139687  4.522596  6.805138
    ## 51 -0.042741935      Adult      EtOH  5.784319  1.148427  4.747933  7.046928
    ## 52 -0.017741935      Adult      EtOH  6.031038  1.166663  4.975611  7.310343
    ## 53  0.007258065      Adult      EtOH  6.288281  1.196556  5.204163  7.598241
    ## 54  0.032258065      Adult      EtOH  6.556497  1.240226  5.432204  7.913482
    ## 55  0.057258065      Adult      EtOH  6.836152  1.299610  5.658565  8.258804
    ## 56  0.082258065      Adult      EtOH  7.127736  1.376354  5.882403  8.636711
    ## 57  0.107258065      Adult      EtOH  7.431756  1.471767  6.103255  9.049434
    ## 58  0.132258065      Adult      EtOH  7.748744  1.586825  6.321022  9.498945
    ## 59  0.157258065      Adult      EtOH  8.079253  1.722243  6.535914  9.987024
    ## 60  0.182258065      Adult      EtOH  8.423859  1.878558  6.748360 10.515355
    ##    Concentration
    ## 1            5.0
    ## 2            7.5
    ## 3           10.0
    ## 4           12.5
    ## 5           15.0
    ## 6           17.5
    ## 7           20.0
    ## 8           22.5
    ## 9           25.0
    ## 10          27.5
    ## 11          30.0
    ## 12          32.5
    ## 13          35.0
    ## 14          37.5
    ## 15          40.0
    ## 16           5.0
    ## 17           7.5
    ## 18          10.0
    ## 19          12.5
    ## 20          15.0
    ## 21          17.5
    ## 22          20.0
    ## 23          22.5
    ## 24          25.0
    ## 25          27.5
    ## 26          30.0
    ## 27          32.5
    ## 28          35.0
    ## 29          37.5
    ## 30          40.0
    ## 31           5.0
    ## 32           7.5
    ## 33          10.0
    ## 34          12.5
    ## 35          15.0
    ## 36          17.5
    ## 37          20.0
    ## 38          22.5
    ## 39          25.0
    ## 40          27.5
    ## 41          30.0
    ## 42          32.5
    ## 43          35.0
    ## 44          37.5
    ## 45          40.0
    ## 46           5.0
    ## 47           7.5
    ## 48          10.0
    ## 49          12.5
    ## 50          15.0
    ## 51          17.5
    ## 52          20.0
    ## 53          22.5
    ## 54          25.0
    ## 55          27.5
    ## 56          30.0
    ## 57          32.5
    ## 58          35.0
    ## 59          37.5
    ## 60          40.0

Then we do the same thing for our ‘Total Ethanol’ graph data, however,
we have 2 x-variables, so we need to make sure we get the right number
of levels of each variable. `Concentration` is pretty easy: we have 3
slices we want to take at each of our measured values (.05, .20, & .40),
so we use those values. Thankfully, `Total Ethanol Consumed` is also
pretty easy. Instead of assigning specific values, we can tell the
effects function to calculate 20 values that are evenly distributed
across the range of that variable by just using ‘= 20’.

``` r
# Mean center xlevel graphing points for ALL of Ethanol's, Total Ethanol graphs
        plots$eth$x.axis$total.e$xlvls <- c(.05, .20, .40) - mean(ABHVdata$eth$no.ctrl$recoded.conc) # the points along the curve you want to use to get good error ribbons.
        
        # Pull the effects from model & calculate confidence intervals for graphing
        plots$eth$avers$total.e$fx <- Effect(c("c.conc", "Age", "c.totale"),
                                       models$eth$avers$total.e,
                                       se = list(level = .68), # se is std err and the level is the confidence level. .68 = actual std err for conf int. lower and upper.
                                       xlevels = list(c.totale = 20, c.conc = plots$eth$x.axis$total.e$xlvls))# the xlevels command is used to increase the number of points calculated for the total ethanol (default = 5)
        
        # Convert effect object to data frame
        plots$eth$avers$total.e$fx <- as.data.frame(plots$eth$avers$total.e$fx)
        # Get original Concentration values (un-center the variable)
        plots$eth$avers$total.e$fx$Concentration <- (plots$eth$avers$total.e$fx$c.conc + mean(ABHVdata$eth$no.ctrl$recoded.conc)) * 100
        # Get original totale values (un-center the variable)
        plots$eth$avers$total.e$fx$totale <- (plots$eth$avers$total.e$fx$c.totale + mean(ABHVdata$eth$no.ctrl$TOTAL.ETOH.Swap.Consumed..g.kg.))
        # Check that everything went smoothly, look at your new data frame
        plots$eth$avers$total.e$fx
```

    ##          c.conc        Age c.totale        fit         se       lower
    ## 1   -0.16679688 Adolescent    -36.0 12.2289725  6.5248223  7.19373515
    ## 2   -0.01679687 Adolescent    -36.0 22.9558775  8.8654395 15.63511877
    ## 3    0.18320312 Adolescent    -36.0 53.1576542 25.1488323 33.20781000
    ## 4   -0.16679688      Adult    -36.0 10.6544679  4.1471164  7.23477472
    ## 5   -0.01679687      Adult    -36.0 11.8187436  3.2994877  8.95363663
    ## 6    0.18320312      Adult    -36.0 13.5713808  4.5944627  9.69198155
    ## 7   -0.16679688 Adolescent    -29.0 10.9194088  5.1444423  6.83478215
    ## 8   -0.01679687 Adolescent    -29.0 19.5550141  6.6644054 13.93380082
    ## 9    0.18320312 Adolescent    -29.0 42.5275282 17.7000926 28.11364520
    ## 10  -0.16679688      Adult    -29.0  8.3164875  2.7812839  5.96353442
    ## 11  -0.01679687      Adult    -29.0  9.6751078  2.3319783  7.61304733
    ## 12   0.18320312      Adult    -29.0 11.8379630  3.4874990  8.83164529
    ## 13  -0.16679688 Adolescent    -23.0  9.9091305  4.1662871  6.52302008
    ## 14  -0.01679687 Adolescent    -23.0 17.0439532  5.1796609 12.59852238
    ## 15   0.18320312 Adolescent    -23.0 35.1250308 12.9941362 24.31332991
    ## 16  -0.16679688      Adult    -23.0  6.7254040  2.0076237  4.99797541
    ## 17  -0.01679687      Adult    -23.0  8.1499851  1.7569326  6.57737614
    ## 18   0.18320312      Adult    -23.0 10.5295078  2.7885839  8.09150034
    ## 19  -0.16679688 Adolescent    -16.0  8.8479917  3.2396968  6.14764594
    ## 20  -0.01679687 Adolescent    -16.0 14.5189285  3.8370131 11.16340769
    ## 21   0.18320312 Adolescent    -16.0 28.1009529  9.0006342 20.43567611
    ## 22  -0.16679688      Adult    -16.0  5.2496041  1.4446270  3.99278900
    ## 23  -0.01679687      Adult    -16.0  6.6717739  1.3208990  5.47942325
    ## 24   0.18320312      Adult    -16.0  9.1846162  2.2328043  7.21220415
    ## 25  -0.16679688 Adolescent     -9.9  8.0163820  2.6053940  5.80244694
    ## 26  -0.01679687 Adolescent     -9.9 12.6255984  2.9563254 10.00285735
    ## 27   0.18320312 Adolescent     -9.9 23.1357400  6.5397161 17.46638066
    ## 28  -0.16679688      Adult     -9.9  4.2302688  1.1631750  3.21820781
    ## 29  -0.01679687      Adult     -9.9  5.6040329  1.0978150  4.61206115
    ## 30   0.18320312      Adult     -9.9  8.1535040  1.9440066  6.43237126
    ## 31  -0.16679688 Adolescent     -3.5  7.2277633  2.1021606  5.41241975
    ## 32  -0.01679687 Adolescent     -3.5 10.9039785  2.2790220  8.85761701
    ## 33   0.18320312 Adolescent     -3.5 18.8665753  4.7457920 14.69108696
    ## 34  -0.16679688      Adult     -3.5  3.3728598  0.9950145  2.51529417
    ## 35  -0.01679687      Adult     -3.5  4.6669707  0.9681939  3.79696982
    ## 36   0.18320312      Adult     -3.5  7.1958835  1.7897368  5.61909592
    ## 37  -0.16679688 Adolescent      2.9  6.5167256  1.7553671  4.98533959
    ## 38  -0.01679687 Adolescent      2.9  9.4171177  1.8178033  7.77230719
    ## 39   0.18320312 Adolescent      2.9 15.3851859  3.5752010 12.21071107
    ## 40  -0.16679688      Adult      2.9  2.6892342  0.8930551  1.93288096
    ## 41  -0.01679687      Adult      2.9  3.8865967  0.8977768  3.08891852
    ## 42   0.18320312      Adult      2.9  6.3507345  1.7325520  4.84172543
    ## 43  -0.16679688 Adolescent      9.3  5.8756369  1.5492936  4.52037454
    ## 44  -0.01679687 Adolescent      9.3  8.1330045  1.5336311  6.74233741
    ## 45   0.18320312 Adolescent      9.3 12.5462063  2.8630870  9.99894577
    ## 46  -0.16679688      Adult      9.3  2.1441687  0.8174926  1.46756085
    ## 47  -0.01679687      Adult      9.3  3.2367107  0.8522720  2.49104592
    ## 48   0.18320312      Adult      9.3  5.6048474  1.7233863  4.12825386
    ## 49  -0.16679688 Adolescent     16.0  5.2719622  1.4534289  4.00779103
    ## 50  -0.01679687 Adolescent     16.0  6.9758902  1.3746657  5.73445397
    ## 51   0.18320312 Adolescent     16.0 10.1337324  2.4392512  7.97648325
    ## 52  -0.16679688      Adult     16.0  1.6915242  0.7464984  1.09063358
    ## 53  -0.01679687      Adult     16.0  2.6724731  0.8111338  1.97619266
    ## 54   0.18320312      Adult     16.0  4.9176790  1.7295898  3.46626389
    ## 55  -0.16679688 Adolescent     22.0  4.7841932  1.4367122  3.54904220
    ## 56  -0.01679687 Adolescent     22.0  6.0801156  1.3062370  4.91050523
    ## 57   0.18320312 Adolescent     22.0  8.3698178  2.2148806  6.43319697
    ## 58  -0.16679688      Adult     22.0  1.3679072  0.6839560  0.83197850
    ## 59  -0.01679687      Adult     22.0  2.2512013  0.7726208  1.60025387
    ## 60   0.18320312      Adult     22.0  4.3741258  1.7322452  2.95022252
    ## 61  -0.16679688 Adolescent     29.0  4.2718685  1.4594931  3.04132303
    ## 62  -0.01679687 Adolescent     29.0  5.1793597  1.2680613  4.06010393
    ## 63   0.18320312 Adolescent     29.0  6.6960754  2.0342883  4.95007253
    ## 64  -0.16679688      Adult     29.0  1.0677383  0.6111249  0.60432701
    ## 65  -0.01679687      Adult     29.0  1.8428876  0.7232455  1.24739374
    ## 66   0.18320312      Adult     29.0  3.8154363  1.7227802  2.43519892
    ## 67  -0.16679688 Adolescent     35.0  3.8766295  1.4916332  2.64407890
    ## 68  -0.01679687 Adolescent     35.0  4.5142777  1.2465684  3.43026158
    ## 69   0.18320312 Adolescent     35.0  5.5305320  1.9028835  3.92796734
    ## 70  -0.16679688      Adult     35.0  0.8634620  0.5497458  0.45842754
    ## 71  -0.01679687      Adult     35.0  1.5523865  0.6774872  1.00581228
    ## 72   0.18320312      Adult     35.0  3.3937145  1.7015848  2.06124632
    ## 73  -0.16679688 Adolescent     41.0  3.5179585  1.5224816  2.28760440
    ## 74  -0.01679687 Adolescent     41.0  3.9345989  1.2236065  2.88794779
    ## 75   0.18320312 Adolescent     41.0  4.5678673  1.7746116  3.10402788
    ## 76  -0.16679688      Adult     41.0  0.6982673  0.4905397  0.34722933
    ## 77  -0.01679687      Adult     41.0  1.3076781  0.6296876  0.81009059
    ## 78   0.18320312      Adult     41.0  3.0186058  1.6685812  1.74210306
    ## 79  -0.16679688 Adolescent     48.0  3.1412310  1.5487202  1.92382903
    ## 80  -0.01679687 Adolescent     48.0  3.3516967  1.1889099  2.35540747
    ## 81   0.18320312 Adolescent     48.0  3.6544146  1.6222895  2.35012903
    ## 82  -0.16679688      Adult     48.0  0.5450419  0.4255230  0.25075424
    ## 83  -0.01679687      Adult     48.0  1.0704967  0.5729618  0.62867508
    ## 84   0.18320312      Adult     48.0  2.6330514  1.6169600  1.42967285
    ## 85  -0.16679688 Adolescent     54.0  2.8506001  1.5599537  1.65421313
    ## 86  -0.01679687 Adolescent     54.0  2.9213050  1.1511338  1.97420645
    ## 87   0.18320312 Adolescent     54.0  3.0183138  1.4897674  1.84754501
    ## 88  -0.16679688      Adult     54.0  0.4407662  0.3740563  0.18953389
    ## 89  -0.01679687      Adult     54.0  0.9017504  0.5247663  0.50553694
    ## 90   0.18320312      Adult     54.0  2.3420191  1.5634761  1.20579255
    ## 91  -0.16679688 Adolescent     61.0  2.5453380  1.5592353  1.38413263
    ## 92  -0.01679687 Adolescent     61.0  2.4885200  1.0985433  1.60430443
    ## 93   0.18320312 Adolescent     61.0  2.4147308  1.3359557  1.39291902
    ## 94  -0.16679688      Adult     61.0  0.3440460  0.3195157  0.13662251
    ## 95  -0.01679687      Adult     61.0  0.7381946  0.4702614  0.39177801
    ## 96   0.18320312      Adult     61.0  2.0428824  1.4928544  0.98772880
    ## 97  -0.16679688 Adolescent     67.0  2.3098399  1.5474186  1.18645089
    ## 98  -0.01679687 Adolescent     67.0  2.1689690  1.0476588  1.34165953
    ## 99   0.18320312 Adolescent     67.0  1.9944139  1.2076242  1.09220947
    ## 100 -0.16679688      Adult     67.0  0.2782242  0.2776254  0.10314232
    ## 101 -0.01679687      Adult     67.0  0.6218303  0.4257524  0.31475217
    ## 102  0.18320312      Adult     67.0  1.8170818  1.4272237  0.83204942
    ## 103 -0.16679688 Adolescent     73.0  2.0961304  1.5263697  1.01607192
    ## 104 -0.01679687 Adolescent     73.0  1.8904515  0.9929732  1.12127853
    ## 105  0.18320312 Adolescent     73.0  1.6472589  1.0847439  0.85577115
    ## 106 -0.16679688      Adult     73.0  0.2249952  0.2401567  0.07783641
    ## 107 -0.01679687      Adult     73.0  0.5238089  0.3837455  0.25279556
    ## 108  0.18320312      Adult     73.0  1.6162389  1.3584945  0.70063505
    ## 109 -0.16679688 Adolescent     80.0  1.8716621  1.4918047  0.84721240
    ## 110 -0.01679687 Adolescent     80.0  1.6103852  0.9262461  0.90890980
    ## 111  0.18320312 Adolescent     80.0  1.3178507  0.9502994  0.64332873
    ## 112 -0.16679688      Adult     80.0  0.1756231  0.2017754  0.05602484
    ## 113 -0.01679687      Adult     80.0  0.4288026  0.3382509  0.19569195
    ## 114  0.18320312      Adult     80.0  1.4098032  1.2761351  0.57308576
    ## 115 -0.16679688 Adolescent     86.0  1.6984935  1.4550531  0.72456534
    ## 116 -0.01679687 Adolescent     86.0  1.4035955  0.8679565  0.75887631
    ## 117  0.18320312 Adolescent     86.0  1.0884607  0.8438122  0.50349908
    ## 118 -0.16679688      Adult     86.0  0.1420234  0.1731368  0.04225303
    ## 119 -0.01679687      Adult     86.0  0.3612088  0.3024141  0.15709781
    ## 120  0.18320312      Adult     86.0  1.2539771  1.2049308  0.48227447
    ##          upper Concentration     totale
    ## 1   20.7886118             5   4.891405
    ## 2   33.7044010            20   4.891405
    ## 3   85.0925189            40   4.891405
    ## 4   15.6905627             5   4.891405
    ## 5   15.6006666            20   4.891405
    ## 6   19.0035830            40   4.891405
    ## 7   17.4451045             5  11.891405
    ## 8   27.4439530            20  11.891405
    ## 9   64.3314179            40  11.891405
    ## 10  11.5978143             5  11.891405
    ## 11  12.2956954            20  11.891405
    ## 12  15.8676399            40  11.891405
    ## 13  15.0529764             5  17.891405
    ## 14  23.0579692            20  17.891405
    ## 15  50.7445007            40  17.891405
    ## 16   9.0498762             5  17.891405
    ## 17  10.0985950            20  17.891405
    ## 18  13.7020985            40  17.891405
    ## 19  12.7344610             5  24.891405
    ## 20  18.8830589            20  24.891405
    ## 21  38.6414206            40  24.891405
    ## 22   6.9020284             5  24.891405
    ## 23   8.1235862            20  24.891405
    ## 24  11.6964485            40  24.891405
    ## 25  11.0750482             5  30.991405
    ## 26  15.9360200            20  30.991405
    ## 27  30.6452995            40  30.991405
    ## 28   5.5606023             5  30.991405
    ## 29   6.8093599            20  30.991405
    ## 30  10.3351665            40  30.991405
    ## 31   9.6519790             5  37.391405
    ## 32  13.4231076            20  37.391405
    ## 33  24.2288173            40  37.391405
    ## 34   4.5228043             5  37.391405
    ## 35   5.7363152            20  37.391405
    ## 36   9.2151371            40  37.391405
    ## 37   8.5185196             5  43.791405
    ## 38  11.4100104            20  43.791405
    ## 39  19.3849436            40  43.791405
    ## 40   3.7415550             5  43.791405
    ## 41   4.8902663            20  43.791405
    ## 42   8.3300528            40  43.791405
    ## 43   7.6372232             5  50.191405
    ## 44   9.8105090            20  50.191405
    ## 45  15.7423889            40  50.191405
    ## 46   3.1327215             5  50.191405
    ## 47   4.2055813            20  50.191405
    ## 48   7.6095887            40  50.191405
    ## 49   6.9348889             5  56.891405
    ## 50   8.4860816            20  56.891405
    ## 51  12.8744120            40  56.891405
    ## 52   2.6234787             5  56.891405
    ## 53   3.6140769            20  56.891405
    ## 54   6.9768395            40  56.891405
    ## 55   6.4492062             5  62.891405
    ## 56   7.5283100            20  62.891405
    ## 57  10.8894302            40  62.891405
    ## 58   2.2490607             5  62.891405
    ## 59   3.1669397            20  62.891405
    ## 60   6.4852656            40  62.891405
    ## 61   6.0003031             5  69.891405
    ## 62   6.6071625            20  69.891405
    ## 63   9.0579332            40  69.891405
    ## 64   1.8865036             5  69.891405
    ## 65   2.7226645            20  69.891405
    ## 66   5.9779734            40  69.891405
    ## 67   5.6837396             5  75.891405
    ## 68   5.9408598            20  75.891405
    ## 69   7.7869242            40  75.891405
    ## 70   1.6263567             5  75.891405
    ## 71   2.3959776            20  75.891405
    ## 72   5.5875409            40  75.891405
    ## 73   5.4100404             5  81.891405
    ## 74   5.3605778            20  81.891405
    ## 75   6.7220440            40  81.891405
    ## 76   1.4041935             5  81.891405
    ## 77   2.1109022            20  81.891405
    ## 78   5.2304488            40  81.891405
    ## 79   5.1290068             5  88.891405
    ## 80   4.7693958            20  88.891405
    ## 81   5.6825586            40  88.891405
    ## 82   1.1847083             5  88.891405
    ## 83   1.8228228            20  88.891405
    ## 84   4.8493330            40  88.891405
    ## 85   4.9122575             5  94.891405
    ## 86   4.3227612            20  94.891405
    ## 87   4.9309857            40  94.891405
    ## 88   1.0250138             5  94.891405
    ## 89   1.6084954            20  94.891405
    ## 90   4.5489197            40  94.891405
    ## 91   4.6807258             5 101.891405
    ## 92   3.8600728            20 101.891405
    ## 93   4.1861190            40 101.891405
    ## 94   0.8663845             5 101.891405
    ## 95   1.3909185            20 101.891405
    ## 96   4.2252172            40 101.891405
    ## 97   4.4969078             5 107.891405
    ## 98   3.5064234            20 107.891405
    ## 99   3.6418718            40 107.891405
    ## 100  0.7505038             5 107.891405
    ## 101  1.2284995            20 107.891405
    ## 102  3.9682572            40 107.891405
    ## 103  4.3242635             5 113.891405
    ## 104  3.1872606            20 113.891405
    ## 105  3.1707798            40 113.891405
    ## 106  0.6503750             5 113.891405
    ## 107  1.0853662            20 113.891405
    ## 108  3.7283723            40 113.891405
    ## 109  4.1348771             5 120.891405
    ## 110  2.8532430            20 120.891405
    ## 111  2.6996002            40 120.891405
    ## 112  0.5505318             5 120.891405
    ## 113  0.9395974            20 120.891405
    ## 114  3.4681462            40 120.891405
    ## 115  3.9815319             5 126.891405
    ## 116  2.5960494            20 126.891405
    ## 117  2.3530267            40 126.891405
    ## 118  0.4773776             5 126.891405
    ## 119  0.8305134            20 126.891405
    ## 120  3.2605054            40 126.891405

Then, repeat this process for our hedonic graphs (code not included, try
on your own for this!)

### Test Graphing & Adjustment

Next we create a basic graph that gets the job done and looks ugly as
sin, then store it in a temporary object. Using some of the code in this
guide, you can start by calculating out what you need for your error
ribbons to be right and getting some of the basic theme elements tested
out like text size etc. Our basic graphs look like this:

**Overall graph:**

``` r
    # Start ggplot
    temporary.ggp <- ggplot(plots$eth$avers$overall$fx,
                                          aes(Concentration, fit,
                                          group = interaction(Age, Condition),
                                          col = interaction(Age, Condition),
                                          fill = interaction(Age, Condition),
                                          linetype = interaction(Age, Condition),
                                          shape = interaction(Age, Condition)))+
      
      # Add Raw Data to graph
      geom_point(data = ABHVdata$eth$ctrl, 
                 aes(x = Concentration, y = Total.Aversive))+
      
      # Add Prediction Line to graph
      geom_smooth(data = plots$eth$avers$overall$fx,
                  se = FALSE, # Don't show Std. Error, we have error ribbons for that
                  method = "glm", # Generalized linear model
                  method.args = list(family = "poisson"))+ # Poisson link function
  
      # Add Error Ribbon to Graph
      geom_ribbon(data = plots$eth$avers$overall$fx,
                  aes(ymin = lower, ymax = upper))+
      
      theme_classic()+
      xlab("Ethanol % (v/v)")+ # X axis label
      ylab("Aversive Responses (+/-SEM)") # Y axis label
    
    temporary.ggp
```

![](README_files/figure-gfm/Preliminary%20Overall%20Graph-1.png)<!-- -->
And our **Total Ethanol graph**:

``` r
    # Start ggplot
    temporary2.ggp <- ggplot(plots$eth$avers$total.e$fx,
                                          aes(totale,
                                              fit,
                                              group = Age,
                                              col = Age,
                                              fill = Age,
                                              shape = Age))+
      
      # Add Raw Data to graph
      geom_point(data = ABHVdata$eth$no.ctrl,
                 aes(x = TOTAL.ETOH.Swap.Consumed..g.kg.,
                     y = Total.Aversive))+
      
      # Add Prediction Line to graph
      geom_smooth(data = plots$eth$avers$total.e$fx,
                  se = FALSE, # Don't show Std. Error, we have error ribbons for that
                  method = "glm", # Generalized linear model
                  method.args = list(family = "poisson"))+ # Poisson link function

      
      # Add Error Ribbon to graph
      geom_ribbon(data = plots$eth$avers$total.e$fx,
                  aes(ymin = lower, ymax = upper))+
      
      
      
      facet_wrap(~ as.factor(Concentration), nrow = 1)+ # Makes the plot multi-panel. One for each level of Concentration
  
      theme_classic()+
      
      xlab("Total Ethanol Consumed (g/kg)")+
      ylab("Aversive Responses (+/-SEM)")+
      ggtitle("Ethanol % (v/v)")
    
    temporary2.ggp
```

![](README_files/figure-gfm/Preliminary%20Total%20Ethanol%20Graph-1.png)<!-- -->

Hot garbage! But, everything seems to be there that we need… along with
a few things that we don’t need and some things covering up other
things. How awful, but that’s OK! We are going to make it much much
nicer. Now, we go through ggplot2’s vignettes for [aesthetic
specifications](https://ggplot2.tidyverse.org/articles/ggplot2-specs.html)
as well as [theme
arguments](https://ggplot2.tidyverse.org/reference/theme.html) to change
the things we want to change. The values that make up these aesthetic
specs and theme arguments are some of the variables that we will need to
store in objects to help us easily adjust all the graphs on the fly.
After playing around for quite some time, I have extensively fleshed
everything out WITHOUT using objects to assign values to our various
specs and arguments below.

**Overall graph**

``` r
    # Start ggplot
    temporary.ggp <- ggplot(plots$eth$avers$overall$fx,
                                          aes(Concentration, fit,
                                          group = interaction(Age, Condition),
                                          col = interaction(Age, Condition),
                                          fill = interaction(Age, Condition),
                                          linetype = interaction(Age, Condition),
                                          shape = interaction(Age, Condition)))+
      
      # Add Raw Data to graph
      geom_point(data = ABHVdata$eth$ctrl, 
                 aes(x = Concentration, y = Total.Aversive),
                 size = 4,
                 alpha = 0.75)+
      
      # Add Prediction Line to graph
      geom_smooth(data = plots$eth$avers$overall$fx,
                  se = FALSE, # Don't show Std. Error, we have error ribbons for that
                  method = "glm", # Generalized linear model
                  method.args = list(family = "poisson"), # Poisson link function
                  linewidth = 1.5)+ # Width of trend lines
      
      # Add Error Ribbon to Graph
      geom_ribbon(data = plots$eth$avers$overall$fx,
                  aes(ymin = lower, ymax = upper),
                  colour = NA, # colour = NA suppresses edges of the ribbon
                  alpha = 0.25)+ # Opacity at 25% (75% Transparent)
      
      labs(tag = "A.")+ # Add image tag for multipanel final image
      
      scale_color_manual("", 
                         values = c("#F8C8C8", "#82BFC4","#F8766D","#00BFC4"), # Line and Point color
                         labels = c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+ # Labels for legend
      
      scale_fill_manual("",
                        values = c("#F8C8C8", "#82BFC4","#F8766D","#00BFC4"), # Ribbon color
                        labels = c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+ # Legend labels

      scale_linetype_manual("",
                            values = c("dashed","dashed","solid","solid"), # Types of lines
                            labels = c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+ # Legend labels
                            
      scale_shape_manual("",
                         values = c(17,18,16,15), # Shapes of points
                         labels = c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+ # Legend labels
                         
      scale_x_continuous(expand = c(0, 0),
                         limits = c(0, 41), # Axis length limits
                         breaks = c(5, 20, 40), # Axis break positions
                         labels = as.character(c(5, 20, 40)))+ # Axis value labels
                         
      scale_y_continuous(expand = c(0, 0),
                         limits = c(0, 110), # Axis length limits
                         breaks = seq(from = 0, to = 100, by = 25), # Axis break positions
                         labels = as.character(seq(from = 0, to = 100, by = 25)))+ # Axis value labels
                         
      theme_classic()+
      theme(# Text
            strip.text.x = element_text(size = 17, face = "bold"), # Modify label text for facets
            axis.title = element_text(size = 18), # Modify axis title text
            axis.text = element_text(size = 17, color = "black", face = "bold"), # Modify axis value text
            legend.title = element_text(size = 16), # Modify legend title text (!) Probably Uneccessary
            legend.text = element_text(size = 14), # Modify legend value text
            plot.tag = element_text(size = 32, face = "bold"), # Modify the plot tag (e.g. "A.") text
            # Positioning
            axis.title.x = element_text(margin = margin(t = 13, r = 0, b = 0, l = 0)), # Modify X axis appearance
            axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)), # Modify Y axis appearance
            plot.tag.position = c(0.05, 0.95), # Modify position of plot tag
            legend.position = c(0.275, .85), # Modify position of legend
            # Other Aesthetics
            axis.line = element_line(size = 1.3), # Modify axis line attributes
            axis.ticks = element_line(size = 1.3, color = "black"), # Modify axis tick mark attributes
            axis.ticks.length = unit(0.2, "cm"), # Modify how long each tick is
            legend.key.size = unit(1.5, "cm"), # Modify the size of the legend key boxes
            strip.background = element_rect(colour="white") # Gets rid of default background
            )+
      xlab("Ethanol % (v/v)")+ # X axis label
      ylab("Aversive Responses (+/-SEM)") # Y axis label
    
    temporary.ggp
```

![](README_files/figure-gfm/No%20Object%20Assignment:%20Overall-1.png)<!-- -->
and the **Total Ethanol graph**

``` r
# Start ggplot
    temporary2.ggp <- ggplot(plots$eth$avers$total.e$fx,
                                          aes(totale,
                                              fit,
                                              group = Age,
                                              col = Age,
                                              fill = Age,
                                              shape = Age))+
      
      # Add Raw Data to graph
      geom_point(data = ABHVdata$eth$no.ctrl,
                 aes(x = TOTAL.ETOH.Swap.Consumed..g.kg.,
                     y = Total.Aversive),
                 size = 4, 
                 alpha = 0.75)+
      
      # Add Prediction Line to graph
      geom_smooth(data = plots$eth$avers$total.e$fx,
                  se = FALSE, # Don't show Std. Error, we have error ribbons for that
                  method = "glm", # Generalized linear model
                  method.args = list(family = "poisson"), # Poisson link function
                  linewidth = 1.5)+ # Width of trend lines
      
      
      # Add Error Ribbon to graph
      geom_ribbon(data = plots$eth$avers$total.e$fx,
                  aes(ymin = lower, ymax = upper),
                  colour = NA, # colour = NA suppresses edges of the ribbon
                  alpha = .25)+ # Opacity at 25% (75% Transparent)
      
      
      
      facet_wrap(~ as.factor(Concentration), nrow = 1)+ # Makes the plot multi-panel. One for each level of Concentration
      
      labs(tag="B.")+ # Add image tag for final image combo.
      
      scale_color_manual("", 
                         values = c("#F8766D", "#00BFC4"), # Line and Point color
                         labels = c('Adolescent+IAE', 'Adult+IAE'))+ # Labels for legend
      
      scale_fill_manual("",
                        values = c("#F8766D", "#00BFC4"), # Ribbon color
                        labels = c('Adolescent+IAE', 'Adult+IAE'))+ # Legend labels
      
      scale_linetype_manual("",
                            values = c("solid", "solid"), # Types of lines
                            labels = c('Adolescent+IAE', 'Adult+IAE'))+ # Legend labels
      
      scale_shape_manual("",
                         values = c(16, 15), # Shapes of points
                         labels = c('Adolescent+IAE', 'Adult+IAE'))+
      
      scale_x_continuous(expand = c(0, 0),
                         limits = c(0, 130), # Axis length limits
                         breaks = seq(0, 125, 25), # Axis break positions
                         labels = as.character(seq(0, 125, 25)))+ # Axis value labels
      
      scale_y_continuous(expand = c(0, 0),
                         limits = c(0, 110), # Axis length limits
                         breaks = seq(from = 0, to = 100, by = 25), # Axis break positions
                         labels = as.character(seq(from = 0, to = 100, by = 25)))+ # makes every other label
      
      theme_classic()+
      theme(# Text
        strip.text.x = element_text(size = 17, face = "bold"), # Modify label text for facets
        axis.title = element_text(size = 18), # Modify axis title text
        axis.text = element_text(size = 17, color = "black", face = "bold"), # Modify axis value text
        legend.title = element_text(size = 16), # Modify legend title text (!) Probably Uneccessary
        legend.text = element_text(size = 14), # Modify legend value text
        plot.tag = element_text(size = 32, face = "bold"), # Modify the plot tag (e.g. "A.") text
        plot.title = element_text(size = 18, hjust = 0.5), # I used the plot title text as axis text here.
        # Positioning
        axis.title.x = element_text(margin = margin(t = 13, r = 0, b = 0, l = 0)), # Modify X axis appearance
        axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)), # Modify Y axis appearance
        plot.tag.position = c(0.035, 0.95), # Modify position of plot tag
        legend.position = c(0.125, .85), # Modify position of legend # Modify position of legend
        # Other Aesthetics
        axis.line = element_line(size = 1.3), # Modify axis line attributes
        axis.ticks = element_line(size = 1.3, color = "black"), # Modify axis tick mark attributes
        axis.ticks.length = unit(0.2, "cm"), # Modify how long each tick is
        legend.key.size = unit(1.5, "cm"), # Modify the size of the legend key boxes
        strip.background = element_rect(colour="white"), # Gets rid of default background
        panel.spacing = unit(1.75, "lines")
      )+
      xlab("Total Ethanol Consumed (g/kg)")+
      ylab("Aversive Responses (+/-SEM)")+
      ggtitle("Ethanol % (v/v)")
    
    temporary2.ggp
```

![](README_files/figure-gfm/No%20Object%20Assignment:%20Total%20Ethanol-1.png)<!-- -->

Less garbage! But the proportions still look weird. This is fine because
we can’t really judge how well everything is going to look until it is
in its final format as a multipanel graph AND it has been saved as the
size we want it. If it looks off in the final result, then we know
something needs to be adjusted.

### Stitching the Graphs

Let’s get an idea of the final result. We’ll use `patchwork` to stitch
our two graphs together and then save it so we can take a look at what
the final output would look like:

``` r
  # Combine the two individual ggplots into one larger multipanel ggplot
    temporary3.ggp <- temporary.ggp + 
                      temporary2.ggp +
                      plot_layout(ncol = 2, widths = c(1, 2)) # Layout 2 columns with 1/3 for first col 2/3s for second plot.
    
    temporary3.ggp
```

![](README_files/figure-gfm/Stitch%20the%20Graphs%20with%20Patchwork-1.png)<!-- -->

``` r
    # Save the plot
    ggsave("Fig 8A&B Ethanol Aversive Fits.png", 
           temporary3.ggp, 
           width = 1600, 
           height = 600, 
           units = "px", 
           dpi = 96)
```

As you can see, the plot that we see in RStudio (Above) is very
different from what we see in the saved plot (Below). So DON’T BE
FOOLED!

![](README_files/Fig%208A&B%20Ethanol%20Aversive%20Fits.png)

### Making & Organizing Graph Variables

Once we have played around and made our aesthetic and theme adjustments,
we can figure out what values we need to store in objects that specify a
variety of attributes for our graphs. We also will start to get a sense
of which variables are shared between graphs and which are not. Then we
can begin organizing these variables into our hierarchy within our
`plots` object and assigning names and values to them. There are global
attributes that our graphs should share to keep things looking
consistent, like axis title text size. However, there are other
attributes that are unique to a specific category. Y-axis break
positions, for example, are different between our two types of responses
(Aversive and Hedonic) because Hedonic Responses tend to be greater in
number than Aversive Responses. We want to be able to compare between
both graphs of Hedonic Responding **OR** both graphs of Aversive
Responding on the same scale for both analyses, so the break positions
and scales are similar between the Hedonic graphs. We also want to be
able to compare between different substances when the response type is
the same, thus, Y-axis breaks are determined by response type regardless
of the substance being analyzed. Other variables like X-axis break
positions are common between the two response types, but differ
depending on the analysis type we are looking at. Mapping out which
attributes should be shared between graphs and what variables that
sharing is based on is important for remaining organized in your
project. I advise sitting down and making a bulleted list as you go
through and add aesthetics and theme arguments to your graph. Below are
several attributes which are organized in this way and share a common
storage object, “plots”. We can see all the variables, including those
for substances that weren’t included here to give you an idea of how the
organization worked for the project as a whole.

``` r
# GLOBAL ATTRIBUTES ####
  
  ## Text ####
  plots$global$strip.text.x <- element_text(size = 17, face = "bold") # Modify label text for facets
  plots$global$axis.title <- element_text(size = 18) # Modify axis title text
  plots$global$axis.text <- element_text(size = 17, color = "black", face = "bold") # Modify axis value text
  plots$global$legend.title <- element_text(size = 16) # Modify legend title text
  plots$global$legend.text <- element_text(size = 14) # Modify legend value text
  plots$global$plot.tag <- element_text(size = 32, face = "bold") # Modify the plot tag (e.g. "A.") text
  plots$global$plot.title <- element_text(size = 18, hjust = 0.5)
  
  ## Positioning ####
  plots$global$axis.title.x <- element_text(margin = margin(t = 13, r = 0, b = 0, l = 0)) # Adjusts margins around axis title text
  plots$global$axis.title.y <- element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)) # Adjusts margins around axis title text
    # Overall
    plots$global$overall$plot.tag.position <- c(0.05, 0.95) # X increasing from left, Y and increasing from bottom
    plots$global$overall$legend.position <- c(0.275, .85) # X increasing from left, Y and increasing from bottom
    # Total Ethanol
    plots$global$total.e$plot.tag.position <- c(0.035, 0.95) # X increasing from left, Y and increasing from bottom
    plots$global$total.e$legend.position <- c(0.125, .85) # X increasing from left, Y and increasing from bottom
  
  ## Aesthetics ####
  plots$global$axis.line <- element_line(size = 1.3) # Thickness of axis lines
  plots$global$axis.ticks <- element_line(size = 1.3, color = "black") # Thickness & color of axis ticks 
  plots$global$axis.ticks.length <- unit(0.2, "cm")
  plots$global$legend.key.size <- unit(1.5, "cm")
  plots$global$panel.spacing <- unit(1.75, "lines")
  
  ## Data Points ####
  
  plots$global$pt.size <- 4 # point size
  plots$global$pt.alpha <- 0.75# opacity proportion
  plots$global$overall$pt.shapes <- c(17, 18, 16, 15) # vector of shape numbers
  plots$global$total.e$pt.shapes <- c(16, 15) # vector of shape numbers
  
  ## Trend Lines ####
  
  plots$global$linewidth <- 1.5 # line thickness
  plots$global$overall$line.types <- c("dashed", "dashed", "solid", "solid") # vector of line type strings e.g. "solid" or "dashed"
  plots$global$total.e$line.types <- c("solid", "solid") # vector of line type strings e.g. "solid" or "dashed"
  
  ## Error Ribbons & Bars ####
  
  plots$global$rib.edge.color <- NA # NA suppresses edges of ribbon
  plots$global$rib.alpha <- 0.25 # Opacity proportion
  plots$global$overall$rib.color <- c("#F8C8C8", # vector of color hex codes or names
                                      "#82BFC4",
                                      "#F8766D",
                                      "#00BFC4") 
  plots$global$total.e$rib.color <- c("#F8766D", # vector of color hex codes or names
                                      "#00BFC4") 
  plots$global$err.bar.width <- 0.2
  
  ## Group ####
  
    # Line & Point Labels
    plots$global$overall$labels <- c('Adolescent+CTRL' , # vector of strings
                                     'Adult+CTRL',       # Common labels combine together in the legend
                                     'Adolescent+IAE',
                                     'Adult+IAE')
    
    plots$global$total.e$labels <- c('Adolescent+IAE', # vector of strings
                                     'Adult+IAE')
    
    # Line & Point Colors
    plots$global$overall$colors <- c("#F8C8C8", # vector of color hex codes or names
                                     "#82BFC4", 
                                     "#F8766D", 
                                     "#00BFC4")
    
    plots$global$total.e$colors <- c("#F8766D", # vector of color hex codes or names
                                     "#00BFC4")


# Y AXIS ###
  
  ## Aversive ####
  
    # Break Positions
    plots$y.axis$avers$breaks <- seq(from = 0, to = 100, by = 25) 
    # Labels
    plots$y.axis$avers$labels <- as.character(plots$y.axis$avers$breaks)
    # Limits
    plots$y.axis$avers$lims <- c(0, 110)
    
  ## Hedonic ####
    
    # Break Positions
    plots$y.axis$hedon$breaks <- seq(0, 550, 50)
    # Labels
    plots$y.axis$hedon$labels <- as.character(ifelse(plots$y.axis$hedon$breaks == 0 |
                                                plots$y.axis$hedon$breaks %% 100 == 0,
                                                plots$y.axis$hedon$breaks,
                                                "")) # makes every other label ""
    # Limits
    plots$y.axis$hedon$lims <- c(0, 580)

  
# X AXIS####
  
  ## Ethanol ####
  
    # Break Positions for Concentration
    plots$eth$x.axis$overall$breaks <- sort(unique(ABHVdata$eth$ctrl$Concentration))
    # Labels for Concentration
    plots$eth$x.axis$overall$labels <- as.character(plots$eth$x.axis$overall$breaks)
    # Overall X Limits
    plots$eth$x.axis$overall$lims <- c(0, 41)
    
    # Break Positions for Total Ethanol Consumed
    plots$eth$x.axis$total.e$breaks <- seq(0, 125, 25)
    # Labels for Total Ethanol Consumed
    plots$eth$x.axis$total.e$labels <- as.character(plots$eth$x.axis$total.e$breaks)
    # Total Ethanol X Limits
    plots$eth$x.axis$total.e$lims <- c(0, 130)
    
  ## Sucrose ####
    
    # X Break Positions for Molarity
    plots$suc$x.axis$overall$breaks <- sort(unique(ABHVdata$suc$ctrl$molarity))
    # X Labels for Molarity
    plots$suc$x.axis$overall$labels <- as.character(plots$suc$x.axis$overall$breaks)
    # Overall X Limits
    plots$suc$x.axis$overall$lims <- c(0, 1)
    
    # X Break Positions for Total Ethanol Consumed
    plots$suc$x.axis$total.e$breaks = seq(0, 125, 25)
    # X Labels for Total Ethanol Consumed
    plots$suc$x.axis$total.e$labels = as.character(plots$suc$x.axis$total.e$breaks)
    # Total Ethanol X Limits
    plots$suc$x.axis$total.e$lims <- c(0, 130)
     
  ## Water ####
    
    # X Break Positions for Total Ethanol Consumed
    plots$h2o$x.axis$total.e$breaks = seq(0, 125, 25)
    # X Labels for Total Ethanol Consumed
    plots$h2o$x.axis$total.e$labels = as.character(plots$h2o$x.axis$total.e$breaks)
    # Total Ethanol X Limits
    plots$h2o$x.axis$total.e$lims <- c(0, 130)


# Save the workspace
save.image("ABHV_workspace.RData")
```

I left them named pretty much the same name they have as a specification
or argument, so they are easy to use.

### The Final Graphs

This leaves us with out final code for both our graphs that we can copy
over to our other graphs with minimal changes and save as .png files to
our drive.

``` r
  # AVERSIVE ####
    
    ## Overall ####
  
    # Start ggplot
    plots$eth$avers$overall$ggp <- ggplot(plots$eth$avers$overall$fx,
                                          aes(Concentration, fit,
                                          group = interaction(Age, Condition),
                                          col = interaction(Age, Condition),
                                          fill = interaction(Age, Condition),
                                          linetype = interaction(Age, Condition),
                                          shape = interaction(Age, Condition)))+
      
      # Add Raw Data to graph
      geom_point(data = ABHVdata$eth$ctrl, 
                 aes(x = Concentration, y = Total.Aversive),
                 size = plots$global$pt.size,
                 alpha = plots$global$pt.alpha)+
      
      # Add Prediction Line to graph
      geom_smooth(data = plots$eth$avers$overall$fx,
                  se = FALSE, # Don't show Std. Error, we have error ribbons for that
                  method = "glm", # Generalized linear model
                  method.args = list(family = "poisson"), # Poisson link function
                  linewidth = plots$global$linewidth)+ # Width of trend lines
      
      # Add Error Ribbon to Graph
      geom_ribbon(data = plots$eth$avers$overall$fx,
                  aes(ymin = lower, ymax = upper),
                  colour = plots$global$rib.edge.color, # colour = NA suppresses edges of the ribbon
                  alpha = plots$global$rib.alpha)+ # Opacity at 25% (75% Transparent)
      
      labs(tag = "A.")+ # Add image tag for multipanel final image
      
      scale_color_manual("", 
                         values = plots$global$overall$colors, # Line and Point color
                         labels = plots$global$overall$labels)+ # Labels for legend
      
      scale_fill_manual("",
                        values = plots$global$overall$rib.color, # Ribbon color
                        labels = plots$global$overall$labels)+ # Legend labels

      scale_linetype_manual("",
                            values = plots$global$overall$line.types, # Types of lines
                            labels = plots$global$overall$labels)+ # Legend labels
                            
      scale_shape_manual("",
                         values = plots$global$overall$pt.shapes, # Shapes of points
                         labels = plots$global$overall$labels)+ # Legend labels
                         
      scale_x_continuous(expand = c(0, 0),
                         limits = plots$eth$x.axis$overall$lims, # Axis length limits
                         breaks = plots$eth$x.axis$overall$breaks, # Axis break positions
                         labels = plots$eth$x.axis$overall$labels)+ # Axis value labels
                         
      scale_y_continuous(expand = c(0, 0),
                         limits = plots$y.axis$avers$lims, # Axis length limits
                         breaks = plots$y.axis$avers$breaks, # Axis break positions
                         labels = plots$y.axis$avers$labels)+ # Axis value labels
                         
      theme_classic()+
      theme(# Text
            strip.text.x = plots$global$strip.text.x, # Modify label text for facets
            axis.title = plots$global$axis.title, # Modify axis title text
            axis.text = plots$global$axis.text, # Modify axis value text
            legend.title = plots$global$legend.title, # Modify legend title text (!) Probably Uneccessary
            legend.text = plots$global$legend.text, # Modify legend value text
            plot.tag = plots$global$plot.tag, # Modify the plot tag (e.g. "A.") text
            # Positioning
            axis.title.x = plots$global$axis.title.x, # Modify X axis appearance
            axis.title.y = plots$global$axis.title.y, # Modify Y axis appearance
            plot.tag.position = plots$global$overall$plot.tag.position, # Modify position of plot tag
            legend.position = plots$global$overall$legend.position, # Modify position of legend
            # Other Aesthetics
            axis.line = plots$global$axis.line, # Modify axis line attributes
            axis.ticks = plots$global$axis.ticks, # Modify axis tick mark attributes
            axis.ticks.length = plots$global$axis.ticks.length, # Modify how long each tick is
            legend.key.size = plots$global$legend.key.size, # Modify the size of the legend key boxes
            strip.background = element_rect(colour="white") # Gets rid of default background
            )+
      xlab("Ethanol % (v/v)")+ # X axis label
      ylab("Aversive Responses (+/-SEM)") # Y axis label
    
    plots$eth$avers$overall$ggp
    
    # Save the plot
    ggsave("Fig 8A Ethanol Aversive Graph.png",
           plots$eth$avers$overall$ggp, 
           width = 533, 
           height = 600, 
           units = "px", 
           dpi = 81)

    ## Total Ethanol ####
    
    # Start ggplot
    plots$eth$avers$total.e$ggp <- ggplot(plots$eth$avers$total.e$fx,
                                          aes(totale,
                                              fit,
                                              group = Age,
                                              col = Age,
                                              fill = Age,
                                              shape = Age))+
      
      # Add Raw Data to graph
      geom_point(data = ABHVdata$eth$no.ctrl,
                 aes(x = TOTAL.ETOH.Swap.Consumed..g.kg.,
                     y = Total.Aversive),
                 size = plots$global$pt.size, 
                 alpha = plots$global$pt.alpha)+
      
      # Add Prediction Line to graph
      geom_smooth(data = plots$eth$avers$total.e$fx,
                  se = FALSE, # Don't show Std. Error, we have error ribbons for that
                  method = "glm", # Generalized linear model
                  method.args = list(family = "poisson"), # Poisson link function
                  linewidth = plots$global$linewidth)+ # Width of trend lines
      
      
      # Add Error Ribbon to graph
      geom_ribbon(data = plots$eth$avers$total.e$fx,
                  aes(ymin = lower, ymax = upper),
                  colour = plots$global$rib.edge.color, # colour = NA suppresses edges of the ribbon
                  alpha = plots$global$rib.alpha)+ # Opacity at 25% (75% Transparent)
      
      
      
      facet_wrap(~ as.factor(Concentration), nrow = 1)+ # Makes the plot multi-panel. One for each level of Concentration
      
      labs(tag="B.")+ # Add image tag for final image combo.
      
      scale_color_manual("", 
                         values = plots$global$total.e$colors, # Line and Point color
                         labels = plots$global$total.e$labels)+ # Labels for legend
      
      scale_fill_manual("",
                        values = plots$global$total.e$rib.color, # Ribbon color
                        labels = plots$global$total.e$labels)+ # Legend labels
      
      scale_linetype_manual("",
                            values = plots$global$total.e$line.types, # Types of lines
                            labels = plots$global$total.e$labels)+ # Legend labels
      
      scale_shape_manual("",
                         values = plots$global$total.e$pt.shapes, # Shapes of points
                         labels = plots$global$total.e$labels)+ # Legend labels
      
      scale_x_continuous(expand = c(0, 0),
                         limits = plots$eth$x.axis$total.e$lims, # Axis length limits
                         breaks = plots$eth$x.axis$total.e$breaks, # Axis break positions
                         labels = plots$eth$x.axis$total.e$labels)+ # Axis value labels
      
      scale_y_continuous(expand = c(0, 0),
                         limits = plots$y.axis$avers$lims, # Axis length limits
                         breaks = plots$y.axis$avers$breaks, # Axis break positions
                         labels = plots$y.axis$avers$labels)+ # Axis value labels
      
      theme_classic()+
      theme(# Text
        strip.text.x = plots$global$strip.text.x, # Modify label text for facets
        axis.title = plots$global$axis.title, # Modify axis title text
        axis.text = plots$global$axis.text, # Modify axis value text
        legend.title = plots$global$legend.title, # Modify legend title text (!) Probably Uneccessary
        legend.text = plots$global$legend.text, # Modify legend value text
        plot.tag = plots$global$plot.tag, # Modify the plot tag (e.g. "A.") text
        plot.title = plots$global$plot.title,
        # Positioning
        axis.title.x = plots$global$axis.title.x, # Modify X axis appearance
        axis.title.y = plots$global$axis.title.y, # Modify Y axis appearance
        plot.tag.position = plots$global$total.e$plot.tag.position, # Modify position of plot tag
        legend.position = plots$global$total.e$legend.position, # Modify position of legend
        # Other Aesthetics
        axis.line = plots$global$axis.line, # Modify axis line attributes
        axis.ticks = plots$global$axis.ticks, # Modify axis tick mark attributes
        axis.ticks.length = plots$global$axis.ticks.length, # Modify how long each tick is
        legend.key.size = plots$global$legend.key.size, # Modify the size of the legend key boxes
        strip.background = element_rect(colour="white"), # Gets rid of default background
        panel.spacing = plots$global$panel.spacing
      )+
      xlab("Total Ethanol Consumed (g/kg)")+
      ylab("Aversive Responses (+/-SEM)")+
      ggtitle("Ethanol % (v/v)")
    
    plots$eth$avers$combo.ggp
    
    # Save the plot
    ggsave("Fig 8B Ethanol Aversive Total Ethanol Graph.png", 
           plots$eth$avers$total.e$ggp, 
           width = 1067, 
           height = 600, 
           units = "px", 
           dpi = 81)

    # Combine the two individual ggplots into one larger multipanel ggplot
    plots$eth$avers$combo.ggp <- plots$eth$avers$overall$ggp + 
      plots$eth$avers$total.e$ggp +
      plot_layout(ncol = 2, widths = c(1, 2)) # Layout 2 columns with 1/3 for first col 2/3s for second plot.
    
    plots$eth$avers$combo.ggp
    
    # Save the plot
    ggsave("Fig 8A&B Ethanol Aversive Fits.png", 
           plots$eth$avers$combo.ggp, 
           width = 1600, 
           height = 600, 
           units = "px", 
           dpi = 96)

# Save the workspace
save.image("ABHV_workspace.RData")
```
