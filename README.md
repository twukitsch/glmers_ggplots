Masters Example for Running GLMER and LMER Models with lme4
================
TJ Wukitsch
July 2023

# DATA BACKGROUND AND STRUCTURE…

blah blah here

# Workspace Setup

## Install and Load Required Packages

If you haven’t already downloaded and installed Rtools, please install
[`Rtools`](https://cran.r-project.org/bin/windows/Rtools/) and restart
R. The C++ compiler is required for some of the packages that you are
about to install.

The following code will check and remind you if you haven’t yet
installed Rtools. Then it will check to see if the package is already
downloaded and installed. If so, it will simply load the package. If
not, it will install the package from CRAN and then load the package
once the installation is complete. You may be prompted to allow some of
the downloads to take place so please remain at your device.

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
  
  # Packages that may be needed, maybe not:
  
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
  
  
  print("******** INSTALLATION AND LOADING COMPLETE ********")
} # end rtools installed == TRUE
```

## Set your working directory

``` r
# Set your working directory to your project folder and check that it is correct
setwd("C:/Users/kieri/Documents/ABHV/")
getwd()
```

# Workspace Overview

To keep an organized and uncluttered workspace, I use a set of
hierarchical lists to store related objects for project analyses. We
have the following object categories with most analyses: \* data \*
models \* comparisons (abbv. “compars”) \* plots

In this specific set of analyses we have a number of needs that
determine the objects in each category.

## Data

The full data requires various sub-setting to ensure the correct
substances are being assessed independently and that controls that never
received access to ethanol are not included when it would be
inappropriate to do so. Thus, my data list object will look like this
with abbreviated names for easier calling:

- data
  - raw
  - ethanol
    - with controls
    - no controls
  - sucrose
    - with controls
    - no controls
  - water
    - with controls
    - no controls

To do this we generate a list called `data` and begin populating it with
subsets of the original data. In addition we adjust the contrast coding
and scale of some of our variables to help us avoid problems later. I
adjust my categorical (factor) variables’ contrast coding attributes
because I want my GLMER models’ categorical variables to be coded like
an ANOVA to ease interpretation of the effects. In other words, I want
my model’s intercept to be at the Grand Mean. Thus, I must recode my
categorical variables using sum-to-zero aka contrast coding instead of
dummy coding or other types. One group is assigned +1 and the other is
assigned -1 and they sum to zero.

``` r
data <- list()

# Read in data file with auto-headings and blanks/ N/As set to blank ("") and my categorical or character data read in as the data type "factor"
data$raw <- read.csv("C:/Users/Kieri/Documents/ABHV/ABHV2018.csv", na.strings="\"\"", stringsAsFactors = TRUE)
data$raw$RatID <- as.factor(data$raw$RatID) # Convert RatID to factor type because it is not a numeric variable although it seems like one.
      # Change variable coding to contrast sum (sum-to-zero) coding. 
      # This attribute will be inherited for all data frames subset from this one, so it is a good idea to perform this step now for parsimonious code.
        # adjust Age to contrast coding
        contrasts(data$raw$Age)=contr.sum(2)
        contrasts(data$raw$Age)
        # adjust Condition to contrast coding
        contrasts(data$raw$Condition)=contr.sum(2)
        contrasts(data$raw$Condition)
      
View(data$raw)

# Subsetting Substances ####

  # Ethanol
    # With Controls
    data$eth$ctrl <- subset(data$raw, Substance == "Ethanol")
      # Rescale -- to avoid issues with eigen values later
      data$eth$ctrl$recoded.conc <- car::recode(data$eth$ctrl$Concentration, "5 =.05; 20 =.20; 40 =.40")
      
    # No Controls
      # Subset rats that could drink ethanol from rats that never had the opportunity
    data$eth$no.ctrl <- subset(data$eth$ctrl, Condition != "CTRL")
    
    # View your data frames in RStudio
    View(data$eth$ctrl)
    View(data$eth$no.ctrl)
  
    
  # Sucrose
    # With Controls
    data$suc$ctrl <- subset(data$raw, Substance == "Sucrose")
      # Rescale -- to avoid issues with eigen values later
      data$suc$ctrl$molarity <- car::recode(data$suc$ctrl$Concentration, ".34=.01; 3.4=.1; 34=1")

    # No Controls
      # Subset rats that could drink ethanol from rats that never had the opportunity
    data$suc$no.ctrl <- subset(data$suc$ctrl, Condition != "CTRL")
    
    # View your data frames in RStudio
    View(data$suc$ctrl)
    View(data$suc$no.ctrl)
  
    
  # Water
    # With Controls
    data$h2o$ctrl <- subset(data$raw, Substance == "Water1" | Substance == "Water2")
      # Get rid of the 4 levels of Substance inherited from the original dataset
      data$h2o$ctrl$Substance <- factor(data$h2o$ctrl$Substance)
      #Adjust contrasts to sum-to-zero
      contrasts(data$h2o$ctrl$Substance)=contr.sum(2)
      contrasts(data$h2o$ctrl$Substance)
    # No Controls
      # Subset rats that could drink ethanol from rats that never had the opportunity
    data$h2o$no.ctrl <- subset(data$h2o$ctrl, Condition != "CTRL")      
    
    # View your data frames in RStudio
    View(data$h2o$ctrl)
    View(data$h2o$no.ctrl)
```

Now we have our data list object with the following hierarchical
structure:

- `data`
  - `$raw`
  - `$eth`
    - `$ctrl`
    - `$no.ctrl`
  - `$suc`
    - `$ctrl`
    - `$no.ctrl`
  - `$h2o`
    - `$ctrl`
    - `$no.ctrl`

And I have performed my coding adjustments. However, I still need to
further prepare our data to avoid issues during analysis such as
problems with variance inflation. If I mean-center the continuous
variables that will be predictors in an LMER or GLMER model, the
variance inflation factor tends to remain within a tolerable range and
doesn’t affect results. Thus I mean-center the variables I need to below
and save my workspace.

``` r
# Centering Variables

  # Ethanol
    # center concentration to avoid issues with variance inflation factor (VIF) tolerances
    data$eth$ctrl$c.conc <- data$eth$ctrl$recoded.conc-mean(data$eth$ctrl$recoded.conc)
    # center TOTAL.ETOH.Swap.Consumed..g.kg. to avoid issues with variance inflation factor (VIF) tolerances
    data$eth$no.ctrl$c.totale <- data$eth$no.ctrl$TOTAL.ETOH.Swap.Consumed..g.kg.-mean(data$eth$no.ctrl$TOTAL.ETOH.Swap.Consumed..g.kg.)
    # center concentration fto avoid issues with variance inflation factor (VIF) tolerances
    data$eth$no.ctrl$c.conc <- data$eth$no.ctrl$recoded.conc-mean(data$eth$no.ctrl$recoded.conc)
    
    # center MAC and ROC to avoid issues with variance inflation factor (VIF) tolerances
    data$eth$no.ctrl$c.MAC <- data$eth$no.ctrl$MAC-mean(data$eth$no.ctrl$MAC)
    data$eth$no.ctrl$c.ROC <- data$eth$no.ctrl$ROC-mean(data$eth$no.ctrl$ROC)
    data$eth$no.ctrl$c.MAC3 <- data$eth$no.ctrl$MAC3-mean(data$eth$no.ctrl$MAC3)
    data$eth$no.ctrl$c.ROC3 <- data$eth$no.ctrl$ROC3-mean(data$eth$no.ctrl$ROC3)
  
    
  # Sucrose
    # Center molarity to avoid issues with variance inflation factor (VIF) tolerances
    data$suc$ctrl$c.molarity <- data$suc$ctrl$molarity-mean(data$suc$ctrl$molarity)
    data$suc$no.ctrl$c.molarity <- data$suc$no.ctrl$molarity-mean(data$suc$no.ctrl$molarity)
    #center TOTAL.ETOH.Swap.Consumed..g.kg. to avoid issues with variance inflation factor (VIF) tolerances
    data$suc$no.ctrl$c.totale <- data$suc$no.ctrl$TOTAL.ETOH.Swap.Consumed..g.kg.-mean(data$suc$no.ctrl$TOTAL.ETOH.Swap.Consumed..g.kg.)
    #center MAC and ROC for noCTRL subset to avoid issues with variance inflation factor (VIF) tolerances
    data$suc$no.ctrl$c.MAC <- data$suc$no.ctrl$MAC-mean(data$suc$no.ctrl$MAC)
    data$suc$no.ctrl$c.ROC <- data$suc$no.ctrl$ROC-mean(data$suc$no.ctrl$ROC)
    

  # Water
    #center TOTAL.ETOH.Swap.Consumed..g.kg. to avoid issues with variance inflation factor (VIF) tolerances
    data$h2o$no.ctrl$c.totale <- data$h2o$no.ctrl$TOTAL.ETOH.Swap.Consumed..g.kg.-mean(data$h2o$no.ctrl$TOTAL.ETOH.Swap.Consumed..g.kg.)
    
    #center MAC and ROC for noCTRL subset to avoid issues with variance inflation factor (VIF) tolerances
    data$h2o$no.ctrl$c.MAC <- data$h2o$no.ctrl$MAC-mean(data$h2o$no.ctrl$MAC)
    data$h2o$no.ctrl$c.ROC <- data$h2o$no.ctrl$ROC-mean(data$h2o$no.ctrl$ROC)

# Save the workspace
save.image("ABHV_workspace.RData")
```

## Models (Analyses) &

To accommodate and store each list object returned by our models I need
another hierarchical list object with a similar structure to the data
above. However, there is more nuance because I am performing analyses
for hedonic and aversive responses and, for each of those, I have
analyses that involve different variables that were calculated from
modeling other related data (total ethanol, MR, and MR3). So I will need
a different list structure, and I may want to generate the list
structure in advance so I can populate it with a list. It should look
something like this:

- models
  - ethanol
    - aversive
      - overall
      - total ethanol
      - Mean Alcohol Consumed & Rate of Change
      - Mean Alcohol Consumed & Rate of Change excluding day 3
    - hedonic
      - overall
      - total ethanol
      - Mean Alcohol Consumed & Rate of Change
      - Mean Alcohol Consumed & Rate of Change excluding day 3
  - sucrose
    - aversive
      - overall
      - total ethanol
      - Mean Alcohol Consumed & Rate of Change
    - hedonic
      - overall
      - total ethanol
      - Mean Alcohol Consumed & Rate of Change
  - water
    - aversive
      - overall
      - total ethanol
      - Mean Alcohol Consumed & Rate of Change
    - hedonic
      - overall
      - total ethanol
      - Mean Alcohol Consumed & Rate of Change

This is count data and has some missing data due to participant
attrition and has a continuous predictor variable that represents my
repeated measure (Concentration) since each animal reacted to each
concentration of each substance. Therefore a poisson Generalized Linear
Mixed Effects Regression (GLMER) is appropriate for analysis. There is
some nesting in the structure of the data and for reasons related to
understanding for the audience the publication was for I opted to do two
separate analyses on this data set instead of properly nesting it. We
will use ethanol as an illustrative example because it also has two
models that I wanted to compare.

``` r
# List setup ####

## Models ####
models <- list()
  models$eth  <- list()
    models$eth$avers  <- list()
      models$eth$avers$overall  <- list() # Delete
      models$eth$avers$total.e  <- list() # Delete
      models$eth$avers$MR1  <- list() # Delete
      models$eth$avers$MR3  <- list() # Delete
    models$eth$hedon  <- list()
      models$eth$hedon$overall  <- list() # Delete
      models$eth$hedon$total.e  <- list() # Delete
      models$eth$hedon$MR1  <- list() # Delete
      models$eth$hedon$MR3  <- list() # Delete
  models$suc  <- list()
    models$suc$avers  <- list()
    models$suc$hedon  <- list()
  models$h2o  <- list()
    models$h2o$avers  <- list()
    models$h2o$hedon  <- list()

## Comparisons ####
compars <- list()
  compars$eth  <- list()
    compars$eth$avers  <- list()
    compars$eth$hedon  <- list()
  compars$suc  <- list()
    compars$suc$avers  <- list()
    compars$suc$hedon  <- list()
  compars$h2o  <- list()
    compars$h2o$avers  <- list()
    compars$h2o$hedon  <- list()
```

Now that we have our lists we can start running our models and
comparisons and storing them as needed.

### Ethanol

``` r
# Ethanol vs Control (Overall)

  # predictors: full factorial fixed effects of centered concentration, age, and condition
  models$eth$avers$overall <- glmer(Total.Aversive ~ c.conc*Age*Condition 
                                      + (c.conc|RatID), # and the random effects of the intercept (RatID) and the slope of concentration
                                      data=data$eth$ctrl, 
                                      family=poisson)

  # Get a summary of our output
  summary(models$eth$avers$overall)
```

Now we need to check to see if our assumption of normally distributed
residuals is correct using a simple plot.

The blue line is a normal distribution. The black line is our residual
distribution. From the graph we can see the residuals are slighly
kurtotic (pointier and a bit thinner than a normal curve) but mostly
normal. I would say that the assumption of normality has been met. Now,
on to interpretation. I have my summary from before, but I need to
compare my significant variables. `emmeans` is a convenient package for
this. I can look at the means from the model and compare my two levels
of my Condition variable to see which direction the difference is in and
how large the difference between means is.

``` r
  # Compare significant variables
  compars$eth$avers$overall <- list() # Create new comparison list for overall model
  compars$eth$avers$overall$condition <- emmeans(models$eth$avers$overall, ~ Condition) # Perform comparison of Condition with emmeans
  
  # We dont need a comparison from c.conc because we have a direction AND a magnitude on a continuous variable! WOO!
  
  summary(compars$eth$avers$overall$condition, type = "response") # get summary in the numerical space of the original variable not the log/Poisson space
  
  #Save Workspace
  save.image("ABHV_workspace.RData")
```

I document these numbers, save my workspace and then move on to the next
model.

``` r
# Ethanol Group Only: Total Ethanol Consumed (total.e)
  
  # predictors: full factorial fixed effects of centered concentration, age, and total ethanol consumed during drinking phase of study
  models$eth$avers$total.e <- glmer(Total.Aversive ~ c.conc*Age*c.totale 
                                    + (c.conc|RatID), # and the random effects of the intercept (RatID) and the slope of concentration
                                    data=data$eth$no.ctrl,
                                    family=poisson)

  # Model did not converge. Grab Theta and fixed effects data from model and put into temporary object
  ss <- getME(models$eth$avers$total.e,c("theta","fixef"))
  # Extend the number of iterations with optimizer and update model
  models$eth$avers$total.e <- update(models$eth$avers$total.e,start=ss,control=glmerControl(optCtrl=list(maxfun=2e6))) 
  
  # Model converged, get summary of output
  summary(models$eth$avers$total.e)
  # Remove temporary object to declutter workspace
  remove(ss)
```

Again, before I begin interpreting, I need to check to see if our
assumption of normally distributed residuals is correct using a plot of
probability density.

Again, the blue line is a normal distribution. The black line is our
residual distribution. From the graph we can see the residuals are,
again, slightly kurtotic but mostly normal. I would say that the
assumption of normality has been met her as well. Now, on to
interpretation. I need to look at the means from the model and compare
my two levels of my Condition variable to see which direction and how
large the difference is.

``` r
    # Compare significant variables
    compars$eth$avers$total.e <- list() # Create new comparison list for total.e model
    compars$eth$avers$total.e$age<- emmeans(models$eth$avers$total.e, ~ Age) # Perform comparison of Condition with emmeans
    summary(compars$eth$avers$total.e$age, type = "response") # get summary in the numerical space of the original variable not the log/Poisson space
    
      #Save Workspace
  save.image("ABHV_workspace.RData")
```

Copy down the data you need, save, rinse, and repeat…

``` r
  # predictors: note that these are not the full factorial fixed effects. I'm not interested in the higher level interactions here.
models$eth$avers$MR1 <-glmer(Total.Aversive ~ c.conc+Age+c.MAC+c.ROC
                 + c.conc:Age
                 + c.conc:c.MAC
                 + c.conc:c.ROC
                 + Age:c.MAC
                 + Age:c.ROC
                 + c.conc:Age:c.MAC
                 + c.conc:Age:c.ROC
                 + (c.conc|RatID), data=data$eth$no.ctrl, family=poisson)

# Model did not converge, used code below to extend # of iterations and start from where the previous model left off.
ss <- getME(models$eth$avers$MR1,c("theta","fixef"))
models$eth$avers$MR1 <- update(models$eth$avers$MR1,start=ss,control=glmerControl(optCtrl=list(maxfun=2e6)))
summary(models$eth$avers$MR1)
remove(ss)

# Check variance inflation factor (VIF)
vif(models$eth$avers$MR1)

# Compare with the previous model
AIC(models$eth$avers$MR1, models$eth$avers$total.e)
BIC(models$eth$avers$MR1, models$eth$avers$total.e)
```

Again, check normality of residuals

Similar to previous residuals. No planned comparisons here. This model
was not selected for interpretation (considerably higher BIC), so I
didn’t perform any. Then we repeat for the 4th model which excludes the
first day of drinking from calculations of MAC and RoC.

``` r
  # predictors: note that these are not the full factorial fixed effects. I'm not interested in the higher level interactions here.
models$eth$avers$MR3 <-glmer(Total.Aversive ~ c.conc+Age+c.MAC3+c.ROC3
                 + c.conc:Age
                 + c.conc:c.MAC3
                 + c.conc:c.ROC3
                 + Age:c.MAC3
                 + Age:c.ROC3
                 + c.conc:Age:c.MAC3
                 + c.conc:Age:c.ROC3
                 + (c.conc|RatID), data=data$eth$no.ctrl, family=poisson)

# Model did not converge, used code below to extend # of iterations and start from where the previous model left off.
ss <- getME(models$eth$avers$MR3, c("theta","fixef"))
models$eth$avers$MR3 <- update(models$eth$avers$MR3,start=ss,control=glmerControl(optCtrl=list(maxfun=2e9)))
remove(ss)
summary(models$eth$avers$MR3)

vif(models$eth$avers$MR3)
AIC(models$eth$avers$MR3, models$eth$avers$MR1, models$eth$avers$total.e)
BIC(models$eth$avers$MR3, models$eth$avers$MR1, models$eth$avers$total.e)
```

Again, check normality of residuals

And again, things look pretty normal. This one also has no planned
comparisons and was not selected for the same reason as the previous.
The remaining model runs are not shown here for brevity.

# Plotting/Graphing GLMERs with ggplot2

In the end, our specific lists of objects with abbreviations looks like
this:

- data
  - raw
  - ethanol
    - with controls
    - no controls
  - sucrose
    - with controls
    - no controls
  - water
    - with controls
    - no controls
- models
  - ethanol
    - aversive
      - overall
      - total ethanol
      - MR (Mean Alcohol Consumed & Rate of Change)
      - MR3 (Mean Alcohol Consumed & Rate of Change excluding day 3)
    - hedonic
      - overall
      - total ethanol
      - MR (Mean Alcohol Consumed & Rate of Change)
      - MR3 (Mean Alcohol Consumed & Rate of Change excluding day 3)
  - sucrose
    - aversive
      - overall
      - total ethanol
      - MR (Mean Alcohol Consumed & Rate of Change)
    - hedonic
      - overall
      - total ethanol
      - MR (Mean Alcohol Consumed & Rate of Change)
  - water
    - aversive
      - overall
      - total ethanol
      - MR (Mean Alcohol Consumed & Rate of Change)
    - hedonic
      - overall
      - total ethanol
      - MR (Mean Alcohol Consumed & Rate of Change)
- comparisons
- plots

## Including Plots

You can also embed plots, for example:

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.