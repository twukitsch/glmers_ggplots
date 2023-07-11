# Load Packages ####

# See if rtools is installed. It has necessary C++ compiler for installs of some of the libraries that are required.
if (Sys.which("make") = = "") { # Check to see if "make" command from rtools is found in system's PATH. If an empty string is retuned, rtools isn't installed.
 message("Rtools is not found. Please install Rtools from https://cran.r-project.org/bin/windows/Rtools/ and restart R.")
} else { #rtools installed = = TRUE
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
} #end rtools installed = = TRUE


#Read in data file with auto-headings and blanks/ N/As set to blank ("")
data <- list()

#Read in data file with auto-headings and blanks/ N/As set to blank ("")
data$raw <- read.csv("C:/Users/Kieri/Documents/ABHV/ABHV2018.csv", na.strings = "\"\"", stringsAsFactors = TRUE)
data$raw$RatID <- as.factor(data$raw$RatID) # Convert RatID to factor because it is not a numeric variable
View(data$raw)


# ETHANOL ##### (!)FIX FLAG

###Data Frame Preparation###

##Subset for Substance
 # With Controls
  data$eth$ctrl <- subset(data$raw, Substance = = "Ethanol")
 # Rescaling concentration to avoid issues with eigen values
  data$eth$ctrl$recoded.conc <- car::recode(data$eth$ctrl$Concentration, "5 = .05; 20 = .20; 40 = .40")
 # No Controls
  # Subset rats that could drink ethanol from rats that never had the opportunity
  data$eth$no.ctrl <- subset(data$eth$ctrl, Condition ! = "CTRL")
  
  View(data$eth$ctrl)
  View(data$eth$no.ctrl)

##Centering
 #center concentration to avoid issues with variance inflation
  data$eth$ctrl$c.conc <- data$eth$ctrl$recoded.conc-mean(data$eth$ctrl$recoded.conc)
 #center TOTAL.ETOH.Swap.Consumed..g.kg. to avoid issues with variance inflation
  data$eth$no.ctrl$c.totale <- data$eth$no.ctrl$TOTAL.ETOH.Swap.Consumed..g.kg.-mean(data$eth$no.ctrl$TOTAL.ETOH.Swap.Consumed..g.kg.)
 #center concentration for noCTRL subset to avoid issues with variance inflation
  data$eth$no.ctrl$c.conc <- data$eth$no.ctrl$recoded.conc-mean(data$eth$no.ctrl$recoded.conc)
 #center MAC and ROC for noCTRL subset to avoid issues with variance inflation
  data$eth$no.ctrl$c.MAC <- data$eth$no.ctrl$MAC - mean(data$eth$no.ctrl$MAC)
  data$eth$no.ctrl$c.ROC <- data$eth$no.ctrl$ROC - mean(data$eth$no.ctrl$ROC)
  data$eth$no.ctrl$c.MAC3 <- data$eth$no.ctrl$MAC3 - mean(data$eth$no.ctrl$MAC3)
  data$eth$no.ctrl$c.ROC3 <- data$eth$no.ctrl$ROC3 - mean(data$eth$no.ctrl$ROC3)

### Variable Coding Adjustment###

 # adjust Age to contrast coding
  contrasts(data$eth$ctrl$Age) = contr.sum(2)
  contrasts(data$eth$ctrl$Age)
 
 # adjust Condition to contrast coding
  contrasts(data$eth$ctrl$Condition) = contr.sum(2)
  contrasts(data$eth$ctrl$Condition)
 
 
  contrasts(data$eth$no.ctrl$Age) = contr.sum(2)
  contrasts(data$eth$no.ctrl$Age)
  
  contrasts(data$eth$no.ctrl$Condition) = contr.sum(2)
  contrasts(data$eth$no.ctrl$Condition)

 
## ETHANOL AVERSIVES ANALYSES####

  ### Ethanol Aversives GLMER (with EtOH vs CTRL)####
    models$eth$avers$overall <- glmer(Total.Aversive ~ c.conc*Age*Condition 
                                      + (c.conc|RatID),
                                      data = data$eth$ctrl,
                                      family = poisson)
  
    summary(models$eth$avers$overall)
 
    # Post Hocs & Planned Contrasts
    # Checking to see if Adolescent vs Adult IAE rats were different as this is the only relevant comparison here
      compars$eth$avers$overall <- list() # Create new comparison list for 'overall' model
      compars$eth$avers$overall$condition <- emmeans(models$eth$avers$overall, ~ Condition)
      summary(compars$eth$avers$overall$condition, type = "response")

      
    # Make and save residual distribution plot
      # make PNG file
      png("Eavers Residual Probability Density.png", width = 300, height = 300)
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
            yaxt = "n")
      remove(MEAN, STDEV) # Clean workspace
      # close the file
      dev.off()
      
      
  ### Ethanol Aversives GLMER (EtOH Group Only: Total EtOH Consumed) ####
    models$eth$avers$total.e <- glmer(Total.Aversive ~ c.conc*Age*c.totale 
                                      + (c.conc|RatID),
                                      data = data$eth$no.ctrl,
                                      family = poisson)
 
    # Model did not converge, used code below to extend # of iterations and start from where the previous model left off.
      ss <- getME(models$eth$avers$total.e, c("theta", "fixef"))
      models$eth$avers$total.e <- update(models$eth$avers$total.e,
                                         start = ss,
                                         control = glmerControl(optCtrl = list(maxfun = 2e6)))
        remove(ss) # Clean workspace
   
    summary(models$eth$avers$total.e)
 
    # Post Hocs & Planned Concrasts
      compars$eth$avers$total.e <- list() # Create new comparison list for total.e model
      compars$eth$avers$total.e$age<- emmeans(models$eth$avers$total.e, ~ Age) # Perform comparison of Conditions with emmeans
      summary(compars$eth$avers$total.e$age, type = "response")
      
    # Make and save residual distribution plot
      # make PNG file
      png("EaversTot Residual Probability Density.png", width = 300, height = 300)
      # plot residual density function
      plot(density(residuals(models$eth$avers$total.e)), 
           main = "",
           xlab = "", 
           frame = FALSE)
      # Add normal distribution to the residual plot for comparison to check assumption of normality
      MEAN = mean(residuals(models$eth$avers$total.e))
      STDEV = sqrt(var(residuals(models$eth$avers$total.e)))
      curve(dnorm(x, mean = MEAN, sd = STDEV),
            col = "darkblue",
            lwd = 2,
            add = TRUE,
            yaxt = "n")
      remove(MEAN, STDEV) # Clean workspace
      # close the file
      dev.off()

      
  ### Ethanol Aversives GLMER (EtOH Group Only: MAC & ROC) ####
    models$eth$avers$MR1 <- glmer(Total.Aversive ~ c.conc+Age+c.MAC+c.ROC
                                  + c.conc:Age
                                  + c.conc:c.MAC
                                  + c.conc:c.ROC
                                  + Age:c.MAC
                                  + Age:c.ROC
                                  + c.conc:Age:c.MAC
                                  + c.conc:Age:c.ROC
                                  + (c.conc|RatID),
                                  data = data$eth$no.ctrl, 
                                  family = poisson)

    # Model did not converge, used code below to extend # of iterations and start from where the previous model left off.
      ss <- getME(models$eth$avers$MR1, c("theta", "fixef"))
      models$eth$avers$MR1 <- update(models$eth$avers$MR1,
                                     start = ss,
                                     control = glmerControl(optCtrl = list(maxfun = 2e6)))
      remove(ss) # Clean workspace
   
    summary(models$eth$avers$MR1)
  
    # Variance inflation check & model comparison
      vif(models$eth$avers$MR1)
      AIC(models$eth$avers$MR1, models$eth$avers$total.e)
      BIC(models$eth$avers$MR1, models$eth$avers$total.e)
  
    # Make and save residual distribution plot
      # Make PNG file
      png("EaversMR Residual Probability Density.png", width = 300, height = 300)
      # Start by plotting a density function of model's residuals
      plot(density(residuals(models$eth$avers$MR1)), 
           main = "", 
           xlab = "", 
           frame = FALSE)
       # Add normal distribution to the residual plot for comparison to check assumption of normality
       MEAN = mean(residuals(models$eth$avers$MR1)) # get the residual mean
       STDEV = sqrt(var(residuals(models$eth$avers$MR1))) # get the st dev
       curve(dnorm(x, mean = MEAN, sd = STDEV),
             col = "darkblue",
             lwd = 2,
             add = TRUE,
             yaxt = "n") # Generate normal curve
       remove(MEAN, STDEV) # Clean workspace
       # close the file
       dev.off()


  ### Ethanol Aversives GLMER (EtOH Group Only: MAC3 & ROC3) ######
    models$eth$avers$MR3 <- glmer(Total.Aversive ~ c.conc+Age+c.MAC3+c.ROC3
                                  + c.conc:Age
                                  + c.conc:c.MAC3
                                  + c.conc:c.ROC3
                                  + Age:c.MAC3
                                  + Age:c.ROC3
                                  + c.conc:Age:c.MAC3
                                  + c.conc:Age:c.ROC3
                                  + (c.conc|RatID),
                                  data = data$eth$no.ctrl,
                                  family = poisson)
 
    # Model did not converge, used code below to extend # of iterations and start from where the previous model left off.
      ss <- getME(models$eth$avers$MR3, c("theta","fixef"))
      models$eth$avers$MR3 <- update(models$eth$avers$MR3,
                                     start = ss,
                                     control = glmerControl(optCtrl = list(maxfun = 2e9)))
      remove(ss) # Clean workspace
    
    summary(models$eth$avers$MR3)
  
    # Variance inflation check & model comparison
      vif(models$eth$avers$MR3)
      AIC(models$eth$avers$MR3, models$eth$avers$MR1, models$eth$avers$total.e)
      BIC(models$eth$avers$MR3, models$eth$avers$MR1, models$eth$avers$total.e)
  
    # Make and save residual distribution plot
      #make PNG file
      png("EaversMR3 Residual Probability Density.png", width = 300, height = 300)
      # Start by plotting a density function of model's residuals
      plot(density(residuals(models$eth$avers$MR3)), 
          main = "",
          xlab = "",
          frame = FALSE)
      # Add normal distribution to the residual plot for comparison to check assumption of normality
      MEAN = mean(residuals(models$eth$avers$MR3)) # get the residual mean
      STDEV = sqrt(var(residuals(models$eth$avers$MR3))) # get the st dev
      curve(dnorm(x, mean = MEAN, sd = STDEV),
            col = "darkblue",
            lwd = 2,
            add = TRUE,
            yaxt = "n") # Generate normal curve
      remove(MEAN, STDEV) # Clean workspace
      # close the file
      dev.off()



## ETHANOL HEDONIC ANALYSES ####

  ### Ethanol Hedonics GLMER (with EtOH vs CTRL)####
    models$eth$hedon$overall <- glmer(Total.Hedonic...MM. ~ c.conc*Age*Condition
                                      + (c.conc|RatID),
                                      data = data$eth$ctrl,
                                      family = poisson)
  
    summary(models$eth$hedon$overall)
 
    # Post Hocs & Planned Contrasts
      compars$eth$hedon$overall <- list() # Create new comparison list for overall model
      # Use emmeans to get means for Conditions and summary to back-transform using 'type = "response"'
      compars$eth$hedon$overall$condition <- emmeans(models$eth$hedon$overall, ~ Condition)
      summary(compars$eth$hedon$overall$condition, type = "response")
      
      # Getting b's for Condition. Remember that c.conc was rescaled so move the decimal to the left 2 times.
      compars$eth$hedon$overall$conc.x.cond <- emtrends(models$eth$hedon$overall,
                                                        ~ Condition,
                                                        var = "c.conc")
      # If we wanted these slopes in terms of the response variable AT THE GRAND
      # MEAN we can add the `regrid = "response"` argument
      # However, this is frequently NOT what you want when you graph the log 
      # curve or talk about log change across a response variable
      summary(compars$eth$hedon$overall$conc.x.cond)
    
    #Make and save residual distribution plot
      #make PNG file
      png("Ehed Residual Probability Density.png", width = 300, height = 300)
      #plot residual density function
      plot(density(residuals(models$eth$hedon$overall)), 
           main = "",
           xlab = "",
           frame = FALSE)
      #Add normal distribution to the residual plot for comparison to check assumption of normality
      MEAN = mean(residuals(models$eth$hedon$overall))
      STDEV = sqrt(var(residuals(models$eth$hedon$overall)))
      curve(dnorm(x, mean = MEAN, sd = STDEV),
            col = "darkblue",
            lwd = 2,
            add = TRUE,
            yaxt = "n")
      remove(MEAN, STDEV)
      #close the file
      dev.off()


  ### Ethanol Hedonics GLMER (EtOH Group Only: Total EtOH Consumed) ######
    models$eth$hedon$total.e <- glmer(Total.Hedonic...MM. ~ c.conc*Age*c.totale 
                                      + (c.conc|RatID),
                                      data = data$eth$no.ctrl,
                                      family = poisson)
  
    summary(models$eth$hedon$total.e)
 
    # Post Hocs and Planned Contrasts
      compars$eth$hedon$total.e <- list() # Create new comparison list for overall model
      compars$eth$hedons$total.e$age <- emmeans(models$eth$hedon$total.e, ~ Age) # Age post hoc comparison
      summary(compars$eth$hedon$total.e$age, type = "response")
      
    #Make and save residual distribution plot
      #make PNG file
      png("EhedTot Residual Probability Density.png", width = 300, height = 300)
      #plot residual density function
      plot(density(residuals(models$eth$hedon$total.e)), 
           main = "",
           xlab = "",
           frame = FALSE)
      #Add normal distribution to the residual plot for comparison to check assumption of normality
      MEAN = mean(residuals(models$eth$hedon$total.e))
      STDEV = sqrt(var(residuals(models$eth$hedon$total.e)))
      curve(dnorm(x, mean = MEAN, sd = STDEV),
            col = "darkblue",
            lwd = 2,
            add = TRUE,
            yaxt = "n")
      remove(MEAN, STDEV)
      #close the file
      dev.off()
 
  ### Ethanol Hedonics GLMER (EtOH Group Only: MAC & ROC) ######
    models$eth$hedon$MR1 <- glmer(Total.Hedonic...MM. ~ c.conc+Age+c.MAC+c.ROC
                                  + c.conc:Age
                                  + c.conc:c.MAC
                                  + c.conc:c.ROC
                                  + Age:c.MAC
                                  + Age:c.ROC
                                  + c.conc:Age:c.MAC
                                  + c.conc:Age:c.ROC
                                  + (c.conc|RatID),
                                  data = data$eth$no.ctrl,
                                  family = poisson)
    
    summary(models$eth$hedon$MR1)
 
    # Variance inflation check & model comparison
      vif(models$eth$hedon$MR1)
      AIC(models$eth$hedon$MR1, models$eth$hedon$total.e)
      BIC(models$eth$hedon$MR1, models$eth$hedon$total.e)
   
  
    #Make and save residual distribution plot
      #make PNG file
      png("EhedMR Residual Probability Density.png", width = 300, height = 300)
      #plot residual density function
      plot(density(residuals(models$eth$hedon$MR1)), 
           main = "",
           xlab = "",
           frame = FALSE)
      #Add normal distribution to the residual plot for comparison to check assumption of normality
      MEAN = mean(residuals(models$eth$hedon$MR1))
      STDEV = sqrt(var(residuals(models$eth$hedon$MR1)))
      curve(dnorm(x, mean = MEAN, sd = STDEV),
            col = "darkblue",
            lwd = 2,
            add = TRUE,
            yaxt = "n")
      remove(MEAN, STDEV)
      #close the file
      dev.off()
 
 
  ### Ethanol Hedonics GLMER (EtOH Group Only: MAC3 & ROC3) ######
    models$eth$hedon$MR3 <- glmer(Total.Hedonic...MM. ~ c.conc+Age+c.MAC3+c.ROC3
                                  + c.conc:Age
                                  + c.conc:c.MAC3
                                  + c.conc:c.ROC3
                                  + Age:c.MAC3
                                  + Age:c.ROC3
                                  + c.conc:Age:c.MAC3
                                  + c.conc:Age:c.ROC3
                                  + (c.conc|RatID),
                                  data = data$eth$no.ctrl,
                                  family = poisson)
    
    summary(models$eth$hedon$MR3)
    
    # Variance inflation check & model comparison
      vif(models$eth$hedon$MR3)
      AIC(models$eth$hedon$MR3, models$eth$hedon$MR1, models$eth$hedon$total.e)
      BIC(models$eth$hedon$MR3, models$eth$hedon$MR1, models$eth$hedon$total.e)
 
 
    # Make and save residual distribution plot
      # make PNG file
      png("EhedMR3 Residual Probability Density.png", width = 300, height = 300)
      # plot residual density function
      plot(density(residuals(models$eth$hedon$MR3)), 
         main = "",
         xlab = "", 
         frame = FALSE)
      # Add normal distribution to the residual plot for comparison to check assumption of normality
      MEAN = mean(residuals(models$eth$hedon$MR3))
      STDEV = sqrt(var(residuals(models$eth$hedon$MR3)))
      curve(dnorm(x, mean = MEAN, sd = STDEV),
            col = "darkblue",
            lwd = 2,
            add = TRUE,
            yaxt = "n")
      remove(MEAN, STDEV)
      # close the file
      dev.off()
 
 # MR3 Models no longer appear after this point: Similar AICs, with BICs favoring the less complex model.
 # Use less complex Total Ethanol model for interpretation from here on out.

# SUCROSE ####

###Data Frame Preparation###

##subset for Substance
data$suc$ctrl <- subset(data@raw, Substance = = "Sucrose")
View(data$suc$ctrl)
##Subset for Consumption Pattern Variables
data$suc$no.ctrl <- subset(data$suc$ctrl, Condition ! = "CTRL")
View(data$suc$no.ctrl)

##Rescaling
#convert concentration to molarity and center to avoid issues with scaling & vif
data$suc$ctrl$molarity <-recode(data$suc$ctrl$Concentration, ".34 = .01; 3.4 = .1; 34 = 1")
data$suc$ctrl$c.molarity <-data$suc$ctrl$molarity-mean(data$suc$ctrl$molarity)
#convert concentration to molarity and center to avoid issues with scaling & vif
data$suc$no.ctrl$molarity <-recode(data$suc$no.ctrl$Concentration, ".34 = .01; 3.4 = .1; 34 = 1")
data$suc$no.ctrl$c.molarity <-data$suc$no.ctrl$molarity-mean(data$suc$no.ctrl$molarity)

##Centering
#center TOTAL.ETOH.Swap.Consumed..g.kg. to avoid issues with variance inflation
data$suc$no.ctrl$c.totale <- data$suc$no.ctrl$TOTAL.ETOH.Swap.Consumed..g.kg.-mean(data$suc$no.ctrl$TOTAL.ETOH.Swap.Consumed..g.kg.)
#center MAC and ROC for noCTRL subset to avoid issues with variance inflation
data$suc$no.ctrl$c.MAC <- data$suc$no.ctrl$MAC-mean(data$suc$no.ctrl$MAC)
data$suc$no.ctrl$c.ROC <- data$suc$no.ctrl$ROC-mean(data$suc$no.ctrl$ROC)

###Variable Coding Adjustment###

#adjust Age and Condition to contrast coding for both datasets
contrasts(data$suc$ctrl$Age) = contr.sum(2)
contrasts(data$suc$ctrl$Age)

contrasts(data$suc$ctrl$Condition) = contr.sum(2)
contrasts(data$suc$ctrl$Condition)

contrasts(data$suc$no.ctrl$Age) = contr.sum(2)
contrasts(data$suc$no.ctrl$Age)

contrasts(data$suc$no.ctrl$Condition) = contr.sum(2)
contrasts(data$suc$no.ctrl$Condition)

## SUCROSE AVERSIVES #####

### Sucrose Aversives GLMER (with EtOH vs CTRL)####
Savers <-glmer(Total.Aversive ~ c.molarity*Age*Condition 
        + (c.molarity|RatID), data = data$suc$ctrl, family = poisson)
# Model did not converge, used code below to extend # of iterations and start from where the previous model left off.
ss4 <- getME(Savers,c("theta","fixef"))
Savers <- update(Savers,start = ss4,control = glmerControl(optCtrl = list(maxfun = 2e6)))
summary(Savers)

#Post Hocs & Planned Contrasts
Savers.emm.a <- emmeans(Savers, ~ Age)
summary(Savers.emm.a, type = "response")


### Sucrose Aversives GLMER (EtOH Group Only: Total EtOH Consumed) ######

SaversTot <-glmer(Total.Aversive ~ c.molarity*Age*c.totale 
         + (c.molarity|RatID), data = data$suc$no.ctrl, family = poisson)

# Model did not converge, used code below to extend # of iterations and start from where the previous model left off.
ss5 <- getME(SaversTot,c("theta","fixef"))
SaversTot <- update(SaversTot,start = ss5,control = glmerControl(optCtrl = list(maxfun = 2e6)))
summary(SaversTot)

##Post Hocs & Planned Contrasts

#Age*Total Ethanol Manual Calculation
#Adolescent
Savers.Ado.b <- fixef(SaversTot)[4] + fixef(SaversTot)[7]
Savers.Ado.b
#Adult
Savers.Adu.b <- fixef(SaversTot)[4] - fixef(SaversTot)[7]
Savers.Adu.b

## SUCROSE AVERSIVES ANALYSES####

  ### Sucrose Aversives GLMER (with EtOH vs CTRL)####
    models$suc$avers$overall <- glmer(Total.Aversive ~ c.molarity*Age*Condition 
                                      + (c.molarity|RatID),
                                      data = data$suc$ctrl,
                                      family = poisson)
    # Model did not converge, used code below to extend # of iterations and start from where the previous model left off.
      ss <- getME(models$suc$avers$overall, c("theta", "fixef"))
      models$suc$avers$overall <- update(models$suc$avers$overall,
                                         start = ss,
                                         control = glmerControl(optCtrl = list(maxfun = 2e6)))
      remove(ss) # Clean workspace
      
    summary(models$suc$avers$overall)
      
    # Make and save residual distribution plot
      # make PNG file
      png("Savers Residual Probability Density.png", width = 300, height = 300)
      # plot residual density function
      plot(density(residuals(models$suc$avers$overall)), 
           main = "",
           xlab = "", 
           frame = FALSE)
      # Add normal distribution to the residual plot for comparison to check assumption of normality
      MEAN = mean(residuals(models$suc$avers$overall))
      STDEV = sqrt(var(residuals(models$suc$avers$overall)))
      curve(dnorm(x, mean = MEAN, sd = STDEV),
            col = "darkblue",
            lwd = 2,
            add = TRUE,
            yaxt = "n")
      remove(MEAN, STDEV) # Clean workspace
      # close the file
      dev.off()
      
      
  ### Sucrose Aversives GLMER (EtOH Group Only: Total EtOH Consumed) ####
    models$suc$avers$total.e <- glmer(Total.Aversive ~ c.molarity*Age*c.totale 
                                      + (c.molarity|RatID),
                                      data = data$suc$no.ctrl,
                                      family = poisson)
 
    # Model did not converge, used code below to extend # of iterations and start from where the previous model left off.
      ss <- getME(models$suc$avers$total.e, c("theta", "fixef"))
      models$suc$avers$total.e <- update(models$suc$avers$total.e,
                                         start = ss,
                                         control = glmerControl(optCtrl = list(maxfun = 2e6)))
        remove(ss) # Clean workspace
   
    summary(models$suc$avers$total.e)
 
    # Post Hocs & Planned Concrasts
      compars$suc$avers$total.e <- list() # Create new comparison list for overall model
      # Get the slopes (trends) for each level of age
      compars$suc$avers$total.e$molar.x.age <- emtrends(models$suc$avers$total.e,
                                                        ~ Age,
                                                        var = "c.molarity")
      
      summary(compars$suc$avers$total.e$molar.x.age)
      
    # Make and save residual distribution plot
      # make PNG file
      png("SaversTot Residual Probability Density.png", width = 300, height = 300)
      # plot residual density function
      plot(density(residuals(models$suc$avers$total.e)), 
           main = "",
           xlab = "", 
           frame = FALSE)
      # Add normal distribution to the residual plot for comparison to check assumption of normality
      MEAN = mean(residuals(models$suc$avers$total.e))
      STDEV = sqrt(var(residuals(models$suc$avers$total.e)))
      curve(dnorm(x, mean = MEAN, sd = STDEV),
            col = "darkblue",
            lwd = 2,
            add = TRUE,
            yaxt = "n")
      remove(MEAN, STDEV) # Clean workspace
      # close the file
      dev.off()


## SUCROSE HEDONIC ANALYSES ####

  ### Sucrose Hedonics GLMER (with EtOH vs CTRL)####
    models$suc$hedon$overall <- glmer(Total.Hedonic...MM. ~ c.molarity*Age*Condition
                                      + (c.molarity|RatID),
                                      data = data$suc$ctrl,
                                      family = poisson)
  
    summary(models$suc$hedon$overall)
 
    # Post Hocs & Planned Contrasts
      compars$suc$hedon$overall <- list() # Create new comparison list for overall model
      # Use emmeans to get means for Age and summary to back-transform using 'type = "response"'
      compars$suc$hedon$overall$age <- emmeans(models$suc$hedon$overall, ~ Age)
      summary(compars$suc$hedon$overall$age, type = "response")
      
      # Getting b's for Age. Remember that c.molarity was rescaled so move the decimal to the left 2 times.
      compars$suc$hedon$overall$molar.x.age <- emtrends(models$suc$hedon$overall,
                                                        ~ Age,
                                                        var = "c.molarity")
      # If we wanted these slopes in terms of the response variable AT THE GRAND
      # MEAN we can add the `regrid = "response"` argument
      # However, this is frequently NOT what you want when you graph the log 
      # curve or talk about log change across a response variable
      summary(compars$suc$hedon$overall$molar.x.age)
    
    #Make and save residual distribution plot
      #make PNG file
      png("Shed Residual Probability Density.png", width = 300, height = 300)
      #plot residual density function
      plot(density(residuals(models$suc$hedon$overall)), 
           main = "",
           xlab = "",
           frame = FALSE)
      #Add normal distribution to the residual plot for comparison to check assumption of normality
      MEAN = mean(residuals(models$suc$hedon$overall))
      STDEV = sqrt(var(residuals(models$suc$hedon$overall)))
      curve(dnorm(x, mean = MEAN, sd = STDEV),
            col = "darkblue",
            lwd = 2,
            add = TRUE,
            yaxt = "n")
      remove(MEAN, STDEV)
      #close the file
      dev.off()


  ### Sucrose Hedonics GLMER (EtOH Group Only: Total EtOH Consumed) ######
    models$suc$hedon$total.e <- glmer(Total.Hedonic...MM. ~ c.molarity*Age*c.totale 
                                      + (c.molarity|RatID),
                                      data = data$suc$no.ctrl,
                                      family = poisson)
  
    summary(models$suc$hedon$total.e)
 
    # Post Hocs and Planned Contrasts
      compars$suc$hedon$total.e <- list() # Create new comparison list for overall model
      compars$suc$hedon$total.e$age <- emmeans(models$suc$hedon$total.e, ~ Age) # Look at Age means
      summary(compars$suc$hedon$total.e$age, type = "response") # Mean estimates at GRAND MEAN
      
      # Getting b's for Age. Remember that c.molarity was rescaled so move the decimal to the left 2 times.
      compars$suc$hedon$total.e$molar.x.age <- emtrends(models$suc$hedon$overall,
                                                        ~ Age,
                                                        var = "c.molarity")
      # If we wanted these slopes in terms of the response variable AT THE GRAND
      # MEAN we can add the `regrid = "response"` argument
      # However, this is frequently NOT what you want when you graph the log 
      # curve or talk about log change across a response variable
      summary(compars$suc$hedon$total.e$molar.x.age)
      
      
    #Make and save residual distribution plot
      #make PNG file
      png("ShedTot Residual Probability Density.png", width = 300, height = 300)
      #plot residual density function
      plot(density(residuals(models$suc$hedon$total.e)), 
           main = "",
           xlab = "",
           frame = FALSE)
      #Add normal distribution to the residual plot for comparison to check assumption of normality
      MEAN = mean(residuals(models$suc$hedon$total.e))
      STDEV = sqrt(var(residuals(models$suc$hedon$total.e)))
      curve(dnorm(x, mean = MEAN, sd = STDEV),
            col = "darkblue",
            lwd = 2,
            add = TRUE,
            yaxt = "n")
      remove(MEAN, STDEV)
      #close the file
      dev.off()


# WATER #####

###Data Frame Preparation###

#Load new dataset made in excel to get rid of additional variables in Substance column. subset(), and select(filter(),cols) will not work for this.
#Laptop
data$h2o$ctrl <- subset(data$raw, Substance = = "Water1" | Substance = = "Water2")
View(data$h2o$ctrl)

#Adjust contrasts to sum-to-zero
contrasts(data$h2o$ctrl$Substance) = contr.sum(2)
contrasts(data$h2o$ctrl$Substance)

contrasts(data$h2o$ctrl$Age) = contr.sum(2)
contrasts(data$h2o$ctrl$Age)

contrasts(data$h2o$ctrl$Condition) = contr.sum(2)
contrasts(data$h2o$ctrl$Condition)

##Filter for Consumption Pattern Variables
library("dplyr", lib.loc = "~/R/win-library/3.6")
data$h2o$no.ctrl <- filter(data$h2o$ctrl, Condition ! = "CTRL")
View(data$h2o$no.ctrl)
detach("package:dplyr", unload = TRUE)

##Centering
#center TOTAL.ETOH.Swap.Consumed..g.kg. to avoid issues with variance inflation
data$h2o$no.ctrl$c.totale <- data$h2o$no.ctrl$TOTAL.ETOH.Swap.Consumed..g.kg.-mean(data$h2o$no.ctrl$TOTAL.ETOH.Swap.Consumed..g.kg.)
#center MAC and ROC for noCTRL subset to avoid issues with variance inflation
data$h2o$no.ctrl$c.MAC <- data$h2o$no.ctrl$MAC-mean(data$h2o$no.ctrl$MAC)
data$h2o$no.ctrl$c.ROC <- data$h2o$no.ctrl$ROC-mean(data$h2o$no.ctrl$ROC)

###Variable Coding Adjustment###

# Get rid of the 4 levels of Substance inherited from the original dataset
data$h2o$ctrl$Substance <- factor(data$h2o$ctrl$Substance)
#Adjust contrasts to sum-to-zero
contrasts(data$h2o$no.ctrl$Substance) = contr.sum(2)
contrasts(data$h2o$no.ctrl$Substance)



## WATER AVERSIVES #####

### Water Aversives GLMER (with EtOH vs CTRL)####
Havers <-glmer(Total.Aversive ~ Substance*Age*Condition 
        + (1|RatID), data = data$h2o$ctrl, family = poisson)
summary(Havers)

#Post Hocs & Planned Contrasts

#Condition
Havers.emm.c <- emmeans(Havers, ~ Condition)
summary(Havers.emm.c, type = "response")
#Substance*Age
Havers.emm.s_a <- emmeans(Havers, ~ Substance*Age)
#run Z-tests no adjustment for familywise error rate
pairs(Havers.emm.s_a, adjust = "None")
Havers.emm.s_a
#Visualizing the differences
plot(Havers.emm.s_a)

#Age*Condition
Havers.emm.s_c <- emmeans(Havers, ~ Substance*Condition)
#run Z-tests no adjustment for familywise error rate
pairs(Havers.emm.s_c, adjust = "None")
Havers.emm.s_c
#Visualizing the differences
plot(Havers.emm.s_c)

#Substance*Age*Condition
Havers.emm.s_a_c <- emmeans(Havers, ~ Substance*Age*Condition)
#run Z-tests no adjustment for familywise error rate
pairs(Havers.emm.s_a_c, adjust = "None")
Havers.emm.s_a_c
#Visualizing the differences
plot(Havers.emm.s_a_c)


### Water Aversives GLMER (EtOH Group Only: Total EtOH Consumed) ######

HaversTot <-glmer(Total.Aversive ~ Substance*Age*c.totale 
         + (1|RatID), data = data$h2o$no.ctrl, family = poisson)

# Model did not converge, used code below to extend # of iterations and start from where the previous model left off.
ss8 <- getME(HaversTot,c("theta","fixef"))
HaversTot <- update(HaversTot,start = ss8,control = glmerControl(optCtrl = list(maxfun = 2e6)))
summary(HaversTot)

#Post Hocs & Planned Contrasts

#Substance*Age
HaversTot.emm.s_a <- emmeans(HaversTot, ~ Substance*Age)
#run Z-tests no adjustment for familywise error rate
pairs(HaversTot.emm.s_a, adjust = "None")
HaversTot.emm.s_a
#Visualizing the differences
plot(HaversTot.emm.s_a)

#Substance*c.totale
HaversTot.emt.s_c <- emtrends(HaversTot, ~ Substance, var = "c.totale")
#run Z-tests no adjustment for familywise error rate
pairs(HaversTot.emt.s_c, adjust = "None")
HaversTot.emt.s_c
#Visualizing the differences
plot(HaversTot.emt.s_c)

#Substance*Age*c.totale
HaversTot.emt.s_a_c <- emtrends(HaversTot, ~ Substance*Age, var = "c.totale")
#run Z-tests no adjustment for familywise error rate
pairs(HaversTot.emt.s_a_c, adjust = "None")
#pairs looks alot like Substance*Age but it is different, it is the comparisons of slopes
HaversTot.emt.s_a_c
#Visualizing the differences
plot(HaversTot.emt.s_a_c)


### Water Aversives GLMER (EtOH Group Only: MAC & ROC) ######

HaversMR <-glmer(Total.Aversive ~ Substance+Age+c.MAC+c.ROC
         + Substance:Age
         + Substance:c.MAC
         + Substance:c.ROC
         + Age:c.MAC
         + Age:c.ROC
         + Substance:Age:c.MAC
         + Substance:Age:c.ROC
         + (1|RatID), data = data$h2o$no.ctrl, family = poisson)

# Model did not converge, used code below to extend # of iterations and start from where the previous model left off.
ss9 <- getME(HaversMR,c("theta","fixef"))
HaversMR <- update(HaversMR,start = ss9,control = glmerControl(optCtrl = list(maxfun = 2e9)))
summary(HaversMR)
vif(HaversMR)
AIC(HaversMR, HaversTot)
BIC(HaversMR, HaversTot)


#Make and save residual distribution plot
#make PNG file
png("HaversMR Residual Probability Density.png", width = 300, height = 300)
#plot residual density function
plot(density(residuals(HaversMR)), 
   main = "", xlab = "", frame = FALSE)
#Add normal distribution to the residual plot for comparison to check assumption of normality
HaversMR.res = residuals(HaversMR)
HavMR.m = mean(HaversMR.res)
HavMR.std = sqrt(var(HaversMR.res))
curve(dnorm(x, mean = HavMR.m, sd = HavMR.std), col = "darkblue", lwd = 2, add = TRUE, yaxt = "n")
#close the file
dev.off()


## WATER HEDONICS #####

### Water Hedonics GLMER (with EtOH vs CTRL)####
Hhed <-glmer(Total.Hedonic...MM. ~ Substance*Age*Condition 
        + (1|RatID), data = data$h2o$ctrl, family = poisson)
summary(Hhed)

#Post Hocs & Planned Contrasts

#Substance*Condition
Hhed.emm.s_c <- emmeans(Hhed, ~ Substance*Condition)
#run Z-tests no adjustment for familywise error rate
pairs(Hhed.emm.s_c, adjust = "None")
Hhed.emm.s_c
#Visualizing the differences
plot(Hhed.emm.s_c)


### Water Hedonics GLMER (EtOH Group Only: Total EtOH Consumed) ######

HhedTot <-glmer(Total.Hedonic...MM. ~ Substance*Age*c.totale 
         + (1|RatID), data = data$h2o$no.ctrl, family = poisson)
summary(HhedTot)

#Post Hocs & Planned Contrasts
#Substance*c.totale
HhedTot.emt.s_c <- emtrends(HhedTot, ~ Substance, var = "c.totale")
#run Z-tests no adjustment for familywise error rate
pairs(HhedTot.emt.s_c, adjust = "None")
HhedTot.emt.s_c
#Visualizing the differences
plot(HhedTot.emt.s_c)

#Substance*Age*c.totale
HhedTot.emt.s_a_c <- emtrends(HhedTot, ~ Substance*Age, var = "c.totale")
#run Z-tests no adjustment for familywise error rate
pairs(HhedTot.emt.s_a_c, adjust = "None")
#pairs looks alot like Substance*Age but it is different, it is the comparisons of slopes
HhedTot.emt.s_a_c
#Visualizing the differences
plot(HhedTot.emt.s_a_c)