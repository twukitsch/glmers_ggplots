#

# LOAD PACKAGES ####

  # Add Divider
    cat("################################################################################\n
################################# SETUP  START #################################\n
################################################################################\n")
  
  # See if rtools is installed. It has necessary C++ compiler for installs of some of the libraries that are required.
    if (Sys.which("make") == "") {  # Check to see if "make" command from rtools is found in system's PATH. If an empty string is retuned, rtools isn't installed.
      message("Rtools is not found. Please install Rtools from https://cran.r-project.org/bin/windows/Rtools/, restart R, and rerun this code.")
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
      
      # Add Divider
      cat("################################################################################\n
############ PACKAGES DOWNLOADED & LOADED - CHECK OUTPUT FOR ERRORS ############\n
################################################################################\n")
  } # end rtools installed == TRUE


# SET WORKING DIRECTORY
setwd("C:/Users/kieri/Documents/ABHV")
getwd()


# DATA LOADING AND SETUP ####
  
# Add Divider
cat("################################################################################\n
########################## DATA LOADING & SETUP START ##########################\n
################################################################################\n")

  ## Data Object & Inherited Attributes ####
  ABHVdata <- list() # create list object to store data
  
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
        # Rescale -- to avoid issues with eigen values later
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

    
    # Add Divider
    cat("################################################################################\n
######################### DATA LOADING & SETUP  COMPLETE #########################\n
################################################################################\n")
    
    