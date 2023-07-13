#

# LOAD PACKAGES ####

  # Add Divider
    cat("################################################################################\n
############################### SETUP  START ###################################\n
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
    
      # Add Divider
      cat("################################################################################\n
########## PACKAGES DOWNLOADED & LOADED - CHECK OUTPUT FOR ERRORS ##############\n
################################################################################\n")
  } # end rtools installed == TRUE


# SET WORKING DIRECTORY
setwd("C:/Users/kieri/Documents/ABHV")
getwd()


# DATA LOADING AND SETUP ####
  
  ## Data Object & Inherited Attributes ####
  data <- list() # create list object to store data
  
    # Read in data file 
    data$raw <- read.csv("C:/Users/Kieri/Documents/ABHV/ABHV2018.csv",
                         na.strings="\"\"", #with blanks/ N/As set to blank ("\"\"")
                         stringsAsFactors = TRUE) # categorical/character data as "factor"
    data$raw$RatID <- as.factor(data$raw$RatID) # Convert RatID to factor type because it is not a numeric variable although it seems like one.
      
    # Change to Contrast Sum Coding (sum-to-zero). 
      # This attribute will be inherited for all data frames subset from this one,
      # so it is a good idea to perform this step now, instead of many times later.
    
      # adjust Age to contrast coding
      contrasts(data$raw$Age)=contr.sum(2)
      contrasts(data$raw$Age)
      # adjust Condition to contrast coding
      contrasts(data$raw$Condition)=contr.sum(2)
      contrasts(data$raw$Condition)
        
  ## Subset Ethanol Data ####
      
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
    
  ## Subset Sucrose Data ####
