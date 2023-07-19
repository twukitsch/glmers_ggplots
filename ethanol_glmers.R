# INITIAL OBJECT SETUP

  # Add lists for storage
  models <- list()
  compars <- list()

# ETHANOL AVERSIVES ANALYSES####

# Add Divider
cat("################################################################################\n
############################ ETHANOL ANALYSES START ############################\n
################################################################################\n")

  ## Ethanol Aversives GLMER (with EtOH vs CTRL)####
    models$eth$avers$overall <- glmer(Total.Aversive ~ c.conc*Age*Condition # predictors: full factorial fixed effects of centered concentration, age, and condition
                                      + (c.conc|RatID), # and the random effects of the intercept (RatID) and the slope of concentration
                                      data = ABHVdata$eth$ctrl,
                                      family = poisson)
    # Get a summary of our output
    summary(models$eth$avers$overall)
 
    # Post Hocs & Planned Contrasts
    # Checking to see which group (Adolescent vs Adult) had the higher mean and by how much 
      compars$eth$avers$overall$condition <- emmeans(models$eth$avers$overall, ~ Condition)
      summary(compars$eth$avers$overall$condition, type = "response") # Gives the summary transformed back to the outcome varaible's units

      
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
      
      
  ## Ethanol Aversives GLMER (EtOH Group Only: Total EtOH Consumed) ####
    models$eth$avers$total.e <- glmer(Total.Aversive ~ c.conc*Age*c.totale 
                                      + (c.conc|RatID),
                                      data = ABHVdata$eth$no.ctrl,
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

      
  ## Ethanol Aversives GLMER (EtOH Group Only: MAC & ROC) ####
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


  ## Ethanol Aversives GLMER (EtOH Group Only: MAC3 & ROC3) ######
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
  
    # Variance inflation check & model comparison
      vif(models$eth$avers$MR3)
      AIC(models$eth$avers$MR3, models$eth$avers$MR1, models$eth$avers$total.e)
      BIC(models$eth$avers$MR3, models$eth$avers$MR1, models$eth$avers$total.e)
  
    # Make and save residual distribution plot
      # make PNG file
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



# ETHANOL HEDONIC ANALYSES ####

  ## Ethanol Hedonics GLMER (with EtOH vs CTRL)####
    models$eth$hedon$overall <- glmer(Total.Hedonic...MM. ~ c.conc*Age*Condition
                                      + (c.conc|RatID),
                                      data = ABHVdata$eth$ctrl,
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
    
    # Make and save residual distribution plot
      #make PNG file
      png("Ehed Residual Probability Density.png", width = 300, height = 300)
      #plot residual density function
      plot(density(residuals(models$eth$hedon$overall)), 
           main = "",
           xlab = "",
           frame = FALSE)
      # Add normal distribution to the residual plot for comparison to check assumption of normality
      MEAN = mean(residuals(models$eth$hedon$overall))
      STDEV = sqrt(var(residuals(models$eth$hedon$overall)))
      curve(dnorm(x, mean = MEAN, sd = STDEV),
            col = "darkblue",
            lwd = 2,
            add = TRUE,
            yaxt = "n")
      remove(MEAN, STDEV)
      #c lose the file
      dev.off()


  ## Ethanol Hedonics GLMER (EtOH Group Only: Total EtOH Consumed) ######
    models$eth$hedon$total.e <- glmer(Total.Hedonic...MM. ~ c.conc*Age*c.totale 
                                      + (c.conc|RatID),
                                      data = ABHVdata$eth$no.ctrl,
                                      family = poisson)
  
    summary(models$eth$hedon$total.e)
 
    # Post Hocs and Planned Contrasts
      compars$eth$hedon$total.e <- list() # Create new comparison list for overall model
      compars$eth$hedon$total.e$age <- emmeans(models$eth$hedon$total.e, ~ Age) # Age post hoc comparison
      summary(compars$eth$hedon$total.e$age, type = "response")
      
    # Make and save residual distribution plot
      #make PNG file
      png("EhedTot Residual Probability Density.png", width = 300, height = 300)
      # plot residual density function
      plot(density(residuals(models$eth$hedon$total.e)), 
           main = "",
           xlab = "",
           frame = FALSE)
      # Add normal distribution to the residual plot for comparison to check assumption of normality
      MEAN = mean(residuals(models$eth$hedon$total.e))
      STDEV = sqrt(var(residuals(models$eth$hedon$total.e)))
      curve(dnorm(x, mean = MEAN, sd = STDEV),
            col = "darkblue",
            lwd = 2,
            add = TRUE,
            yaxt = "n")
      remove(MEAN, STDEV)
      # close the file
      dev.off()
 
  ## Ethanol Hedonics GLMER (EtOH Group Only: MAC & ROC) ######
    models$eth$hedon$MR1 <- glmer(Total.Hedonic...MM. ~ c.conc+Age+c.MAC+c.ROC
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
    
    summary(models$eth$hedon$MR1)
 
    # Variance inflation check & model comparison
      vif(models$eth$hedon$MR1)
      AIC(models$eth$hedon$MR1, models$eth$hedon$total.e)
      BIC(models$eth$hedon$MR1, models$eth$hedon$total.e)
   
  
    # Make and save residual distribution plot
      # make PNG file
      png("EhedMR Residual Probability Density.png", width = 300, height = 300)
      # plot residual density function
      plot(density(residuals(models$eth$hedon$MR1)), 
           main = "",
           xlab = "",
           frame = FALSE)
      # Add normal distribution to the residual plot for comparison to check assumption of normality
      MEAN = mean(residuals(models$eth$hedon$MR1))
      STDEV = sqrt(var(residuals(models$eth$hedon$MR1)))
      curve(dnorm(x, mean = MEAN, sd = STDEV),
            col = "darkblue",
            lwd = 2,
            add = TRUE,
            yaxt = "n")
      remove(MEAN, STDEV)
      # close the file
      dev.off()
 
 
  ## Ethanol Hedonics GLMER (EtOH Group Only: MAC3 & ROC3) ######
    models$eth$hedon$MR3 <- glmer(Total.Hedonic...MM. ~ c.conc+Age+c.MAC3+c.ROC3
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
 
 # MR1 & MR3 Models no longer appear after this point: Similar AICs, with BICs favoring the less complex Total Ethanol model. 
 # I use the less complex Total Ethanol model for interpretation from here on out.
      
# Save the workspace
  save.image("ABHV_workspace.RData")
  
# Add Divider
  cat("################################################################################\n
############################# ETHANOL ANALYSES END #############################\n
################################################################################\n")
