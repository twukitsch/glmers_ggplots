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
      

# Save the workspace
save.image("ABHV_workspace.RData")