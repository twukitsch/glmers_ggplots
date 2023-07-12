## WATER AVERSIVES ANALYSES####

  ### Water Aversives GLMER (with EtOH vs CTRL)####
    models$h2o$avers$overall <- glmer(Total.Aversive ~ Substance*Age*Condition 
                                      + (1|RatID), # Intercept Only
                                      data = data$h2o$ctrl,
                                      family = poisson)

    summary(models$h2o$avers$overall)
    
    # Post Hocs & Planned Concrasts
      compars$h2o$avers$overall <- list()
      
      # Get means of Substance
        compars$h2o$avers$overall$subst <- emmeans(models$h2o$avers$overall,
                                                   ~ Substance)
        summary(compars$h2o$avers$overall$subst, type = "response")
      # Get means of Condition
        compars$h2o$avers$overall$cond <- emmeans(models$h2o$avers$overall,
                                                  ~ Condition)
        summary(compars$h2o$avers$overall$cond, type = "response")
      # Get means of Substance*Age interaction
        compars$h2o$avers$overall$subst.x.age <- emmeans(models$h2o$avers$overall,
                                                         ~ Substance
                                                         * Age)
        summary(compars$h2o$avers$overall$subst.x.age, type = "response")
      # Get means of Substance*Condition interaction
        compars$h2o$avers$overall$subst.x.cond <- emmeans(models$h2o$avers$overall,
                                                   ~ Substance
                                                   * Condition)
        summary(compars$h2o$avers$overall$subst.x.cond, type = "response")
      # Get means of Substance*Age*Condition interaction
        compars$h2o$avers$overall$threeway <- emmeans(models$h2o$avers$overall,
                                                      ~ Substance
                                                      * Age
                                                      * Condition)
        summary(compars$h2o$avers$overall$threeway, type = "response")
        # Get pairwise comparisons with no familywise adjustment
          compars$h2o$avers$overall$pairs3way <- pairs(compars$h2o$avers$overall$threeway, 
                                           adjust = "None") # "tukey" will make tukey adjustment, same for "bonferroni"
          compars$h2o$avers$overall$pairs3way
    
    # Make and save residual distribution plot
      # make PNG file
      png("Wavers Residual Probability Density.png", width = 300, height = 300)
      # plot residual density function
      plot(density(residuals(models$h2o$avers$overall)), 
           main = "",
           xlab = "", 
           frame = FALSE)
      # Add normal distribution to the residual plot for comparison to check assumption of normality
      MEAN = mean(residuals(models$h2o$avers$overall))
      STDEV = sqrt(var(residuals(models$h2o$avers$overall)))
      curve(dnorm(x, mean = MEAN, sd = STDEV),
            col = "darkblue",
            lwd = 2,
            add = TRUE,
            yaxt = "n")
      remove(MEAN, STDEV) # Clean workspace
      # close the file
      dev.off()
      
      
  ### Water Aversives GLMER (EtOH Group Only: Total EtOH Consumed) ####
    models$h2o$avers$total.e <- glmer(Total.Aversive ~ Substance*Age*c.totale 
                                      + (1|RatID), # Intercept Only
                                      data = data$h2o$no.ctrl,
                                      family = poisson)
 
    # Model did not converge, used code below to extend # of iterations and start from where the previous model left off.
      ss <- getME(models$h2o$avers$total.e, c("theta", "fixef"))
      models$h2o$avers$total.e <- update(models$h2o$avers$total.e,
                                         start = ss,
                                         control = glmerControl(optCtrl = list(maxfun = 2e6)))
      remove(ss) # Clean workspace
   
    summary(models$h2o$avers$total.e)
 
    # Post Hocs & Planned Concrasts
      compars$h2o$avers$total.e <- list() # Create new comparison list for overall model
      # Get means of Substance*Age interaction
        compars$h2o$avers$total.e$subst.x.age <- emmeans(models$h2o$avers$total.e,
                                                         ~ Substance
                                                         * Age)
        summary(compars$h2o$avers$total.e$subst.x.age, type = "response")
      # Get means of Substance*Total Ethanol Consumed interaction
        compars$h2o$avers$total.e$subst.x.totale <- emtrends(models$h2o$avers$total.e,
                                                   ~ Substance,
                                                   var = "c.totale")
        summary(compars$h2o$avers$total.e$subst.x.totale)
      # Get means of Substance*Age*Condition interaction
        compars$h2o$avers$total.e$threeway <- emtrends(models$h2o$avers$total.e,
                                                      ~ Substance
                                                      * Age,
                                                      var = "c.totale")
        summary(compars$h2o$avers$total.e$threeway)
        # Get pairwise comparisons with no familywise adjustment
          compars$h2o$avers$total.e$pairs3way <- pairs(compars$h2o$avers$total.e$threeway, 
                                           adjust = "None") # "tukey" will make tukey adjustment, same for "bonferroni"
          compars$h2o$avers$total.e$pairs3way
      
    # Make and save residual distribution plot
      # make PNG file
      png("WaversTot Residual Probability Density.png", width = 300, height = 300)
      # plot residual density function
      plot(density(residuals(models$h2o$avers$total.e)), 
           main = "",
           xlab = "", 
           frame = FALSE)
      # Add normal distribution to the residual plot for comparison to check assumption of normality
      MEAN = mean(residuals(models$h2o$avers$total.e))
      STDEV = sqrt(var(residuals(models$h2o$avers$total.e)))
      curve(dnorm(x, mean = MEAN, sd = STDEV),
            col = "darkblue",
            lwd = 2,
            add = TRUE,
            yaxt = "n")
      remove(MEAN, STDEV) # Clean workspace
      # close the file
      dev.off()


## WATER HEDONIC ANALYSES ####

  ### Water Hedonics GLMER (with EtOH vs CTRL)####
    models$h2o$hedon$overall <- glmer(Total.Hedonic...MM. ~ Substance*Age*Condition
                                      + (1|RatID),
                                      data = data$h2o$ctrl,
                                      family = poisson)
  
    summary(models$h2o$hedon$overall)
 
    # Post Hocs & Planned Contrasts
      # Get means of Substance
        compars$h2o$hedon$overall$subst <- emmeans(models$h2o$hedon$overall,
                                                   ~ Substance)
        summary(compars$h2o$hedon$overall$subst, type = "response")
      # Get means of Substance*Condition interaction
        compars$h2o$hedon$overall$subst.x.cond <- emmeans(models$h2o$hedon$overall,
                                                          ~ Substance
                                                          * Condition)
        summary(compars$h2o$hedon$overall$subst.x.cond, type = "response")
        # Get pairwise comparisons with no familywise adjustment
          compars$h2o$hedon$overall$pairs.subst.x.cond <- pairs(compars$h2o$hedon$overall$subst.x.cond, 
                                                       adjust = "None") # "tukey" will make Tukey adjustment, same for "bonferroni"
          compars$h2o$hedon$overall$pairs.subst.x.cond
    
    # Make and save residual distribution plot
      # make PNG file
      png("Hhed Residual Probability Density.png", width = 300, height = 300)
      # plot residual density function
      plot(density(residuals(models$h2o$hedon$overall)), 
           main = "",
           xlab = "",
           frame = FALSE)
      # Add normal distribution to the residual plot for comparison to check assumption of normality
      MEAN = mean(residuals(models$h2o$hedon$overall))
      STDEV = sqrt(var(residuals(models$h2o$hedon$overall)))
      curve(dnorm(x, mean = MEAN, sd = STDEV),
            col = "darkblue",
            lwd = 2,
            add = TRUE,
            yaxt = "n")
      remove(MEAN, STDEV)
      # close the file
      dev.off()


  ### Water Hedonics GLMER (EtOH Group Only: Total EtOH Consumed) ######
    models$h2o$hedon$total.e <- glmer(Total.Hedonic...MM. ~ Substance*Age*c.totale 
                                      + (1|RatID),
                                      data = data$h2o$no.ctrl,
                                      family = poisson)
  
    summary(models$h2o$hedon$total.e)
 
    # Post Hocs and Planned Contrasts
      compars$h2o$hedon$total.e <- list() # Create new comparison list for overall model
      # Get means of Substance
      compars$h2o$hedon$total.e$subst <- emmeans(models$h2o$hedon$total.e,
                                                       ~ Substance)
      summary(compars$h2o$hedon$total.e$subst, type = "response")
      # Get means of Substance*Total Ethanol Consumed interaction
      compars$h2o$hedon$total.e$subst.x.totale <- emtrends(models$h2o$hedon$total.e,
                                                           ~ Substance,
                                                           var = "c.totale")
      summary(compars$h2o$hedon$total.e$subst.x.totale)
      # Get means of Substance*Age*Condition interaction
      compars$h2o$hedon$total.e$threeway <- emtrends(models$h2o$hedon$total.e,
                                                     ~ Substance
                                                     * Age,
                                                     var = "c.totale")
      summary(compars$h2o$hedon$total.e$threeway)
      # Get pairwise comparisons with no familywise adjustment
      compars$h2o$hedon$total.e$pairs3way <- pairs(compars$h2o$hedon$total.e$threeway, 
                                                   adjust = "None") # "tukey" will make tukey adjustment, same for "bonferroni"
      compars$h2o$hedon$total.e$pairs3way
      
    # Make and save residual distribution plot
      # make PNG file
      png("HhedTot Residual Probability Density.png", width = 300, height = 300)
      # plot residual density function
      plot(density(residuals(models$h2o$hedon$total.e)), 
           main = "",
           xlab = "",
           frame = FALSE)
      #Add normal distribution to the residual plot for comparison to check assumption of normality
      MEAN = mean(residuals(models$h2o$hedon$total.e))
      STDEV = sqrt(var(residuals(models$h2o$hedon$total.e)))
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