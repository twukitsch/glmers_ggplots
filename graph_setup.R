# Graph Setup ####

  # Create list for storage
  plots <- list()

  
# Y AXIS BREAKS & LABELS ####
  
  # Aversive Y Break Positions
  plots$y.axis$avers$breaks <- seq(from = 0, to = 100, by = 25) 
  # Aversive Y Labels
  plots$y.axis$avers$labels <- as.character(plots$y.axis$avers$breaks)
    
  # Hedonic Y Break Positions
  plots$y.axis$hedon$breaks <- seq(0, 550, 50)
  # Hedonic Y Labels
  plots$y.axis$hedon$labels <- as.character(ifelse(plots$y.axis$hedon$breaks == 0 |
                                              plots$y.axis$hedon$breaks %% 100 == 0,
                                              plots$y.axis$hedon$breaks,
                                              "")) # makes every other label ""

  
# X AXIS BREAKS & LABELS ####
  
  ## Ethanol ####
  
    # X Break Positions for Concentration
    plots$eth$x.axis$overall$breaks <- sort(unique(data$eth$ctrl$Concentration))
    # X Labels for Concentration
    plots$eth$x.axis$overall$labels <- as.character(plots$eth$x.axis$overall$breaks)
    
    # X Break Positions for Total Ethanol Consumed
    plots$eth$x.axis$total.e$breaks <- seq(0, 125, 25)
    # X Labels for Total Ethanol Consumed
    plots$eth$x.axis$total.e$labels <- as.character(plots$eth$x.axis$total.e$breaks)
    
  ## Sucrose ####
    
    # X Break Positions for Molarity
    plots$suc$x.axis$overall$breaks <- sort(unique(data$suc$ctrl$molarity))
    # X Labels for Molarity
    plots$suc$x.axis$overall$labels <- as.character(plots$suc$x.axis$overall$breaks)
    
    # X Break Positions for Total Ethanol Consumed
    plots$suc$x.axis$total.e$breaks = seq(0, 125, 25)
    # X Labels for Total Ethanol Consumed
    plots$suc$x.axis$total.e$labels = as.character(plots$suc$x.axis$total.e$breaks)
    
  ## Water ####
    
    # X Break Positions for Total Ethanol Consumed
    plots$h2o$x.axis$total.e$breaks = seq(0, 125, 25)
    # X Labels for Total Ethanol Consumed
    plots$h2o$x.axis$total.e$labels = as.character(plots$h2o$x.axis$total.e$breaks)

        
# EFFECTS, CONVIDENCE INTERVALS, & NUMBER OF X-LEVELS OF LINES FOR CURVES ####
    
  ## Ethanol ####
    
    ### Aversive ####
      
      #### Overall ####
        # Mean center xlevel graphing points 
        plots$eth$x.axis$overall$xlvls <- seq(.05, .40, .025) - mean(data$eth$ctrl$recoded.conc) # the points along the curve you want to use to get good error ribbons.
        
        # Pull the effects from model & calculate confidence intervals for graphing
        plots$eth$avers$overall$fx <- Effect(c("c.conc", "Age", "Condition"),
                                        models$eth$avers$overall,
                                        se = list(level = .68), # se is std err and the level is the confidence level. .68 = actual std err for conf int. lower and upper.
                                        xlevels = list(c.conc = plots$eth$x.axis$overall$xlvls))# the xlevels command is used to increase the number of points calculated for the total ethanol (default = 5)
        
        # Convert effect object to data frame
        plots$eth$avers$overall$fx <- as.data.frame(plots$eth$avers$overall$fx)
        # Get original Concentration values (un-center the variable)
        plots$eth$avers$overall$fx$Concentration <- (plots$eth$avers$overall$fx$c.conc + mean(data$eth$ctrl$recoded.conc)) * 100
        plots$eth$avers$overall$fx
      
      #### Total Ethanol ####
        # Mean center xlevel graphing points 
        plots$eth$x.axis$total.e$xlvls <- c(.05, .20, .40) - mean(data$eth$no.ctrl$recoded.conc) # the points along the curve you want to use to get good error ribbons.
        
        # Pull the effects from model & calculate confidence intervals for graphing
        plots$eth$avers$total.e$fx <- Effect(c("c.conc", "Age", "c.totale"),
                                       models$eth$avers$total.e,
                                       se = list(level = .68), # se is std err and the level is the confidence level. .68 = actual std err for conf int. lower and upper.
                                       xlevels = list(c.conc = plots$eth$avers$total.e$xlvls))# the xlevels command is used to increase the number of points calculated for the total ethanol (default = 5)
        
        # Convert effect object to data frame
        plots$eth$avers$total.e$fx <- as.data.frame(plots$eth$avers$total.e$fx)
        # Get original Concentration values (un-center the variable)
        plots$eth$avers$total.e$fx$Concentration <- (plots$eth$avers$total.e$fx$c.conc + mean(data$eth$no.ctrl$recoded.conc)) * 100
        # Get original totale values (un-center the variable)
        plots$eth$avers$total.e$fx$totale <- (plots$eth$avers$total.e$fx$c.totale + mean(data$eth$no.ctrl$TOTAL.ETOH.Swap.Consumed..g.kg.))
        plots$eth$avers$total.e$fx
        
    ### Hedonic ####
        
      #### Overall ####

        # Pull the effects from model & calculate confidence intervals for graphing
        plots$eth$hedon$overall$fx <- Effect(c("c.conc", "Age", "Condition"),
                                        models$eth$hedon$overall,
                                        se = list(level = .68), # se is std err and the level is the confidence level. .68 = actual std err for conf int. lower and upper.
                                        xlevels = list(c.conc = plots$eth$x.axis$overall$xlvls))# the xlevels command is used to increase the number of points calculated for the total ethanol (default = 5)
        
        # Convert effect object to data frame
        plots$eth$hedon$overall$fx <- as.data.frame(plots$eth$hedon$overall$fx)
        # Get original Concentration values (un-center the variable)
        plots$eth$hedon$overall$fx$Concentration <- (plots$eth$hedon$overall$fx$c.conc + mean(data$eth$ctrl$recoded.conc)) * 100
        plots$eth$hedon$overall$fx
      
      #### Total Ethanol ####

        # Pull the effects from model & calculate confidence intervals for graphing
        plots$eth$hedon$total.e$fx <- Effect(c("c.conc", "Age", "c.totale"),
                                       models$eth$hedon$total.e,
                                       se = list(level = .68), # se is std err and the level is the confidence level. .68 = actual std err for conf int. lower and upper.
                                       xlevels = list(c.conc = plots$eth$x.axis$total.e$xlvls))# the xlevels command is used to increase the number of points calculated for the total ethanol (default = 5)
        
        # Convert effect object to data frame
        plots$eth$hedon$total.e$fx <- as.data.frame(plots$eth$hedon$total.e$fx)
        # Get original Concentration values (un-center the variable)
        plots$eth$hedon$total.e$fx$Concentration <- (plots$eth$hedon$total.e$fx$c.conc + mean(data$eth$no.ctrl$recoded.conc)) * 100
        # Get original totale values (un-center the variable)
        plots$eth$hedon$total.e$fx$totale <- (plots$eth$hedon$total.e$fx$c.totale + mean(data$eth$no.ctrl$TOTAL.ETOH.Swap.Consumed..g.kg.))
        plots$eth$hedon$total.e$fx
        
  ## Sucrose ####
    
    ### Aversive ####
      
      #### Overall ####
        # Mean center xlevel graphing points 
        plots$suc$x.axis$overall$xlvls <- c(seq(.01, .09, .01) - mean(data$suc$ctrl$molarity), 
                                           seq(.1, 1, .1) - mean(data$suc$ctrl$molarity)) # the points along the curve you want to use to get good error ribbons.
        
        # Pull the effects from model & calculate confidence intervals for graphing
        plots$suc$avers$overall$fx <- Effect(c("c.molarity", "Age", "Condition"),
                                        models$suc$avers$overall,
                                        se=list(level=.68), # se is std err and the level is the confidence level. .68 = actual std err for conf int. lower and upper.
                                        xlevels=list(c.molarity = plots$suc$x.axis$overall$xlvls))# the xlevels command is used to increase the number of points calculated for the total ethanol (default = 5)
        
        # Convert effect object to data frame
        plots$suc$avers$overall$fx <- as.data.frame(plots$suc$avers$overall$fx)
        # Get original Concentration values (un-center the variable)
        plots$suc$avers$overall$fx$molarity <- (plots$suc$avers$overall$fx$c.molarity + mean(data$suc$ctrl$molarity))

      
      #### Total Ethanol ####
        # Mean center xlevel graphing points 
        plots$suc$x.axis$total.e$xlvls <- c(.05, .20, .40) - mean(data$suc$no.ctrl$molarity) # the points along the curve you want to use to get good error ribbons.
        
        # Pull the effects from model & calculate confidence intervals for graphing
        plots$suc$avers$total.e$fx <- Effect(c("c.molarity", "Age", "c.totale"),
                                       models$suc$avers$total.e,
                                       se=list(level=.68), # se is std err and the level is the confidence level. .68 = actual std err for conf int. lower and upper.
                                       xlevels=list(c.molarity = plots$suc$x.axis$total.e$xlvls))# the xlevels command is used to increase the number of points calculated for the total ethanol (default = 5)
        
        # Convert effect object to data frame
        plots$suc$avers$total.e$fx <- as.data.frame(plots$suc$avers$total.e$fx)
        # Get original Concentration values (un-center the variable)
        plots$suc$avers$total.e$fx$molarity <- (plots$suc$avers$total.e$fx$c.molarity + mean(data$suc$no.ctrl$molarity))
        # Get original totale values (un-center the variable)
        plots$suc$avers$total.e$fx$totale <- (plots$suc$avers$total.e$fx$c.totale + mean(data$suc$no.ctrl$TOTAL.ETOH.Swap.Consumed..g.kg.))
        plots$suc$avers$total.e$fx
        
    ### Hedonic ####
        
      #### Overall ####

        # Pull the effects from model & calculate confidence intervals for graphing
        plots$suc$hedon$overall$fx <- Effect(c("c.molarity", "Age", "Condition"),
                                        models$suc$hedon$overall,
                                        se=list(level=.68), # se is std err and the level is the confidence level. .68 = actual std err for conf int. lower and upper.
                                        xlevels=list(c.molarity = plots$suc$x.axis$overall$xlvls))# the xlevels command is used to increase the number of points calculated for the total ethanol (default = 5)
        
        # Convert effect object to data frame
        plots$suc$hedon$overall$fx <- as.data.frame(plots$suc$hedon$overall$fx)
        # Get original Concentration values (un-center the variable)
        plots$suc$hedon$overall$fx$molarity <- (plots$suc$hedon$overall$fx$c.molarity + mean(data$suc$ctrl$molarity))
        plots$suc$hedon$overall$fx
      
      #### Total Ethanol ####

        # Pull the effects from model & calculate confidence intervals for graphing
        plots$suc$hedon$total.e$fx <- Effect(c("c.molarity", "Age", "c.totale"),
                                       models$suc$hedon$total.e,
                                       se=list(level=.68), # se is std err and the level is the confidence level. .68 = actual std err for conf int. lower and upper.
                                       xlevels=list(c.molarity = plots$suc$x.axis$total.e$xlvls))# the xlevels command is used to increase the number of points calculated for the total ethanol (default = 5)
        
        # Convert effect object to data frame
        plots$suc$hedon$total.e$fx <- as.data.frame(plots$suc$hedon$total.e$fx)
        # Get original Concentration values (un-center the variable)
        plots$suc$hedon$total.e$fx$molarity <- (plots$suc$hedon$total.e$fx$c.molarity + mean(data$suc$no.ctrl$molarity))
        # Get original c.totale values
        plots$suc$hedon$total.e$fx$totale <- (plots$suc$hedon$total.e$fx$c.totale + mean(data$suc$no.ctrl$TOTAL.ETOH.Swap.Consumed..g.kg.))
        plots$suc$hedon$total.e$fx
        
  ## Water ####
    
    ### Aversive ####
      
      #### Overall ####

        # Pull the effects from model & calculate confidence intervals for graphing
        plots$h2o$avers$overall$fx <- Effect(c("Substance", "Age", "Condition"),
                                        models$h2o$avers$overall,
                                        se = list(level = .68)) # se is std err and the level is the confidence level. .68 = actual std err for conf int. lower and upper.

        # Convert effect object to data frame
        plots$h2o$avers$overall$fx <- as.data.frame(plots$h2o$avers$overall$fx)
        # Add trial values
        plots$h2o$avers$overall$fx$trial <- rep(c(1, 2), 4)
        plots$h2o$avers$overall$fx
      
      #### Total Ethanol ####

        # Pull the effects from model & calculate confidence intervals for graphing
        plots$h2o$avers$total.e$fx <- Effect(c("Substance", "Age", "c.totale"),
                                       models$h2o$avers$total.e,
                                       se = list(level =.68), # se is std err and the level is the confidence level. .68 = actual std err for conf int. lower and upper.
                                       xlevels = list(c.totale = 20))# the xlevels command is used to increase the number of points calculated for total ethanol (default = 5)
        
        # Convert effect object to data frame
        plots$h2o$avers$total.e$fx <- as.data.frame(plots$h2o$avers$total.e$fx)
        # Get original totale values (un-center the variable)
        plots$h2o$avers$total.e$fx$totale <- (plots$h2o$avers$total.e$fx$c.totale + mean(data$h2o$no.ctrl$TOTAL.ETOH.Swap.Consumed..g.kg.))
        plots$h2o$avers$total.e$fx
        
    ### Hedonic ####
        
            #### Overall ####

        # Pull the effects from model & calculate confidence intervals for graphing
        plots$h2o$hedon$overall$fx <- Effect(c("Substance", "Age", "Condition"),
                                        models$h2o$hedon$overall,
                                        se = list(level = .68)) # se is std err and the level is the confidence level. .68 = actual std err for conf int. lower and upper.

        # Convert effect object to data frame
        plots$h2o$hedon$overall$fx <- as.data.frame(plots$h2o$hedon$overall$fx)
        # Add trial values
        plots$h2o$hedon$overall$fx$trial <- rep(c(1, 2), 4)
        plots$h2o$hedon$overall$fx
      
      #### Total Ethanol ####

        # Pull the effects from model & calculate confidence intervals for graphing
        plots$h2o$hedon$total.e$fx <- Effect(c("Substance", "Age", "c.totale"),
                                       models$h2o$hedon$total.e,
                                       se = list(level =.68), # se is std err and the level is the confidence level. .68 = actual std err for conf int. lower and upper.
                                       xlevels = list(c.totale = 20))# the xlevels command is used to increase the number of points calculated for total ethanol (default = 5)
        
        # Convert effect object to data frame
        plots$h2o$hedon$total.e$fx <- as.data.frame(plots$h2o$hedon$total.e$fx)
        # Get original totale values (un-center the variable)
        plots$h2o$hedon$total.e$fx$totale <- (plots$h2o$hedon$total.e$fx$c.totale + mean(data$h2o$no.ctrl$TOTAL.ETOH.Swap.Consumed..g.kg.))
        plots$h2o$hedon$total.e$fx


# Save the workspace
save.image("ABHV_workspace.RData")