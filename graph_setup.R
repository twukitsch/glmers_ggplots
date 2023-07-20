# Graph Setup ####

  # Create list for storage
  plots <- list()

# EFFECTS, CONVIDENCE INTERVALS, & NUMBER OF X-LEVELS OF LINES FOR CURVES ####
    
  ## Ethanol ####
    
    ### Aversive ####
      
      #### Overall ####
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
        plots$eth$avers$overall$fx
      
      #### Total Ethanol ####
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
        plots$eth$hedon$overall$fx$Concentration <- (plots$eth$hedon$overall$fx$c.conc + mean(ABHVdata$eth$ctrl$recoded.conc)) * 100
        plots$eth$hedon$overall$fx
      
      #### Total Ethanol ####

        # Pull the effects from model & calculate confidence intervals for graphing
        plots$eth$hedon$total.e$fx <- Effect(c("c.conc", "Age", "c.totale"),
                                       models$eth$hedon$total.e,
                                       se = list(level = .68), # se is std err and the level is the confidence level. .68 = actual std err for conf int. lower and upper.
                                       xlevels = list(c.totale = 20, c.conc = plots$eth$x.axis$total.e$xlvls))# the xlevels command is used to increase the number of points calculated for the total ethanol (default = 5)
        
        # Convert effect object to data frame
        plots$eth$hedon$total.e$fx <- as.data.frame(plots$eth$hedon$total.e$fx)
        # Get original Concentration values (un-center the variable)
        plots$eth$hedon$total.e$fx$Concentration <- (plots$eth$hedon$total.e$fx$c.conc + mean(ABHVdata$eth$no.ctrl$recoded.conc)) * 100
        # Get original totale values (un-center the variable)
        plots$eth$hedon$total.e$fx$totale <- (plots$eth$hedon$total.e$fx$c.totale + mean(ABHVdata$eth$no.ctrl$TOTAL.ETOH.Swap.Consumed..g.kg.))
        plots$eth$hedon$total.e$fx
        
  ## Sucrose ####
    
    ### Aversive ####
      
      #### Overall ####
        # Mean center xlevel graphing points 
        plots$suc$x.axis$overall$xlvls <- c(seq(.01, .09, .01) - mean(ABHVdata$suc$ctrl$molarity), 
                                           seq(.1, 1, .1) - mean(ABHVdata$suc$ctrl$molarity)) # the points along the curve you want to use to get good error ribbons.
        
        # Pull the effects from model & calculate confidence intervals for graphing
        plots$suc$avers$overall$fx <- Effect(c("c.molarity", "Age", "Condition"),
                                        models$suc$avers$overall,
                                        se=list(level=.68), # se is std err and the level is the confidence level. .68 = actual std err for conf int. lower and upper.
                                        xlevels=list(c.molarity = plots$suc$x.axis$overall$xlvls))# the xlevels command is used to increase the number of points calculated for the total ethanol (default = 5)
        
        # Convert effect object to data frame
        plots$suc$avers$overall$fx <- as.data.frame(plots$suc$avers$overall$fx)
        # Get original Concentration values (un-center the variable)
        plots$suc$avers$overall$fx$molarity <- (plots$suc$avers$overall$fx$c.molarity + mean(ABHVdata$suc$ctrl$molarity))

      
      #### Total Ethanol ####
        # Mean center xlevel graphing points 
        plots$suc$x.axis$total.e$xlvls <- c(.01, .1, 1) - mean(ABHVdata$suc$no.ctrl$molarity) # the points along the curve you want to use to get good error ribbons.
        
        # Pull the effects from model & calculate confidence intervals for graphing
        plots$suc$avers$total.e$fx <- Effect(c("c.molarity", "Age", "c.totale"),
                                       models$suc$avers$total.e,
                                       se=list(level=.68), # se is std err and the level is the confidence level. .68 = actual std err for conf int. lower and upper.
                                       xlevels=list(c.totale = 20, c.molarity = plots$suc$x.axis$total.e$xlvls))# the xlevels command is used to increase the number of points calculated for the total ethanol (default = 5)
        
        # Convert effect object to data frame
        plots$suc$avers$total.e$fx <- as.data.frame(plots$suc$avers$total.e$fx)
        # Get original Concentration values (un-center the variable)
        plots$suc$avers$total.e$fx$molarity <- (plots$suc$avers$total.e$fx$c.molarity + mean(ABHVdata$suc$no.ctrl$molarity))
        # Get original totale values (un-center the variable)
        plots$suc$avers$total.e$fx$totale <- (plots$suc$avers$total.e$fx$c.totale + mean(ABHVdata$suc$no.ctrl$TOTAL.ETOH.Swap.Consumed..g.kg.))
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
        plots$suc$hedon$overall$fx$molarity <- (plots$suc$hedon$overall$fx$c.molarity + mean(ABHVdata$suc$ctrl$molarity))
        plots$suc$hedon$overall$fx
      
      #### Total Ethanol ####

        # Pull the effects from model & calculate confidence intervals for graphing
        plots$suc$hedon$total.e$fx <- Effect(c("c.molarity", "Age", "c.totale"),
                                       models$suc$hedon$total.e,
                                       se=list(level=.68), # se is std err and the level is the confidence level. .68 = actual std err for conf int. lower and upper.
                                       xlevels=list(c.totale = 20, c.molarity = plots$suc$x.axis$total.e$xlvls))# the xlevels command is used to increase the number of points calculated for the total ethanol (default = 5)
        
        # Convert effect object to data frame
        plots$suc$hedon$total.e$fx <- as.data.frame(plots$suc$hedon$total.e$fx)
        # Get original Concentration values (un-center the variable)
        plots$suc$hedon$total.e$fx$molarity <- (plots$suc$hedon$total.e$fx$c.molarity + mean(ABHVdata$suc$no.ctrl$molarity))
        # Get original c.totale values
        plots$suc$hedon$total.e$fx$totale <- (plots$suc$hedon$total.e$fx$c.totale + mean(ABHVdata$suc$no.ctrl$TOTAL.ETOH.Swap.Consumed..g.kg.))
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
        plots$h2o$avers$overall$fx$trial <- rep(c("Trial 1", "Trial 2"), 4)
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
        plots$h2o$avers$total.e$fx$totale <- (plots$h2o$avers$total.e$fx$c.totale + mean(ABHVdata$h2o$no.ctrl$TOTAL.ETOH.Swap.Consumed..g.kg.))
        # Add trial values
        plots$h2o$avers$total.e$fx$trial <- rep(c("Trial 1", "Trial 2"), 40)
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
        plots$h2o$hedon$overall$fx$trial <- rep(c("Trial 1", "Trial 2"), 4)
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
        plots$h2o$hedon$total.e$fx$totale <- (plots$h2o$hedon$total.e$fx$c.totale + mean(ABHVdata$h2o$no.ctrl$TOTAL.ETOH.Swap.Consumed..g.kg.))
        # Add trial values
        plots$h2o$hedon$total.e$fx$trial <- rep(c("Trial 1", "Trial 2"), 40)
        plots$h2o$hedon$total.e$fx
  

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