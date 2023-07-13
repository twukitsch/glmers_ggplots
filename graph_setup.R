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
  # (!) Fiddle with labels and renames to make this simpler to understand.