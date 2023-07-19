# ETHANOL GRAPHS

  # AVERSIVE ####
    
    ## Overall ####
  
    # Start ggplot
    plots$eth$avers$overall$ggp <- ggplot(plots$eth$avers$overall$fx,
                                          aes(Concentration, fit,
                                          group = interaction(Age, Condition),
                                          col = interaction(Age, Condition),
                                          fill = interaction(Age, Condition),
                                          linetype = interaction(Age, Condition),
                                          shape = interaction(Age, Condition)))+
      
      # Add Raw Data to graph
      geom_point(data = ABHVdata$eth$ctrl, 
                 aes(x = Concentration, y = Total.Aversive),
                 size = plots$global$pt.size,
                 alpha = plots$global$pt.alpha)+
      
      # Add Prediction Line to graph
      geom_smooth(data = plots$eth$avers$overall$fx,
                  se = FALSE, # Don't show Std. Error, we have error ribbons for that
                  method = "glm", # Generalized linear model
                  method.args = list(family = "poisson"), # Poisson link function
                  linewidth = plots$global$linewidth)+ # Width of trend lines
      
      # Add Error Ribbon to Graph
      geom_ribbon(data = plots$eth$avers$overall$fx,
                  aes(ymin = lower, ymax = upper),
                  colour = plots$global$rib.edge.color, # colour = NA suppresses edges of the ribbon
                  alpha = plots$global$rib.alpha)+ # Opacity at 25% (75% Transparent)
      
      labs(tag = "A.")+ # Add image tag for multipanel final image
      
      scale_color_manual("", 
                         values = plots$global$overall$colors, # Line and Point color
                         labels = plots$global$overall$labels)+ # Labels for legend
      
      scale_fill_manual("",
                        values = plots$global$overall$rib.color, # Ribbon color
                        labels = plots$global$overall$labels)+ # Legend labels

      scale_linetype_manual("",
                            values = plots$global$overall$line.types, # Types of lines
                            labels = plots$global$overall$labels)+ # Legend labels
                            
      scale_shape_manual("",
                         values = plots$global$overall$pt.shapes, # Shapes of points
                         labels = plots$global$overall$labels)+ # Legend labels
                         
      scale_x_continuous(expand = c(0, 0),
                         limits = plots$eth$x.axis$overall$lims, # Axis length limits
                         breaks = plots$eth$x.axis$overall$breaks, # Axis break positions
                         labels = plots$eth$x.axis$overall$labels)+ # Axis value labels
                         
      scale_y_continuous(expand = c(0, 0),
                         limits = plots$y.axis$avers$lims, # Axis length limits
                         breaks = plots$y.axis$avers$breaks, # Axis break positions
                         labels = plots$y.axis$avers$labels)+ # Axis value labels
                         
      theme_classic()+
      theme(# Text
            strip.text.x = plots$global$strip.text.x, # Modify label text for facets
            axis.title = plots$global$axis.title, # Modify axis title text
            axis.text = plots$global$axis.text, # Modify axis value text
            legend.title = plots$global$legend.title, # Modify legend title text (!) Probably Uneccessary
            legend.text = plots$global$legend.text, # Modify legend value text
            plot.tag = plots$global$plot.tag, # Modify the plot tag (e.g. "A.") text
            # Positioning
            axis.title.x = plots$global$axis.title.x, # Modify X axis appearance
            axis.title.y = plots$global$axis.title.y, # Modify Y axis appearance
            plot.tag.position = plots$global$overall$plot.tag.position, # Modify position of plot tag
            legend.position = plots$global$overall$legend.position, # Modify position of legend
            # Other Aesthetics
            axis.line = plots$global$axis.line, # Modify axis line attributes
            axis.ticks = plots$global$axis.ticks, # Modify axis tick mark attributes
            axis.ticks.length = plots$global$axis.ticks.length, # Modify how long each tick is
            legend.key.size = plots$global$legend.key.size, # Modify the size of the legend key boxes
            strip.background = element_rect(colour="white") # Gets rid of default background
            )+
      xlab("Ethanol % (v/v)")+ # X axis label
      ylab("Aversive Responses (+/-SEM)") # Y axis label
    
    plots$eth$avers$overall$ggp
    
    # Save the plot
    ggsave("Fig 8A Ethanol Aversive Graph.png",
           plots$eth$avers$overall$ggp, 
           width = 533, 
           height = 600, 
           units = "px", 
           dpi = 81)


    ## Total Ethanol ####
    
    # Start ggplot
    plots$eth$avers$total.e$ggp <- ggplot(plots$eth$avers$total.e$fx,
                                          aes(totale,
                                              fit,
                                              group = Age,
                                              col = Age,
                                              fill = Age,
                                              shape = Age))+
      
      # Add Raw Data to graph
      geom_point(data = ABHVdata$eth$no.ctrl,
                 aes(x = TOTAL.ETOH.Swap.Consumed..g.kg.,
                     y = Total.Aversive),
                 size = plots$global$pt.size, 
                 alpha = plots$global$pt.alpha)+
      
      # Add Prediction Line to graph
      geom_smooth(data = plots$eth$avers$total.e$fx,
                  se = FALSE, # Don't show Std. Error, we have error ribbons for that
                  method = "glm", # Generalized linear model
                  method.args = list(family = "poisson"), # Poisson link function
                  linewidth = plots$global$linewidth)+ # Width of trend lines
      
      
      # Add Error Ribbon to graph
      geom_ribbon(data = plots$eth$avers$total.e$fx,
                  aes(ymin = lower, ymax = upper),
                  colour = plots$global$rib.edge.color, # colour = NA suppresses edges of the ribbon
                  alpha = plots$global$rib.alpha)+ # Opacity at 25% (75% Transparent)
      
      
      
      facet_wrap(~ as.factor(Concentration), nrow = 1)+ # Makes the plot multi-panel. One for each level of Concentration
      
      labs(tag="B.")+ # Add image tag for final image combo.
      
      scale_color_manual("", 
                         values = plots$global$total.e$colors, # Line and Point color
                         labels = plots$global$total.e$labels)+ # Labels for legend
      
      scale_fill_manual("",
                        values = plots$global$total.e$rib.color, # Ribbon color
                        labels = plots$global$total.e$labels)+ # Legend labels
      
      scale_linetype_manual("",
                            values = plots$global$total.e$line.types, # Types of lines
                            labels = plots$global$total.e$labels)+ # Legend labels
      
      scale_shape_manual("",
                         values = plots$global$total.e$pt.shapes, # Shapes of points
                         labels = plots$global$total.e$labels)+ # Legend labels
      
      scale_x_continuous(expand = c(0, 0),
                         limits = plots$eth$x.axis$total.e$lims, # Axis length limits
                         breaks = plots$eth$x.axis$total.e$breaks, # Axis break positions
                         labels = plots$eth$x.axis$total.e$labels)+ # Axis value labels
      
      scale_y_continuous(expand = c(0, 0),
                         limits = plots$y.axis$avers$lims, # Axis length limits
                         breaks = plots$y.axis$avers$breaks, # Axis break positions
                         labels = plots$y.axis$avers$labels)+ # Axis value labels
      
      theme_classic()+
      theme(# Text
        strip.text.x = plots$global$strip.text.x, # Modify label text for facets
        axis.title = plots$global$axis.title, # Modify axis title text
        axis.text = plots$global$axis.text, # Modify axis value text
        legend.title = plots$global$legend.title, # Modify legend title text (!) Probably Uneccessary
        legend.text = plots$global$legend.text, # Modify legend value text
        plot.tag = plots$global$plot.tag, # Modify the plot tag (e.g. "A.") text
        plot.title = plots$global$plot.title,
        # Positioning
        axis.title.x = plots$global$axis.title.x, # Modify X axis appearance
        axis.title.y = plots$global$axis.title.y, # Modify Y axis appearance
        plot.tag.position = plots$global$total.e$plot.tag.position, # Modify position of plot tag
        legend.position = plots$global$total.e$legend.position, # Modify position of legend
        # Other Aesthetics
        axis.line = plots$global$axis.line, # Modify axis line attributes
        axis.ticks = plots$global$axis.ticks, # Modify axis tick mark attributes
        axis.ticks.length = plots$global$axis.ticks.length, # Modify how long each tick is
        legend.key.size = plots$global$legend.key.size, # Modify the size of the legend key boxes
        strip.background = element_rect(colour="white"), # Gets rid of default background
        panel.spacing = plots$global$panel.spacing
      )+
      xlab("Total Ethanol Consumed (g/kg)")+
      ylab("Aversive Responses (+/-SEM)")+
      ggtitle("Ethanol % (v/v)")
    
    plots$eth$hedon$combo.ggp
    
    # Save the plot
    ggsave("Fig 8B Ethanol Aversive Total Ethanol Graph.png", 
           plots$eth$avers$total.e$ggp, 
           width = 1067, 
           height = 600, 
           units = "px", 
           dpi = 81)

    # Combine the two individual ggplots into one larger multipanel ggplot
    plots$eth$avers$combo.ggp <- plots$eth$avers$overall$ggp + 
      plots$eth$avers$total.e$ggp +
      plot_layout(ncol = 2, widths = c(1, 2)) # Layout 2 columns with 1/3 for first col 2/3s for second plot.
    
    plots$eth$avers$combo.ggp
    
    # Save the plot
    ggsave("Fig 8A&B Ethanol Aversive Fits.png", 
           plots$eth$avers$combo.ggp, 
           width = 1600, 
           height = 600, 
           units = "px", 
           dpi = 96)
    
    
  # HEDONIC ####
    
    ## Overall ####
    
    # Start ggplot
    plots$eth$hedon$overall$ggp <- ggplot(plots$eth$hedon$overall$fx,
                                          aes(Concentration, fit,
                                              group = interaction(Age, Condition),
                                              col = interaction(Age, Condition),
                                              fill = interaction(Age, Condition),
                                              linetype = interaction(Age, Condition),
                                              shape = interaction(Age, Condition)))+
      
      # Add Raw Data to graph
      geom_point(data = ABHVdata$eth$ctrl, 
                 aes(x = Concentration, y = Total.Hedonic...MM.),
                 size = plots$global$pt.size,
                 alpha = plots$global$pt.alpha)+
      
      # Add Prediction Line to graph
      geom_smooth(data = plots$eth$hedon$overall$fx,
                  se = FALSE, # Don't show Std. Error, we have error ribbons for that
                  method = "glm", # Generalized linear model
                  method.args = list(family = "poisson"), # Poisson link function
                  linewidth = plots$global$linewidth)+ # Width of trend lines
      
      # Add Error Ribbon to Graph
      geom_ribbon(data = plots$eth$hedon$overall$fx,
                  aes(ymin = lower, ymax = upper),
                  colour = plots$global$rib.edge.color, # colour = NA suppresses edges of the ribbon
                  alpha = plots$global$rib.alpha)+ # Opacity at 25% (75% Transparent)
      
      labs(tag = "A.")+ # Add image tag for multipanel final image
      
      scale_color_manual("", 
                         values = plots$global$overall$colors, # Line and Point color
                         labels = plots$global$overall$labels)+ # Labels for legend
      
      scale_fill_manual("",
                        values = plots$global$overall$rib.color, # Ribbon color
                        labels = plots$global$overall$labels)+ # Legend labels
      
      scale_linetype_manual("",
                            values = plots$global$overall$line.types, # Types of lines
                            labels = plots$global$overall$labels)+ # Legend labels
      
      scale_shape_manual("",
                         values = plots$global$overall$pt.shapes, # Shapes of points
                         labels = plots$global$overall$labels)+ # Legend labels
      
      scale_x_continuous(expand = c(0, 0),
                         limits = plots$eth$x.axis$overall$lims, # Axis length limits
                         breaks = plots$eth$x.axis$overall$breaks, # Axis break positions
                         labels = plots$eth$x.axis$overall$labels)+ # Axis value labels
      
      scale_y_continuous(expand = c(0, 0),
                         limits = plots$y.axis$hedon$lims, # Axis length limits
                         breaks = plots$y.axis$hedon$breaks, # Axis break positions
                         labels = plots$y.axis$hedon$labels)+ # Axis value labels
      
      theme_classic()+
      theme(# Text
        strip.text.x = plots$global$strip.text.x, # Modify label text for facets
        axis.title = plots$global$axis.title, # Modify axis title text
        axis.text = plots$global$axis.text, # Modify axis value text
        legend.title = plots$global$legend.title, # Modify legend title text (!) Probably Uneccessary
        legend.text = plots$global$legend.text, # Modify legend value text
        plot.tag = plots$global$plot.tag, # Modify the plot tag (e.g. "A.") text
        # Positioning
        axis.title.x = plots$global$axis.title.x, # Modify X axis appearance
        axis.title.y = plots$global$axis.title.y, # Modify Y axis appearance
        plot.tag.position = plots$global$overall$plot.tag.position, # Modify position of plot tag
        legend.position = plots$global$overall$legend.position, # Modify position of legend
        # Other Aesthetics
        axis.line = plots$global$axis.line, # Modify axis line attributes
        axis.ticks = plots$global$axis.ticks, # Modify axis tick mark attributes
        axis.ticks.length = plots$global$axis.ticks.length, # Modify how long each tick is
        legend.key.size = plots$global$legend.key.size, # Modify the size of the legend key boxes
        strip.background = element_rect(colour="white") # Gets rid of default background
      )+
      xlab("Ethanol % (v/v)")+ # X axis label
      ylab("Hedonic Responses (+/-SEM)") # Y axis label
    
    # Save the plot
    ggsave("Fig 6A Ethanol Hedonic Graph.png", 
           plots$eth$hedon$overall$ggp, 
           width = 533, 
           height = 600, 
           units = "px", 
           dpi = 81)
    
    
    ## Total Ethanol ####
    
    # Start ggplot
    plots$eth$hedon$total.e$ggp <- ggplot(plots$eth$hedon$total.e$fx,
                                          aes(totale,
                                              fit,
                                              group = Age,
                                              col = Age,
                                              fill = Age,
                                              shape = Age))+
      
      # Add Raw Data to graph
      geom_point(data = ABHVdata$eth$no.ctrl,
                 aes(x = TOTAL.ETOH.Swap.Consumed..g.kg.,
                     y = Total.Hedonic...MM.),
                 size = plots$global$pt.size, 
                 alpha = plots$global$pt.alpha)+
      
      # Add Prediction Line to graph
      geom_smooth(data = plots$eth$hedon$total.e$fx,
                  se = FALSE, # Don't show Std. Error, we have error ribbons for that
                  method = "glm", # Generalized linear model
                  method.args = list(family = "poisson"), # Poisson link function
                  linewidth = plots$global$linewidth)+ # Width of trend lines
      
      
      # Add Error Ribbon to graph
      geom_ribbon(data = plots$eth$hedon$total.e$fx,
                  aes(ymin = lower, ymax = upper),
                  colour = plots$global$rib.edge.color, # colour = NA suppresses edges of the ribbon
                  alpha = plots$global$rib.alpha)+ # Opacity at 25% (75% Transparent)
      
      
      
      facet_wrap(~ as.factor(Concentration), nrow = 1)+ # Makes the plot multi-panel. One for each level of Concentration
      
      labs(tag="B.")+ # Add image tag for final image combo.
      
      scale_color_manual("", 
                         values = plots$global$total.e$colors, # Line and Point color
                         labels = plots$global$total.e$labels)+ # Labels for legend
      
      scale_fill_manual("",
                        values = plots$global$total.e$rib.color, # Ribbon color
                        labels = plots$global$total.e$labels)+ # Legend labels
      
      scale_linetype_manual("",
                            values = plots$global$total.e$line.types, # Types of lines
                            labels = plots$global$total.e$labels)+ # Legend labels
      
      scale_shape_manual("",
                         values = plots$global$total.e$pt.shapes, # Shapes of points
                         labels = plots$global$total.e$labels)+ # Legend labels
      
      scale_x_continuous(expand = c(0, 0),
                         limits = plots$eth$x.axis$total.e$lims, # Axis length limits
                         breaks = plots$eth$x.axis$total.e$breaks, # Axis break positions
                         labels = plots$eth$x.axis$total.e$labels)+ # Axis value labels
      
      scale_y_continuous(expand = c(0, 0),
                         limits = plots$y.axis$hedon$lims, # Axis length limits
                         breaks = plots$y.axis$hedon$breaks, # Axis break positions
                         labels = plots$y.axis$hedon$labels)+ # Axis value labels
      
      theme_classic()+
      theme(# Text
        strip.text.x = plots$global$strip.text.x, # Modify label text for facets
        axis.title = plots$global$axis.title, # Modify axis title text
        axis.text = plots$global$axis.text, # Modify axis value text
        legend.title = plots$global$legend.title, # Modify legend title text (!) Probably Uneccessary
        legend.text = plots$global$legend.text, # Modify legend value text
        plot.tag = plots$global$plot.tag, # Modify the plot tag (e.g. "A.") text
        plot.title = plots$global$plot.title,
        # Positioning
        axis.title.x = plots$global$axis.title.x, # Modify X axis appearance
        axis.title.y = plots$global$axis.title.y, # Modify Y axis appearance
        plot.tag.position = plots$global$total.e$plot.tag.position, # Modify position of plot tag
        legend.position = plots$global$total.e$legend.position, # Modify position of legend
        # Other Aesthetics
        axis.line = plots$global$axis.line, # Modify axis line attributes
        axis.ticks = plots$global$axis.ticks, # Modify axis tick mark attributes
        axis.ticks.length = plots$global$axis.ticks.length, # Modify how long each tick is
        legend.key.size = plots$global$legend.key.size, # Modify the size of the legend key boxes
        strip.background = element_rect(colour="white"), # Gets rid of default background
        panel.spacing = plots$global$panel.spacing
      )+
      xlab("Total Ethanol Consumed (g/kg)")+
      ylab("Hedonic Responses (+/-SEM)")+
      ggtitle("Ethanol % (v/v)")
    
    # Save the plot
    ggsave("Fig 6B Ethanol Hedonic Total Ethanol Graph.png", plots$eth$hedon$total.e$ggp, 
           width = 1067, 
           height = 600, 
           units = "px", 
           dpi = 81)
    
    # put it all together using the simple syntax from the patchwork library
    plots$eth$hedon$combo.ggp <- plots$eth$hedon$overall$ggp +
      plots$eth$hedon$total.e$ggp +
      plot_layout(ncol = 2, widths = c(1, 2)) # Layout 2 columns with 1/3 for first col 2/3s for second plot.
    
    plots$eth$hedon$combo.ggp
    
    # Save the plot
    ggsave("Fig 6A&B Ethanol Hedonic Fits.png", 
           plots$eth$hedon$combo.ggp, 
           width = 1600, 
           height = 600, 
           units = "px", 
           dpi = 96)

        
# Save Workspace
save.image("ABHV_workspace.RData")