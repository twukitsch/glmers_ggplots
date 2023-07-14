# ETHANOL GRAPHS

  # AVERSIVE ####
    
    ## Overall ####
  
    # Make a new png file
    png("Fig 8A Ethanol Aversive Graph.png", width = 800, height = 600)
  
    # Start ggplot
    plots$eth$avers$overall$ggp <- ggplot(plots$eth$avers$overall$fx,
                                          aes(Concentration, fit,
                                          group = interaction(Age, Condition),
                                          col = interaction(Age, Condition),
                                          fill = interaction(Age, Condition),
                                          linetype = interaction(Age, Condition),
                                          shape = interaction(Age, Condition)))+
      
      # Add Raw Data to graph
      geom_point(data = data$eth$ctrl, 
                 aes(x = Concentration,
                     y = Total.Aversive),
                 size = plots$global$pt.size,
                 alpha = plots$global$pt.alpha)+
      
      # Add Prediction Line to graph
      geom_smooth(data = plots$eth$avers$overall$fx,
                  se = FALSE,
                  method = "glm",
                  method.args = list(family = "poisson"), 
                  size = plots$global$line.size)+
      
      # Add Error Ribbon to Graph
      geom_ribbon(data = plots$eth$avers$overall$fx,
                  aes(ymin = lower, ymax = upper),
                  colour = plots$global$rib.edge.color, # colour = NA suppresses edges of the ribbon
                  alpha = plots$global$rib.alpha)+ # Opacity at 25% (75% Transparent)
      
      labs(tag = "A.")+ # Add image tag for multipanel final image
      scale_color_manual("", 
                         values = plots$global$overall$colors, # Line and Point color
                         labels = plots$global$overall$labels)+ #(!) IS THIS NECESSARY????
      scale_fill_manual("",
                        values = plots$global$overall$rib.color, # Ribbon color
                        labels = plots$global$overall$labels)+
      scale_linetype_manual("",
                            values = plots$global$overall$line.types,
                            labels = plots$global$overall$labels)+
      scale_shape_manual("",
                         values = plots$global$overall$pt.shapes,
                         labels = plots$global$overall$labels)+
      scale_x_continuous(expand = c(0, 0),
                         limits = plots$eth$x.axis$overall$lims,
                         breaks = plots$eth$x.axis$overall$breaks,
                         labels = plots$eth$x.axis$overall$labels)+
      scale_y_continuous(expand = c(0, 0),
                         limits = plots$y.axis$avers$lims,
                         breaks = plots$y.axis$avers$breaks,
                         labels = plots$y.axis$avers$labels)+
      theme_classic()+
      theme(# Text
            strip.text.x = plots$global$strip.text.x,
            axis.title = plots$global$axis.title,
            axis.text = plots$global$axis.text,
            legend.title = plots$global$legend.title,
            legend.text = plots$global$legend.text,
            plot.tag = plots$global$plot.tag,
            # Positioning
            axis.title.y = plots$global$axis.title.y,
            axis.title.x = plots$global$axis.title.x,
            plot.tag.position = plots$global$plot.tag.position,
            legend.position = plots$global$legend.position,
            # Aesthetics
            axis.line = plots$global$axis.line,
            axis.ticks = plots$global$axis.ticks,
            axis.ticks.length = plots$global$axis.ticks.length,
            legend.key.size = plots$global$legend.key.size,
            strip.background = element_rect(colour="white") # Gets rid of default background
            )+
      xlab("Ethanol % (v/v)")+
      ylab("Aversive Responses (+/-SEM)")
    
    plots$eth$avers$overall$ggp
    
    #close the file
    dev.off()