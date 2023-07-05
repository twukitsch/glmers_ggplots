library(gridExtra)
library(grid)
library(patchwork)

###RUN ALL MODELS BEFORE RUNNING CODE BELOW###


#Residual Distribution Plots####

##ETHANOL####
  ###Aversive 1
    #Make and save residual distribution plot to working directory
    #make PNG file
    png("Eavers Res Plot.png", width = 300, height = 300)
    #plot residual density function
    plot(density(residuals(Eavers)), 
         main="", xlab="", frame= FALSE)
    #Add normal distribution to the residual plot for comparison to check assumption of normality
    Eavers.res = residuals(Eavers)
    Eav.m = mean(Eavers.res)
    Eav.std = sqrt(var(Eavers.res))
    curve(dnorm(x, mean=Eav.m, sd=Eav.std), col="darkblue", lwd=2, add=TRUE, yaxt="n")
    #close the file
    dev.off()
  
  ###Aversive 2
    #make PNG file
    png("EaversTot Res Plot.png", width = 300, height = 300)
    #plot residual density function
    plot(density(residuals(EaversTot)), 
         main="", xlab="", frame= FALSE)
    #Add normal distribution to the residual plot for comparison to check assumption of normality
    EaversTot.res = residuals(EaversTot)
    EavTot.m = mean(EaversTot.res)
    EavTot.std = sqrt(var(EaversTot.res))
    curve(dnorm(x, mean=EavTot.m, sd=EavTot.std), col="darkblue", lwd=2, add=TRUE, yaxt="n")
    #close the file
    dev.off()
  
  ###Hedonic 1
    #make PNG file
    png("Ehed Res Plot.png", width = 300, height = 300)
    #plot residual density function
    plot(density(residuals(Ehed)), 
         main="", xlab="", frame= FALSE)
    #Add normal distribution to the residual plot for comparison
    Ehed.res = residuals(Ehed)
    Ehed.m = mean(Ehed.res)
    Ehed.std = sqrt(var(Ehed.res))
    curve(dnorm(x, mean=Ehed.m, sd=Ehed.std), col="darkblue", lwd=2, add=TRUE, yaxt="n")
    #close the file
    dev.off()
    
  ###Hedonic 2
    #make PNG file
    png("EhedTot Res Plot.png", width = 300, height = 300)
    #plot residual density function
    plot(density(residuals(EhedTot)), 
         main="", xlab="", frame= FALSE)
    #Add normal distribution to the residual plot for comparison to check assumption of normality
    EhedTot.res = residuals(EhedTot)
    EhedTot.m = mean(EhedTot.res)
    EhedTot.std = sqrt(var(EhedTot.res))
    curve(dnorm(x, mean=EhedTot.m, sd=EhedTot.std), col="darkblue", lwd=2, add=TRUE, yaxt="n")
    #close the file
    dev.off()
    

##SUCROSE####
  ###Aversive 1
    #make PNG file
    png("Savers Res Plot.png", width = 300, height = 300)
    #plot residual density function
    plot(density(residuals(Savers)), 
         main="", xlab="", frame= FALSE)
    #Add normal distribution to the residual plot for comparison to check assumption of normality
    Savers.res = residuals(Savers)
    Sav.m = mean(Savers.res)
    Sav.std = sqrt(var(Savers.res))
    curve(dnorm(x, mean=Sav.m, sd=Sav.std), col="darkblue", lwd=2, add=TRUE, yaxt="n")
    #close the file
    dev.off()
    
  ###Aversive 2
    #make PNG file
    png("SaversTot Res Plot.png", width = 300, height = 300)
    #plot residual density function
    plot(density(residuals(SaversTot)), 
         main="", xlab="", frame= FALSE)
    #Add normal distribution to the residual plot for comparison to check assumption of normality
    SaversTot.res = residuals(SaversTot)
    SavTot.m = mean(SaversTot.res)
    SavTot.std = sqrt(var(SaversTot.res))
    curve(dnorm(x, mean=SavTot.m, sd=SavTot.std), col="darkblue", lwd=2, add=TRUE, yaxt="n")
    #close the file
    dev.off()
    
  ###Hedonic 1
    #make PNG file
    png("Shed Res Plot.png", width = 300, height = 300)
    #plot residual density function
    plot(density(residuals(Shed)), 
         main="", xlab="", frame= FALSE)
    #Add normal distribution to the residual plot for comparison to check assumption of normality
    Shed.res = residuals(Shed)
    Shed.m = mean(Shed.res)
    Shed.std = sqrt(var(Shed.res))
    curve(dnorm(x, mean=Shed.m, sd=Shed.std), col="darkblue", lwd=2, add=TRUE, yaxt="n")
    #close the file
    dev.off()
    
  ###Hedonic 2
    #make PNG file
    png("ShedTot Res Plot.png", width = 300, height = 300)
    #plot residual density function
    plot(density(residuals(ShedTot)), 
         main="", xlab="", frame= FALSE)
    #Add normal distribution to the residual plot for comparison to check assumption of normality
    ShedTot.res = residuals(ShedTot)
    ShedTot.m = mean(ShedTot.res)
    ShedTot.std = sqrt(var(ShedTot.res))
    curve(dnorm(x, mean=ShedTot.m, sd=ShedTot.std), col="darkblue", lwd=2, add=TRUE, yaxt="n")
    #close the file
    dev.off()


##Water####
  ###Aversive 1
    #make PNG file
    png("Havers Res Plot.png", width = 300, height = 300)
    #plot residual density function
    plot(density(residuals(Havers)), 
         main="", xlab="", frame= FALSE)
    #Add normal distribution to the residual plot for comparison to check assumption of normality
    Havers.res = residuals(Havers)
    Hav.m = mean(Havers.res)
    Hav.std = sqrt(var(Havers.res))
    curve(dnorm(x, mean=Hav.m, sd=Hav.std), col="darkblue", lwd=2, add=TRUE, yaxt="n")
    #close the file
    dev.off()
    
  ###Aversive 2
    #make PNG file
    png("HaversTot Res Plot.png", width = 300, height = 300)
    #plot residual density function
    plot(density(residuals(HaversTot)), 
         main="", xlab="", frame= FALSE)
    #Add normal distribution to the residual plot for comparison to check assumption of normality
    HaversTot.res = residuals(HaversTot)
    HavTot.m = mean(HaversTot.res)
    HavTot.std = sqrt(var(HaversTot.res))
    curve(dnorm(x, mean=HavTot.m, sd=HavTot.std), col="darkblue", lwd=2, add=TRUE, yaxt="n")
    #close the file
    dev.off()
    
  ###Hedonic 1
    #make PNG file
    png("Hhed Res Plot.png", width = 300, height = 300)
    #plot residual density function
    plot(density(residuals(Hhed)), 
         main="", xlab="", frame= FALSE)
    #Add normal distribution to the residual plot for comparison to check assumption of normality
    Hhed.res = residuals(Hhed)
    Hhed.m = mean(Hhed.res)
    Hhed.std = sqrt(var(Hhed.res))
    curve(dnorm(x, mean=Hhed.m, sd=Hhed.std), col="darkblue", lwd=2, add=TRUE, yaxt="n")
    #close the file
    dev.off()
    
  ###Hedonic 2
    #make PNG file
    png("HhedTot Res Plot.png", width = 300, height = 300)
    #plot residual density function
    plot(density(residuals(HhedTot)), 
         main="", xlab="", frame= FALSE)
    #Add normal distribution to the residual plot for comparison to check assumption of normality
    HhedTot.res = residuals(HhedTot)
    HhedTot.m = mean(HhedTot.res)
    HhedTot.std = sqrt(var(HhedTot.res))
    curve(dnorm(x, mean=HhedTot.m, sd=HhedTot.std), col="darkblue", lwd=2, add=TRUE, yaxt="n")
    #close the file
    dev.off()
    

#Predicted Graphs####
  
##Graph Setup####
  # generate aversive Y break positions
  avers.ybreaks = c(0,25,50,75,100)
  # and Y labels
  avers.ylabels = as.character(avers.ybreaks)
  
  # generate hedonic Y break positions
  hed.ybreaks = c(0,50,100,150,200,250,300,350,400,450,500,550)
  # and Y labels; the ,"",100... omits the label for 50 but leaves the tick mark.
  # The length of the vectors must be the same so the ""s are necessary for this trick.
  hed.ylabels = as.character(c(0,"",100,"",200,"",300,"",400,"",500,""))
  
  
##ETHANOL####
  #assign X break positions for standard analysis to object
  E.xbreaks = c(5,20,40)
  #and X labels
  E.xlabels = as.character(E.xbreaks)
  
  #assign X break positions for Total Ethanol Consumed to object
  Etot.xbreaks = c(0,25,50,75,100,125)
  #and X labels
  Etot.xlabels = as.character(Etot.xbreaks)
  
  
  ###Aversive______________________________________________________
    #pull the effects from model & calculate confidence intervals for graphing the Eavers lme4 object
    Eavers.eff <- Effect(c("c.conc","Age","Condition"),Eavers,
                         #se is std err and the level is the confidence level. .68 = actual std err for conf int. lower and upper.
                         se=list(level=.68),
                         #the xlevels command is used to increase the number of points calculated for the total ethanol (default = 5)
                         xlevels=list(c.conc=c(.05-mean(mydetoh$recoded.conc),
                                               .075-mean(mydetoh$recoded.conc),
                                               .10-mean(mydetoh$recoded.conc),
                                               .125-mean(mydetoh$recoded.conc),
                                               .15-mean(mydetoh$recoded.conc),
                                               .175-mean(mydetoh$recoded.conc),
                                               .20-mean(mydetoh$recoded.conc),
                                               .225-mean(mydetoh$recoded.conc),
                                               .25-mean(mydetoh$recoded.conc),
                                               .275-mean(mydetoh$recoded.conc),
                                               .30-mean(mydetoh$recoded.conc),
                                               .325-mean(mydetoh$recoded.conc),
                                               .35-mean(mydetoh$recoded.conc),
                                               .375-mean(mydetoh$recoded.conc),
                                               .40-mean(mydetoh$recoded.conc))))
    #convert effect object to dataframe
    Eavers.eff.df <-as.data.frame(Eavers.eff)
    Eavers.eff.df$Concentration <- (Eavers.eff.df$c.conc+mean(mydetoh$recoded.conc))*100
    Eavers.eff.df
    
    #make a new file
    png("Fig 8A Ethanol Aversive Graph.png", width = 800, height = 600)
    #generate plot
    
    Eavers.ggp <-ggplot(Eavers.eff.df,
                        aes(Concentration,fit,
                            group=interaction(Age,Condition),
                            col=interaction(Age,Condition),
                            fill=interaction(Age,Condition),
                            linetype=interaction(Age,Condition),
                            shape=interaction(Age,Condition)))+
      geom_point(data=mydetoh,aes(x=Concentration, y=Total.Aversive), size=6,alpha=0.75)+
      geom_smooth(data=Eavers.eff.df, se=FALSE, method="glm", method.args = list(family = "poisson"),size=1.5)+
      ## colour=NA suppresses edges of the ribbon
      geom_ribbon(data=Eavers.eff.df,colour=NA,alpha=0.25,
                  aes(ymin=lower,ymax=upper))+
      labs(tag="A.")+
      scale_color_manual("",values=c("#F8C8C8", "#82BFC4","#F8766D","#00BFC4"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
      scale_fill_manual("",values=c("#F8C8C8", "#82BFC4","#F8766D","#00BFC4"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
      scale_linetype_manual("",values=c("dashed","dashed","solid","solid"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
      scale_shape_manual("",values=c(17,18,16,15), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
      scale_x_continuous(expand=c(0,0), limits = c(0,41), breaks=E.xbreaks, labels=E.xlabels)+
      scale_y_continuous(expand=c(0,0), limits = c(0,110), breaks=avers.ybreaks, labels=avers.ylabels)+
      theme_classic()+
      theme(strip.background = element_rect(colour="white"),
            strip.text.x = element_text(size=18,face="bold"),
            axis.title = element_text(size=22),
            axis.text = element_text(size=21,color="black",face="bold"),
            axis.line = element_line(size=1.3),
            axis.ticks = element_line(size=1.3, color="black"),
            axis.ticks.length = unit(0.2, "cm"),
            legend.title = element_text(size=20),
            legend.text = element_text(size=18),
            axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 13, r = 0, b = 0, l = 0)),
            legend.key.size = unit(1.5, "cm"),
            legend.position = c(0.25, .85),
            plot.tag = element_text(size=36, face="bold"),
            plot.tag.position = c(0.05, 0.95))+
      xlab("Ethanol % (v/v)")+
      ylab("Aversive Responses (+/-SEM)")
    
    Eavers.ggp
    #Use ggplot_build(Eavers.ggp)$data to view the colors used.
    
    #close the file
    dev.off()
  
  ###Aversive Total EtOH______________________________________________________
    #pull the effects from model & calculate confidence intervals for graphing
    EaversTot.eff <- Effect(c("c.conc","Age","c.totale"),EaversTot,
                            #se is std err and the level is the confidence level. .68 = actual std err for conf int. lower and upper.
                            se=list(level=.68),
                            #the xlevels command is used to increase the number of points calculated for the total ethanol (default = 5)
                            xlevels=list(c.totale=20,c.conc=c(.05-mean(mydetoh.noCTRL$recoded.conc),
                                                              .20-mean(mydetoh.noCTRL$recoded.conc),
                                                              .40-mean(mydetoh.noCTRL$recoded.conc))))
    EaversTot.eff.df <-as.data.frame(EaversTot.eff)
    EaversTot.eff.df$Concentration <- (EaversTot.eff.df$c.conc+mean(mydetoh.noCTRL$recoded.conc))*100
    EaversTot.eff.df

    #make a new file
    png("Fig 8B Ethanol Aversive Total Ethanol Graph.png", width = 1000, height = 600)
    
    EaversTot.ggp <- ggplot(EaversTot.eff.df,
                            aes(c.totale+mean(mydetoh.noCTRL$TOTAL.ETOH.Swap.Consumed..g.kg.),fit,
                                group=Age,col=Age,fill=Age,shape=Age))+
      geom_point(data=mydetoh.noCTRL,aes(x=TOTAL.ETOH.Swap.Consumed..g.kg., y=Total.Aversive), size=4, alpha=.8)+
      geom_smooth(data=EaversTot.eff.df, se=FALSE, method="glm", method.args = list(family = "poisson"),size=1.5)+
      ## colour=NA suppresses edges of the ribbon
      geom_ribbon(data=EaversTot.eff.df,colour=NA,alpha=0.25,
                  aes(ymin=lower,ymax=upper))+
      facet_wrap(~ as.factor(Concentration), nrow=1)+
      labs(tag="B.")+
      scale_color_manual("",values=c("#F8766D","#00BFC4"),labels=c('Adolescent+IAE','Adult+IAE'))+
      scale_fill_manual("",values=c("#F8766D","#00BFC4"),labels=c('Adolescent+IAE','Adult+IAE'))+
      scale_linetype_manual("",values=c("solid","solid"),labels=c('Adolescent+IAE','Adult+IAE'))+
      scale_shape_manual("",values=c(16,15),labels=c('Adolescent+IAE','Adult+IAE'))+
      scale_x_continuous(expand=c(0,0), limits = c(0,130), breaks=Etot.xbreaks, labels=Etot.xlabels)+
      scale_y_continuous(expand=c(0,0), limits = c(0,110), breaks=avers.ybreaks, labels=avers.ylabels)+
      theme_classic()+
      theme(strip.background = element_rect(colour="white"),
            strip.text.x = element_text(size=18,face="bold"),
            axis.title = element_text(size=22),
            axis.text = element_text(size=21,color="black",face="bold"),
            axis.line = element_line(size=1.3),
            axis.ticks = element_line(size=1.3, color="black"),
            axis.ticks.length = unit(0.2, "cm"),
            panel.spacing = unit(1.75, "lines"),
            legend.title = element_text(size=20),
            legend.text = element_text(size=18),
            axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 13, r = 0, b = 0, l = 0)),
            plot.title = element_text(hjust = 0.5, size=22),
            legend.key.size = unit(1.5, "cm"),
            legend.position = c(0.15, .85),
            plot.tag = element_text(size=36, face="bold"),
            plot.tag.position = c(0.05, 0.95))+
      xlab("Total Ethanol Consumed (g/kg)")+
      ylab("Aversive Responses (+/-SEM)")+
      ggtitle("Ethanol % (v/v)")
    EaversTot.ggp
    
    #close the file
    dev.off()
    
    #put it all together using the simple syntax from the patchwork library & save as a single image
    png("Fig 8A&B Ethanol Aversive Fits.png", width = 1600, height = 600)
    
    Eavers.ggp + EaversTot.ggp + plot_layout(ncol=2,widths=c(1,2))
    
    #close the file
    dev.off()
    
  ###Hedonic______________________________________________________
    #pull the effects from model & calculate confidence intervals for graphing
    Ehed.eff <- Effect(c("c.conc","Age","Condition"),Ehed,
                       #se is std err and the level is the confidence level. .68 = actual std err for conf int. lower and upper.
                       se=list(level=.68),
                       #the xlevels command is used to increase the number of points calculated for the total ethanol (default = 5)
                       xlevels=list(c.conc=c(.05-mean(mydetoh$recoded.conc),
                                             .075-mean(mydetoh$recoded.conc),
                                             .10-mean(mydetoh$recoded.conc),
                                             .125-mean(mydetoh$recoded.conc),
                                             .15-mean(mydetoh$recoded.conc),
                                             .175-mean(mydetoh$recoded.conc),
                                             .20-mean(mydetoh$recoded.conc),
                                             .225-mean(mydetoh$recoded.conc),
                                             .25-mean(mydetoh$recoded.conc),
                                             .275-mean(mydetoh$recoded.conc),
                                             .30-mean(mydetoh$recoded.conc),
                                             .325-mean(mydetoh$recoded.conc),
                                             .35-mean(mydetoh$recoded.conc),
                                             .375-mean(mydetoh$recoded.conc),
                                             .40-mean(mydetoh$recoded.conc))))
    Ehed.eff.df <-as.data.frame(Ehed.eff)
    Ehed.eff.df$Concentration <- (Ehed.eff.df$c.conc+mean(mydetoh$recoded.conc))*100
    Ehed.eff.df
    
    #make a new file
    png("Fig 6A Ethanol Hedonic Graph.png", width = 800, height = 600)
    
    Ehed.ggp <-ggplot(Ehed.eff.df,
                      aes(Concentration,fit,
                          group=interaction(Age,Condition),
                          col=interaction(Age,Condition),
                          fill=interaction(Age,Condition),
                          linetype=interaction(Age,Condition),
                          shape=interaction(Age,Condition)))+
      geom_point(data=mydetoh,aes(x=Concentration, y=Total.Hedonic...MM.), size=6,alpha=0.75)+
      geom_smooth(data=Ehed.eff.df, se=FALSE, method="glm", method.args = list(family = "poisson"),size=1.5)+
      ## colour=NA suppresses edges of the ribbon
      geom_ribbon(data=Ehed.eff.df,colour=NA,alpha=0.25,
                  aes(ymin=lower,ymax=upper))+
      labs(tag="A.")+
      scale_color_manual("",values=c("#F8C8C8", "#82BFC4","#F8766D","#00BFC4"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
      scale_fill_manual("",values=c("#F8C8C8", "#82BFC4","#F8766D","#00BFC4"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
      scale_linetype_manual("",values=c("dashed","dashed","solid","solid"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
      scale_shape_manual("",values=c(17,18,16,15), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
      scale_x_continuous(expand=c(0,0), limits = c(0,41), breaks=E.xbreaks, labels=E.xlabels)+
      scale_y_continuous(expand=c(0,0), limits = c(0,580), breaks=hed.ybreaks, labels=hed.ylabels)+
      theme_classic()+
      theme(strip.background = element_rect(colour="white"),
            strip.text.x = element_text(size=18,face="bold"),
            axis.title = element_text(size=22),
            axis.text = element_text(size=21,color="black",face="bold"),
            axis.line = element_line(size=1.3),
            axis.ticks = element_line(size=1.3, color="black"),
            axis.ticks.length = unit(0.2, "cm"),
            legend.title = element_text(size=20),
            legend.text = element_text(size=18),
            axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 13, r = 0, b = 0, l = 0)),
            legend.key.size = unit(1.5, "cm"),
            legend.position = c(0.75, .85),
            plot.tag = element_text(size=36, face="bold"),
            plot.tag.position = c(0.05, 0.95))+
      xlab("Ethanol % (v/v)")+
      ylab("Hedonic Responses (+/-SEM)")
    
    Ehed.ggp
    
    #close the file
    dev.off()
    
  ###Hedonic Total EtOH______________________________________________________
    #pull the effects from model & calculate confidence intervals for graphing
    EhedTot.eff <- Effect(c("c.conc","Age","c.totale"),EhedTot,
                          #se is std err and the level is the confidence level. .68 = actual std err for conf int. lower and upper.
                          se=list(level=.68),
                          #the xlevels command is used to increase the number of points calculated for the total ethanol (default = 5)
                          xlevels=list(c.totale=20,c.conc=c(.05-mean(mydetoh.noCTRL$recoded.conc),
                                                            .20-mean(mydetoh.noCTRL$recoded.conc),
                                                            .40-mean(mydetoh.noCTRL$recoded.conc))))
    EhedTot.eff.df <-as.data.frame(EhedTot.eff)
    EhedTot.eff.df$Concentration <- (EhedTot.eff.df$c.conc+mean(mydetoh.noCTRL$recoded.conc))*100
    EhedTot.eff.df
    
    #make a new file
    png("Fig 6B Ethanol Hedonic Graph.png", width = 1000, height = 600)
    
    EhedTot.ggp <- ggplot(EhedTot.eff.df,
                          aes(c.totale+mean(mydetoh.noCTRL$TOTAL.ETOH.Swap.Consumed..g.kg.),fit,
                              group=Age,col=Age,fill=Age,shape=Age))+
      geom_point(data=mydetoh.noCTRL,aes(x=TOTAL.ETOH.Swap.Consumed..g.kg., y=Total.Hedonic...MM.), size=4, alpha=.8)+
      geom_smooth(data=EhedTot.eff.df, se=FALSE, method="glm", method.args = list(family = "poisson"),size=1.5)+
      ## colour=NA suppresses edges of the ribbon
      geom_ribbon(data=EhedTot.eff.df,colour=NA,alpha=0.25,
                  aes(ymin=lower,ymax=upper))+
      facet_wrap(~ as.factor(Concentration), nrow=1)+
      labs(tag = "B.")+
      scale_color_manual("",values=c("#F8766D","#00BFC4"),labels=c('Adolescent+IAE','Adult+IAE'))+
      scale_fill_manual("",values=c("#F8766D","#00BFC4"),labels=c('Adolescent+IAE','Adult+IAE'))+
      scale_linetype_manual("",values=c("solid","solid"),labels=c('Adolescent+IAE','Adult+IAE'))+
      scale_shape_manual("",values=c(16,15),labels=c('Adolescent+IAE','Adult+IAE'))+
      scale_x_continuous(expand=c(0,0), limits = c(0,130), breaks=Etot.xbreaks, labels=Etot.xlabels)+
      scale_y_continuous(expand=c(0,0), limits = c(0,580), breaks=hed.ybreaks, labels=hed.ylabels)+
      theme_classic()+
      theme(strip.background = element_rect(colour="white"),
            strip.text.x = element_text(size=18,face="bold"),
            axis.title = element_text(size=22),
            axis.text = element_text(size=21,color="black",face="bold"),
            axis.line = element_line(size=1.3),
            axis.ticks = element_line(size=1.3, color="black"),
            axis.ticks.length = unit(0.2, "cm"),
            panel.spacing = unit(1.75, "lines"),
            legend.title = element_text(size=20),
            legend.text = element_text(size=18),
            axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 13, r = 0, b = 0, l = 0)),
            plot.title = element_text(hjust = 0.5, size=22),
            legend.position = c(0.85, .85),
            plot.tag = element_text(size=36, face="bold"),
            plot.tag.position = c(0.05, 0.95),
            legend.key.size = unit(1.5, "cm"))+
      xlab("Total Ethanol Consumed (g/kg)")+
      ylab("Hedonic Responses (+/-SEM)")+
      ggtitle("Ethanol % (v/v)")

    EhedTot.ggp
    
    #close the file
    dev.off()
    
  #put it all together using the simple syntax from the patchwork library & save as a single image
    png("Fig 6A&B Ethanol Hedonic Fits.png", width = 1600, height = 600)
    
    Ehed.ggp + EhedTot.ggp + plot_layout(ncol=2,widths=c(1,2))

    #close the file
    dev.off()
    
    
##SUCROSE####
    # generate break positions for Sucrose x axis
    S.xbreaks = c(.01,.1,1)
    # and labels
    S.xlabels = as.character(S.xbreaks)
    
  
  ###Aversive______________________________________________________
    #pull the effects from model & calculate confidence intervals for graphing
    Savers.eff <- Effect(c("c.molarity","Age","Condition"),Savers,
                         #se is std err and the level is the confidence level. .68 = actual std err for conf int. lower and upper.
                         se=list(level=.68),
                         #the xlevels command is used to increase the number of points calculated for the total ethanol (default = 5)
                         xlevels=list(c.molarity=c(.01-mean(mydsuc$molarity),
                                                   .02-mean(mydsuc$molarity),
                                                   .04-mean(mydsuc$molarity),
                                                   .06-mean(mydsuc$molarity),
                                                   .08-mean(mydsuc$molarity),
                                                   .1-mean(mydsuc$molarity),
                                                   .2-mean(mydsuc$molarity),
                                                   .3-mean(mydsuc$molarity),
                                                   .4-mean(mydsuc$molarity),
                                                   .5-mean(mydsuc$molarity),
                                                   .6-mean(mydsuc$molarity),
                                                   .7-mean(mydsuc$molarity),
                                                   .8-mean(mydsuc$molarity),
                                                   .9-mean(mydsuc$molarity),
                                                   1.0-mean(mydsuc$molarity))))
    Savers.eff.df <-as.data.frame(Savers.eff)
    Savers.eff.df$molarity <- (Savers.eff.df$c.molarity+mean(mydsuc$molarity))
    Savers.eff.df
    
    #make a new file
    png("Fig 12A Sucrose Aversive Graph.png", width = 800, height = 600)
    
    Savers.ggp <-ggplot(Savers.eff.df,
                        aes(molarity,fit,
                            group=interaction(Age,Condition),
                            col=interaction(Age,Condition),
                            fill=interaction(Age,Condition),
                            linetype=interaction(Age,Condition),
                            shape=interaction(Age,Condition)))+
      geom_point(data=mydsuc,aes(x=molarity, y=Total.Aversive), size=6,alpha=0.75)+
      geom_smooth(data=Savers.eff.df, se=FALSE, method="glm", method.args = list(family = "poisson"),size=1.5)+
      ## colour=NA suppresses edges of the ribbon
      geom_ribbon(data=Savers.eff.df,colour=NA,alpha=0.25,
                  aes(ymin=lower,ymax=upper))+
      labs(tag="A.")+
      scale_color_manual("",values=c("#F8C8C8", "#82BFC4","#F8766D","#00BFC4"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
      scale_fill_manual("",values=c("#F8C8C8", "#82BFC4","#F8766D","#00BFC4"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
      scale_linetype_manual("",values=c("dashed","dashed","solid","solid"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
      scale_shape_manual("",values=c(17,18,16,15), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
      scale_x_continuous(limits = c(0,1), breaks=S.xbreaks, labels=S.xlabels)+
      scale_y_continuous(expand=c(0,0), limits = c(0,110), breaks=avers.ybreaks, labels=avers.ylabels)+
      theme_classic()+
      theme(strip.background = element_rect(colour="white"),
            strip.text.x = element_text(size=18,face="bold"),
            axis.title = element_text(size=22),
            axis.text = element_text(size=21,color="black",face="bold"),
            axis.line = element_line(size=1.3),
            axis.ticks = element_line(size=1.3, color="black"),
            axis.ticks.length = unit(0.2, "cm"),
            legend.title = element_text(size=20),
            legend.text = element_text(size=18),
            axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 13, r = 0, b = 0, l = 0)),
            legend.key.size = unit(1.5, "cm"),
            legend.position = c(0.75, .85),
            plot.tag = element_text(size=36, face="bold"),
            plot.tag.position = c(0.05, 0.95))+
      xlab("Sucrose Concentration (M)")+
      ylab("Aversive Responses (+/-SEM)")
    Savers.ggp
    
    #close the file
    dev.off()
    
  ###Aversive Total EtOH______________________________________________________
    #pull the effects from model & calculate confidence intervals for graphing
    SaversTot.eff <- Effect(c("c.molarity","Age","c.totale"),SaversTot,
                            #se is std err and the level is the confidence level. .68 = actual std err for conf int. lower and upper.
                            se=list(level=.68),
                            #the xlevels command is used to increase the number of points calculated for the total ethanol (default = 5)
                            xlevels=list(c.totale=20,c.molarity=c(.01-mean(mydsuc.noCTRL$molarity),
                                                                  .1-mean(mydsuc.noCTRL$molarity),
                                                                  1.0-mean(mydsuc.noCTRL$molarity))))
    SaversTot.eff.df <-as.data.frame(SaversTot.eff)
    SaversTot.eff.df$molarity <- (SaversTot.eff.df$c.molarity+mean(mydsuc.noCTRL$molarity))
    SaversTot.eff.df

    #make a new file
    png("Fig 12B Sucrose Aversive Total Ethanol Graph.png", width = 1000, height = 600)
    
    SaversTot.ggp <- ggplot(SaversTot.eff.df,
                            aes(c.totale+mean(mydsuc.noCTRL$TOTAL.ETOH.Swap.Consumed..g.kg.),fit,
                                group=Age,col=Age,fill=Age,shape=Age))+
      geom_point(data=mydsuc.noCTRL,aes(x=TOTAL.ETOH.Swap.Consumed..g.kg., y=Total.Aversive), size=4, alpha=.8)+
      geom_smooth(data=SaversTot.eff.df, se=FALSE, method="glm", method.args = list(family = "poisson"),size=1.5)+
      ## colour=NA suppresses edges of the ribbon
      geom_ribbon(data=SaversTot.eff.df,colour=NA,alpha=0.25,
                  aes(ymin=lower,ymax=upper))+
      facet_wrap(~ as.factor(molarity), nrow=1)+
      labs(tag="B.")+
      scale_color_manual("",values=c("#F8766D","#00BFC4"),labels=c('Adolescent+IAE','Adult+IAE'))+
      scale_fill_manual("",values=c("#F8766D","#00BFC4"),labels=c('Adolescent+IAE','Adult+IAE'))+
      scale_linetype_manual("",values=c("solid","solid"),labels=c('Adolescent+IAE','Adult+IAE'))+
      scale_shape_manual("",values=c(16,15),labels=c('Adolescent+IAE','Adult+IAE'))+
      scale_x_continuous(expand=c(0,0), limits = c(0,130), breaks=Etot.xbreaks, labels=Etot.xlabels)+
      scale_y_continuous(expand=c(0,0), limits = c(0,110), breaks=avers.ybreaks, labels=avers.ylabels)+
      theme_classic()+
      theme(strip.background = element_rect(colour="white"),
            strip.text.x = element_text(size=18,face="bold"),
            axis.title = element_text(size=22),
            axis.text = element_text(size=21,color="black",face="bold"),
            axis.line = element_line(size=1.3),
            axis.ticks = element_line(size=1.3, color="black"),
            axis.ticks.length = unit(0.2, "cm"),
            panel.spacing = unit(1.75, "lines"),
            legend.title = element_text(size=20),
            legend.text = element_text(size=18),
            axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 13, r = 0, b = 0, l = 0)),
            plot.title = element_text(hjust = 0.5, size=22),
            legend.key.size = unit(1.5, "cm"),
            legend.position = c(0.85, .85),
            plot.tag = element_text(size=36, face="bold"),
            plot.tag.position = c(0.05, 0.95))+
      xlab("Total Ethanol Consumed (g/kg)")+
      ylab("Aversive Responses (+/-SEM)")+
      ggtitle("Sucrose Concentration (M)")
    SaversTot.ggp
    
    #close the file
    dev.off()
    
    #put it all together using the simple syntax from the patchwork library & save as a single image
    png("Fig 12A&B Sucrose Aversive Fits.png", width = 1600, height = 600)
    
    Savers.ggp + SaversTot.ggp + plot_layout(ncol=2,widths=c(1,2))
    
    #close the file
    dev.off()
    
    
  ###Hedonic______________________________________________________
    #pull the effects from model & calculate confidence intervals for graphing
    Shed.eff <- Effect(c("c.molarity","Age","Condition"),Shed,
                       #se is std err and the level is the confidence level. .68 = actual std err for conf int. lower and upper.
                       se=list(level=.68),
                       #the xlevels command is used to increase the number of points calculated for the total ethanol (default = 5)
                       xlevels=list(c.molarity=c(.01-mean(mydsuc$molarity),
                                                 .02-mean(mydsuc$molarity),
                                                 .04-mean(mydsuc$molarity),
                                                 .06-mean(mydsuc$molarity),
                                                 .08-mean(mydsuc$molarity),
                                                 .1-mean(mydsuc$molarity),
                                                 .2-mean(mydsuc$molarity),
                                                 .3-mean(mydsuc$molarity),
                                                 .4-mean(mydsuc$molarity),
                                                 .5-mean(mydsuc$molarity),
                                                 .6-mean(mydsuc$molarity),
                                                 .7-mean(mydsuc$molarity),
                                                 .8-mean(mydsuc$molarity),
                                                 .9-mean(mydsuc$molarity),
                                                 1.0-mean(mydsuc$molarity))))
    Shed.eff.df <-as.data.frame(Shed.eff)
    Shed.eff.df$molarity <- (Shed.eff.df$c.molarity+mean(mydsuc$molarity))
    Shed.eff.df
    
    #make a new file
    png("Fig 10A Sucrose Hedonic Graph.png", width = 800, height = 600)
    
    Shed.ggp <-ggplot(Shed.eff.df,
                      aes(molarity,fit,
                          group=interaction(Age,Condition),
                          col=interaction(Age,Condition),
                          fill=interaction(Age,Condition),
                          linetype=interaction(Age,Condition),
                          shape=interaction(Age,Condition)))+
      geom_point(data=mydsuc,aes(x=molarity, y=Total.Hedonic...MM.), size=6,alpha=0.8)+
      geom_smooth(data=Shed.eff.df, se=FALSE, method="glm", method.args = list(family = "poisson"),size=1.5)+
      ## colour=NA suppresses edges of the ribbon
      geom_ribbon(data=Shed.eff.df,colour=NA,alpha=0.25,
                  aes(ymin=lower,ymax=upper))+
      labs(tag="A.")+
      scale_color_manual("",values=c("#F8C8C8", "#82BFC4","#F8766D","#00BFC4"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
      scale_fill_manual("",values=c("#F8C8C8", "#82BFC4","#F8766D","#00BFC4"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
      scale_linetype_manual("",values=c("dashed","dashed","solid","solid"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
      scale_shape_manual("",values=c(17,18,16,15), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
      scale_x_continuous(limits = c(0,1), breaks=S.xbreaks, labels=S.xlabels)+
      scale_y_continuous(expand=c(0,0), limits = c(0,580), breaks=hed.ybreaks, labels=hed.ylabels)+
      theme_classic()+
      theme(strip.background = element_rect(colour="white"),
            strip.text.x = element_text(size=18,face="bold"),
            axis.title = element_text(size=22),
            axis.text = element_text(size=21,color="black",face="bold"),
            axis.line = element_line(size=1.3),
            axis.ticks = element_line(size=1.3, color="black"),
            axis.ticks.length = unit(0.2, "cm"),
            legend.title = element_text(size=20),
            legend.text = element_text(size=18),
            axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 13, r = 0, b = 0, l = 0)),
            legend.key.size = unit(1.5, "cm"),
            legend.position = c(0.25, .85),
            plot.tag = element_text(size=36, face="bold"),
            plot.tag.position = c(0.05, 0.95))+
      xlab("Sucrose Concentration (M)")+
      ylab("Hedonic Responses (+/-SEM)")
    Shed.ggp
    
    #close the file
    dev.off()
    
  ###Hedonic Total EtOH______________________________________________________
    #pull the effects from model & calculate confidence intervals for graphing
    ShedTot.eff <- Effect(c("c.molarity","Age","c.totale"),ShedTot,
                          #se is std err and the level is the confidence level. .68 = actual std err for conf int. lower and upper.
                          se=list(level=.68),
                          #the xlevels command is used to increase the number of points calculated for the total ethanol (default = 5)
                          xlevels=list(c.totale=20,c.molarity=c(.01-mean(mydsuc.noCTRL$molarity),
                                                                .1-mean(mydsuc.noCTRL$molarity),
                                                                1.0-mean(mydsuc.noCTRL$molarity))))
    ShedTot.eff.df <-as.data.frame(ShedTot.eff)
    ShedTot.eff.df$molarity <- (ShedTot.eff.df$c.molarity+mean(mydsuc.noCTRL$molarity))
    ShedTot.eff.df
    
    #make a new file
    png("Fig 10B Sucrose Hedonic Total Ethanol Graph.png", width = 1000, height = 600)
    
    ShedTot.ggp <- ggplot(ShedTot.eff.df,
                          aes(c.totale+mean(mydsuc.noCTRL$TOTAL.ETOH.Swap.Consumed..g.kg.),fit,
                              group=Age,col=Age,fill=Age,shape=Age))+
      geom_point(data=mydsuc.noCTRL,aes(x=TOTAL.ETOH.Swap.Consumed..g.kg., y=Total.Hedonic...MM.), size=4,alpha=.8)+
      geom_smooth(data=ShedTot.eff.df, se=FALSE, method="glm", method.args = list(family = "poisson"),size=1.5)+
      ## colour=NA suppresses edges of the ribbon
      geom_ribbon(data=ShedTot.eff.df,colour=NA,alpha=0.25,
                  aes(ymin=lower,ymax=upper))+
      facet_wrap(~ as.factor(molarity), nrow=1)+
      labs(tag="B.")+
      scale_color_manual("",values=c("#F8766D","#00BFC4"),labels=c('Adolescent+IAE','Adult+IAE'))+
      scale_fill_manual("",values=c("#F8766D","#00BFC4"),labels=c('Adolescent+IAE','Adult+IAE'))+
      scale_linetype_manual("",values=c("solid","solid"),labels=c('Adolescent+IAE','Adult+IAE'))+
      scale_shape_manual("",values=c(16,15),labels=c('Adolescent+IAE','Adult+IAE'))+
      scale_x_continuous(expand=c(0,0), limits = c(0,130), breaks=Etot.xbreaks, labels=Etot.xlabels)+
      scale_y_continuous(expand=c(0,0), limits = c(0,580), breaks=hed.ybreaks, labels=hed.ylabels)+
      theme_classic()+
      theme(strip.background = element_rect(colour="white"),
            strip.text.x = element_text(size=18,face="bold"),
            axis.title = element_text(size=22),
            axis.text = element_text(size=21,color="black",face="bold"),
            axis.line = element_line(size=1.3),
            axis.ticks = element_line(size=1.3, color="black"),
            axis.ticks.length = unit(0.2, "cm"),
            panel.spacing = unit(1.75, "lines"),
            legend.title = element_text(size=20),
            legend.text = element_text(size=18),
            axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 13, r = 0, b = 0, l = 0)),
            plot.title = element_text(hjust = 0.5, size=22),
            legend.key.size = unit(1.5, "cm"),
            legend.position = c(0.15, .85),
            plot.tag = element_text(size=36, face="bold"),
            plot.tag.position = c(0.05, 0.95))+
      xlab("Total Ethanol Consumed (g/kg)")+
      ylab("Hedonic Responses (+/-SEM)")+
      ggtitle("Sucrose Concentration (M)")
    ShedTot.ggp
    
    #close the file
    dev.off()
    
    #put it all together using the simple syntax from the patchwork library & save as a single image
    png("Fig 10A&B Sucrose Hedonic Fits.png", width = 1600, height = 600)
    
    Shed.ggp + ShedTot.ggp + plot_layout(ncol=2,widths=c(1,2))
    
    #close the file
    dev.off()
    

##Water####
    #Assign strings to a function for relabeling the facets above each Total EtOH graph
    Subst_names <- list("Water1"="1", "Water2"="2")
    Substance.labs <- function(variable,value){
      return(Subst_names[value])
    }
    
  ###Aversive______________________________________________________
    #pull the effects from model & calculate confidence intervals for graphing
    Havers.eff <- Effect(c("Substance","Age","Condition"),Havers,
                         #se is std err and the level is the confidence level. .68 = actual std err for conf int. lower and upper.
                         se=list(level=.68))
    
    Havers.eff.df <-as.data.frame(Havers.eff)
    Havers.eff.df$trial <- c(1,2,1,2,1,2,1,2)
    Havers.eff.df
    
    #make a new file
    png("Fig 16A Water Aversive Graph.png", width = 800, height = 600)
    
    Havers.ggp <-ggplot(Havers.eff.df,
                        aes(as.factor(trial),fit,
                            group=interaction(Age,Condition),
                            col=interaction(Age,Condition),
                            fill=interaction(Age,Condition),
                            linetype=interaction(Age,Condition),
                            shape=interaction(Age,Condition)))+
      geom_line(data=Havers.eff.df,size=1.5)+
      ## colour=NA suppresses edges of the ribbon
      geom_point(data=Havers.eff.df,size=6,alpha=.9)+
      geom_errorbar(data=Havers.eff.df,
                    aes(ymin=lower,ymax=upper),width=0.1,size=1.1,linetype="solid")+
      labs(tag="A.")+
      scale_color_manual("",values=c("#F8C8C8", "#82BFC4","#F8766D","#00BFC4"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
      scale_fill_manual("",values=c("#F8C8C8", "#82BFC4","#F8766D","#00BFC4"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
      scale_linetype_manual("",values=c("dashed","dashed","solid","solid"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
      scale_shape_manual("",values=c(17,18,16,15), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
      scale_y_continuous(expand=c(0,0), limits = c(0,110), breaks=avers.ybreaks, labels=avers.ylabels)+
      theme_classic()+
      theme(strip.background = element_rect(colour="white"),
            strip.text.x = element_text(size=18,face="bold"),
            axis.title = element_text(size=22),
            axis.text = element_text(size=21,color="black",face="bold"),
            axis.line = element_line(size=1.3),
            axis.ticks = element_line(size=1.3, color="black"),
            axis.ticks.length = unit(0.2, "cm"),
            legend.title = element_text(size=20),
            legend.text = element_text(size=18),
            axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 13, r = 0, b = 0, l = 0)),
            legend.key.size = unit(1.5, "cm"),
            legend.position = c(0.25, .85),
            plot.tag = element_text(size=36, face="bold"),
            plot.tag.position = c(0.05, 0.95))+
      xlab("Water Trial")+
      ylab("Aversive Responses (+/-SEM)")
    Havers.ggp
    
    #close the file
    dev.off()
    
  ###Aversive Total EtOH______________________________________________________
    #pull the effects from model & calculate confidence intervals for graphing
    HaversTot.eff <- Effect(c("Substance","Age","c.totale"),HaversTot,
                            se=list(level=.68), 
                            xlevels=list(c.totale=20))
    
    HaversTot.eff.df <-as.data.frame(HaversTot.eff)
    HaversTot.eff.df
    
    #make a new file
    png("Fig 16B Water Aversive Total Ethanol Graph.png", width = 1000, height = 600)
    
    HaversTot.ggp <- ggplot(HaversTot.eff.df,
                           aes(c.totale+mean(mydh2o.noCTRL$TOTAL.ETOH.Swap.Consumed..g.kg.),fit,
                               group=Age,col=Age,fill=Age,shape=Age))+
      geom_smooth(se=FALSE, method="glm", method.args = list(family = "poisson"),size=1.5)+
      ## colour=NA suppresses edges of the ribbon
      geom_ribbon(data=HaversTot.eff.df,colour=NA,alpha=0.25,
                  aes(ymin=lower,ymax=upper))+
      facet_grid(.~ Substance,labeller = as_labeller(Substance.labs))+
      geom_point(data=mydh2o.noCTRL,aes(x=TOTAL.ETOH.Swap.Consumed..g.kg., y=Total.Aversive),size=4,alpha=.75)+
      labs(tag="B.")+
      scale_color_manual("",values=c("#F8766D","#00BFC4"),labels=c('Adolescent+IAE','Adult+IAE'))+
      scale_fill_manual("",values=c("#F8766D","#00BFC4"),labels=c('Adolescent+IAE','Adult+IAE'))+
      scale_linetype_manual("",values=c("solid","solid"),labels=c('Adolescent+IAE','Adult+IAE'))+
      scale_shape_manual("",values=c(16,15),labels=c('Adolescent+IAE','Adult+IAE'))+
      scale_x_continuous(expand=c(0,0), limits = c(0,130), breaks=Etot.xbreaks, labels=Etot.xlabels)+
      scale_y_continuous(expand=c(0,0), limits = c(0,110), breaks=avers.ybreaks, labels=avers.ylabels)+
      theme_classic()+
      theme(strip.background = element_rect(colour="white"),
            strip.text.x = element_text(size=18,face="bold"),
            axis.title = element_text(size=22),
            axis.text = element_text(size=21,color="black",face="bold"),
            axis.line = element_line(size=1.3),
            axis.ticks = element_line(size=1.3, color="black"),
            axis.ticks.length = unit(0.2, "cm"),
            panel.spacing = unit(1.75, "lines"),
            legend.title = element_text(size=20),
            legend.text = element_text(size=18),
            axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 13, r = 0, b = 0, l = 0)),
            plot.title = element_text(hjust = 0.5, size=22),
            legend.key.size = unit(1.5, "cm"),
            legend.position = c(0.15, .85),
            plot.tag = element_text(size=36, face="bold"),
            plot.tag.position = c(0.05, 0.95))+
      xlab("Total Ethanol Consumed (g/kg)")+
      ylab("Aversive Responses (+/-SEM)")+
      ggtitle("Water Trial")
    HaversTot.ggp
    
    #close the file
    dev.off()
    
    #put it all together using the simple syntax from the patchwork library & save as a single image
    png("Fig 16A&B Water Aversive Fits.png", width = 1600, height = 600)
    
    Havers.ggp + HaversTot.ggp + plot_layout(ncol=2,widths=c(1,2))
    
    #close the file
    dev.off()
    
  ###Hedonic______________________________________________________ 
    #pull the effects from model & calculate confidence intervals for graphing
    Hhed.eff <- Effect(c("Substance","Age","Condition"),Hhed,
                       #se is std err and the level is the confidence level. .68 = actual std err for conf int. lower and upper.
                       se=list(level=.68))
    
    Hhed.eff.df <-as.data.frame(Hhed.eff)
    Hhed.eff.df$trial <- c(1,2,1,2,1,2,1,2)
    Hhed.eff.df

    #make a new file
    png("Fig 14A Water Hedonic Graph.png", width = 800, height = 600)
    
    Hhed.ggp <-ggplot(Hhed.eff.df,
                      aes(as.factor(trial),fit,
                          group=interaction(Age,Condition),
                          col=interaction(Age,Condition),
                          fill=interaction(Age,Condition),
                          linetype=interaction(Age,Condition),
                          shape=interaction(Age,Condition)))+
      geom_line(data=Hhed.eff.df,size=1.5)+
      ## colour=NA suppresses edges of the ribbon
      geom_point(data=Hhed.eff.df,size=6,alpha=.9)+
      geom_errorbar(data=Hhed.eff.df,
                    aes(ymin=lower,ymax=upper),width=0.1,size=1.1,linetype="solid")+
      labs(tag="A.")+
      scale_color_manual("",values=c("#F8C8C8", "#82BFC4","#F8766D","#00BFC4"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
      scale_fill_manual("",values=c("#F8C8C8", "#82BFC4","#F8766D","#00BFC4"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
      scale_linetype_manual("",values=c("dashed","dashed","solid","solid"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
      scale_shape_manual("",values=c(17,18,16,15), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
      scale_y_continuous(expand=c(0,0), limits = c(0,580), breaks=hed.ybreaks, labels=hed.ylabels)+
      theme_classic()+
      theme(strip.background = element_rect(colour="white"),
            strip.text.x = element_text(size=18,face="bold"),
            axis.title = element_text(size=22),
            axis.text = element_text(size=21,color="black",face="bold"),
            axis.line = element_line(size=1.3),
            axis.ticks = element_line(size=1.3, color="black"),
            axis.ticks.length = unit(0.2, "cm"),
            legend.title = element_text(size=20),
            legend.text = element_text(size=18),
            axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 13, r = 0, b = 0, l = 0)),
            legend.key.size = unit(1.5, "cm"),
            legend.position = c(0.75, .85),
            plot.tag = element_text(size=36, face="bold"),
            plot.tag.position = c(0.05, 0.95))+
      xlab("Water Trial")+
      ylab("Hedonic Responses (+/-SEM)")
    Hhed.ggp
    
    #close the file
    dev.off()
    
  ###Hedonic Total EtOH______________________________________________________
    #pull the effects from model & calculate confidence intervals for graphing
    HhedTot.eff <- Effect(c("Substance","Age","c.totale"),HhedTot,
                          se=list(level=.68), 
                          xlevels=list(c.totale=20))
    
    HhedTot.eff.df <-as.data.frame(HhedTot.eff)
    HhedTot.eff.df
    
    #make a new file
    png("Fig 14B Water Hedonic Total Ethanol Graph.png", width = 1000, height = 600)
    
    HhedTot.ggp <- ggplot(HhedTot.eff.df,
                         aes(c.totale+mean(mydh2o.noCTRL$TOTAL.ETOH.Swap.Consumed..g.kg.),fit,
                             group=Age,col=Age,fill=Age,shape=Age))+
      geom_smooth(se=FALSE, method="glm", method.args = list(family = "poisson"),size=1.5)+
      ## colour=NA suppresses edges of the ribbon
      geom_ribbon(data=HhedTot.eff.df,colour=NA,alpha=0.25,
                  aes(ymin=lower,ymax=upper))+
      facet_grid(.~ Substance,labeller = as_labeller(Substance.labs))+
      geom_point(data=mydh2o.noCTRL,aes(x=TOTAL.ETOH.Swap.Consumed..g.kg., y=Total.Hedonic...MM.),size=6,alpha =.9)+
      labs(tag="B.")+
      scale_color_manual("",values=c("#F8766D","#00BFC4"),labels=c('Adolescent+IAE','Adult+IAE'))+
      scale_fill_manual("",values=c("#F8766D","#00BFC4"),labels=c('Adolescent+IAE','Adult+IAE'))+
      scale_linetype_manual("",values=c("solid","solid"),labels=c('Adolescent+IAE','Adult+IAE'))+
      scale_shape_manual("",values=c(16,15),labels=c('Adolescent+IAE','Adult+IAE'))+
      scale_x_continuous(expand=c(0,0), limits = c(0,130), breaks=Etot.xbreaks, labels=Etot.xlabels)+
      scale_y_continuous(expand=c(0,0), limits = c(0,580), breaks=hed.ybreaks, labels=hed.ylabels)+
      theme_classic()+
      theme(strip.background = element_rect(colour="white"),
            strip.text.x = element_text(size=18,face="bold"),
            axis.title = element_text(size=22),
            axis.text = element_text(size=21,color="black",face="bold"),
            axis.line = element_line(size=1.3),
            axis.ticks = element_line(size=1.3, color="black"),
            axis.ticks.length = unit(0.2, "cm"),
            panel.spacing = unit(1.75, "lines"),
            legend.title = element_text(size=20),
            legend.text = element_text(size=18),
            axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 13, r = 0, b = 0, l = 0)),
            plot.title = element_text(hjust = 0.5, size=22),
            legend.key.size = unit(1.5, "cm"),
            legend.position = c(0.85, .85),
            plot.tag = element_text(size=36, face="bold"),
            plot.tag.position = c(0.05, 0.95))+
      xlab("Total Ethanol Consumed (g/kg)")+
      ylab("Hedonic Responses (+/-SEM)")+
      ggtitle("Water Trial")
    HhedTot.ggp
    
    #close the file
    dev.off()
    
    #put it all together using the simple syntax from the patchwork library & save as a single image
    png("Fig 14A&B Water Hedonic Fits.png", width = 1600, height = 600)
    
    Hhed.ggp + HhedTot.ggp + plot_layout(ncol=2,widths=c(1,2))
    
    #close the file
    dev.off()


#Other Graphs####
    
##DRINKING####
  ###Day 1-36
    # generate break positions
    drink1.breaks = c(1,3,5,8,10,12,15,17,19,22,24,26,29,31,33,36)
    # and labels
    drink1.labels = as.character(drink1.breaks)
    
    #pull the effects from model & calculate confidence intervals for graphing
    drink1.eff <- Effect(c("c.Day","Age"),drink1,
                         #se is std err and the level is the confidence level. .68 = actual std err for conf int. lower and upper.
                         se=list(level=.68),
                         #the xlevels command is used to increase the number of points calculated for the total ethanol (default = 5)
                         xlevels=list(c.Day=c(1-mean(myddrink$Day),
                                              3-mean(myddrink$Day),
                                              5-mean(myddrink$Day),
                                              8-mean(myddrink$Day),
                                              10-mean(myddrink$Day),
                                              12-mean(myddrink$Day),
                                              15-mean(myddrink$Day),
                                              17-mean(myddrink$Day),
                                              19-mean(myddrink$Day),
                                              22-mean(myddrink$Day),
                                              24-mean(myddrink$Day),
                                              26-mean(myddrink$Day),
                                              29-mean(myddrink$Day),
                                              31-mean(myddrink$Day),
                                              33-mean(myddrink$Day),
                                              36-mean(myddrink$Day))))
    
    drink1.eff.df <-as.data.frame(drink1.eff)
    drink1.eff.df$Day <- (drink1.eff.df$c.Day+mean(myddrink$Day))
    drink1.eff.df

    #make a new file
    png("Fig 1a IAE Start Day1.png", width = 1000, height = 600)
    
    drink1.ggp <-ggplot(drink1.eff.df,
                        aes(Day,fit,
                            group=Age,
                            col=Age,
                            fill=Age,
                            shape=Age))+
      geom_point(data=mean.drink,aes(x=Day, y=fit), size=6,alpha=0.75)+
      geom_line(data = mean.drink,
                aes(x=Day, y=fit), stat = "identity",position = "identity",size=1.3)+
      geom_errorbar(data=mean.drink,
                    aes(ymin=lower,ymax=upper),width=0.3,size=1.1,linetype="solid")+
      geom_line(data=drink1.eff.df, size=1.5)+
      ## colour=NA suppresses edges of the ribbon
      geom_ribbon(data=drink1.eff.df,colour=NA,alpha=0.25,
                  aes(ymin=lower,ymax=upper))+
      scale_color_manual("",values=c("#F8766D","#00BFC4"), labels=c('Adolescent+IAE','Adult+IAE'))+
      scale_fill_manual("",values=c("#F8766D","#00BFC4"), labels=c('Adolescent+IAE','Adult+IAE'))+
      scale_shape_manual("",values=c(16,15), labels=c('Adolescent+IAE','Adult+IAE'))+
      scale_x_continuous(expand=c(0,0), limits = c(0,37), breaks=drink1.breaks, labels=drink1.labels)+
      scale_y_continuous(expand=c(0,0), limits = c(0,8.5))+
      theme_classic()+
      theme(strip.background = element_rect(colour="white"),
            strip.text.x = element_text(size=18,face="bold"),
            axis.title = element_text(size=22),
            axis.text = element_text(size=21,color="black",face="bold"),
            axis.line = element_line(size=1.3),
            axis.ticks = element_line(size=1.3, color="black"),
            axis.ticks.length = unit(0.2, "cm"),
            legend.title = element_text(size=20),
            legend.text = element_text(size=18),
            axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 13, r = 0, b = 0, l = 0)),
            legend.key.size = unit(1.5, "cm"))+
      xlab("Day")+
      ylab("Mean Ethanol Consumed (g/kg/24hr) +/- S.E.M.")
    
    drink1.ggp
    #Use ggplot_build(drink1.ggp)$data to view the colors used.
    
    #close the file
    dev.off()
    
    
  ###Day 3-36
    # generate break positions
    drink3.breaks = c(3,5,8,10,12,15,17,19,22,24,26,29,31,33,36)
    # and labels
    drink3.labels = as.character(c(3,5,8,10,12,15,17,19,22,24,26,29,31,33,36))
    
    #pull the effects from model & calculate confidence intervals for graphing
    drink3.eff <- Effect(c("c.Day","Age"),drink3,
                         #se is std err and the level is the confidence level. .68 = actual std err for conf int. lower and upper.
                         se=list(level=.68),
                         #the xlevels command is used to increase the number of points calculated for the total ethanol (default = 5)
                         xlevels=list(c.Day=c(3-mean(myddrink3$Day),
                                              5-mean(myddrink3$Day),
                                              8-mean(myddrink3$Day),
                                              10-mean(myddrink3$Day),
                                              12-mean(myddrink3$Day),
                                              15-mean(myddrink3$Day),
                                              17-mean(myddrink3$Day),
                                              19-mean(myddrink3$Day),
                                              22-mean(myddrink3$Day),
                                              24-mean(myddrink3$Day),
                                              26-mean(myddrink3$Day),
                                              29-mean(myddrink3$Day),
                                              31-mean(myddrink3$Day),
                                              33-mean(myddrink3$Day),
                                              36-mean(myddrink3$Day))))
    
    drink3.eff.df <-as.data.frame(drink3.eff)
    drink3.eff.df$Day <- (drink3.eff.df$c.Day+mean(myddrink3$Day))
    drink3.eff.df
    
    #make a new file
    png("Fig 1b IAE Start Day3.png", width = 1000, height = 600)
    
    drink3.ggp <-ggplot(drink3.eff.df,
                        aes(Day,fit,
                            group=Age,
                            col=Age,
                            fill=Age,
                            shape=Age))+
      geom_point(data=mean.drink3,aes(x=Day, y=fit), size=6,alpha=0.75)+
      geom_line(data = mean.drink3,
                aes(x=Day, y=fit), stat = "identity",position = "identity",size=1.5)+
      geom_errorbar(data=mean.drink3,
                    aes(ymin=lower,ymax=upper),width=0.3,size=1.1,linetype="solid")+
      geom_line(data=drink3.eff.df, size=1.5)+
      ## colour=NA suppresses edges of the ribbon
      geom_ribbon(data=drink3.eff.df,colour=NA,alpha=0.25,
                  aes(ymin=lower,ymax=upper))+
      scale_color_manual("",values=c("#F8766D","#00BFC4"), labels=c('Adolescent+IAE','Adult+IAE'))+
      scale_fill_manual("",values=c("#F8766D","#00BFC4"), labels=c('Adolescent+IAE','Adult+IAE'))+
      scale_shape_manual("",values=c(16,15), labels=c('Adolescent+IAE','Adult+IAE'))+
      scale_x_continuous(expand=c(0,0), limits = c(0,37), breaks=drink3.breaks, labels=drink3.labels)+
      scale_y_continuous(expand=c(0,0), limits = c(0,8.5))+
      theme_classic()+
      theme(strip.background = element_rect(colour="white"),
            strip.text.x = element_text(size=18,face="bold"),
            axis.title = element_text(size=22),
            axis.text = element_text(size=21,color="black",face="bold"),
            axis.line = element_line(size=1.3),
            axis.ticks = element_line(size=1.3, color="black"),
            axis.ticks.length = unit(0.2, "cm"),
            legend.title = element_text(size=20),
            legend.text = element_text(size=18),
            axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 13, r = 0, b = 0, l = 0)),
            legend.key.size = unit(1.5, "cm"))+
      xlab("Day")+
      ylab("Mean Ethanol Consumed (g/kg/24hr) +/- S.E.M.")
    drink3.ggp
    
    #close the file
    dev.off()

#MEAN GRAPHS####
  ##Mean Data:####
    #Read in data file with means with auto-headings and blanks/ N/As set to blank ("")
    #Laptop
    mean.myd <- read.csv("C:/Users/Thomas/Google Drive/Grad/Lab/Projects/Phase II Projects - Analysis & Write-up/Masters/Results/Analyses/ABHV2018 TR Means.csv", na.strings="\"\"")
    #Home
    #mean.myd <- read.csv("C:/Users/Kiersten/Google Drive/Grad/Lab/Projects/Phase II Projects - Analysis & Write-up/Masters/Results/Analyses/ABHV2018 TR Means.csv", na.strings="\"\"")
    View(mean.myd)
    
    #Separate into dataframes
    mean.mydetoh <- subset(mean.myd, datafrm == "mean.mydetoh")
    View(mean.mydetoh)
    
    mean.mydsuc <- subset(mean.myd, datafrm == "mean.mydsuc")
    View(mean.mydsuc)
    
    mean.mydh2o <- subset(mean.myd, datafrm == "mean.mydh2o")
    View(mean.mydh2o)
    
  ##Graph Setup (Means)####
    
    #Ethanol
    # generate mean aversive Y break positions
    metoh.avers.ybreaks = c(0,10,20,30,40,50,60,70)
    # and Y labels
    metoh.avers.ylabels = as.character(metoh.avers.ybreaks)
    # generate hedonic Y break positions
    metoh.hed.ybreaks = c(0,25,50,75,100,125,150,175,200,225)
    # The length of the vectors must be the same so the ""s are necessary for this trick.
    metoh.hed.ylabels = as.character(metoh.hed.ybreaks)
    
    #Sucrose
    # generate mean aversive Y break positions
    msuc.avers.ybreaks = c(0,2,4,6,8,10,12,14,16,18,20)
    # and Y labels
    msuc.avers.ylabels = as.character(msuc.avers.ybreaks)
    # generate hedonic Y break positions
    msuc.hed.ybreaks = c(0,25,50,75,100,125,150,175,200,225,250)
    # The length of the vectors must be the same so the ""s are necessary for this trick.
    msuc.hed.ylabels = as.character(msuc.hed.ybreaks)
    
    #Water
    # generate mean aversive Y break positions
    mh2o.avers.ybreaks = c(0,2,4,6,8,10,12,14,16,18,20,22)
    # and Y labels
    mh2o.avers.ylabels = as.character(mh2o.avers.ybreaks)
    # generate hedonic Y break positions
    mh2o.hed.ybreaks = c(0,25,50,75,100,125,150,175)
    # The length of the vectors must be the same so the ""s are necessary for this trick.
    mh2o.hed.ylabels = as.character(mh2o.hed.ybreaks)
    
    
  ##Ethanol####
    ###Hedonics
      #make a new file
      png("Fig 5 Ethanol Hedonic Means.png", width = 800, height = 600)
      #generate plot
      
      m.Ehed.ggp <-ggplot(mean.mydetoh,
                          aes(Concentration,Mean.Hedonic,
                              group=interaction(Age,Condition),
                              col=interaction(Age,Condition),
                              fill=interaction(Age,Condition),
                              linetype=interaction(Age,Condition),
                              shape=interaction(Age,Condition)))+
        geom_point(data=mean.mydetoh,aes(x=Concentration, y=Mean.Hedonic), size=6,alpha=0.75)+
        geom_line(data = mean.mydetoh,
                  aes(x=Concentration, y=Mean.Hedonic), stat = "identity",position = "identity",size=1.5)+
        geom_errorbar(data=mean.mydetoh,
                      aes(ymin=Hlower,ymax=Hupper),width=1,size=1.3,linetype="solid")+
        scale_color_manual("",values=c("#F8C8C8", "#82BFC4","#F8766D","#00BFC4"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
        scale_fill_manual("",values=c("#F8C8C8", "#82BFC4","#F8766D","#00BFC4"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
        scale_linetype_manual("",values=c("dashed","dashed","solid","solid"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
        scale_shape_manual("",values=c(17,18,16,15), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
        scale_x_continuous(expand=c(0,0), limits = c(0,41), breaks=E.xbreaks, labels=E.xlabels)+
        scale_y_continuous(expand=c(0,0), limits = c(0,230), breaks=metoh.hed.ybreaks, labels=metoh.hed.ylabels)+
        theme_classic()+
        theme(strip.background = element_rect(colour="white"),
              strip.text.x = element_text(size=18,face="bold"),
              axis.title = element_text(size=22),
              axis.text = element_text(size=21,color="black",face="bold"),
              axis.line = element_line(size=1.3),
              axis.ticks = element_line(size=1.3, color="black"),
              axis.ticks.length = unit(0.2, "cm"),
              legend.title = element_text(size=20),
              legend.text = element_text(size=18),
              axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)),
              axis.title.x = element_text(margin = margin(t = 13, r = 0, b = 0, l = 0)),
              legend.key.size = unit(1.5, "cm"))+
        xlab("Ethanol % (v/v)")+
        ylab("Mean Hedonic Responses (+/-SEM)")
      m.Ehed.ggp
    
    #close the file
    dev.off()
    
    
    ###Aversives
      #make a new file
      png("Fig 7 Ethanol Aversive Graph (Means).png", width = 800, height = 600)
      #generate plot
      
      m.Eavers.ggp <-ggplot(mean.mydetoh,
                            aes(Concentration,Mean.Aversive,
                                group=interaction(Age,Condition),
                                col=interaction(Age,Condition),
                                fill=interaction(Age,Condition),
                                linetype=interaction(Age,Condition),
                                shape=interaction(Age,Condition)))+
        geom_point(data=mean.mydetoh,aes(x=Concentration, y=Mean.Aversive), size=6,alpha=0.75)+
        geom_line(data = mean.mydetoh,
                  aes(x=Concentration, y=Mean.Aversive), stat = "identity",position = "identity",size=1.5)+
        geom_errorbar(data=mean.mydetoh,
                      aes(ymin=Alower,ymax=Aupper),width=1,size=1.3,linetype="solid")+
        scale_color_manual("",values=c("#F8C8C8", "#82BFC4","#F8766D","#00BFC4"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
        scale_fill_manual("",values=c("#F8C8C8", "#82BFC4","#F8766D","#00BFC4"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
        scale_linetype_manual("",values=c("dashed","dashed","solid","solid"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
        scale_shape_manual("",values=c(17,18,16,15), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
        scale_x_continuous(expand=c(0,0), limits = c(0,41), breaks=E.xbreaks, labels=E.xlabels)+
        scale_y_continuous(expand=c(0,0), limits = c(0,72.5), breaks=metoh.avers.ybreaks, labels=metoh.avers.ylabels)+
        theme_classic()+
        theme(strip.background = element_rect(colour="white"),
              strip.text.x = element_text(size=18,face="bold"),
              axis.title = element_text(size=22),
              axis.text = element_text(size=21,color="black",face="bold"),
              axis.line = element_line(size=1.3),
              axis.ticks = element_line(size=1.3, color="black"),
              axis.ticks.length = unit(0.2, "cm"),
              legend.title = element_text(size=20),
              legend.text = element_text(size=18),
              axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)),
              axis.title.x = element_text(margin = margin(t = 13, r = 0, b = 0, l = 0)),
              legend.key.size = unit(1.5, "cm"))+
        xlab("Ethanol % (v/v)")+
        ylab("Mean Aversive Responses (+/-SEM)")
      m.Eavers.ggp
      
      #close the file
      dev.off()
    
  ###Sucrose#### 
    ###Hedonics
      #make a new file
      png("Fig 9 Sucrose Hedonic Means.png", width = 800, height = 600)
      #generate plot
      
      m.Shed.ggp <-ggplot(mean.mydsuc,
                          aes(Concentration,Mean.Hedonic,
                              group=interaction(Age,Condition),
                              col=interaction(Age,Condition),
                              fill=interaction(Age,Condition),
                              linetype=interaction(Age,Condition),
                              shape=interaction(Age,Condition)))+
        geom_point(data=mean.mydsuc,aes(x=Concentration, y=Mean.Hedonic), size=6,alpha=0.75)+
        geom_line(data = mean.mydsuc,
                  aes(x=Concentration, y=Mean.Hedonic), stat = "identity",position = "identity",size=1.5)+
        geom_errorbar(data=mean.mydsuc,
                      aes(ymin=Hlower,ymax=Hupper),width=0.03,size=1.3,linetype="solid")+
        scale_color_manual("",values=c("#F8C8C8", "#82BFC4","#F8766D","#00BFC4"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
        scale_fill_manual("",values=c("#F8C8C8", "#82BFC4","#F8766D","#00BFC4"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
        scale_linetype_manual("",values=c("dashed","dashed","solid","solid"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
        scale_shape_manual("",values=c(17,18,16,15), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
        scale_x_continuous(breaks=S.xbreaks, labels=S.xlabels)+
        scale_y_continuous(expand=c(0,0), limits = c(0,260), breaks=msuc.hed.ybreaks, labels=msuc.hed.ylabels)+
        theme_classic()+
        theme(strip.background = element_rect(colour="white"),
              strip.text.x = element_text(size=18,face="bold"),
              axis.title = element_text(size=22),
              axis.text = element_text(size=21,color="black",face="bold"),
              axis.line = element_line(size=1.3),
              axis.ticks = element_line(size=1.3, color="black"),
              axis.ticks.length = unit(0.2, "cm"),
              legend.title = element_text(size=20),
              legend.text = element_text(size=18),
              axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)),
              axis.title.x = element_text(margin = margin(t = 13, r = 0, b = 0, l = 0)),
              legend.key.size = unit(1.5, "cm"))+
        xlab("Sucrose Concentration (M)")+
        ylab("Mean Hedonic Responses (+/-SEM)")
      m.Shed.ggp

      #close the file
      dev.off()
    
    
    ###Aversives
      #make a new file
      png("Fig 11 Sucrose Aversive Means.png", width = 800, height = 600)
      #generate plot
      
      m.Savers.ggp <-ggplot(mean.mydsuc,
                            aes(Concentration,Mean.Aversive,
                                group=interaction(Age,Condition),
                                col=interaction(Age,Condition),
                                fill=interaction(Age,Condition),
                                linetype=interaction(Age,Condition),
                                shape=interaction(Age,Condition)))+
        geom_point(data=mean.mydsuc,aes(x=Concentration, y=Mean.Aversive), size=6,alpha=0.75)+
        geom_line(data = mean.mydsuc,
                  aes(x=Concentration, y=Mean.Aversive), stat = "identity",position = "identity",size=1.5)+
        geom_errorbar(data=mean.mydsuc,
                      aes(ymin=Alower,ymax=Aupper),width=0.03,size=1.3,linetype="solid")+
        scale_color_manual("",values=c("#F8C8C8", "#82BFC4","#F8766D","#00BFC4"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
        scale_fill_manual("",values=c("#F8C8C8", "#82BFC4","#F8766D","#00BFC4"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
        scale_linetype_manual("",values=c("dashed","dashed","solid","solid"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
        scale_shape_manual("",values=c(17,18,16,15), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
        scale_x_continuous(breaks=S.xbreaks, labels=S.xlabels)+
        scale_y_continuous(expand=c(0,0), limits = c(0,20.5), breaks=msuc.avers.ybreaks, labels=msuc.avers.ylabels)+
        theme_classic()+
        theme(strip.background = element_rect(colour="white"),
              strip.text.x = element_text(size=18,face="bold"),
              axis.title = element_text(size=22),
              axis.text = element_text(size=21,color="black",face="bold"),
              axis.line = element_line(size=1.3),
              axis.ticks = element_line(size=1.3, color="black"),
              axis.ticks.length = unit(0.2, "cm"),
              legend.title = element_text(size=20),
              legend.text = element_text(size=18),
              axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)),
              axis.title.x = element_text(margin = margin(t = 13, r = 0, b = 0, l = 0)),
              legend.key.size = unit(1.5, "cm"))+
        xlab("Sucrose Concentration (M)")+
        ylab("Mean Aversive Responses (+/-SEM)")
      m.Savers.ggp
      
      #close the file
      dev.off()
    
    
  ###Water####
    ###Hedonics
      #make a new file
      png("Fig 13 Water Hedonic Means.png", width = 800, height = 600)
      #generate plot
      
      m.Hhed.ggp <-ggplot(mean.mydh2o,
                          aes(as.factor(Trial),Mean.Hedonic,
                              group=interaction(Age,Condition),
                              col=interaction(Age,Condition),
                              fill=interaction(Age,Condition),
                              linetype=interaction(Age,Condition),
                              shape=interaction(Age,Condition)))+
        geom_point(data=mean.mydh2o,aes(x=as.factor(Trial), y=Mean.Hedonic), size=6,alpha=0.75)+
        geom_line(data = mean.mydh2o,
                  aes(x=as.factor(Trial), y=Mean.Hedonic), stat = "identity",position = "identity",size=1.5)+
        geom_errorbar(data=mean.mydh2o,
                      aes(ymin=Hlower,ymax=Hupper),width=0.06,size=1.3,linetype="solid")+
        scale_color_manual("",values=c("#F8C8C8", "#82BFC4","#F8766D","#00BFC4"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
        scale_fill_manual("",values=c("#F8C8C8", "#82BFC4","#F8766D","#00BFC4"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
        scale_linetype_manual("",values=c("dashed","dashed","solid","solid"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
        scale_shape_manual("",values=c(17,18,16,15), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
        scale_y_continuous(expand=c(0,0), limits = c(0,180), breaks=mh2o.hed.ybreaks, labels=mh2o.hed.ylabels)+
        theme_classic()+
        theme(strip.background = element_rect(colour="white"),
              strip.text.x = element_text(size=18,face="bold"),
              axis.title = element_text(size=22),
              axis.text = element_text(size=21,color="black",face="bold"),
              axis.line = element_line(size=1.3),
              axis.ticks = element_line(size=1.3, color="black"),
              axis.ticks.length = unit(0.2, "cm"),
              legend.title = element_text(size=20),
              legend.text = element_text(size=18),
              axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)),
              axis.title.x = element_text(margin = margin(t = 13, r = 0, b = 0, l = 0)),
              legend.key.size = unit(1.5, "cm"))+
        xlab("Water Trial")+
        ylab("Mean Hedonic Responses (+/-SEM)")
      m.Hhed.ggp
      
      #close the file
      dev.off()
    
    
    ###Aversives
      #make a new file
      png("Fig 15 Water Aversive Means.png", width = 800, height = 600)
      #generate plot
      
      m.Havers.ggp <-ggplot(mean.mydh2o,
                            aes(as.factor(Trial),Mean.Aversive,
                                group=interaction(Age,Condition),
                                col=interaction(Age,Condition),
                                fill=interaction(Age,Condition),
                                linetype=interaction(Age,Condition),
                                shape=interaction(Age,Condition)))+
        geom_point(data=mean.mydh2o,aes(x=as.factor(Trial), y=Mean.Aversive), size=6,alpha=0.75)+
        geom_line(data = mean.mydh2o,
                  aes(x=as.factor(Trial), y=Mean.Aversive), stat = "identity",position = "identity",size=1.5)+
        geom_errorbar(data=mean.mydh2o,
                      aes(ymin=Alower,ymax=Aupper),width=0.06,size=1.3,linetype="solid")+
        scale_color_manual("",values=c("#F8C8C8", "#82BFC4","#F8766D","#00BFC4"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
        scale_fill_manual("",values=c("#F8C8C8", "#82BFC4","#F8766D","#00BFC4"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
        scale_linetype_manual("",values=c("dashed","dashed","solid","solid"), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
        scale_shape_manual("",values=c(17,18,16,15), labels=c('Adolescent+CTRL','Adult+CTRL','Adolescent+IAE','Adult+IAE'))+
        scale_y_continuous(expand=c(0,0), limits = c(0,23), breaks=mh2o.avers.ybreaks, labels=mh2o.avers.ylabels)+
        theme_classic()+
        theme(strip.background = element_rect(colour="white"),
              strip.text.x = element_text(size=18,face="bold"),
              axis.title = element_text(size=22),
              axis.text = element_text(size=21,color="black",face="bold"),
              axis.line = element_line(size=1.3),
              axis.ticks = element_line(size=1.3, color="black"),
              axis.ticks.length = unit(0.2, "cm"),
              legend.title = element_text(size=20),
              legend.text = element_text(size=18),
              axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)),
              axis.title.x = element_text(margin = margin(t = 13, r = 0, b = 0, l = 0)),
              legend.key.size = unit(1.5, "cm"))+
        xlab("Water Trial")+
        ylab("Mean Aversive Responses (+/-SEM)")
      m.Havers.ggp
      
      #close the file
      dev.off()
    