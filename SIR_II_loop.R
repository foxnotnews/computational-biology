library("readr")
library("ggplot2")
library("tidyverse")
library("reshape2")
library("plotly")
library("RColorBrewer")
library("deSolve")
library("gridExtra")

## Create an SIR function
sir <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I-dv*S
    dI <-  beta * S * I - gamma * I
    dR <-                 gamma * I+dv*S
    
    return(list(c(dS, dI, dR)))
  })
}

### Set parameters
## Proportion in each compartment
init<- c(S = 1-1e-6, I = 1e-6, R = 0.0)



## Time frame
times      <- seq(0, 200, by = 0.01)

#vaccination rate
dv=seq(0,0.045, length.out=6)

plots=list()
for (n in 1:length(dv)){
  
  ## beta: infection parameter; gamma: recovery parameter
  parameters <- c(beta = 1.5, gamma = 0.5 ,dv=dv[n])
  ## Solve using ode (General Solver for Ordinary Differential Equations)
  out <- ode(y = init, times = times, func = sir, parms = parameters)
  
  ## change to data frame
  out <- as.data.frame(out)
  
  head(out)
  
  
  names(out) = c("Time","Susceptible","Infected","Removed")
  
  
  dat.SIR = melt(out,id="Time",measure = c("Susceptible","Infected","Removed"))
  
  head(dat.SIR)
  names(dat.SIR) = c("Time","Compartment","Value")
  vaccination_rate= round(dv[n]*100,2)
  
  
  pp1 = ggplot(dat.SIR) +
    geom_line(aes(x = Time,y = Value,color=Compartment),size=1.2) +
    theme_minimal()+
    xlab ("Time")  +
    ylab("Proportion of Population")+
    theme_classic() + 
    theme(text = element_text(size = 20)) +
    ylim(0,1)+ 
    scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
    ggtitle(paste(vaccination_rate, "% Vaccinated by unit \n  of time"))+
    theme(plot.title = element_text(size = 20, face = "bold"))

  plots[[length(plots)+1]]=pp1
  
}

grid.arrange(grobs=plots,ncol=3)
