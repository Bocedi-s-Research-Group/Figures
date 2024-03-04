##
setwd("D:\\PredPrey_outputs")

library(ggplot2)
library(plyr)
# library(lattice)
# library(latticeExtra)
# library(HH)

newtheme <- theme_bw() + 
  theme(
    line =               element_line(colour = "black", size = 0.5, linetype = 1,
                                      lineend = "butt"),
    rect =               element_rect(fill = "white", colour = "black", size = 1, linetype = 1),
    text =               element_text(family = "sans", face = "plain",
                                      colour = "black", size = 12 ),
    
    axis.text =          element_text(size = 30, colour = "black"),
    strip.text =         element_text(size = 35, colour = "black"),
    
    axis.line =          element_line(colour = "black", size = 0.5),
    axis.text.x =        element_text(size=30, vjust = 1),
    axis.text.y =        element_text(size=30, hjust = 1),
    axis.title =         element_text(colour = "black"),
    axis.title.x =       element_text(size = 35,vjust = 1, margin = margin(10,0,0,0,"pt")),
    axis.title.y =       element_text(size = 35,angle = 90, margin = margin(0,10,0,0,"pt")),
    
    legend.background =  element_rect(colour = NA),
    legend.key =         element_rect(fill = "black", colour = "white"),
     legend.key.size = unit(2.5, "cm"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        element_text(size = 25, colour = "black"),
    legend.text.align =  NULL,
    legend.title =       element_text(size = 25, hjust = 0, colour = "black"),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   "vertical",
    legend.justification = "center",
    legend.box =         NULL,
    
    panel.background =   element_rect(fill = NA, colour = NA),
    panel.border =       element_rect(fill = NA, colour = "black"),
    panel.grid.major =   element_blank(),
    panel.grid.minor =   element_blank(),
    
    strip.background =   element_rect(fill = "grey", colour = "grey10"),
    strip.text.x =       element_text(size=35,face="bold",margin = margin(6,0,6,0,"pt")),
    strip.text.y =       element_text(size=35,face="bold",angle = -90,margin = margin(0,6,0,6,"pt")),
    
    plot.background =    element_rect(colour = NA, fill = NA)
  )



#####################################################################
##Fig8A-D
pop1 <- read.table("Sim604_Pops.txt", header=T) #def 1, pred 100, prey 100 
pop2 <- read.table("Sim608_Pops.txt", header=T) #def evolving, pred 100, prey 100
pop3 <- read.table("Sim605_Pops.txt", header=T) #def 1, pred 100, prey 400
pop4 <- read.table("Sim609_Pops.txt", header=T) #def evolving, pred 100, prey 400


##Fig8E-H ###################################
pop1 <- read.table("Sim606_Pops.txt", header=T)  #def 1, pred 400, prey 100 
pop2 <- read.table("Sim625_Pops.txt", header=T)  #def evolving, pred 400, prey 100
pop3 <- read.table("Sim607_Pops.txt", header=T)  #def 1, pred 400, prey 400
pop4 <- read.table("Sim626_Pops.txt", header=T)  #def evolving, pred 100, prey 400

pop1a <- read.table("Sim6061_Pops.txt", header=T)  
pop2a <- read.table("Sim6251_Pops.txt", header=T)  
pop3a <- read.table("Sim6071_Pops.txt", header=T)  
pop4a <- read.table("Sim6261_Pops.txt", header=T) 

pop1b <- read.table("Sim6062_Pops.txt", header=T)  
pop2b <- read.table("Sim6252_Pops.txt", header=T)  
pop3b <- read.table("Sim6072_Pops.txt", header=T)  
pop4b <- read.table("Sim6262_Pops.txt", header=T) 

pop1c <- read.table("Sim6063_Pops.txt", header=T)  
pop2c <- read.table("Sim6253_Pops.txt", header=T)  
pop3c <- read.table("Sim6073_Pops.txt", header=T)  
pop4c <- read.table("Sim6263_Pops.txt", header=T) 

pop1a$rep <- pop1a$rep + max(pop1$rep)  + 1
pop2a$rep <- pop2a$rep + max(pop2$rep)  + 1
pop3a$rep <- pop3a$rep + max(pop3$rep)  + 1
pop4a$rep <- pop4a$rep + max(pop4$rep)  + 1

pop1b$rep <- pop1b$rep + max(pop1a$rep)  + 1
pop2b$rep <- pop2b$rep + max(pop2a$rep)  + 1
pop3b$rep <- pop3b$rep + max(pop3a$rep)  + 1
pop4b$rep <- pop4b$rep + max(pop4a$rep)  + 1

pop1c$rep <- pop1c$rep + max(pop1b$rep)  + 1
pop2c$rep <- pop2c$rep + max(pop2b$rep)  + 1
pop3c$rep <- pop3c$rep + max(pop3b$rep)  + 1
pop4c$rep <- pop4c$rep + max(pop4b$rep)  + 1

pop1 <- rbind(pop1, pop1a, pop1b, pop1c)
pop2 <- rbind(pop2, pop2a, pop2b, pop2c)
pop3 <- rbind(pop3, pop3a, pop3b, pop3c)
pop4 <- rbind(pop4, pop4a, pop4b, pop4c)


####################

pop1b <- ddply(pop1, .(rep, gen, y), summarise, popsize = mean(N1),preysize = mean(N0))
pop1c <- ddply(pop1b, .(gen, y), summarise, pred = mean(popsize),prey = mean(preysize))

pop2b <- ddply(pop2, .(rep, gen, y), summarise, popsize = mean(N1),preysize = mean(N0))
pop2c <- ddply(pop2b, .(gen, y), summarise, pred = mean(popsize),prey = mean(preysize))

pop3b <- ddply(pop3, .(rep, gen, y), summarise, popsize = mean(N1),preysize = mean(N0))
pop3c <- ddply(pop3b, .(gen, y), summarise, pred = mean(popsize),prey = mean(preysize))

pop4b <- ddply(pop4, .(rep, gen, y), summarise, popsize = mean(N1),preysize = mean(N0))
pop4c <- ddply(pop4b, .(gen, y), summarise, pred = mean(popsize),prey = mean(preysize))



pop1c <- pop1c[pop1c$gen > 0 & pop1c$pred > 0,]
pop1c$gen <- as.factor(pop1c$gen)

pop2c <- pop2c[pop2c$gen > 0 & pop2c$pred > 0,]
pop2c$gen <- as.factor(pop2c$gen)

pop3c <- pop3c[pop3c$gen > 0 & pop3c$pred > 0,]
pop3c$gen <- as.factor(pop3c$gen)

pop4c <- pop4c[pop4c$gen > 0 & pop4c$pred > 0,]
pop4c$gen <- as.factor(pop4c$gen)

##Fig8A-D
temp <- expression(paste(Delta[prey] == 100, "m"))
temp2 <- expression(paste(Delta[predator] == 100, "m"))

ggplot(pop1c, aes(gen, y)) + newtheme+
  geom_raster(aes(fill = pred)) +
  scale_fill_gradientn(colours = rainbow(20), limits = c(0,13)) +
  scale_x_discrete(labels = c("", "", "","","500","", "", "","","1000","", "", "","","1500","", "", "","","2000","", "", "","","2500","", "", "","","2600","", "", "","","","", "", "3000","", "", "","","3500","", "", "","","4000","", "", "","","4500","", "", "","","5000","")) +
  scale_y_continuous(limits=c(20,230)) +
  labs(fill= "Mean\npredator\nabundance", x="Years") +
  geom_vline(xintercept = 25, linetype = 2, size = 1.2) +
  geom_vline(xintercept = 30, linetype = 2, size = 1.2) +
  annotate("text",x = 4, y = 230, label = "(A)", size=15) + 
  annotate("text",x = 12, y = 209, parse=T, label = as.character(temp), size=15)+
  annotate("text",x = 13, y = 189, parse=T, label = as.character(temp2), size=15)


ggplot(pop2c, aes(gen, y)) + newtheme+
  geom_raster(aes(fill = pred)) +
  scale_fill_gradientn(colours = rainbow(20), limits = c(0,13)) +
  scale_x_discrete(labels = c("", "", "","","500","", "", "","","1000","", "", "","","1500","", "", "","","2000","", "", "","","2500","", "", "","","2600","", "", "","","","", "", "3000","", "", "","","3500","", "", "","","4000","", "", "","","4500","", "", "","","5000","")) +
  scale_y_continuous(limits=c(20,230)) +
  labs(fill= "Mean\npredator\nabundance", x="Years") +
  geom_vline(xintercept = 25, linetype = 2, size = 1.2) +
  geom_vline(xintercept = 30, linetype = 2, size = 1.2) +
  annotate("text",x = 4, y = 230, label = "(B)", size=15) + 
  annotate("text",x = 12, y = 209, parse=T, label = as.character(temp), size=15)+
  annotate("text",x = 13, y = 189, parse=T, label = as.character(temp2), size=15)


###
temp <- expression(paste(Delta[prey] == 400, "m"))
temp2 <- expression(paste(Delta[predator] == 100, "m"))

ggplot(pop3c, aes(gen, y)) + newtheme+
  geom_raster(aes(fill = pred)) +
  scale_fill_gradientn(colours = rainbow(20), limits = c(0,13)) +
  scale_x_discrete(labels = c("", "", "","","500","", "", "","","1000","", "", "","","1500","", "", "","","2000","", "", "","","2500","", "", "","","2600","", "", "","","","", "", "3000","", "", "","","3500","", "", "","","4000","", "", "","","4500","", "", "","","5000","")) +
  scale_y_continuous(limits=c(20,230)) +
  labs(fill= "Mean\npredator\nabundance", x="Years") +
  geom_vline(xintercept = 25, linetype = 2, size = 1.2) +
  geom_vline(xintercept = 30, linetype = 2, size = 1.2) +
  annotate("text",x = 4, y = 230, label = "(C)", size=15) + 
  annotate("text",x = 12, y = 209, parse=T, label = as.character(temp), size=15)+
  annotate("text",x = 13, y = 189, parse=T, label = as.character(temp2), size=15)


ggplot(pop4c, aes(gen, y)) + newtheme+
  geom_raster(aes(fill = pred)) +
  scale_fill_gradientn(colours = rainbow(20), limits = c(0,13)) +
  scale_x_discrete(labels = c("", "", "","","500","", "", "","","1000","", "", "","","1500","", "", "","","2000","", "", "","","2500","", "", "","","2600","", "", "","","","", "", "3000","", "", "","","3500","", "", "","","4000","", "", "","","4500","", "", "","","5000","")) +
  scale_y_continuous(limits=c(20,230)) +
  labs(fill= "Mean\npredator\nabundance", x="Years") +
  geom_vline(xintercept = 25, linetype = 2, size = 1.2) +
  geom_vline(xintercept = 30, linetype = 2, size = 1.2) +
  annotate("text",x = 4, y = 230, label = "(D)", size=15) + 
  annotate("text",x = 12, y = 209, parse=T, label = as.character(temp), size=15)+
  annotate("text",x = 13, y = 189, parse=T, label = as.character(temp2), size=15)


##Fig8E-H ------------------------------------
temp <- expression(paste(Delta[prey] == 100, "m"))
temp2 <- expression(paste(Delta[predator] == 400, "m"))

ggplot(pop1c, aes(gen, y)) + newtheme+
  geom_raster(aes(fill = pred)) +
  scale_fill_gradientn(colours = rainbow(20), limits = c(0,13)) +
  scale_x_discrete(labels = c("", "", "","","500","", "", "","","1000","", "", "","","1500","", "", "","","2000","", "", "","","2500","", "", "","","2600","", "", "","","","", "", "3000","", "", "","","3500","", "", "","","4000","", "", "","","4500","", "", "","","5000","")) +
  scale_y_continuous(limits=c(20,230)) +
  labs(fill= "Mean\npredator\nabundance", x="Years") +
  geom_vline(xintercept = 25, linetype = 2, size = 1.2) +
  geom_vline(xintercept = 30, linetype = 2, size = 1.2) +
  annotate("text",x = 4, y = 230, label = "(E)", size=15) + 
  annotate("text",x = 12, y = 209, parse=T, label = as.character(temp), size=15)+
  annotate("text",x = 13, y = 189, parse=T, label = as.character(temp2), size=15)



ggplot(pop2c, aes(gen, y)) + newtheme+
  geom_raster(aes(fill = pred)) +
  scale_fill_gradientn(colours = rainbow(20), limits = c(0,13)) +
  scale_x_discrete(labels = c("", "", "","","500","", "", "","","1000","", "", "","","1500","", "", "","","2000","", "", "","","2500","", "", "","","2600","", "", "","","","", "", "3000","", "", "","","3500","", "", "","","4000","", "", "","","4500","", "", "","","5000","")) +
  labs(fill= "Mean\npredator\nabundance", x="Years") +
  scale_y_continuous(limits=c(20,230)) +
  geom_vline(xintercept = 25, linetype = 2, size = 1.2) +
  geom_vline(xintercept = 30, linetype = 2, size = 1.2) +
  annotate("text",x = 4, y = 230, label = "(F)", size=15) + 
  annotate("text",x = 12, y = 209, parse=T, label = as.character(temp), size=15)+
  annotate("text",x = 13, y = 189, parse=T, label = as.character(temp2), size=15)

####
temp <- expression(paste(Delta[prey] == 400, "m"))
temp2 <- expression(paste(Delta[predator] == 400, "m"))

ggplot(pop3c, aes(gen, y)) + newtheme+
  geom_raster(aes(fill = pred)) +
  scale_fill_gradientn(colours = rainbow(20), limits = c(0,13)) +
  scale_x_discrete(labels = c("", "", "","","500","", "", "","","1000","", "", "","","1500","", "", "","","2000","", "", "","","2500","", "", "","","2600","", "", "","","","", "", "3000","", "", "","","3500","", "", "","","4000","", "", "","","4500","", "", "","","5000","")) +
  labs(fill= "Mean\npredator\nabundance", x="Years") +
  scale_y_continuous(limits=c(20,230)) +
  geom_vline(xintercept = 25, linetype = 2, size = 1.2) +
  geom_vline(xintercept = 30, linetype = 2, size = 1.2) +
  annotate("text",x = 4, y = 230, label = "(G)", size=15) + 
  annotate("text",x = 12, y = 209, parse=T, label = as.character(temp), size=15)+
  annotate("text",x = 13, y = 189, parse=T, label = as.character(temp2), size=15)


ggplot(pop4c, aes(gen, y)) + newtheme+
  geom_raster(aes(fill = pred)) +
  scale_fill_gradientn(colours = rainbow(20), limits = c(0,13)) +
  scale_x_discrete(labels = c("", "", "","","500","", "", "","","1000","", "", "","","1500","", "", "","","2000","", "", "","","2500","", "", "","","2600","", "", "","","","", "", "3000","", "", "","","3500","", "", "","","4000","", "", "","","4500","", "", "","","5000","")) +
  scale_y_continuous(limits=c(20,230)) +
  labs(fill= "Mean\npredator\nabundance", x="Years") +
  geom_vline(xintercept = 25, linetype = 2, size = 1.2) +
  geom_vline(xintercept = 30, linetype = 2, size = 1.2) +
  annotate("text",x = 4, y = 230, label = "(H)", size=15) + 
  annotate("text",x = 12, y = 209, parse=T, label = as.character(temp), size=15)+
  annotate("text",x = 13, y = 189, parse=T, label = as.character(temp2), size=15)





################################################################
## Alternative Fig8 with fix defense  == 0.5
#####################################################################
##Fig8A-D
pop1 <- read.table("Sim203-223\\Sim204_Pops.txt", header=T) #def 0.5, pred 100, prey 100 
pop3 <- read.table("Sim117-125\\Sim121_Pops.txt", header=T) #def 0.5, pred 100, prey 400

##Fig8E-H
pop1 <- read.table("Sim30-53\\Sim37_Pops.txt", header=T)  #def 0.5, pred 400, prey 100 
pop3 <- read.table("Sim30-53\\Sim32_Pops.txt", header=T)  #def 0.5, pred 400, prey 400



pop1b <- ddply(pop1, .(rep, gen, y), summarise, popsize = mean(N1),preysize = mean(N0))
pop1c <- ddply(pop1b, .(gen, y), summarise, pred = mean(popsize),prey = mean(preysize))

pop3b <- ddply(pop3, .(rep, gen, y), summarise, popsize = mean(N1),preysize = mean(N0))
pop3c <- ddply(pop3b, .(gen, y), summarise, pred = mean(popsize),prey = mean(preysize))

pop1c <- pop1c[pop1c$gen > 0 & pop1c$pred > 0,]
pop1c$gen <- as.factor(pop1c$gen)

pop3c <- pop3c[pop3c$gen > 0 & pop3c$pred > 0,]
pop3c$gen <- as.factor(pop3c$gen)


##Fig8A-D
ggplot(pop1c, aes(gen, y)) + theme_white()+
  geom_raster(aes(fill = pred)) +
  scale_fill_gradientn(colours = rainbow(20), limits = c(0,10)) +
  scale_x_discrete(labels = c("", "", "","","500","", "", "","","1000","", "", "","","1500","", "", "","","2000","", "", "","","2500","", "", "","","2600","", "", "","","2700","", "", "3000","", "", "","","3500","", "", "","","4000","", "", "","","4500","", "", "","","5000","")) +
  scale_y_continuous(limits=c(20,230)) +
  labs(fill= "Mean\npredator\nabundance", x="Years") +
  geom_vline(xintercept = 25, linetype = 2, size = 1.2) +
  geom_vline(xintercept = 30, linetype = 2, size = 1.2) +
  annotate("text",x = 3, y = 230, label = "A", size=8)


ggplot(pop3c, aes(gen, y)) + theme_white()+
  geom_raster(aes(fill = pred)) +
  scale_fill_gradientn(colours = rainbow(20), limits = c(0,10)) +
  scale_x_discrete(labels = c("", "", "","","500","", "", "","","1000","", "", "","","1500","", "", "","","2000","", "", "","","2500","", "", "","","2600","", "", "","","2700","", "", "3000","", "", "","","3500","", "", "","","4000","", "", "","","4500","", "", "","","5000","")) +
  scale_y_continuous(limits=c(20,230)) +
  labs(fill= "Mean\npredator\nabundance", x="Years") +
  geom_vline(xintercept = 25, linetype = 2, size = 1.2) +
  geom_vline(xintercept = 30, linetype = 2, size = 1.2) +
  annotate("text",x = 3, y = 230, label = "C", size=8)

ggplot(pop4c, aes(gen, y)) + theme_white()+
  geom_raster(aes(fill = pred)) +
  scale_fill_gradientn(colours = rainbow(20), limits = c(0,10)) +
  scale_x_discrete(labels = c("", "", "","","500","", "", "","","1000","", "", "","","1500","", "", "","","2000","", "", "","","2500","", "", "","","2600","", "", "","","2700","", "", "3000","", "", "","","3500","", "", "","","4000","", "", "","","4500","", "", "","","5000","")) +
  scale_y_continuous(limits=c(20,230)) +
  labs(fill= "Mean\npredator\nabundance", x="Years") +
  geom_vline(xintercept = 25, linetype = 2, size = 1.2) +
  geom_vline(xintercept = 30, linetype = 2, size = 1.2) +
  annotate("text",x = 3, y = 230, label = "D", size=8)

##Fig8E-H ------------------------------------
ggplot(pop1c, aes(gen, y)) + theme_white()+
  geom_raster(aes(fill = pred)) +
  scale_fill_gradientn(colours = rainbow(20), limits = c(0,10)) +
  scale_x_discrete(labels = c("", "", "","","500","", "", "","","1000","", "", "","","1500","", "", "","","2000","", "", "","","2500","", "", "","","2600","", "", "","","2700","", "", "3000","", "", "","","3500","", "", "","","4000","", "", "","","4500","", "", "","","5000","")) +
  scale_y_continuous(limits=c(20,230)) +
  labs(fill= "Mean\npredator\nabundance", x="Years") +
  geom_vline(xintercept = 25, linetype = 2, size = 1.2) +
  geom_vline(xintercept = 30, linetype = 2, size = 1.2) +
  annotate("text",x = 2, y = 230, label = "E", size=8)



ggplot(pop3c, aes(gen, y)) + theme_white()+
  geom_raster(aes(fill = pred)) +
  scale_fill_gradientn(colours = rainbow(20), limits = c(0,10)) +
  scale_x_discrete(labels = c("", "", "","","500","", "", "","","1000","", "", "","","1500","", "", "","","2000","", "", "","","2500","", "", "","","2600","", "", "","","2700","", "", "3000","", "", "","","3500","", "", "","","4000","", "", "","","4500","", "", "","","5000","")) +
  labs(fill= "Mean\npredator\nabundance", x="Years") +
  scale_y_continuous(limits=c(20,230)) +
  geom_vline(xintercept = 25, linetype = 2, size = 1.2) +
  geom_vline(xintercept = 30, linetype = 2, size = 1.2) +
  annotate("text",x = 2, y = 230, label = "G", size=8)
