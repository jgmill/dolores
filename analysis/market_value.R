
library(foreign)
setwd("C:/Users/Lenovo/Documents/GitHub/dolores/analysis")

dat.market <- read.csv("market_value.csv", header = TRUE)

# Calculate market value for each scenario

calc.market.value = function(col.gen){
  
  market.value <-list()
  
  for (i in 2:18){
  
  mean.price        <- mean(dat.market[,i])
  earnings          <- sum(dat.market[,col.gen]*dat.market[,i])
  av.rev            <- earnings / sum(dat.market[,col.gen])
  market.value[[i]] <- av.rev / mean.price
  
  }
  
  return(market.value)
  
  
}

pv1 <- calc.market.value(col.gen = 20)
pv2 <- calc.market.value(col.gen = 21)
pv3 <- calc.market.value(col.gen = 22)
pv4 <- calc.market.value(col.gen = 23)
pv5 <- calc.market.value(col.gen = 24)
pv6 <- calc.market.value(col.gen = 25)

wind1 <- calc.market.value(col.gen = 27)
wind2 <- calc.market.value(col.gen = 28)
wind3 <- calc.market.value(col.gen = 29)
wind4 <- calc.market.value(col.gen = 30)
wind5 <- calc.market.value(col.gen = 31)
wind6 <- calc.market.value(col.gen = 32)

# Save in dataframe
pv.dat <- data.frame(rbind(unlist(pv1), unlist(pv2), unlist(pv3),
              unlist(pv4), unlist(pv5), unlist(pv6)))
colnames(pv.dat) <- c(seq.int(from = 20, to = 100, by = 5))
pv.dat$region <- as.factor(1:6)


wind.dat <-data.frame(rbind(unlist(wind1), unlist(wind2), unlist(wind3),
                unlist(wind4), unlist(wind5), unlist(wind6)))
colnames(wind.dat) <- c(seq.int(from = 20, to = 100, by = 5))
wind.dat$region <- as.factor(1:6)


library(ggplot2)
library(reshape2)

wind           <- melt(wind.dat, id.vars="region")
colnames(wind) <- c("region", "RES_share", "Value_Factor")

ggplot(wind, aes(y = Value_Factor, x=RES_share, group = region, colour = region )) + 
  ggtitle("Wind value factor per region") +
  geom_point(size = 4) + 
  stat_smooth() 


colnames(wind.dat) <- c(seq.int(from = 20, to = 100, by = 5))
wind.dat$region <- as.factor(1:6)


library(ggplot2)
library(reshape2)

pv           <- melt(pv.dat, id.vars="region")
colnames(pv) <- c("region", "RES_share", "Value_Factor")

ggplot(pv, aes(y = Value_Factor, x=RES_share, group = region, colour = region )) + 
  geom_point(size = 4) + 
  ggtitle("PV value factor per region") +
  stat_smooth() 

ggplot(pv, aes(region,Value_Factor)) + 
  geom_point() + 
  stat_smooth() +
  facet_wrap(~RES_share)

