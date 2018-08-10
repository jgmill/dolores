# Installting packages 
# See documentation for installing gdx: https://github.com/pik-piam/gdx
library(readxl)
library("dplyr")
library('gdx')
library('ggplot2')


###############################################################
################ SET IMPORT SETTINGS ##########################
###############################################################

# set scenario:
#              noCap
#              EqualCap
#              mincap
#              XXXXXX

stscen <- "mincap"

# set the amount of regions to import:
#                                     1, 2, 5, 6 , 8, 10, all

regimport <- "8"


###############################################################
###############################################################
# Reuslts Import
###############################################################
###############################################################

setwd('/Users/andrew/Documents/School/EnergyInformatics/DELORES.B1/dolores/model/results')
#setwd('~/dolores/model/results')


# Import all results

d.rrsh = NULL 

RegNum <- c("2C","5F","6G","8I","10K")
cname1 <- c("type1","rshr","type2","hour","val")
cname2 <- c("type1","rshr","type2","val")
cname3 <- c("type1","rshr","type2","region","val")

tempRsh <- seq(from = 20, to = 100, by = 5)
tempType <- rep(0, length(tempRsh))
tempValue <- rep(0, length(tempRsh))
tempReg <- rep(0, length(tempRsh))

temp6 <- data.frame(tempReg,tempType,tempRsh,tempValue)
colnames(temp6) <- c("NRegions","Type","ReShr","Value")
temp7 <- data.frame(tempReg,tempType,tempRsh,tempValue)
colnames(temp7) <- c("NRegions","Type","ReShr","Value")
temp8 <- data.frame(tempReg,tempType,tempRsh,tempValue)
colnames(temp8) <- c("NRegions","Type","ReShr","Value")
temp9 <- data.frame(tempReg,tempType,tempRsh,tempValue)
colnames(temp9) <- c("NRegions","Type","ReShr","Value")
temp10 <- data.frame(tempReg,tempType,tempRsh,tempValue)
colnames(temp10) <- c("NRegions","Type","ReShr","Value")
temp11 <- data.frame(tempReg,tempType,tempRsh,tempValue)
colnames(temp11) <- c("NRegions","Type","ReShr","Value")
temp12 <- data.frame(tempReg,tempType,tempRsh,tempValue)
colnames(temp12) <- c("NRegions","Type","ReShr","Value")
temp13 <- data.frame(tempReg,tempType,tempRsh,tempValue)
colnames(temp13) <- c("NRegions","Type","ReShr","Value")


for(i in RegNum)
  {
     # Load in Demand and Res Gen 
  
     tempin1 <- as.data.frame(readGDX(paste0("Germany",i,"_",stscen,"_results"), name= 'report_hours'))
     tempin1 <- tempin1[,c(-1,-2,-3)]
     colnames(tempin1) <- cname1
     

     
     #  load in storage capacity
     
     tempin2 <- as.data.frame(readGDX(paste0("Germany",i,"_",stscen,"_results"), name= 'report_tech'))
     tempin2 <- tempin2[,c(-1,-2,-3)]
     colnames(tempin2) <- cname2
     
     # load in capactiy + curtailment 
     
     tempin3 <- as.data.frame(readGDX(paste0("Germany",i,"_",stscen,"_results"), name= 'report_tech_r'))
     tempin3 <- tempin3[,c(-1,-2,-3)]
     colnames(tempin3) <- cname3
     
     ii <- 1
     
     for(j in tempRsh)
           { 
             
             
             RegN <- (strsplit(i,"")[[1]][1])
             
             if((strsplit(i,"")[[1]][2]) == "0")
             {RegN = "10"}
             
             temp6[ii,1] <- RegN
             temp7[ii,1] <- RegN
             temp8[ii,1] <- RegN
             temp9[ii,1] <- RegN
             temp10[ii,1] <- RegN
             temp11[ii,1] <- RegN
             temp12[ii,1] <- RegN
             temp13[ii,1] <- RegN
             
             
             # max demand & renewable gen without curtailment
             
             temp1Rsh <- filter(tempin1, rshr == as.character(j))
             tempD <- filter(temp1Rsh, type1 == "demand", type2 == "demand")
             DD <- sum(tempD$val)
             tempGC <- filter(temp1Rsh,type1 == "generation conventional", type2 == "demand")
             GC <- sum(tempGC$val)
             tempR <- filter(temp1Rsh,type1 == "residual load (before curt and sto)", type2 == "demand")
              #not sure if RL should be muliplied by 1000 or not
             RL <- sum(tempR$val)
             temp7[ii,2] <- "DemandMax"
             temp7[ii,4] <- max(tempD$val)
             temp6[ii,4] <- DD-RL-GC
             temp6[ii,2] <- "ResGen no curtailment"
             
             # storage capacity
             
             temp2Rsh <- filter(tempin2, rshr == as.character(j))
             tempSto1  <- filter(temp2Rsh, type1 == "capacities storage GWh", type2 == "storage")
             temp8[ii,2] <- "Storage Gwh"
             temp8[ii,4] <- tempSto1$val
             tempSto2  <- filter(temp2Rsh, type1 == "capacities storage GW", type2 == "storage")
             temp9[ii,2] <- "Storage GW"
             temp9[ii,4] <- tempSto2$val
             
             # absolute curtailment
             
             temp3Rsh <- filter(tempin3, rshr == as.character(j))
             tempAbCur <- filter(temp3Rsh, type1 == "renewables curtailed absolute")
             AC <- sum(tempAbCur$val)
             temp10[ii,2] <- "Absolute Curtailment"
             temp10[ii,4] <- AC
             
             # solar capacity

             tempSol <- filter(temp3Rsh, type1 == "capacities renewable GW", type2 == "solar")
             TS <- sum(tempSol$val)
             temp11[ii,2] <- "Total Solar Capacity"
             temp11[ii,4] <- TS
             
             # wind capacity 
             
             tempWD <- filter(temp3Rsh, type1 == "capacities renewable GW", type2 == "wind")
             TW <- sum(tempWD$val)
             temp12[ii,2] <- "Total Wind Capacity"
             temp12[ii,4] <- TW
             
             # relative curtailment 
             
             temp13[ii,4] <- AC/(DD-RL)
             temp13[ii,2] <- "Relative Curtailment"
             
             ii <- ii + 1
     
           }
     
     d.rrsh <- rbind(d.rrsh, temp7,temp8,temp9,temp10,temp11,temp12,temp13)
     
     
     
     
  }

###########################################################
# Import Schill-Zerrahn Results
###########################################################

    tempin1 <- as.data.frame(readGDX(paste0("Germany_original_results"), name= 'report_hours'))
    tempin1 <- tempin1[,c(-1,-2,-3)]
    colnames(tempin1) <- cname1
    
    tempin2 <- as.data.frame(readGDX(paste0("Germany_original_results"), name= 'report_tech'))
    tempin2 <- tempin2[,c(-1,-2,-3)]
    colnames(tempin2) <- cname2

##########################################################
#### if using the Wind/Solar no region results ###########
#####     be sure change the switch below to   ###########
#####                  1 else 0                ###########
##########################################################
# They will be set as region 0 
    
resWS <- 1 
ii <- 1
    
    if(resWS == 1)
    {
    
          for(j in tempRsh)
          { 
            
            
            temp6[ii,1] <- "0"
            temp7[ii,1] <- "0"
            temp8[ii,1] <- "0"
            temp9[ii,1] <- "0"
            temp10[ii,1] <- "0"
            temp11[ii,1] <- "0"
            temp12[ii,1] <- "0"
            temp13[ii,1] <- "0"
            
            
            # max demand & renewable gen without curtailment
            
            temp1Rsh <- filter(tempin1, rshr == as.character(j))
            tempD <- filter(temp1Rsh, type1 == "demand", type2 == "demand")
            DD <- sum(tempD$val)
            tempGC <- filter(temp1Rsh,type1 == "generation conventional", type2 == "demand")
            GC <- sum(tempGC$val)
            tempR <- filter(temp1Rsh,type1 == "residual load (before curt and sto)", type2 == "demand")
            #not sure if RL should be muliplied by 1000 or not
            RL <- sum(tempR$val)
            temp7[ii,2] <- "DemandMax"
            temp7[ii,4] <- max(tempD$val)
            temp6[ii,4] <- DD-RL-GC
            temp6[ii,2] <- "ResGen no curtailment"
            
            # storage capacity
            
            temp2Rsh <- filter(tempin2, rshr == as.character(j))
            tempSto1  <- filter(temp2Rsh, type1 == "capacities storage GWh", type2 == "storage")
            temp8[ii,2] <- "Storage Gwh"
            temp8[ii,4] <- tempSto1$val
            tempSto2  <- filter(temp2Rsh, type1 == "capacities storage GW", type2 == "storage")
            temp9[ii,2] <- "Storage GW"
            temp9[ii,4] <- tempSto2$val
            
            # absolute curtailment
            
            tempAbCur <- filter(temp2Rsh, type1 == "renewables curtailed absolute")
            AC <- sum(tempAbCur$val)
            temp10[ii,2] <- "Absolute Curtailment"
            temp10[ii,4] <- AC
            
            # solar capacity
            
            tempSol <- filter(temp2Rsh, type1 == "capacities renewable GW", type2 == "solar")
            TS <- sum(tempSol$val)
            temp11[ii,2] <- "Total Solar Capacity"
            temp11[ii,4] <- TS
            
            # wind capacity 
       
            tempWD <- filter(temp2Rsh, type1 == "capacities renewable GW", type2 == "wind")
            TW <- sum(tempWD$val)
            temp12[ii,2] <- "Total Wind Capacity"
            temp12[ii,4] <- TW
            
            # relative curtailment 
            
            temp13[ii,4] <- AC/(DD-RL)
            temp13[ii,2] <- "Relative Curtailment"
            
            ii <- ii + 1
            
          }
      d.rrsh <- rbind(d.rrsh, temp7,temp8,temp9,temp10,temp11,temp12,temp13)
      
      
    }else
      {
        for(j in tempRsh)
        { 
          
          
          temp6[ii,1] <- "0"
          temp7[ii,1] <- "0"
          temp8[ii,1] <- "0"
          temp9[ii,1] <- "0"
          temp10[ii,1] <- "0"
          temp13[ii,1] <- "0"
          
          
          # max demand & renewable gen without curtailment
          
          temp1Rsh <- filter(tempin1, rshr == as.character(j))
          tempD <- filter(temp1Rsh, type1 == "demand", type2 == "demand")
          DD <- sum(tempD$val)
          tempGC <- filter(temp1Rsh,type1 == "generation conventional", type2 == "demand")
          GC <- sum(tempGC$val)
          tempR <- filter(temp1Rsh,type1 == "residual load (before curt and sto)", type2 == "demand")
          #not sure if RL should be muliplied by 1000 or not
          RL <- sum(tempR$val)
          temp7[ii,2] <- "DemandMax"
          temp7[ii,4] <- max(tempD$val)
          temp6[ii,4] <- DD-RL-GC
          temp6[ii,2] <- "ResGen no curtailment"
          
          # storage capacity
          
          temp2Rsh <- filter(tempin2, rshr == as.character(j))
          tempSto1  <- filter(temp2Rsh, type1 == "capacities storage GWh", type2 == "storage")
          temp8[ii,2] <- "Storage Gwh"
          temp8[ii,4] <- tempSto1$val
          tempSto2  <- filter(temp2Rsh, type1 == "capacities storage GW", type2 == "storage")
          temp9[ii,2] <- "Storage GW"
          temp9[ii,4] <- tempSto2$val
          
          # absolute curtailment
          
          tempAbCur <- filter(temp2Rsh, type1 == "renewables curtailed absolute")
          AC <- sum(tempAbCur$val)
          temp10[ii,2] <- "Absolute Curtailment"
          temp10[ii,4] <- AC
          
          
          # relative curtailment 
          
          tempRelCur <- filter(temp2Rsh, type1 == "renewables curtailed relative")
          temp13[ii,4] <- tempRelCur$Val
          temp13[ii,2] <- "Relative Curtailment"
          
          ii <- ii + 1
          
        }
        d.rrsh <- rbind(d.rrsh, temp7,temp8,temp9,temp10,temp13)
      
      
    }
    
  
# Plot 1 - Storage per GWH histogram against Schill, renewale percentage 

data <- filter(d.rrsh, Type == "Storage GW")
data1 <- subset(data,NRegions == regimport, select = c("NRegions","ReShr","Value"))
data2 <- subset(data,NRegions == "0", select = c("NRegions","ReShr","Value"))
data <- rbind(data1,data2)

colnames(data) <- c("ModelType","ReShr","Value")

p <- ggplot(data, aes(x = ReShr, y = Value))+geom_col(aes(fill = ModelType))

p <- p + scale_fill_manual(name = "Model Type",
                           values = c("#24576D",
                                      "#099DD7"))
                                       #"#28AADC",
                                       #"#248E84",
                                       #"#F2583F",
                                       #"#96503F"))

p <- p + ggtitle("Installed Storage Capacity") +
  labs( x = "Renewable Share %", y = "GW", fill = "Model Type")

p <- p +   theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=17, hjust=0))
p <- p + theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=10))


setwd('/Users/andrew/Documents/School/EnergyInformatics/DELORES.B1/dolores/model/plots')

fname1 <- paste(paste0("ResultsComparison_regions_",regimport),".png",sep ="")
ggsave(filename=fname1, plot=p , width = 20, height = 15, units = "cm")

# Facet Plots all regions 

dataa <- filter(d.rrsh, Type == "Storage GW")
dataa <- dataa[,-2]

colnames(dataa) <- c("ModelType","ReShr","Value")

p <- ggplot(dataa, aes(x = ReShr, y = Value))+
     geom_col(aes(fill = ModelType)) +
     facet_wrap(~ModelType, ncol = 2)

p <- p + ggtitle("Installed Storage Capacity") +
  labs( x = "Renewable Share %", y = "GW", fill = "Model Type")

p <- p +   theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=17, hjust=0))
p <- p + theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=10))


setwd('/Users/andrew/Documents/School/EnergyInformatics/DELORES.B1/dolores/model/plots')

fname1 <- paste(paste0("ResultsComparison_regions_all"),".png",sep ="")
ggsave(filename=fname1, plot=p , width = 22, height = 15, units = "cm")

# Plot 2 - 3D Storage, curtailment, regions granularity



# Plot 3 - Storage per GWH histogram curtailment

data <- filter(d.rrsh, NRegions == regimport)
data1 <- subset(data,Type == "Storage GW", select = c("ReShr","Value"))
colnames(data1) = c("ReShr","Storage")
data2 <- subset(data,Type == "Relative Curtailment", select = c("ReShr","Value"))
colnames(data2) = c("ReShr","RelativeCurtailment")
data <- cbind(data1,data2$RelativeCurtailment)
colnames(data) <- c("ReShr","Storage","RelativeCurtailment")


p <- ggplot(data, aes( x = ReShr))
p <- p+ geom_col(aes(y= Storage))
p <- p+ geom_point(aes(y=RelativeCurtailment*1))

p <- p + scale_y_continuous(sec.axis = RelativeCurtailment)
