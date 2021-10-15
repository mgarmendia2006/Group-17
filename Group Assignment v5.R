
library(tidyverse)
library(stargazer)
library(dplyr)
library(reshape2)
library(ggplot2)
library(plyr)
library(zoo)
library(plm)
library(car)
library(lmtest)
library(AER)
library(MASS)

# Clean the global environment
remove(list=ls())

## Load Data Bases
## https://www.ecdc.europa.eu/en/publications-data/download-data-response-measures-covid-19
## https://github.com/owid/covid-19-data/tree/master/public/data   (ourwolrdindata.org)
## https://population.un.org/wpp/Download/Standard/CSV/

dir <- ("C:/Users/mgarm/Desktop/RSM/ASAP/Group Assignment/")
Panel_data<-read.csv(file=paste0(dir,"owid-covid-data.csv")) 
Measures<-read.csv(file=paste0(dir,"response_graphs_data_2021-09-20.csv")) 
Population<-read.csv(file=paste0(dir,"WPP2019_TotalPopulationBySex.csv")) 
  
## Remove unnecessary variables from the panel data and rename test variable
Panel_data<-Panel_data[c(3:4,17,26,30,35:37)]  
colnames(Panel_data)[colnames(Panel_data)=="new_tests_smoothed"]<-  "Tests"

## Remove countries not in the Response measures data set 
Panel_data<-Panel_data%>%filter(location %in% Measures$Country)
Graph_Panel_data<-Panel_data
Graph_Panel_data$date<-as.Date(Graph_Panel_data$date)
## Transform Population data base to only necessary data
## and add information to panel data with left join
Population <- Population %>% filter(Time=="2021"&Variant=="Medium")
Population <- Population[c("Location","PopTotal")]
Population$PopTotal <- Population$PopTotal*1000
colnames(Population)[colnames(Population)=="Location"]<-  "location"

Panel_data <- merge(x=Panel_data, y=Population, by="location",all.x=TRUE)

## Replace NA date_end in Measures for present date
## given the measure is still up

Measures$date_end <- Measures$date_end %>% replace_na(list(x = "2021-10-06"))

## Keep only relevant measures
Measures <- Measures %>% filter(Response_measure == "StayHomeOrderPartial"|
                                  Response_measure == "StayHomeOrder"|
                                  Response_measure == "WorkplaceClosures"|
                                  Response_measure == "NonEssentialShops"|
                                  Response_measure == "ClosSec")

##Linear interpolate for missing data in some countries
##from variables fully and partially vaccinated 
for(i in 1:(nrow(Panel_data)-1)){
  Panel_data$people_fully_vaccinated[i+1]<-
    ifelse(Panel_data$location[i]==Panel_data$location[i+1],
           Panel_data$people_fully_vaccinated[i+1],0)
}
for(i in 1:(nrow(Panel_data)-1)){
  Panel_data$people_vaccinated[i+1]<-
    ifelse(Panel_data$location[i]==Panel_data$location[i+1],
           Panel_data$people_vaccinated[i+1],0)
}

Panel_data <- Panel_data %>%  
  mutate(Partially_Vaccinated_Daily = 
           na.approx(Panel_data$people_vaccinated, na.rm=FALSE)) 
Panel_data <- Panel_data %>%  
  mutate(Fully_Vaccinated_Daily = 
           na.approx(Panel_data$people_fully_vaccinated, na.rm=FALSE)) 


## Drop NAs in Panel Data

Panel_data[is.na(Panel_data)]<-0

## Normalize number of tests, people fully and partially vaccinated 
## as a % of the population

Panel_data$Fully_Vaccinated<-
  Panel_data$Fully_Vaccinated/Panel_data$PopTotal
Panel_data$Partially_Vaccinated<-
  Panel_data$Partially_Vaccinated/Panel_data$PopTotal
Panel_data$Tests<-Panel_data$Tests/Panel_data$PopTotal

Panel_data<-Panel_data[c(1:3,5,12:13)]  

##############################################
####  CREATE DUMMY EXPLANATORY VARIABLES  ####
##############################################

## Create Curfew Variable
for (i in 1:nrow(Panel_data)){
  for (j in 1:nrow(Measures)){
    if((Panel_data$location[i]==Measures$Country[j])&&
       (Measures$Response_measure[j]=="StayHomeOrderPartial")&&
       (Panel_data$date[i]>=Measures$date_start[j]) &&
       (Panel_data$date[i]<=Measures$date_end[j])){
            Panel_data$Curfew[i]<-1
            i=i+1
            j=j+1
          }
            else{
           Panel_data$Curfew[i]<-0
           j=j+1
          }
  }
}
## Create Full Lock Down variable
for (i in 1:nrow(Panel_data)){
  for (j in 1:nrow(Measures)){
    if((Panel_data$location[i]==Measures$Country[j])&&
       (Measures$Response_measure[j]=="StayHomeOrder")&&
       (Panel_data$date[i]>=Measures$date_start[j]) &&
       (Panel_data$date[i]<=Measures$date_end[j])){
      Panel_data$LockDown[i]<-1
      i=i+1
      j=j+1
    }
    else{
      Panel_data$LockDown[i]<-0
      j=j+1
    }
  }
}

##############################################
######  CREATE DUMMY CONTROL VARIABLES  ######
##############################################

## Create closure of work places variable
for (i in 1:nrow(Panel_data)){
  for (j in 1:nrow(Measures)){
    if((Panel_data$location[i]==Measures$Country[j])&&
       (Measures$Response_measure[j]=="WorkplaceClosures")&&
       (Panel_data$date[i]>=Measures$date_start[j]) &&
       (Panel_data$date[i]<=Measures$date_end[j])){
      Panel_data$Work[i]<-1
      i=i+1
      j=j+1
    }
    else{
      Panel_data$Work[i]<-0
      j=j+1
    }
  }
}
## Create closure of non-essential shops
for (i in 1:nrow(Panel_data)){
  for (j in 1:nrow(Measures)){
    if((Panel_data$location[i]==Measures$Country[j])&&
       (Measures$Response_measure[j]=="NonEssentialShops")&&
       (Panel_data$date[i]>=Measures$date_start[j]) &&
       (Panel_data$date[i]<=Measures$date_end[j])){
      Panel_data$Shops[i]<-1
      i=i+1
      j=j+1
    }
    else{
      Panel_data$Shops[i]<-0
      j=j+1
    }
  }
}

## Create close of institutions: Secondary schools
for (i in 1:nrow(Panel_data)){
  for (j in 1:nrow(Measures)){
    if((Panel_data$location[i]==Measures$Country[j])&&
       (Measures$Response_measure[j]=="ClosSec")&&
       (Panel_data$date[i]>=Measures$date_start[j]) &&
       (Panel_data$date[i]<=Measures$date_end[j])){
      Panel_data$Schools[i]<-1
      i=i+1
      j=j+1
    }
    else{
      Panel_data$Schools[i]<-0
      j=j+1
    }
  }
}

## Save panel data in another data frame
FPanel_data<-Panel_data
FPanel_data$date<-as.Date(FPanel_data$date)




##############################################
#########    DESCRIPTIVE ANALYSIS    #########
##############################################

# Make table of the results
stargazer(FPanel_data,type="text")

  

# Reproduction rate behaviour 
ggplot(FPanel_data, aes(x=reproduction_rate)) +
  geom_bar(fill="Blue3", colour="black") +
  theme(axis.title = element_text(size=rel(1.15)),
        axis.text  = element_text(size=rel(1.15)))+
  xlim(0.001,4)


# Reproduction rate evolution
ggplot(Graph_Panel_data, aes(x=date,y=reproduction_rate))+
  geom_point(size=0.1)+
  stat_smooth(method="loess",formula=y~x)+
  scale_x_date(date_breaks= "1 month",date_labels="%b %Y")+
  theme(axis.title=element_text(size=rel(1.2)),
        axis.text=element_text(size=rel(1.2)),
        axis.text.x=element_text(angle=60,hjust=1))+
  guides(colours=guide_legend(override.aes=list(size=5)))

##############################################
#############    DEFINE MODELS    ############
##############################################


mdlA <- reproduction_rate ~  Curfew + LockDown + Tests + Fully_Vaccinated + 
  Partially_Vaccinated + Work + Shops + Schools
  


#-------------------------------------------------------------------------
# Check Multicollinearity
#-------------------------------------------------------------------------

Multicollinearity <- vif(lm(mdlA,data=FPanel_data))
stargazer(Multicollinearity)


mdlB <- reproduction_rate ~ Tests + Fully_Vaccinated + 
  Curfew + LockDown + Work + Shops + Schools
full_model <- lm(mdlB,data=FPanel_data)

#-------------------------------------------------------------------------
# Check Heteroskedasticity
#-------------------------------------------------------------------------

mdl1<- lm(reproduction_rate ~ Tests,data=FPanel_data)
mdl2<- lm(reproduction_rate ~ Fully_Vaccinated,data=FPanel_data)
mdl3<- lm(reproduction_rate ~ Curfew,data=FPanel_data)
mdl4<- lm(reproduction_rate ~ LockDown,data=FPanel_data)
mdl5<- lm(reproduction_rate ~ Work,data=FPanel_data)
mdl6<- lm(reproduction_rate ~ Shops,data=FPanel_data)
mdl7<- lm(reproduction_rate ~ Schools,data=FPanel_data)


par(mfrow=c(1,1))
plot(lm(mdl3,data=FPanel_data))
Breusch <- rbind(bptest(lm(mdl1,data=FPanel_data)),
bptest(lm(mdl2,data=FPanel_data)),
bptest(lm(mdl3,data=FPanel_data)),
bptest(lm(mdl4,data=FPanel_data)),
bptest(lm(mdl5,data=FPanel_data)),
bptest(lm(mdl6,data=FPanel_data)),
bptest(lm(mdl7,data=FPanel_data)))
stargazer(Breusch)

## Variables Tests, Fully_Vaccinated and Curfew have p-value lower than 0.05,
## therefore they are heteroskedastics



################################################
#############    ESTIMATE MODELS    ############
################################################

rsltPooled <-   plm(mdlB, data = FPanel_data, 
                    index=c("location", "date"), model = "pooling") 

rsltWithin <-   plm(mdlB, data = FPanel_data, 
                    index=c("location", "date"), model = "within") 

rsltRandom <-   plm(mdlB, data = FPanel_data, 
                    index=c("location", "date"), model = "random") 


stargazer(rsltPooled,rsltWithin,rsltRandom)

#-------------------------------------------------------------------------
# Remedy using Robust Standard errors #
#-------------------------------------------------------------------------

seBasicPooled <- coeftest(rsltPooled,vcov(rsltPooled))
seWhitePooled <- 
  coeftest(rsltPooled,vcovHC(rsltPooled,type="HC0",cluster="group"))
seClusteredPooled <- 
  coeftest(rsltPooled,vcovHC(rsltPooled,type="HC1",cluster="group"))

seBasicWithin <- coeftest(rsltWithin,vcov(rsltWithin))
seWhiteWithin <- 
  coeftest(rsltWithin,vcovHC(rsltWithin,type="HC0",cluster="group"))
seClusteredWithin <- 
  coeftest(rsltWithin,vcovHC(rsltWithin,type="HC1",cluster="group"))

seBasicRandom <- coeftest(rsltRandom,vcov(rsltRandom))
seWhiteRandom <- 
  coeftest(rsltRandom,vcovHC(rsltRandom,type="HC0",cluster="group"))
seClusteredRandom <- 
  coeftest(rsltRandom,vcovHC(rsltRandom,type="HC1",cluster="group"))

stargazer(seBasicPooled,seClusteredPooled,seBasicWithin,seClusteredWithin,
          seBasicRandom,seClusteredRandom)

# Compare different standard error models, Appendix table
stargazer(seWhitePooled,seClusteredPooled,seWhiteWithin,seClusteredWithin,
          seWhiteRandom,seClusteredRandom)

##############################################
############    COMPARE MODELS    ############
##############################################


##Test pool model vs within model 

pooltest(rsltPooled,rsltWithin)

##Hausman test: Compare random and fixed effect models.
##Under H0 no correlation between disturbance and explanatory
##variables, both RE and FE are consistent(though FE is not efficient),
##under H1, correlation between disturbances, only FE consistent

phtest(rsltWithin,rsltRandom)



