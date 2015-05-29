
#### birth model fro one single year #####

#install.packages("data.table")
rm(list = ls())

library(data.table)
library(bnlearn)
library(ggplot2)
library(pryr)  # object_size
library(gRain) 

#devtools::install_github("hadley/lineprof")

memory.limit(size=25000)
setwd("D:/Users/rapko/R Bayes/Birth data model")

gc()
dataCSV <- fread("natl2013.csv", nrows = -1) #R Problem: Daten sind sortiert, die ersten 10 sind also nicht representativ!


# Filter interesting data
data <- data.frame(birth_place = dataCSV$ubfacil,  # Krankenhaus, daheim, etc.; noch zusammenfügen!
                   mother_age = dataCSV$mager9,
                   prenatal_visits = dataCSV$previs_rec, # wie oft bei Vorsorgeuntersuchungen
                   cigarette = dataCSV$cig_rec, 
                   method_of_delivery = dataCSV$me_rout, # Kaiserschnitt, Zange, Vakuum, etc..
                   apgar5 = dataCSV$apgar5r, stringsAsFactors = FALSE)
rm(dataCSV)
gc()


# merge "home"
data$birth_place[which((data$birth_place == 3)|(data$birth_place == 4)|(data$birth_place == 5))] <- 3
#delete "unknown, etc."
data <- data[-which((data$birth_place == 9)|(data$birth_place == 6)|(data$birth_place == 7)|(data$birth_place == " ")),] 
# combine blanck with unknown
data$cigarette[which(data$cigarette == "")] <- "U"
data$method_of_delivery[which(data$method_of_delivery == "")] <- 9
# delete unkown apgar
data <- data[-which(data$apgar5 == 5),] 

# transform into factor
for(i in 1:length(data[1,])){
  data[,i] <- as.factor(data[,i])
}

#data$apgar5 <- droplevels(data$apgar5)

# blancks trotzdem bei cigarette und method of delivery da???

# blacklist
arc.set<-matrix(c("apgar5","birth_place",                
                  "apgar5","mother_age",                  
                  "apgar5", "prenatal_visits",                  
                  "apgar5", "cigarette",
                  "apgar5", "method_of_delivery"         
),byrow=TRUE,ncol=2,dimnames=list(NULL,c("from","to")))

# start dag
dag_in <- model2network("[mother_age][birth_place|mother_age][prenatal_visits|mother_age][cigarette|mother_age]
                     [method_of_delivery|birth_place:mother_age]
                     [apgar5|method_of_delivery:birth_place:mother_age:prenatal_visits:cigarette]")

#bn_learned <- hc(data) #R hier passendes stat DAG geben?
bn_learned_supervised <- hc(data, start = dag_in, blacklist = arc.set)
score(bn_learned)


##########################
dag <- empty.graph(nodes(bn_learned_supervised))
arcs(dag) <- arcs(bn_learned_supervised)

 # transforms the BN into a junction tree
bn <- bn.fit(dag, data)
junction <- compile(as.grain(bn)) # as.grain builds the junction tree, compile computes the prob tables
# takes a bn.fit object

###### smoking - apgar #######
j_smoking <- setEvidence(junction, nodes = "cigarette", states = "Y")
j_not_smoking <- setEvidence(junction, nodes = "cigarette", states = "N")
apgar_smoking <- as.vector(querygrain(j_smoking, nodes = "apgar5")$apgar5)
apgar_not_smoking <- as.vector(querygrain(j_not_smoking, nodes = "apgar5")$apgar5)
plot(apgar_not_smoking - apgar_smoking, type = "o", col = "blue")
title(main = "APGAR conditional on smoking")
#lines(apgar_smoking, type="o", pch=22, lty=2, col="red")
axis(1, at=1:4, lab=c("1","2","3","4"))
###############################

querygrain(junction, nodes = "apgar")
j <- setEvidence(junction, nodes = "prenatal_visits", states = "1")
querygrain(j, nodes = c("apgar5"))     


