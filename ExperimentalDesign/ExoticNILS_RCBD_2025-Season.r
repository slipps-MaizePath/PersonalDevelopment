# DESIGNING EXPERIMENT FOR LARGE TEOSINTE DERIVED MAIZE NIL POPULATION
# THERE ARE 3 SUBPOPULATIONS - EACH EXPERIMENT DESIGNED SEPARATELY

# PACKAGES
library(agricolae)
library(tidyverse)

# GET NUMBERS/LOGISTICS OF POPULATIONS
all.nils <- read.csv("./../Data/Inventory_Bzea-LandB-LanTeo_TRUE_20250421.csv", header = T)

all.nils$Pop <- as.factor(all.nils$Pop)

levels(all.nils$Pop)
all.nils$Line_Source <- paste(all.nils$Line, all.nils$Source, sep = "_")

bzea <- all.nils[all.nils$Pop == "BZ", ] #1171
landb <- all.nils[all.nils$Pop == "LB", ] #218
lanteo <- all.nils[all.nils$Pop == "LT", ] #129
1171 + 218 + 129 == (nrow(all.nils)) #everything accounted for

# GET LCM - HOW MANY BLOCKS CAN I HAVE
## GOAL: FOR POP SIZE, WANT DECENTLY LARGE BLOCKS
find_factors <- function(n) {
  factors <- vector("numeric")
  for (i in 1:n) {
    if (n %% i == 0) {
      factors <- c(factors, i)
    }
  }
  return(factors)
}

# BZ EXPERIMENT
factors <- find_factors(1173) #add an R (NC344) and S (Oh7B) check to the population. Appears once.
factors
find_factors(72)
colnames(BZ)
BZ.cks <- data.frame(keep = c("TRUE", "TRUE"), Line = c("NC344", "Oh7B"), 
                       Source = c("19TB1019_001b", "19TB1006_001b"), Pop = "",
                       note = "", Line_Source = c("NC344", "Oh7B"))

BZ2 <- rbind(BZ, BZ.cks)

## DESIGN IT
r = (1173/69) #17 incomplete blocks, 1 block = 1 range
trt1 <- "B73" #appears in every block
seed = 202504211

bz.dau <- design.dau(trt1 = trt1,  trt2 = as.character(BZ2$Line_Source), r=r, seed = seed, serie = 3, kinds = "Super-Duper")
#get your book and format everything
book <- bz.dau$book
book$Rep <- 1 #only 1 rep
colnames(book)[c(1:3)] <- c("Row_Number", "Block", "Row_Name")
book$Row_Number <- seq(1001, 2190, by = 1)

#double check everything
head(book, 71)
tail(book, 71)

#things unique to this populations
book$Experiment_Name <- "25BZ" #2025 BZ
book$Population <- "BZ"

# LB EXPERIMENT
factors <- find_factors(220) #add a check
factors
factors.r <- find_factors(70)
factors.r
#add the BZ checks to the LB experiment too
LB2 <- rbind(LB, BZ.cks)

r = (220/22) #10 incomplete blocks, 1 block = ~1/3 of a range
trt1 <- "B73" #appears in every block
seed = 202504212

#randomize
lb.dau <- design.dau(trt1 = trt1, trt2 = as.character(LB2$Line_Source), r = r, seed = seed, serie = 3, kinds = "Super-Duper")
book2 <- lb.dau$book
book2$Rep <- 1 #only one rep
colnames(book2)[c(1:3)] <- c("Row_Number", "Block", "Row_Name")
book2$Row_Number <- seq(1001, 1230, by = 1)

#double check everything
head(book2, 70)
tail(book2, 70)

#things unique to this populations
book2$Experiment_Name <- "25LB" #2025 LB
book2$Population <- "LB"

# LT EXPERIMENT
factors <- find_factors(132) #add a R+S+MS checks
factors
factors.r <- find_factors(70)
factors.r

#add BZ checks and PHZ51 check to the "checks dataframe"
LT.cks <- rbind(BZ.cks, c("TRUE", "PHZ51", "PR005-1", "", "", "PHZ51"))
LT2 <- rbind(LT, LT.cks)

r = (132/22) #6 incomplete blocks, 1 block = ~1/3 of a range
trt1 <- "B73" #appears in every block
seed = 20250422

#randomize
lt.dau <- design.dau(trt1 = trt1, trt2 = as.character(LT2$Line_Source), r = r, seed = seed, serie = 3, kinds = "Super-Duper")
book3 <- lt.dau$book
book3$Rep <- 1 #only one rep
colnames(book3)[c(1:3)] <- c("Row_Number", "Block", "Row_Name")
book3$Row_Number <- seq(1001, 1138, by = 1)

#double check everything
head(book3, 70)
tail(book3, 70)

#things unique to this populations
book3$Experiment_Name <- "25LT" #2025 LT
book3$Population <- "LT"

# ADD SSIDs AND OTHER INFO
#add generation infor
book$Generation <- with(book, ifelse(book$Row_Name %in% c("B73", "NC344", "Oh7B"), "", "BC2S3"))
book2$Generation <- with(book2, ifelse(book2$Row_Name %in% c("B73", "NC344", "Oh7B"), "", "BC1S3"))
book3$Generation <- with(book3, ifelse(book3$Row_Name %in% c("B73", "NC344", "Oh7B"), "", 
                                       substring(book3$Row_Name, 15, 19)))
#add the plot info (just make it fit on a rowband easier)
#PLOT WILL BE FAMILY/DONOR IN THIS FILE
book$Plot <- sub("_.*", "", book$Row_Name)
book2$Plot <- sub("_.*", "", book2$Row_Name)
book3$Plot <- sub("_.*", "", book3$Row_Name)

#combine all books into 1
exps <- do.call('rbind', list(book, book2, book3))
#add info for checks to the main file
LT.cks
head(all.nils)
all.nils <- rbind(all.nils, LT.cks)
all.nils <- rbind(all.nils, c("TRUE", "B73", "24NN0022_001b", "", "", "B73"))
all.nilsfilt <- all.nils[ ,c(2,3,6)] #remove columns i dont want
#merge to add source seed ids
exps2 <- merge(exps, all.nilsfilt, by.x = "Row_Name", by.y = "Line_Source", all.x = T)

exps2 <- exps2[, -1] #removing row_name decided to only use it for randomization
colnames(exps2)[c(8,9)] <- c("Plot_Name",  "Source_Seed_ID")

# FORMATTING FOR RESEARCH LAB DATABASE
purpose <- "NLB"
first <- "Sarah"
last <- "Lipps"

exps2$Planting_Year <- "2025"
exps2$Organization <- "UIUC"
exps2$Field_name <- "CFAR500"
exps2$Kernel_Num <- 20

exps2$Plot_ID <- paste0(exps2$Experiment_Name, exps2$Row_Number)
exps2$Pollination_Type <- ""
exps2$Shell_Type <- ""
exps2$Seed_Name <- exps2$Source_Seed_ID
exps2$Is_Male <- ""
exps2$Cross_Target <- ""
exps2$Row <- ""
exps2$Range <- ""
exps2$Planting_Date <- ""
exps2$Harvest_Date <- ""
exps2$Plot_Comments <- purpose
exps2$First_Name <- first
exps2$Last_Name <- last
exps2$Pedigree <- exps2$Plot_Name

# MUST ORDER SPECIFIC FOR DATABASE UPLOAD
exps3 <- exps2[,c("Plot_ID","Experiment_Name", "Source_Seed_ID", "Seed_Name", "Is_Male", "Cross_Target", "Population", "Pedigree", "Field_name", "Planting_Year", "Plot_Name", "Generation", "Pollination_Type", "Shell_Type", "Row", "Range", "Plot", "Block", "Rep","Kernel_Num", "Planting_Date", "Harvest_Date", "Plot_Comments", "Organization","First_Name", "Last_Name")]

head(exps3, 5)
tail(exps3, 5)

# SAVE/EXPORT
write.csv(exps3, "./../Output/dbo_exoticnils_20250422.csv", row.names = F)