# "Author - Ayesha S. Dina"

rm(list=ls())

library("readxl")
library("ggfortify")
library("dict")
library("Hmisc")
library("NMF")


setwd("/Users/dina/Documents/M-Scope/Script/PCA_NMFpreparedness")

source("functions_rawData.R")

input_path <- "/Users/dina/Documents/M-Scope/Input/"
fileName <- "MSCOPE-DataAnalytics1_DATA_2020-07-06_1442.csv"
file_path <- paste(input_path,fileName,sep="")
inputData <- read.table(file = file_path,header = TRUE,sep = ",")
### input of features list
features <- read.table(file = "features.txt",header = TRUE,sep = "")

####################### physical performance ############
num_phyP_com <- 7
####### dictionary for phy performance 
phy_dict <- dict(c("speed","agility","power","strength","ana_perf","aer_perf","body_com"),c(list(as.character(features$features[1:2])),as.character(features$features[3]),as.character(features$features[4]),
                                                                                            list(as.character(features$features[5:6])),
                                                                                            as.character(features$features[7]),list(as.character(features$features[8:10])),
                                                                                            list(as.character(features$features[11:12]))))
####### dictionary for the weight

phy_dict_W <- dict(c("speed","agility","power","strength","ana_perf","aer_perf","body_com"),c(list(as.character(features$weight[1:2])),as.character(features$weight[3]),as.character(features$weight[4]),
                                                                                              list(as.character(features$weight[5:6])),
                                                                                              as.character(features$weight[7]),list(as.character(features$weight[8:10])),
                                                                                              list(as.character(features$weight[11:12]))))

norm_phy <- vector()
for (i in c(1:num_phyP_com))
{
  
  name<- paste("phyperf_",phy_dict$keys()[[i]],sep = "")
  sel_features <- phy_dict[[phy_dict$keys()[[i]]]]
  sel_weight <- phy_dict_W[[phy_dict_W$keys()[[i]]]]
  data <- features_analysis(inputData,sel_features,sel_weight,name)
  norm_phy <- c(norm_phy,data)
}

norm_phy <- as.data.frame(norm_phy)
norm_phy <- cbind(inputData$record_id,norm_phy)
file_name <- paste("output/norm_data/","phy_perf_norm.csv",sep = "")
write.csv(norm_phy,file_name,sep = ",",row.names = F)

####################### MSK ############

# # ############# MSK new dictionary ####
num_MSK_com <- 4


MSK_dict <- dict(c("boby_com","mobility","stability", "loading_sym"),c(list(as.character(features$features[13:14])) ,list(as.character(features$features[15:22])) ,
                                                                       list(as.character(features$features[23:27])), list(as.character(features$features[28:33])))) ## dictionary for MSK
MSK_dict_W <- dict(c("boby_com","mobility","stability", "loading_sym"),c(list(as.character(features$weight[13:14])),list(as.character(features$weight[15:22])),
                                                                         list(as.character(features$features[23:27])), list(as.character(features$features[28:33])))) ### dictionary for weight
norm_MSK <- vector()

for (i in c(1:num_MSK_com))
{
  
  name<- paste("MSKperf_",MSK_dict$keys()[[i]],sep = "")
  sel_features <- MSK_dict[[MSK_dict$keys()[[i]]]]
  sel_weight <- MSK_dict_W[[MSK_dict_W$keys()[[i]]]]
  if (MSK_dict$keys()[i] == "loading_sym")
  {
    inputData["ohs_avg_abs_asym"] <- abs(inputData["ohs_avg_lt"] - inputData["ohs_avg_rt"])
    inputData["ohs_l_rf_ff_asym"] <- abs(inputData["ohs_avg_l_rf"] - inputData["ohs_avg_l_ff"])
    inputData["ohs_r_rf_ff_asym"] <- abs(inputData["ohs_avg_r_rf"] - inputData["ohs_avg_r_ff"])
    
    # old_data["ohs_avg_abs_asym"] <- abs(old_data["ohs_avg_lt"] - old_data["ohs_avg_rt"])
    # old_data["ohs_l_rf_ff_asym"] <- abs(old_data["ohs_avg_l_rf"] - old_data["ohs_avg_l_ff"])
    # old_data["ohs_r_rf_ff_asym"] <- abs(old_data["ohs_avg_r_rf"] - old_data["ohs_avg_r_ff"])
    
    sel_features <- c("ohs_avg_abs_asym","ohs_l_rf_ff_asym","ohs_r_rf_ff_asym")
    sel_weight   <- c("L","L","L")
    
    data <- features_analysis(inputData,sel_features,sel_weight,name)
  }
  else
  {
    data <- features_analysis(inputData,sel_features,sel_weight,name)
  }
  norm_MSK <- c(norm_MSK,data)
}

norm_MSK <- as.data.frame(norm_MSK)
norm_MSK <- cbind(inputData$record_id,norm_MSK)
file_name <- paste("output/norm_data/","MSK_perf_norm.csv",sep = "")
write.csv(norm_MSK,file_name,sep = ",",row.names = F)

# # # ############# Cognitive performance ####
num_cog_com <- 3
norm_cog <- vector()


cog_dict <- dict(c("reac_time","spe_pro","memory"),c(list(as.character(features$features[34:37])),as.character(features$features[38]),list(as.character(features$features[39:41])))) ## dictionary for cognitive
cog_dict_W <- dict(c("reac_time","spe_pro","memory"),c(list(as.character(features$weight[34:37])),as.character(features$weight[38]),list(as.character(features$weight[39:41])))) ## dictionary for cognitive weight


for (i in c(1:num_cog_com))
{
  
  name<- paste("cogperf_",cog_dict$keys()[[i]],sep = "")
  sel_features <- cog_dict[[cog_dict$keys()[[i]]]]
  sel_weight<- cog_dict_W[[cog_dict_W$keys()[[i]]]]
  data <- features_analysis(inputData,sel_features,sel_weight,name)
  norm_cog <- c(norm_cog,data)
}
norm_cog <- as.data.frame(norm_cog)
norm_cog <- cbind(inputData$record_id,norm_cog)
file_name <- paste("output/norm_data/","cog_perf_norm.csv",sep = "")
write.csv(norm_cog,file_name,sep = ",",row.names = F)


# # # ############# phychological health ####
num_phyco_com <- 2
norm_phycho <- vector()


phycho_dict <- dict(c("depression","sleep"),c(list(as.character(features$features[42:43])),list(as.character(features$features[44:45])))) ## dictionary for behavioral
phycho_dict_W <- dict(c("depression","sleep"),c(list(as.character(features$weight[42:43])),list(as.character(features$weight[44:45])))) ## dictionary for behavioral weight


for (i in c(1:num_phyco_com))
{
  
  name<- paste("phychoperf_",phycho_dict$keys()[[i]],sep = "")
  sel_features <- phycho_dict[[phycho_dict$keys()[[i]]]]
  sel_weight <- phycho_dict_W[[phycho_dict_W$keys()[[i]]]]
  data <- features_analysis(inputData,sel_features,sel_weight,name)
  norm_phycho <- c(norm_phycho,data)
}

norm_phycho <- as.data.frame(norm_phycho)
norm_phycho <- cbind(inputData$record_id,norm_phycho)
file_name <- paste("output/norm_data/","phycho_perf_norm.csv",sep = "")
write.csv(norm_phycho,file_name,sep = ",",row.names = F)
