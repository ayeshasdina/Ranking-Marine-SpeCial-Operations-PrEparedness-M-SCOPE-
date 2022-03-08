# "Author - Ayesha S. Dina"

#### 5. Filtering the data for LR

filtering_data <- function(individual_score, output_path, name)
{
  
  file_path <- "/Users/dina/Documents/M-Scope/Input/"
  fileName <- "MSCOPEPROs-PreparednessSurvey_DATA_2020-11-02_1024_Scores Only.xlsx"
  file <- paste(file_path,fileName,sep = "")
  label <- read_excel(file, sheet = 1)
  
  filterData <- individual_score[individual_score$rowNames %in% label$record_id,]
  
  file_filter <- paste(output_path,"/",name,"filter.csv",sep = "")
  write.csv(filterData,file_filter,sep = ",",row.names = F)
  
}



## 1. principle component ananysis (PCA) model
PCA <- function(data,rowNames,eventName,output_file,output_figure,output_inividual_score,output_score,selected_weight,component_name)
{
  individual_score <- data.frame()
  
  input <- data 
  
  
  ############## changing normalized value to "lower is better."
  
  for (i in c(1: length(colnames(input))))
  {
    if (selected_weight[i] == "L")
    {
      temp <- 1 - input[,i]
      input[,i] <- temp
    }
  }
  
  file_score <- paste(output_score,"/",component_name,"score.csv",sep = "")
  write.csv(input,file_score,sep = ",")
  
  ##############
  
  pca <- prcomp(input, scale = FALSE)
  variance <- (pca$sdev)^2 
  varPercent <- variance/sum(variance) * 100 
  
  ### scree ploting 
  file_screeplot <- paste(output_figure,"/",component_name,"screeplot.pdf",sep = "")
  pdf(file_screeplot)
  screeplot(x= pca, type = "line", main = "Scree plot", cex.main = 1.8)
  dev.off()
  
  
  ######## saving all the PCs scores ###
  pca_score <- as.data.frame(pca$rotation)
  # rownames(pca_score) <- record_id ## if pca$x
  file_score <- paste(output_file,"/",component_name,"score.csv",sep = "")
  write.csv(pca_score,file_score,sep = ",")
  
 
  for (i in c(1:length(pca_score[1,])))
  {
    if (pca_score[,i] < 0)
    {
      pca_score[,i] <- pca_score[,i] * (-1)
    }
    temp <- rowSums(data.frame(mapply('*',input, pca_score[,i])))
    individual_score <- c(individual_score,data.frame(temp))
  }
  
  # ######## saving all the PCs scores ###
  # pca_score <- as.data.frame(pca$rotation)
  # # rownames(pca_score) <- record_id ## if pca$x
  # file_score <- paste(output_file,"/",component_name,"score.csv",sep = "")
  # write.csv(pca_score,file_score,sep = ",")
  
  
  ######## saving PCs for all the individuals ###
  
  individual_score <- data.frame(individual_score)
  colnames(individual_score) <- c(1: length(pca_score[1,]))
  individual_score <- cbind(eventName,individual_score) ### add redcap event name here.
  individual_score <- cbind(rowNames,individual_score) ## to stick with the record_ID.
 
  ############# filtering data for linear regression
  
  filtering_data(individual_score,"output/filterLR_PCA",component_name)
  
  ########### Normalizing the individual score 
  # individual_score <- as.data.frame(apply(individual_score,2,normalize))
  
  file_score <- paste(output_inividual_score,"/",component_name,"score.csv",sep = "")
  write.csv(individual_score,file_score,sep = ",",row.names = F)
  
}

# 4. Non negative Matrix factorization

NMF <- function(data,rowNames,eventName,output_A,output_W,output_H,selected_weight,component_name)
{
  col_len <- length(colnames(data))
  row_len <- length(rownames(data))
  # r <- col_len
  
  r <- 1
  
  ############## changing normalized value to "lower is better."
  for (i in c(1: length(colnames(data))))
  {
    if (selected_weight[i] == "L")
    {
      temp <- 1 - data[,i]
      data[,i] <- temp
    }
  }
  
  
  #### running the NMF
  res <- nmf(t(data), r,"lee")
  V.hat <- fitted(res) 
  #### saving multiplication of basis and coef. therefore W*H ~~ A
  V.hat <- as.data.frame(t(V.hat))
  file_score <- paste(output_A,"/",component_name,".multi.csv",sep = "")
  write.csv(V.hat,file_score,sep = ",")
  ### taking basis matrix
  w <- basis(res)
  w <- as.data.frame(w)
  file_score <- paste(output_W,"/",component_name,".basis.csv",sep = "")
  write.csv(w,file_score,sep = ",")
  ### taking coeficient matrix
  h <- coef(res)
  h <- as.data.frame(t(h))
  h <- cbind(eventName,h)
  h <- cbind(rowNames,h)
  ############# filtering data for linear regression
  
  filtering_data(h,"output/filterLR_NMF",component_name)
  
  #### saving the score
  file_score <- paste(output_H,"/",component_name,".coef.csv",sep = "")
  write.csv(h,file_score,sep = ",",row.names = F)
  
}

### 3. Min Max Normalization 
normalize <- function(x)
{
  return((x- min(x)) /(max(x)-min(x)))
}

### 2. Feature imputation 
features_analysis <- function(input,selected_features,selected_weight,component_name)
{
  # record_id <- c(input$record_id,old_data$record_id)
  data <- input[,as.character(selected_features)]
  data <- as.data.frame(data)
  data_save <- cbind(input$redcap_event_name,data)
  data_save <- cbind(input$record_id,data_save)
  file_score <- paste("output/merged/",component_name,"merged.csv",sep = "")
  write.csv(data_save,file_score,sep = ",",row.names = F)
  imp_data <- vector()
  
  
  for (i in c(1: length(colnames(data))))                    # for (i in colnames(data)) ######
  {
    temp <- impute(data[[i]])
    
    # imp_data <- cbind(imp_data,temp)
    
    ########### 01/27/21 ... calculation of variables for ... close to 90, close to 0 and close to 50.
    
    if (selected_weight[i] == "N")
    {
      temp_90 <- abs(temp - 90)
      imp_data <- cbind(imp_data,temp_90)
      selected_weight[i] <- "L"
    }
    
    else if (selected_weight[i] == "Z")
    {
      
      temp_0 <- abs(temp - 0)
      imp_data <- cbind(imp_data,temp_0)
      selected_weight[i] <- "L"
    }
    
    else if (selected_weight[i] == "M")
    {
      
      temp_50 <- abs(temp - 50)
      imp_data <- cbind(imp_data,temp_50)
      selected_weight[i] <- "L"
    }
    
    else
    {
      imp_data <- cbind(imp_data,temp)
    }
    
    
    
    ############# 
    
    
  }
  colnames(imp_data) <- selected_features
  # rownames(imp_data) <- record_id #rownames(imp_data) <- input$record_id
  imp_data <- as.data.frame(imp_data)
  imp_data_row <- cbind(input$record_id,imp_data)
  
  file_score <- paste("output/imputed/",component_name,"imputed.csv",sep = "")
  write.csv(imp_data_row,file_score,sep = ",",row.names = F)
  
  
  # imp_data$physperf_prepar_recent <- input$physperf_prepar_recent ### add phyperf_prepare_recent and consider as label
  imp_data <- as.data.frame(apply(imp_data,2,normalize))
  ##### saveing normalized data 
  
  
  
  PCA(imp_data,input$record_id,input$redcap_event_name,"output/files","output/figure","output/individual_score","output/norm_score",selected_weight,component_name) ### when PCA needed
  NMF(imp_data,input$record_id,input$redcap_event_name,"output/nmf_multi","output/nmf_basis","output/nmf_coef",selected_weight,component_name) ## NMF works
 
  return(imp_data)
}

