

###Read files
main_df <- data.frame()
serum_vec <- c()

for (i in dir(pattern = ".csv")) {
  S<-read.csv(i)
  for (j in c(1:8)){
    serum_mean <- mean(as.numeric(S[j,1:3]), trim = 2)
    serum_sd <- round(sd(as.numeric(S[j,1:3])), digits = 2)
    serum_vec <- c(serum_vec,serum_mean, serum_sd)
  }
  main_df <- rbind(main_df, serum_vec)
  serum_vec <- c()
}


###Create dataframe
colnames(main_df) <- c("pureNP_25600", "sd1", "pureNP_6400","sd2", "pureNP_1600","sd3", "pureNP_400","sd4", "NP_RNA_25600","sd5", "NP_RNA_6400","sd6", "NP_RNA_1600","sd7", "NP_RNA_400", "sd8")
main_df$NCBI_Serum_Table <- c(2.7,2.1,6.2,3.7,5.8,3.1,2.9,5,5.6,5.5,1.4,3.4,4.3,3.3,3.9,2.4,3.1,4.9,5.1,3.3,4.9,3.5,3.4,0.05,0.1,0.17,0.03,0.03,0.02,0.15,0.09,0.04,0.08,0.18,0.05,0.2,0.06)
main_df$Positive <- ifelse(NCBI_Serum_Table > 1, 'Yes', 'No')
main_df$Positive <- as.factor(main_df$Positive)


###Calculation OD/CO value
library(tibble)
OD_OC <- function(var1){
  mean_od <- mean(var1[main_df$Positive == "No"], na.rm = T, trim = 2)
  mean_sd <- round(sd(var1[main_df$Positive == "No"], na.rm = T), digits = 2)
  OC <- (mean_od + 3*mean_sd)
  return(round((var1 / OC),digits = 2)) 
}

#
main_df$OD_OC_pureNP_1600 <- OD_OC(main_df$pureNP_1600)
main_df$OD_OC_NP_RNA_1600 <- OD_OC(main_df$NP_RNA_1600)
#
main_df$OD_OC_pureNP_400 <- OD_OC(main_df$pureNP_400)
main_df$OD_OC_NP_RNA_400 <- OD_OC(main_df$NP_RNA_400)

