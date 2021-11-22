#library for imputation
install.packages("mice")
library(mice)

#imputation for numerical missing values - socio-demographic data; The data inside c(" ") are the names of the columns

imp_nc <- mice(df[,c("Pt_Age","Family_Income","NoEducation")], m=5, maxit = 10, method = 'pmm', seed = 500)
df[,c("Pt_Age","Family_Income","NoEducation")] <- complete(imp_nc,1)

#imputation for numerical missing values - Adult Temperament Questionnaure,Mini-IPIP personality data
imp_tp <- mice(df[,c("ATQ_NEGATIVE_AFFECT","ATQ_EFFORTFUL_CONTROL","ATQ_EXTRAVERSION","ATQ_SENSITIVITY",
                     "Openness","Conscientiousness","Extraversion","Agreeableness","Neuroticism")], m=5, maxit = 10, method = 'pmm', seed = 500)

df[,c("ATQ_NEGATIVE_AFFECT","ATQ_EFFORTFUL_CONTROL","ATQ_EXTRAVERSION","ATQ_SENSITIVITY",
      "Openness","Conscientiousness","Extraversion","Agreeableness","Neuroticism")] <- complete(imp_tp,1)

#imputation for numerical missing values - Adverse Childhood Experiances data
imp_ace <- mice(df[,c("Binary.Score","Frequency.Score","Total.Score","Abuse.Score",
                     "Neglect.Score","Household.Challenges.Score","Community.Challenges.Score","AP_B",              
                     "AE_B","AS_B","FAD_B","FI_B","FM_B",            
                     "FV_B","FP_B","NE_B","NP_B","BUL_B",                     
                     "ComV_B","ColV_B","AP_F","AE_F","AS_F",                      
                     "FAD_F","FI_F","FM_F","FV_F","FP_F",                      
                     "NE_F","NP_F","BUL_F","ComV_F","ColV_F")], m=5, maxit = 10, method = 'pmm', seed = 500)

df[,c("Binary.Score","Frequency.Score","Total.Score","Abuse.Score",
      "Neglect.Score","Household.Challenges.Score","Community.Challenges.Score","AP_B",              
      "AE_B","AS_B","FAD_B","FI_B","FM_B",            
      "FV_B","FP_B","NE_B","NP_B","BUL_B",                     
      "ComV_B","ColV_B","AP_F","AE_F","AS_F",                      
      "FAD_F","FI_F","FM_F","FV_F","FP_F",                      
      "NE_F","NP_F","BUL_F","ComV_F","ColV_F")] <- complete(imp_ace,1)

# capping the outliers

capping <- function(x){
  quantiles <- quantile( x, c(.05, .95 ) )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}

#capping the outliers for variables required and final df; The data inside c(" ") are the names of the columns
df[,c("Pt_Age","Family_Income","NoEducation","ATQ_NEGATIVE_AFFECT","ATQ_EFFORTFUL_CONTROL","ATQ_EXTRAVERSION","ATQ_SENSITIVITY",
       "Openness","Conscientiousness","Extraversion","Agreeableness","Neuroticism","Binary.Score","Frequency.Score","Total.Score","Abuse.Score",
       "Neglect.Score")] <- lapply(df[,c("Pt_Age","Family_Income","NoEducation","ATQ_NEGATIVE_AFFECT","ATQ_EFFORTFUL_CONTROL","ATQ_EXTRAVERSION","ATQ_SENSITIVITY",
                                         "Openness","Conscientiousness","Extraversion","Agreeableness","Neuroticism","Binary.Score","Frequency.Score",
                                         "Total.Score","Abuse.Score","Neglect.Score")], function(x) capping(x))

