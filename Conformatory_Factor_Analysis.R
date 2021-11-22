#conformatory factor analysis with Neurocognition tests data
vars <- c("CT1_TIME", "CT2_TIME", "NB1_Acc", "NB2_Acc",
          "AVLT_IR", "AVLT_TOTAL")

#scaling the data for the model
scaled_np <-  data.frame(lapply(final_df[,vars], scale))
scaled_np <- cbind(scaled_np,final_df[,c("SOT","FOT")])
scaled_np$CT1_TIME = scaled_np$CT1_TIME * -1
scaled_np$CT2_TIME = scaled_np$CT2_TIME * -1

# Specify the latent variables in the variable below
mod.1fac <- 'ProcSpeed =~  CT1_TIME +  CT2_TIME
             NewLearn =~ AVLT_IR  + AVLT_TOTAL
             WorkMem =~ NB1_Acc +NB2_Acc
             ToM =~ SOT + FOT
             gFac =~ ProcSpeed + NewLearn + WorkMem + ToM'

#libraries for CFA model
library(lavaan)
library(semPlot)

# fit the factors into cfa model
cfa.fit <- cfa(mod.1fac, data=scaled_np) 
# predict latenet variables using lavPredict
cfa.scores <- lavPredict(cfa.fit)

#display summary of the model
summary(cfa.fit, fit.measures=T)

# visualisation for the Confirmatory factor analysis
semPlot::semPaths(cfa.fit,"std", edge.color =  "black", what = "mod" )

