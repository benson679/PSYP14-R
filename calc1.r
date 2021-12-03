# Load packages

library(tidyverse)	
library(psych)
library(gridExtra)
library(car)
library(dplyr)
library(lmtest)
library(jtools)
library(cAIC4)

# functions

# table function, lifted from zoltan
coef_table = function(model){	
  require(lm.beta)	
  mod_sum = summary(model)	
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))		
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))		
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"		
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)		
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
  mod_sum_table["(Intercept)","Std.Beta"] = "0"		
  return(mod_sum_table)	
}	


# Load data set

painManagementOne = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_1.csv")

# Describe data

painManagementOne %>% 	
  summary()	

describe(painManagementOne)	


painManagementOneFixed = painManagementOne [-c(88, 34), ]

# Model

modelOne = lm(pain ~ age + sex, data = painManagementOneFixed)	

modelTwo = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = painManagementOneFixed)

modelOne %>% 	
  summary()	


modelTwo %>%
  summary()

modelTwo %>% 	
  plot(which = 5)	

modelTwo %>% 	
  plot(which = 4)	

# Test of assumptions

modelTwo %>% 	
  plot(which = 2)	

modelOne %>% 	
  plot(which = 2)	

modelOne %>% 	
  plot(which = 3)	

modelTwo %>% 	
  plot(which = 3)	

modelOne %>% 	
  ncvTest()

modelOne %>% 	
  bptest() 

modelTwo %>% 	
  ncvTest()

modelTwo %>% 	
  bptest()

residualsModelOne = enframe(residuals(modelOne))	
residualsModelOne %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	


residualsModelTwo = enframe(residuals(modelTwo))	
residualsModelTwo %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	

describe(residuals(modelOne))

describe(residuals(modelTwo))	

modelOne %>% 	
  vif()	

modelTwo %>% 	
  vif()	

#Justering av data

modelThree = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = painManagementOneFixed)

modelThree %>%
  vif()

describe(residuals(modelThree))	

modelThree %>% 	
  ncvTest()

modelThree %>% 	
  bptest()

residualsModelThree = enframe(residuals(modelThree))	
residualsModelThree %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()


modelThree %>%
  summary()


AIC(modelOne)

AIC(modelThree)

summ(modelOne)

summ(modelThree)

cAIC(modelThree)



resultTableTwo = coef_table(modelThree)

resultTableOne = coef_table(modelOne)

resultTableTwo

resultTableOne

summary(modelOne)

summary(modelThree)

anova(modelOne, modelThree)
