#==== Nesseaccry library and clearing the worksapce ====

# install.packages("lme4", "lmerTest", "emmeans", "moments")  
library(lme4)
library(lmerTest)
library(emmeans) 
library(moments)
library(ggplot2)

rm(list = ls())
#================================================================================================================================
#==== Nesseaccry Functions

# Calculate ICC 
calculate_icc_lmer = function(x) { # x is the MLM model object for your baseline model, created by lmer
    random_effects = data.frame(VarCorr(x))
    intercept_variance  = random_effects[random_effects$var1 == "(Intercept)", 'vcov'][1]
    residual_variance  = random_effects[random_effects$grp == "Residual", 'vcov']
    icc  = intercept_variance / (residual_variance + intercept_variance)
    return(icc)
}

# Calculate partial R2 for each predictor
semipartial_R2 = function(x) {
  require(lmerTest)
  anova_results = as.data.frame(anova(x, type = 3, ddf="Kenward-Roger")) # Run ANOVA and save results in dataframe
  model_paramaters = row.names(anova_results)
  R2 = with(anova_results, round(((NumDF / DenDF) * `F value` )/(1 + (NumDF / DenDF) * `F value` ), 3))
  return(cbind(model_paramaters, R2))
}

#================================================================================================================================
#Load data 

address = "./Data/performance_questionnaire_data_first.csv"
# dataset = read.csv(address, sep = "", col.names = )

dataset = read.table(address, header = TRUE,  sep = ',')[, c('mturkID', 'global_score', 'nogo2win_first_session', 'first_rel_nogo2win', 'nogo2win_second_session','nogo2win_mean_performance', 'BMI', 'gender_first_session' )]
#================================================================================================================================
#==== Data prep
# Check skewness of variables
skewness(as.numeric(factor(dataset$nogo2win_first_session)))
skewness(dataset$global_score)
skewness(dataset$first_rel_nogo2win)
# skewness(dataset$money)
# log transforms
dataset$acres <- log(dataset$acres+ 0.01) # log transformation because the skwness of the variable is more than 1
dataset$money <- log(dataset$money+2.51) # log transformation because the skwness of the variable is more than 1
                                         # there are a few number less than one in my opnion there should be removed 
                                         # but I go with the pre-registation doc and insted use a 
                                         # 2.51 to make every value more than zero
# Check skewness after the transform
skewness(dataset$acres)
skewness(dataset$money)

# Centering
# aggregate.dataset <- data.frame(state=names(with(dataset, tapply(money, state, mean, na.rm=T))))
# aggregate.dataset$mean.money <- as.numeric(with(dataset, tapply(money, state, mean, na.rm=T)))
# aggregate.dataset$mean.acres <- as.numeric(with(dataset, tapply(acres, state, mean, na.rm = TRUE)))

# Grand-mean centre within the aggregate data set
# aggregate.dataset <- within(aggregate.dataset, {c.acres <- mean.acres - mean(mean.acres, na.rm=T)})
# Merge data sets
# dataset <- merge(dataset, aggregate.dataset, by="state")
# Group-mean centre within long-form data set
# dataset <- within(dataset, {c.money <- money - mean.money})
#================================================================================================================================
#==== MLM 

# Definening the model and printing the result
model <- lmer(voting ~ (1+money|state), na.action = "na.exclude", data=dataset)
summary(model)

# Calculate the ICC
baseline_model = lmer(voting ~ (1|state) + 1, na.action = "na.exclude", data=dataset)
calculate_icc_lmer(baseline_model)

# Calculate the partial R^2 s
semipartial_R2(model)
 
#================================================================================================================================
#==== estimated marginal means and plots
# emm for money
emm_money = emmeans(model, specs = "c.money", at=list(c.money = c(-1*sd(dataset$c.money,na.rm=T), 
                                                                                     sd(dataset$c.money,na.rm=T))))

# emm for acres
emm_acres = emmeans(model, specs = "c.acres", at=list(c.acres = c(-1*sd(dataset$c.acres,na.rm=T), 
                                                                                     sd(dataset$c.acres,na.rm=T))))

emm_money_summary_df  = as.data.frame(summary(emm_money))[c('emmean', 'c.money', 'lower.CL', 'upper.CL')]
emm_money_summary_df$c.money = c("-1 SD", "+1 SD")
emm_money_summary_df$c.money <- factor(emm_money_summary_df$c.money, levels=c("-1 SD","+1 SD"))

emm_acres_summary_df  = as.data.frame(summary(emm_acres))[c('emmean', 'c.acres', 'lower.CL', 'upper.CL')]
emm_acres_summary_df$c.acres = c("-1 SD", "+1 SD")
emm_acres_summary_df$c.acres <- factor(emm_acres_summary_df$c.acres, levels=c("-1 SD","+1 SD"))

# Plot for High-low money
ggplot(aes(x= c.money, y = emmean, group = 1), data=emm_money_summary_df) + geom_line(size = 0.9, color = "red")  + 
  geom_point(color="red", size = 3, shape = 21, fill = "white", stroke = 3) + 
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL),width=.05, color = 'darkgray', size = 1)+ 
  scale_x_discrete(expand = c(0.02, 0.02),breaks = NULL)+ theme_light()+
  theme(panel.border = element_rect(fill=NA, colour = "black", size=1), aspect.ratio = 9 / 16, axis.title=element_text(size=21))  + 
  labs(x = "Money received from tobacco companies", y= "Pro-tobacco vote proportion")

# Plot for High-low acres
ggplot(aes(x= c.acres, y = emmean, group = 1), data=emm_acres_summary_df) + geom_line(size = 0.9, color = "blue")  + 
  geom_point(color="blue", size = 3, shape = 21, fill = "white", stroke = 3) + 
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL),width=.05, color = 'darkgray', size = 1)+ 
  scale_x_discrete(expand = c(0.02, 0.02),breaks = NULL)+ theme_light()+
  theme(panel.border = element_rect(fill=NA, colour = "black", size=1), aspect.ratio = 9 / 16, axis.title=element_text(size=21))  + 
  labs(x = "Acres of tobacco farm", y= "Pro-tobacco vote proportion", size = 20)

