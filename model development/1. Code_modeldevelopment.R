#################################################
##              Statistical Model              ##
##               Agronomy Journal              ##
##                                             ##
##            University of Florida            ## 
##               Agroecology Lab               ##         
##                                             ##
#################################################

if(!require("readr")) install.packages("readr")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("leaps")) install.packages("leaps")
if(!require("ggpmisc")) install.packages("ggpmisc")
if(!require("olsrr")) install.packages("olsrr")
if(!require("caret")) install.packages("caret")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("cowplot")) install.packages("cowplot")
if(!require("extrafont")) install.packages("extrafont")
if(!require("RColorBrewer")) install.packages("RColorBrewer")
if(!require("lubridate")) install.packages("lubridate")

library(readr)
library(tidyverse)  
library(leaps)      
library(ggpmisc)
library(olsrr)
library(caret)
library(ggplot2)
library(cowplot)
library(extrafont)
library(RColorBrewer)
library(lubridate)

fonts()

##################################################

## Data Pre-processing

set.seed(181)  # for reproducibility

# Loading 48 variables data (Rcode: "PGT20")
data_fr <- read_csv("5year_pgt20_may15.csv")

# Select 48 variables
data <- data_fr %>% 
  select(3:51)

# Loading PGT20 by County (Rcode: "PGT20")
# Store the potential explanatory(predictors)/dependent(response) variables
predictors <- data_fr[,c(3:50)]
response <- data_fr[,c(51)]

# 5 years data 
data_al_p <- cbind.data.frame(predictors,response)
colnames(data_al_p)[49] <- "PGT20"

# Bad /High Incidence (HI) aflatoxin years
data_PGT20_observed_byear <- data_fr %>% 
  filter(crop_year %in% c(2018,2019,2020)) %>% 
  left_join(observed_PGT20[,c(1,2,4)], by = c("crop_year", "County"))

predictors_hi <- data_PGT20_observed_byear[,c(3:50)]
response_hi <- data_PGT20_observed_byear[,c(51)]

data_al_bad_year <- cbind.data.frame(predictors_hi,response_hi)

colnames(data_al_bad_year)[49] <- "PGT20"

# Good /Low Incidence (LI) aflatoxin years
data_PGT20_observed_gyear <- data_fr %>%
  filter(crop_year %in% c(2021,2022)) %>% 
  left_join(observed_PGT20[,c(1,2,4)], by = c("crop_year", "County"))


predictors_li <- data_PGT20_observed_gyear[,c(3:50)]
response_li <- data_PGT20_observed_gyear[,c(51)]

data_al_good_year <- cbind.data.frame(predictors_li,response_li)

colnames(data_al_good_year)[49]<-"PGT20"


#################################################################


formula <-  PGT20 ~ 
  d1d2wk  +   d2d2wk  +   d3d2wk  +   d4d2wk   + 
  MaxT2wk +   MinT2wk +   Ave2wk  +   Rain2wk  + 
  d1d3wk  +   d2d3wk  +   d3d3wk  +   d4d3wk   + 
  MaxT3wk +   MinT3wk +   Ave3wk  +   Rain3wk  + 
  d1d4wk  +   d2d4wk  +   d3d4wk  +   d4d4wk   + 
  MaxT4wk +   MinT4wk +   Ave4wk  +   Rain4wk  + 
  d1d5wk  +   d2d5wk  +   d3d5wk  +   d4d5wk   + 
  MaxT5wk +   MinT5wk +   Ave5wk  +   Rain5wk  + 
  d1d6wk  +   d2d6wk  +   d3d6wk  +   d4d6wk   + 
  MaxT6wk +   MinT6wk +   Ave6wk  +   Rain6wk  + 
  d1d7wk  +   d2d7wk  +   d3d7wk  +   d4d7wk   + 
  MaxT7wk +   MinT7wk +   Ave7wk  +   Rain7wk 

data = data_al_p


# Create empty list to store models
i = 1
nboot = 10000

rmse_boot <- numeric(nboot)
r_boot <- numeric(nboot)
r2_boot<- numeric(nboot)
adj_r2_boot <- numeric(nboot)
MAE_boot<-numeric(nboot)
nash_sutcliffe<-numeric(nboot)
Nt<-numeric(nboot)
mean_observed<-numeric(nboot)

models <- list()
variable_sel_step <- list()
rst1 <- data.frame()
   



# the number of bootstrap iterations = 10000
   
for (i in seq_len(nboot)) {
  
  # to the how far the step goes in the long run
  print(i)
  
  ## Sampling and Handling Linear Dependencies (considering the possibility of collinearity-related errors)
  tryCatch({
  # Sample rows with replacement
  boot_data <- data[sample(seq_len(nrow(data)), replace = TRUE), ]
  # Create a matrix of predictor variables
  X1 <- as.matrix(boot_data[,-49])
  # Use findLinearCombos() to identify linear dependencies
  combos <- findLinearCombos(X1)
  # Remove linearly dependent variable
  boot_data1 = boot_data[, -c(combos$remove)]
    
  # Sample for training(70%) and testing(30%)
  sample <- sample(c(TRUE, FALSE), nrow(boot_data1), 
                   replace = T, prob = c(0.7,0.3))
  train <- boot_data1[sample, ]
  test <- boot_data1[!sample, ]
  
  response <- colnames(train)[length(colnames(train))]
  predictors <- colnames(train)[-length(colnames(train))]
   

  # Create formula by pasting predictors and response together
  formula <- as.formula(paste(response, "~",
                              paste(predictors, collapse = "+")))
  
  
    
  # Model training and selection
  
  fit_full <- lm(formula,data=train)
  boh_step_wise <- ols_step_both_p(fit_full,
                                   details=F,
                                   prem = 0.05,        # variables with p more than prem will be removed from the model.
                                   pent = 0.20)$ model # variables with p value less than pent will enter into the model.
  # Lower pent and higher prem: Creates a more conservative model with fewer variables, reducing the risk of overfitting but possibly omitting variables that could improve model performance.
    
  # Extract predictor variables from term.labels attribute
  pr_varaible <- attr(boh_step_wise$terms, "term.labels")
       
  # Extract response variable from first element of variables attribute
  response_variable <- as.character(attr(boh_step_wise$terms,"variables")[[2]])
    
  # Create formula by pasting predictors and response together
  formula <- as.formula(paste(response_variable, "~",
                              paste(pr_varaible, collapse = "+")))
    
  # Extract coefficients from model
  coefs <- coef(boh_step_wise)
       
  # Create character vector to store formula and coefficients
  output <- character(length(coefs) + 1)
       
  # Store formula in first element of output vector
  output[1] <- paste0(response_variable,
                      paste("="), round(coefs[1],2), " + ",
                      paste(names(coefs)[-1], 
                      round(coefs[-1],2), 
                      sep = "ร—", collapse = " + "))
    
  vrbl<-output[1] 
  

  # Store model in the list
  models[[i]] <- vrbl
  
  # Print model for current iteration
  cat("Model", i, ":", output[1], "\n")
    
  # Store variable selection for the current iteration
  variable_sel_step[[i]] <- pr_varaible
    
  # Make predictions on original data
  pred <- predict(boh_step_wise,newdata=test)
    
    
  
  # Calculate evaluation metrics
  
  rmse_boot[i] <- sqrt(mean((test[[response]] - pred)^2))
  r2_boot[i] <- cor(test[[response]] ,pred)^2
  r_boot[i] <- 1*(cor(test[[response]] ,pred))
  MAE_boot[i] <- mean(abs(test[[response]]  - pred))
  mean_observed[i] <- mean(test[[response]])
  nash_sutcliffe[i] <- 1 - sum((test[[response]] - pred)^2) / sum((test[[response]] -  mean_observed[i])^2)
  Nt[i] <- (sd(test[[response]])/rmse_boot[i])-1    
  
  # Create a data frame with evaluation metrics
  model_N <- i
  out <- data.frame(model_N = i,  
                    RMSE=rmse_boot[i],
                    r2 = r2_boot[i],
                    r=r_boot[i],
                    MAE=MAE_boot[i],
                    Nt=Nt[i],
                    NSE=nash_sutcliffe[i])
  rst1 <- bind_rows(rst1, out) 
  }, error = function(e) {
  
  # Print error message and continue to next iteration
  cat("Error in iteration", i, ":", conditionMessage(e), "\n")
  return()
      
  })
}

setwd("C:/Users/da94_/OneDrive - University of Florida/Peanuts in Homestead/Peanuts/AuburnModel/Review/Rcode")
# a list of linear regression model equations
mdls2 = models[1:10000] 

# save 10000 models
saveRDS(mdls2, file="Model_seed181.RData")
# read 10000 models 
mdls2 <- readRDS("Model_seed181.RData")
mdls2[[1]]
# to calculate the predicted values for PGT20 based on a given linear regression equation
PGT20_prop_ossrr <- function(dfi, equation) {
  
  # Extract the response variable from the equation
  response_var <- strsplit(equation, "=")[[1]][1]
  
  # Extract the predictor variables from the equation
  predictor_vars <- strsplit(equation, "=")[[1]][2]
  predictor_varq <- gsub("[/^]", "", predictor_vars)
  predictor_vart <- unlist(strsplit(predictor_varq, "[+_\\*]"))
  predictor_final<- gsub("\\s+", "", predictor_vart)
  
  # Check if each element starts with a number
  is_numeric <- grepl("^[0-9-]", predictor_final)
  
  # Convert numeric elements to numeric
  char_coff<- as.numeric(predictor_final[is_numeric])
  
  # Replace non-numeric elements with variable names
  pr_vrbl <- gsub("[^[:alpha:][:digit:]]", "", 
                  predictor_final[!is_numeric])
  
  # Initialize variables
  coef_first <- char_coff[1] 
  coef_list <- char_coff[-1] 
  pred_vars <- pr_vrbl
  predicted <- char_coff[1] 
  
  
  # Loop to calculate the predicted values
  for (i in 1:length(coef_list)) {
    predicted <- predicted+ coef_list[i] * dfi[, pred_vars[i]] 
  }
  
  return(predicted)
}


# to evaluate the performance of models
bst_funct <- function(data, mdls) {
  rst <- data.frame()
  
  # Loop through each model
  for (i in 1:length(mdls)) {
    # Check if the model exists
    if (length(mdls[[i]]) > 0) {
      
      # Calculate predicted values using "PGT20_prop_ossrr" function
      PGT20_predicted <- PGT20_prop_ossrr(data, equation = mdls[[i]])
      # Extract observed values from the data
      observed <- data$PGT20
      
      # Calculate evaluation metrics
      rmse <- sqrt(mean((PGT20_predicted - observed)^2))
      r2 <- cor(PGT20_predicted,observed)^2
      mae <- mean(abs(PGT20_predicted - observed))
      mean_observed <- mean(observed)
      nash_sutcliffe <- 1 - sum((observed - PGT20_predicted)^2) / sum((observed - mean_observed)^2)
      Nt <- (sd(observed)/rmse)-1   
      
      # Create a data frame with evaluation metrics
      model_N <- i
      out <- data.frame(model_N = model_N,
                        RMSE=rmse,
                        r2 = r2,
                        MAE=mae,
                        Nt=Nt,
                        NSE=nash_sutcliffe)
      
      # Add the new row to the data frame
      rst <- bind_rows(rst, out) 
    }
  }
  
  return(rst)
} 



# to perform bootstrapping on a given dataset and evaluates the performance of models 
bootstrap_best_model <- function(df, n_samples, mdls) {
  data_out <- data.frame()
  
  # Loop through each bootstrap sample
  for (i in 1:n_samples) {
    
    # Create a bootstrap sample by randomly sampling rows with replacement
    sample_data <- df[sample(nrow(df), replace = TRUE), ]
    
    # Evaluate model performance using "bst_funct" on the bootstrap sample
    out <- bst_funct(sample_data, mdls)
    
    # Combine results into the data_out data frame
    if (nrow(data_out) == 0) {
      data_out <- out
    } else {
      data_out <- cbind.data.frame(data_out, out[,c(2:6)])
    }
  }
  
  # Return the aggregated results from all bootstrap samples
  return(data_out)
}


# to compute summary statistics for performance metrics across multiple runs of models
model_matrix_eff <- function(data) {
  colnames(data) <- c("Model", paste0("Run_", rep(1:50, each = 5), 
                                          "_", rep(c("RMSE","R-sqared","MAE", "Nt", "NSE"), 50)))
  

  RMSE_mean <- apply(data[, grep("RMSE", colnames(data))], 1, mean, na.rm = TRUE)
  RMSE_sd <- apply(data[, grep("RMSE", colnames(data))], 1, sd, na.rm = TRUE)
  
  Rsqared_mean <- apply(data[, grep("R-sqared", colnames(data))], 1, mean, na.rm = TRUE)
  Rsqared_sd <- apply(data[, grep("R-sqared", colnames(data))], 1, sd, na.rm = TRUE)  
  
  MAE_mean <- apply(data[, grep("MAE", colnames(data))], 1, mean, na.rm = TRUE)
  MAE_sd <- apply(data[, grep("MAE", colnames(data))], 1, sd, na.rm = TRUE)
  
  NSE_mean <- apply(data[, grep("NSE", colnames(data))], 1, mean, na.rm = TRUE)
  NSE_sd <- apply(data[, grep("NSE", colnames(data))], 1, sd, na.rm = TRUE)
  
  Nt_mean <- apply(data[, grep("Nt", colnames(data))], 1, mean, na.rm = TRUE)

  data_all_model <- cbind.data.frame(
    data$Model,
    RMSE_mean,
    RMSE_sd,    
    Rsqared_mean,
    Rsqared_sd,    
    MAE_mean,
    MAE_sd,
    NSE_mean,
    NSE_sd,
    Nt_mean
  )
  
  colnames(data_all_model)[1] <- "Model_Number"
  
  return(data_all_model)

}



## Assess the stability and variability of evaluation metrics, providing insights into the robustness of your linear regression models on different subsets of the data 

# Evaluate 10000 models' performance using resampling from a subset of five years data
set.seed(20)# for reproducibility
data_frt <- bootstrap_best_model(data_al_p,
                                 n_samples=100,
                                 mdls=mdls2)
allyears <- model_matrix_eff(data_frt)




# Categorize the models into different groups based on Nt and NSE
allyears$Category <- ifelse(allyears$Nt_mean >= 2.2 & 
                              allyears$NSE_mean >= 0.9, "Very Good"
                            ,
                            ifelse(allyears$Nt_mean >= 1.2 & 
                                     allyears$Nt_mean < 2.2 & 
                                     allyears$NSE_mean >= 0.8 &
                                     allyears$NSE_mean < 0.9, "Good",
                                   ifelse(allyears$Nt_mean >= 0.7 & 
                                            allyears$Nt_mean < 1.2 & 
                                            allyears$NSE_mean >= 0.65 & 
                                            allyears$NSE_mean < 0.8, "Acceptable",
                                          ifelse(allyears$Nt_mean < 0.7 |
                                                   allyears$NSE_mean < 0.65, "Unsatisfactory", NA))))

write_csv(rst1, "rst1.csv")
# Calculate the top 10% threshold for each metric
top_10_percent <- rst1 %>% 
  summarize(bot_10_MAE = quantile(MAE, probs = .3,na.rm = TRUE),   # the value of the 30th percentile of the MAE
            bot_10_RMSE = quantile(RMSE, probs = .3,na.rm = TRUE), # the value of the 30th percentile of the RMSE
            top_10_r2 = quantile(r2, probs = .7,na.rm = TRUE))     # the value of the 70th percentile of the R2


# Selected model based on RMSE, MAE, and R-squared 
Selected_model <- allyears %>%  
  filter(Category=="Acceptable") %>%
  filter(MAE_mean <= top_10_percent$bot_10_MAE,
         RMSE_mean <= top_10_percent$bot_10_RMSE,
         Rsqared_mean >= top_10_percent$top_10_r2) 

Selected_model <- Selected_model %>% 
  mutate(model = models[c(Selected_model$Model_Number)])

Selectedmodel <- models[c(Selected_model$Model_Number)] 


# Assess how well the "Selected" model perform on all years
set.seed(21)# for reproducibility
aydata <- model_matrix_eff(bootstrap_best_model(data_al_p,
                                                n_samples=100,
                                                mdls = models[c(Selected_model$Model_Number)]))%>% 
  mutate(model = models[c(Selected_model$Model_Number)])




# Assess how well the "Selected" model perform on Bad / High Incidence(HI) aflatoxin years
set.seed(21)# for reproducibility
hidata <- model_matrix_eff(bootstrap_best_model(data_al_bad_year,
                                                n_samples = 100, 
                                                mdls = models[c(Selected_model$Model_Number)]))%>% 
  mutate(model = models[c(Selected_model$Model_Number)])




# Assess how well the "Selected" model perform on Good / Low Incidence(LI) aflatoxin years
set.seed(21)# for reproducibility
lidata <- model_matrix_eff(bootstrap_best_model(data_al_good_year,
                                                n_samples = 100, 
                                                mdls = Selectedmodel))%>% 
  mutate(model = models[c(Selected_model$Model_Number)] )




################################################################################
# "Acceptable" models (only considering NSE and Nt)

Acceptable_Model <- allyears %>%  
  filter(Category=="Acceptable")

AcceptableModel <- models[c(Acceptable_Model$Model_Number)] 

# Assess how well the "Acceptable" model perform on all years
all_ydata <- allyears %>%  
  filter(Category=="Acceptable")%>% 
  mutate(model = models[c(Acceptable_Model$Model_Number)])

set.seed(21)# for reproducibility
aydata_acceptable <- model_matrix_eff(bootstrap_best_model(data_al_p,
                                                           n_samples = 100, 
                                                           mdls = AcceptableModel))%>% 
  mutate(model = models[c(Acceptable_Model$Model_Number)])


# Assess how well the "Acceptable" model perform on Bad / High Incidence(HI) aflatoxin years
set.seed(21)# for reproducibility
hidata_acceptable <- model_matrix_eff(bootstrap_best_model(data_al_bad_year,
                                                           n_samples = 100, 
                                                           mdls = models[c(Acceptable_Model$Model_Number)]))%>% 
  mutate(model = models[c(Acceptable_Model$Model_Number)])

# Assess how well the "Acceptable" model perform on Good / Low Incidence(LI) aflatoxin years
set.seed(21)# for reproducibility
lidata_acceptable <- model_matrix_eff(bootstrap_best_model(data_al_good_year,
                                                           n_samples = 100, 
                                                           mdls = models[c(Acceptable_Model$Model_Number)]))%>% 
  mutate(model = models[c(Acceptable_Model$Model_Number)])


################################################################################

# Graph

theme_graph <- theme_classic() +
  theme(text = element_text(family = "Arial", size = 12),
        legend.position = 'none',
        axis.title.y = element_blank(),
        axis.title.x = element_text(family = "Arial", size = 12),
        axis.text.y = element_text(family = "Arial", size = 12),
        axis.text.x = element_text(family = "Arial", size = 12),
        plot.title = element_text(family = "Arial", size = 12))

  
# Figure 2 
dev.off()

fig2data <- read_csv("figure2data.csv")

f2_1 <- ggplot(fig2data, mapping=aes(x=factor(year),
                                     y = max_mean, 
                                     fill=ifelse(factor(year) %in% c("2018","2019","2020"), "#f1a340","#af8dc3")))+
  geom_boxplot() +
  scale_fill_manual(values = c("#ffffff","#999999")) +
  labs(y='Average daily \n maximum temperatures (กษ)')+
  theme_classic() +
  theme(legend.position = 'none',
        axis.text.y = element_text(family = "Arial", size = 12),
        axis.text.x =element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(family = "Arial", size = 12),
        plot.title = element_text(family = "Arial", size = 12),
        plot.margin = unit(c(0.5,0.5,0.5,0.9), "cm"))

f2_1

f2_2 <- ggplot(fig2data, mapping=aes(x=factor(year),
                                     y = min_mean, 
                                     fill=ifelse(factor(year) %in% c("2018","2019","2020"), "#f1a340","#af8dc3")))+
  geom_boxplot() +
  scale_fill_manual(values = c("#ffffff","#999999")) +
  labs(y='Average daily \n minimum temperatures (กษ)')+
  theme_classic() +
  theme(legend.position = 'none',
        axis.text.y = element_text(family = "Arial", size = 12),
        axis.text.x =element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(family = "Arial", size = 12),
        plot.title = element_text(family = "Arial", size = 12),
        plot.margin = unit(c(0.5,0.5,0.5,0.9), "cm"))


f2_2


f2_3 <- ggplot(fig2data, mapping=aes(x=factor(year),
                                     y = ave_mean, 
                                     fill=ifelse(factor(year) %in% c("2018","2019","2020"), "#f1a340","#af8dc3")))+
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values = c("#ffffff","#999999")) +
  labs(y='Average of average \n daily temperatures (กษ)')+
  theme_classic() +
  theme(legend.position = 'none',
        axis.text.y = element_text(family = "Arial", size = 12),
        axis.text.x =element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(family = "Arial", size = 12),
        plot.title = element_text(family = "Arial", size = 12),
        plot.margin = unit(c(0.5,0.5,0.5,0.9), "cm"))


f2_3

f2_4 <- ggplot(fig2data, mapping=aes(x=factor(year),
                                     y = raintotal_mean, 
                                     fill=ifelse(factor(year) %in% c("2018","2019","2020"), "#f1a340","#af8dc3")))+
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values = c("#ffffff","#999999")) +
  labs(y = 'Accumulated rainfall (mm)')+
  theme(legend.position = 'none',
        axis.text.y = element_text(family = "Arial", size = 12),
        axis.text.x =element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(family = "Arial", size = 12),
        plot.title = element_text(family = "Arial", size = 12),
        plot.margin = unit(c(0.5,0.5,0.5,0.9), "cm"))

f2_4

f2_5 <- ggplot(fig2data, mapping=aes(x=factor(year),
                                     y = M, 
                                     fill=ifelse(factor(year) %in% c("2018","2019","2020"), "#f1a340","#af8dc3")))+
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values = c("#ffffff","#999999")) +
  labs(y='Aflatoxin concentration \n(ฅ์g/kg, ppb)')+
  theme(legend.position = 'none',
        axis.text.y = element_text(family = "Arial", size = 12),
        axis.text.x =element_text(family = "Arial", size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(family = "Arial", size = 12),
        plot.title = element_text(family = "Arial", size = 12))


f2_5

f2_6 <- ggplot(fig2data, mapping=aes(x=factor(year),
                                     y = PGT20_prop, 
                                     fill=ifelse(factor(year) %in% c("2018","2019","2020"), "#f1a340","#af8dc3")))+
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values = c("#ffffff","#999999")) +
  labs(y ='Percentage of peanut loads \n over 20 ppb  aflatoxin (PGT20,%)')+
  theme(legend.position = 'none',
        axis.text.y = element_text(family = "Arial", size = 12),
        axis.text.x =element_text(family = "Arial", size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(family = "Arial", size = 12, 
                                    margin = margin(r = 20, unit = "pt")),
        plot.title = element_text(family = "Arial", size = 12))


f2_6

f2 <- plot_grid(f2_1, f2_2, f2_3, f2_4, f2_5, f2_6,
                labels =c('(a)','(b)','(c)','(d)','(e)','(f)'),
                nrow = 3,
                ncol = 2,
                label_size = 12,
                label_x = 0,
                label_y = 1,
                align="hv")
f2
ggsave(plot = f2, filename = "figure2_newnew.jpg",
       width = 174, height = 234, units = 'mm' )

# Figure 3
f3 <- ggplot(allyears, aes(x = Nt_mean,
                           y = NSE_mean,
                           color = Category,
                           shape = Category)) +
  geom_point() +
  scale_y_continuous(limits = c(min(allyears$Nt_mean), 
                                max(allyears$Nt_mean)),
                     breaks=c(0,0.5,0.65)) +
  scale_x_continuous(breaks=c(0,0.5,0.7))+
  scale_shape_manual(values = c(1, 4)) +
  scale_color_manual(values = c("blue","grey"))+
  labs(title='Nash-Sutcliffe coefficient Efficiency (NSE)',
       x = "Number of times that observed data dispersion \n is larger than prediciton error (Nt)")+
  theme_classic() +
  theme(text = element_text(family = "Arial", size = 12),
        legend.position = c(0.2, 0.5),
        axis.title.y = element_blank(),
        axis.title.x = element_text(family = "Arial", size = 12),
        axis.text.y = element_text(family = "Arial", size = 12),
        axis.text.x = element_text(family = "Arial", size = 12),
        plot.title = element_text(family = "Arial", size = 12))+
  geom_hline(yintercept= 0.65, linetype='dashed', color = 'red') +
  geom_vline(xintercept= 0.7, linetype='dashed', color = 'red') 
  #annotate("text", x=0.7, y=0, color ="black", label="Unsatisfactory",family = "Arial")+
  #annotate("text", x=0.81, y=0.8, color ="blue", label="Acceptable",family = "Arial")

f3

ggsave("figure3_181_f.png", plot = f3,
       width = 120, height = 120, units = 'mm')


# Figure 4

# Create a data frame from the model list
df <- data.frame(variable = unlist(variable_sel_step))

parameter_selection <- df %>% 
  count(variable) %>% 
  mutate(Frequency = n/10000 *100)

f4_1 <- ggplot(parameter_selection)+
  geom_bar(aes(x=reorder(variable,-Frequency), y=Frequency),
           stat="identity",
           width = 0.8,
           fill='#a6bddb',
           color='black')+
  scale_y_continuous(expand = c(0,0))+
  theme_classic() +
  labs(title = "Frequency (%) of each variable in all models") + 
  xlab("Variables") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family = "Arial", size = 12),
        axis.text.x = element_text(family = "Arial", size = 12,angle = 90, hjust =1, vjust = 0.3),
        plot.title = element_text(family = "Arial", size = 12))

f4_1

# Create a data frame from the list
model_acceptable <- variable_sel_step[Acceptable_Model$Model_Number]
df2 <- data.frame(variable = unlist(model_acceptable))

parameter_selection2 <- df2 %>% 
  count(variable)%>% 
  mutate(Frequency = n/20 * 100)

f4_2 <- ggplot(parameter_selection2)+
  geom_bar(aes(x=reorder(variable,-Frequency), y=Frequency),
           stat="identity",
           width = 0.8,
           fill='#a6bddb',
           color='black')+
  scale_y_continuous(expand = c(0,0))+
  theme_classic() +
  labs(title = "Frequency(%) of each variable in acceptable models") + 
  xlab("Variables") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family = "Arial", size = 12),
        axis.text.x = element_text(family = "Arial", size = 12,angle = 90,hjust =1,vjust = 0.3),
        plot.title = element_text(family = "Arial", size = 12))

f4_2

f4 <- plot_grid(f4_1, f4_2, 
                labels =c('(a)','(b)'),
                nrow = 2,
                label_size = 12,
                label_x = 0,
                label_y = 1,
                align="hv")
f4

ggsave(plot = f4, filename = "figure4_181.jpg",
       width = 174, height = 150, units = 'mm')


# Figure 5

## graph pgt 20 model4
FinalModel <- "PGT20=-114.45 +  MinT4wk*-10.26 + MinT2wk*8.04 + MaxT5wk*26.26 + Rain4wk*-0.86 +  MaxT4wk*-20.87"

fig5_1 <- data.frame(x = data_al_p$PGT20, 
                     y = PGT20_prop_ossrr(data_al_p, FinalModel),
                     year="All Years")

fig5_2 <- data.frame(x = data_al_bad_year$PGT20,
                     y = PGT20_prop_ossrr(data_al_bad_year, FinalModel), 
                     year="HI Years")

fig5_3 <- data.frame(x = data_al_good_year$PGT20,
                     y = PGT20_prop_ossrr(data_al_good_year,FinalModel), 
                     year="LI Years")

fig5_data <- rbind(fig5_1,fig5_2,fig5_3)


f5_1 <- ggplot(fig5_data,
               aes(y, x,
                   group = year, color = year, shape = year))+
  geom_point()+
  geom_smooth(method = "lm", aes(linetype=year), se = FALSE) +
  scale_color_manual(values = c("#003f5c","#bc5090","#ffa600"))+
  theme_classic() +
  scale_shape_manual(values=c(1,4,8))+
  scale_y_continuous(limits = c(-3, 45))+
  labs(y = "Observed PGT20 (%)") + 
  xlab("Predicted PGT20 (%)") +

  guides(color = guide_legend("Year"),
         shape = guide_legend(""),
         linetype = guide_legend())+
  
  theme(text = element_text(family = "Arial", size = 12),
        axis.title.y = element_text(family = "Arial", size = 12),
        legend.position = c(0.2, 0.8),
        legend.title = element_blank(),
        legend.text = element_text(family = "Arial", size = 12),
        axis.title.x = element_text(family = "Arial", size = 12),
        axis.text.y = element_text(family = "Arial", size = 12),
        axis.text.x = element_text(family = "Arial", size = 12),
        plot.title = element_text(family = "Arial", size = 12)) 
  #annotate("text", x=18, y=23, color ="#003f5c", label="All 5 years",family = "Arial")+
  #annotate("text", x=19, y=8, color ="#bc5090", label="High Incidence years",family = "Arial")+
  #annotate("text", x=4, y=-3, color ="#ffa600", label="Low Incidence years",family = "Arial")

f5_1

################################################################################
# PGT 4
# Loading 48 variables data (Rcode: "PGT4")
data_fr2 <- read_csv("5year_pgt4_may15.csv")

# Select 48 variables
data2 <- data_fr2 %>%
  select(3:51)

# Store the potential explanatory(predictors)/dependent(response) variables
predictors2 <- data_fr2[,c(3:50)]
response2 <- data_fr2[,c(51)]

# 5 years PGT4 data 
data_al_p2 <- cbind.data.frame(predictors2,response2)
colnames(data_al_p2)[49] <- "PGT4"

# Bad /High Incidence (HI) aflatoxin years
PGT4_hi <- data_fr2  %>% 
  filter(crop_year %in% c(2018,2019,2020)) %>% 
  left_join(observed_PGT20[,c(1,2)], by = c("crop_year", "County"))


predictors4_hi <- PGT4_hi[,c(3:50)]
response4_hi <- PGT4_hi[,c(51)]

PGT4_hi_tidy <- cbind.data.frame(predictors4_hi,response4_hi)

colnames(PGT4_hi_tidy)[49] <- "PGT4"


# Good /Low Incidence (LI) aflatoxin years
PGT4_li <- data_fr2 %>%
  filter(crop_year %in% c(2021,2022)) %>% 
  left_join(observed_PGT20[,c(1,2,4)], by = c("crop_year", "County"))

predictors4_li <- PGT4_li[,c(3:50)]
response4_li <- PGT4_li[,c(51)]

PGT4_li_tidy <- cbind.data.frame(predictors4_li,response4_li)

colnames(PGT4_li_tidy)[49]<-"PGT4"

################################################################################


fig5_4 <- data.frame(x = data_fr2$PGT4, 
                      y = PGT20_prop_ossrr(data_al_p2, FinalModel), 
                      year="All Years")

fig5_5 <- data.frame(x = PGT4_hi_tidy$PGT4, 
                      y = PGT20_prop_ossrr(PGT4_hi_tidy, FinalModel), 
                      year="HI Years")

fig5_6 <- data.frame(x = PGT4_li_tidy$PGT4,
                     y = PGT20_prop_ossrr(PGT4_li_tidy,FinalModel), 
                     year="LI Years")
fig5_data2 <- rbind(fig5_4, fig5_5, fig5_6)

f5_2 <- ggplot(fig5_data2,
               aes(y, x,
                   group = year, color = year, shape = year))+
  geom_point()+
  stat_poly_line(aes(linetype=year), se = F) +
  #stat_poly_eq(use_label(c("eq", "R2"))) +
  scale_color_manual(values = c("#003f5c","#bc5090","#ffa600"))+
  theme_classic() +
  scale_shape_manual(values=c(1,4,8))+
  scale_y_continuous(limits = c(-3, 45))+
  labs(title = "Observed PGT4 (%)") + 
  xlab("Predicted PGT20 (%)") +
  theme(text = element_text(family = "Arial", size = 12),
        axis.title.y = element_blank(),
        legend.position ='none',
        axis.title.x = element_text(family = "Arial", size = 12),
        axis.text.y = element_text(family = "Arial", size = 12),
        axis.text.x = element_text(family = "Arial", size = 12),
        plot.title = element_text(family = "Arial", size = 12),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

f5_2

# revision 

fig5_data_new <- rbind(fig5_1,fig5_2)

fig5_primary <-  ggplot(fig5_data_new,
                        aes(y, x,
                            group = year, shape = year, color = year))+
  geom_point(size = 3)+
  geom_smooth(method = "lm", aes(linetype=year), se = FALSE) +
  scale_color_manual(values = c("All Years" = "black", "HI Years" = "black"))+
  
  theme_classic() +
  scale_shape_manual(values=c(1, 4))+
  scale_y_continuous(limits = c(-3, 40))+
  scale_x_continuous(limits = c(-3, 40))+
  labs(y = "Observed PGT20 (%)") + 
  xlab("Predicted PGT20 (%)") +
  
  guides(shape = guide_legend(""),
         linetype = guide_legend())+
  geom_abline(intercept = 0, slope = 1, linetype = "dotted", color = "black") +
  theme(legend.position = c(0.2,0.8),
        legend.title = element_blank(),
        legend.text = element_text(family = "Arial", size = 12),
        axis.title.x = element_text(family = "Arial", size = 12),
        axis.title.y = element_text(family = "Arial", size = 12, 
                                   margin = margin(r = 20, unit = "pt")),
        axis.text.y = element_text(family = "Arial", size = 12),
        axis.text.x = element_text(family = "Arial", size = 12),
        plot.title = element_text(family = "Arial", size = 12),
        legend.key.width = unit(1, "cm"))

fig5_primary

fig5_data2_new <- rbind(fig5_4,fig5_5)

fig5_second <-  ggplot(fig5_data2_new,
                        aes(y, x,
                            group = year, shape = year, color = year))+
  geom_point(size = 3)+
  geom_smooth(method = "lm", aes(linetype=year), se = FALSE) +
  scale_color_manual(values = c("All Years" = "black", "HI Years" = "black"))+
  
  theme_classic() +
  scale_shape_manual(values=c(1, 4))+
  scale_y_continuous(limits = c(-3, 40))+
  scale_x_continuous(limits = c(-3, 40))+
  labs(y = "Observed PGT4 (%)") + 
  xlab("Predicted PGT20 (%)") +
  
  guides(shape = guide_legend(""),
         linetype = guide_legend())+
  geom_abline(intercept = 0, slope = 1, linetype = "dotted", color = "black") +
  theme(legend.position = 'none',
        axis.title.x = element_text(family = "Arial", size = 12),
        axis.title.y = element_text(family = "Arial", size = 12, 
                                    margin = margin(r = 20, unit = "pt")),
        axis.text.y = element_text(family = "Arial", size = 12),
        axis.text.x = element_text(family = "Arial", size = 12),
        plot.title = element_text(family = "Arial", size = 12),
        legend.key.width = unit(1, "cm"))
fig5_primary
fig5_second



fig5_third <-  ggplot(fig5_3,
                       aes(y, x))+
  geom_point(aes(color = year), size = 3)+
  geom_smooth(aes(color = year),method = "lm", se = FALSE) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dotted", color = "black") +
  scale_color_manual(values = c("LI Years" = "black")) + 

  theme_classic() +

  scale_y_continuous(limits = c(0, 5))+
  scale_x_continuous(limits = c(0, 5))+
  labs(y = "Observed PGT20 (%)", x = "Predicted PGT20 (%)") +
  theme(legend.position = c(0.2,0.8),
        legend.title = element_blank(),
        legend.text = element_text(family = "Arial", size = 12),
        axis.title.x = element_text(family = "Arial", size = 12),
        axis.title.y = element_text(family = "Arial", size = 12, 
                                    margin = margin(r = 20, unit = "pt")),
        axis.text.y = element_text(family = "Arial", size = 12),
        axis.text.x = element_text(family = "Arial", size = 12),
        plot.title = element_text(family = "Arial", size = 12),
        legend.key.width = unit(1, "cm"))

fig5_third



fig5_fourth <-  ggplot(fig5_6,
                      aes(y, x))+
  geom_point(aes(color = year), size = 3)+
  geom_smooth(aes(color = year),method = "lm", se = FALSE) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dotted", color = "black") +
  scale_color_manual(values = c("LI Years" = "black")) + 
  
  theme_classic() +
  
  labs(y = "Observed PGT4 (%)", x = "Predicted PGT20 (%)") +
  theme(legend.position = 'none',
        axis.title.x = element_text(family = "Arial", size = 12),
        axis.title.y = element_text(family = "Arial", size = 12, 
                                    margin = margin(r = 20, unit = "pt")),
        axis.text.y = element_text(family = "Arial", size = 12),
        axis.text.x = element_text(family = "Arial", size = 12),
        plot.title = element_text(family = "Arial", size = 12),
        legend.key.width = unit(1, "cm"))

fig5_fourth

fig5 <- plot_grid(fig5_primary, fig5_third,fig5_second,fig5_fourth,
                  labels = c('(a)','(b)', '(c)', '(d)'), 
                  nrow = 2,ncol=2,                
                  label_size = 12,
                  label_x = -0.01,
                  label_y = 1.01)

fig5

ggsave(plot = fig5, filename = "figure5_.png",
       width = 174, height = 170, units = 'mm')




# figure 6 

BowenModel <- "PGT20=-328.5 + d3d4wk*3.34 + MaxT6wk*9.136"

fig6_1 <- data.frame(x = data_al_p$PGT20, 
                     y = PGT20_prop_ossrr(data_al_p, BowenModel),
                     year="All Years")

fig6_2 <- data.frame(x = data_al_bad_year$PGT20,
                     y = PGT20_prop_ossrr(data_al_bad_year, BowenModel), 
                     year="HI Years")

fig6_3 <- data.frame(x = data_al_good_year$PGT20,
                     y = PGT20_prop_ossrr(data_al_good_year,BowenModel), 
                     year="LI Years")

fig6_data <- rbind(fig6_1,fig6_2,fig6_3)
write_csv(fig6_data, "fig6data.csv")
#fig6_data$y <- ifelse(fig6_data$y <0, 0, fig6_data$y)

f6_1 <- ggplot(fig6_data,
               aes(y, x,
                   group = year, color = year, shape = year))+
  geom_point()+
  stat_poly_line(aes(linetype=year), se = F) +
  #stat_poly_eq(use_label(c("eq", "R2"))) +
  scale_color_manual(values = c("#003f5c","#bc5090","#ffa600"))+
  theme_classic() +
  scale_shape_manual(values=c(1,4,8))+
  labs(title = "Observed PGT20 (%)") + 
  xlab("Predicted PGT20 (%)") +
  theme(text = element_text(family = "Arial", size = 12),
        axis.title.y = element_blank(),
        legend.position = c(0.2, 0.8),
        legend.title = element_blank(),
        legend.text = element_text(family = "Arial", size = 12),
        axis.title.x = element_text(family = "Arial", size = 12),
        axis.text.y = element_text(family = "Arial", size = 12),
        axis.text.x = element_text(family = "Arial", size = 12),
        plot.title = element_text(family = "Arial", size = 12),
        plot.margin = unit(c(0.5,0.5,0.5,0.9), "cm"))  
  #annotate("text", x=20, y=55, color ="#003f5c", label="All 5 years",family = "Arial")+
  #annotate("text", x=27, y=20, color ="#bc5090", label="High Incidence \n years",family = "Arial")+
  #annotate("text", x=15, y=-0.1, color ="#ffa600", label="Low Incidence years",family = "Arial")


f6_1


fig6_4 <- data.frame(x = data_fr2$PGT4, 
                     y = PGT20_prop_ossrr(data_al_p2, BowenModel), 
                     year="All Years")

fig6_5 <- data.frame(x = PGT4_hi_tidy$PGT4, 
                     y = PGT20_prop_ossrr(PGT4_hi_tidy, BowenModel), 
                     year="HI Years")

fig6_6 <- data.frame(x = PGT4_li_tidy$PGT4,
                     y = PGT20_prop_ossrr(PGT4_li_tidy,BowenModel), 
                     year="LI Years")
fig6_data2 <- rbind(fig6_4, fig6_5, fig6_6)

f6_2 <- ggplot(fig6_data2,
               aes(y, x,
                   group = year, color = year, shape = year))+
  geom_point()+
  stat_poly_line(aes(linetype=year), se = F) +
  #stat_poly_eq(use_label(c("eq", "R2"))) +
  scale_color_manual(values = c("#003f5c","#bc5090","#ffa600"))+
  theme_classic() +
  scale_shape_manual(values=c(1,4,8))+
  labs(title = "Observed PGT4 (%)") + 
  xlab("Predicted PGT20 (%)") +
  theme(text = element_text(family = "Arial", size = 12),
        axis.title.y = element_blank(),
        legend.position ='none',
        axis.title.x = element_text(family = "Arial", size = 12),
        axis.text.y = element_text(family = "Arial", size = 12),
        axis.text.x = element_text(family = "Arial", size = 12),
        plot.title = element_text(family = "Arial", size = 12),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) 

f6_2


fig6 <- plot_grid(f6_1, f6_2,
                  labels = c('(a)','(b)'), 
                  nrow = 1, label_x = -0.03, label_y = 1.01)

fig6


ggsave("figure6__f.png", plot = fig6,
        width = 174, height = 80, units = 'mm')

# revision 

fig6_data_new <- rbind(fig6_1,fig6_2)

fig6_primary <-  ggplot(fig6_data_new,
                        aes(y, x,
                            group = year, shape = year, color = year))+
  geom_point(size = 3)+
  geom_smooth(method = "lm", aes(linetype=year), se = FALSE) +
  scale_color_manual(values = c("All Years" = "black", "HI Years" = "black"))+
  
  theme_classic() +
  scale_shape_manual(values=c(1, 4))+
  scale_y_continuous(limits = c(-3, 40))+
  scale_x_continuous(limits = c(-3, 40))+
  labs(y = "Observed PGT20 (%)") + 
  xlab("Predicted PGT20 (%)") +
  
  guides(shape = guide_legend(""),
         linetype = guide_legend())+
  geom_abline(intercept = 0, slope = 1, linetype = "dotted", color = "black") +
  theme(legend.position = c(0.2,0.8),
        legend.title = element_blank(),
        legend.text = element_text(family = "Arial", size = 12),
        axis.title.x = element_text(family = "Arial", size = 12),
        axis.title.y = element_text(family = "Arial", size = 12, 
                                    margin = margin(r = 20, unit = "pt")),
        axis.text.y = element_text(family = "Arial", size = 12),
        axis.text.x = element_text(family = "Arial", size = 12),
        plot.title = element_text(family = "Arial", size = 12),
        legend.key.width = unit(1, "cm"))

fig6_primary

fig6_data2_new <- rbind(fig6_4,fig6_5)

fig6_second <-  ggplot(fig6_data2_new,
                       aes(y, x,
                           group = year, shape = year, color = year))+
  geom_point(size = 3)+
  geom_smooth(method = "lm", aes(linetype=year), se = FALSE) +
  scale_color_manual(values = c("All Years" = "black", "HI Years" = "black"))+
  
  theme_classic() +
  scale_shape_manual(values=c(1, 4))+
  scale_y_continuous(limits = c(-3, 48))+
  scale_x_continuous(limits = c(-3, 48))+
  labs(y = "Observed PGT4 (%)") + 
  xlab("Predicted PGT20 (%)") +
  
  guides(shape = guide_legend(""),
         linetype = guide_legend())+
  geom_abline(intercept = 0, slope = 1, linetype = "dotted", color = "black") +
  theme(legend.position = 'none',
        axis.title.x = element_text(family = "Arial", size = 12),
        axis.title.y = element_text(family = "Arial", size = 12, 
                                    margin = margin(r = 20, unit = "pt")),
        axis.text.y = element_text(family = "Arial", size = 12),
        axis.text.x = element_text(family = "Arial", size = 12),
        plot.title = element_text(family = "Arial", size = 12),
        legend.key.width = unit(1, "cm"))
fig6_primary
fig6_second



fig6_third <-  ggplot(fig6_3,
                      aes(y, x))+
  geom_point(aes(color = year), size = 3)+
  geom_smooth(aes(color = year),method = "lm", se = FALSE) + 

  scale_color_manual(values = c("LI Years" = "black")) + 
  
  theme_classic() +
  

  labs(y = "Observed PGT20 (%)", x = "Predicted PGT20 (%)") +
  theme(legend.position = c(0.2,0.8),
        legend.title = element_blank(),
        legend.text = element_text(family = "Arial", size = 12),
        axis.title.x = element_text(family = "Arial", size = 12),
        axis.title.y = element_text(family = "Arial", size = 12, 
                                    margin = margin(r = 20, unit = "pt")),
        axis.text.y = element_text(family = "Arial", size = 12),
        axis.text.x = element_text(family = "Arial", size = 12),
        plot.title = element_text(family = "Arial", size = 12),
        legend.key.width = unit(1, "cm"))

fig6_third



fig6_fourth <-  ggplot(fig6_6,
                       aes(y, x))+
  geom_point(aes(color = year), size =3)+
  geom_smooth(aes(color = year),method = "lm", se = FALSE) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dotted", color = "black") +
  scale_color_manual(values = c("LI Years" = "black")) + 
  
  theme_classic() +
  
  labs(y = "Observed PGT4 (%)", x = "Predicted PGT20 (%)") +
  theme(legend.position = 'none',
        axis.title.x = element_text(family = "Arial", size = 12),
        axis.title.y = element_text(family = "Arial", size = 12, 
                                    margin = margin(r = 20, unit = "pt")),
        axis.text.y = element_text(family = "Arial", size = 12),
        axis.text.x = element_text(family = "Arial", size = 12),
        plot.title = element_text(family = "Arial", size = 12),
        legend.key.width = unit(1, "cm"))

fig6_fourth

fig6 <- plot_grid(fig6_primary, fig6_third,fig6_second,fig6_fourth,
                  labels = c('(a)','(b)', '(c)', '(d)'), 
                  nrow = 2,ncol=2,                
                  label_size = 12,
                  label_x = -0.01,
                  label_y = 1.01)

fig6


ggsave(plot = fig6, filename = "figure6_.png",
       width = 174, height = 170, units = 'mm')

# supplementary table
colnames(data_al_p2)[49]<-"PGT20"

set.seed(21)# for reproducibility
aydata_acceptable_4 <- model_matrix_eff(bootstrap_best_model(data_al_p2,
                                                             n_samples = 100, 
                                                             mdls = models[c(Selected_model$Model_Number)]))%>% 
  mutate(model = models[c(Selected_model$Model_Number)])


# Assess how well the "Acceptable" model perform on Bad / High Incidence(HI) aflatoxin years
colnames(PGT4_hi_tidy)[49]<-"PGT20"
set.seed(21)# for reproducibility
hidata_acceptable_4 <- model_matrix_eff(bootstrap_best_model(PGT4_hi_tidy,
                                                             n_samples = 100, 
                                                             mdls = models[c(Selected_model$Model_Number)]))%>% 
  mutate(model = models[c(Selected_model$Model_Number)])


# Assess how well the "Acceptable" model perform on Good / Low Incidence(LI) aflatoxin years
colnames(PGT4_li_tidy)[49]<-"PGT20"
set.seed(21)# for reproducibility
lidata_acceptable_4 <- model_matrix_eff(bootstrap_best_model(PGT4_li_tidy,
                                                             n_samples = 100, 
                                                             mdls = models[c(Selected_model$Model_Number)]))%>% 
  mutate(model = models[c(Selected_model$Model_Number)])

r_181_pgt4 <- data.frame(a1 = aydata_acceptable_4$RMSE_mean,
                          a2 = aydata_acceptable_4$Rsqared_mean,
                          a3 = aydata_acceptable_4$MAE_mean,
                          b1 = hidata_acceptable_4$RMSE_mean,
                          b2 = hidata_acceptable_4$Rsqared_mean,
                          b3 = hidata_acceptable_4$MAE_mean, 
                          g1 = lidata_acceptable_4$RMSE_mean,
                          g2 = lidata_acceptable_4$Rsqared_mean,
                          g3 = lidata_acceptable_4$MAE_mean)

write_csv(r_181_pgt4, "R2_acceptable181_pgt4.csv")


###############################################################################
# PGT 15
# Loading 48 variables data (Rcode: "PGT4")
data_fr3 <- read_csv("5year_pgt15.csv")

# Select 48 variables
data3 <- data_fr3 %>%
  select(3:51)

# Store the potential explanatory(predictors)/dependent(response) variables
predictors3 <- data_fr3[,c(3:50)]
response3 <- data_fr3[,c(51)]

# 5 years PGT15 data 
data_al_p3 <- cbind.data.frame(predictors3,response3)
colnames(data_al_p3)[49] <- "PGT20"

# Bad /High Incidence (HI) aflatoxin years
PGT15_hi <- data_fr3  %>% 
  filter(crop_year %in% c(2018,2019,2020)) %>% 
  left_join(observed_PGT20[,c(1,2)], by = c("crop_year", "County"))


predictors15_hi <- PGT15_hi[,c(3:50)]
response15_hi <- PGT15_hi[,c(51)]

PGT15_hi_tidy <- cbind.data.frame(predictors15_hi,response15_hi)

colnames(PGT15_hi_tidy)[49] <- "PGT20"


# Good /Low Incidence (LI) aflatoxin years
PGT15_li <- data_fr3 %>%
  filter(crop_year %in% c(2021,2022)) %>% 
  left_join(observed_PGT20[,c(1,2,4)], by = c("crop_year", "County"))

predictors15_li <- PGT15_li[,c(3:50)]
response15_li <- PGT15_li[,c(51)]

PGT15_li_tidy <- cbind.data.frame(predictors15_li,response15_li)

colnames(PGT15_li_tidy)[49]<-"PGT20"

# supplementary table
set.seed(21)# for reproducibility
aydata_acceptable_15 <- model_matrix_eff(bootstrap_best_model(data_al_p3,
                                                              n_samples = 100, 
                                                              mdls = models[c(Selected_model$Model_Number)]))%>% 
  mutate(model = models[c(Selected_model$Model_Number)])


# Assess how well the "Acceptable" model perform on Bad / High Incidence(HI) aflatoxin years
set.seed(21)# for reproducibility
hidata_acceptable_15 <- model_matrix_eff(bootstrap_best_model(PGT15_hi_tidy,
                                                              n_samples = 100, 
                                                              mdls = models[c(Selected_model$Model_Number)]))%>% 
  mutate(model = models[c(Selected_model$Model_Number)])

# Assess how well the "Acceptable" model perform on Good / Low Incidence(LI) aflatoxin years
set.seed(21)# for reproducibility
lidata_acceptable_15 <- model_matrix_eff(bootstrap_best_model(PGT15_li_tidy,
                                                              n_samples = 100, 
                                                              mdls = models[c(Selected_model$Model_Number)]))%>% 
  mutate(model = models[c(Selected_model$Model_Number)])

# save the model
aydata_acceptable_15$model2 <- sapply(aydata_acceptable_15$model, function(x) paste(x, collapse = ","))
variable_sel_step[Acceptable_Model$Model_Number]
write_csv(aydata_acceptable_15, file = "acceptable181_pgt15.csv")

r_181_pgt15 <- data.frame(a1 = aydata_acceptable_15$RMSE_mean,
                          a2 = aydata_acceptable_15$Rsqared_mean,
                          a3 = aydata_acceptable_15$MAE_mean,
                          b1 = hidata_acceptable_15$RMSE_mean,
                          b2 = hidata_acceptable_15$Rsqared_mean,
                          b3 = hidata_acceptable_15$MAE_mean, 
                          g1 = lidata_acceptable_15$RMSE_mean,
                          g2 = lidata_acceptable_15$Rsqared_mean,
                          g3 = lidata_acceptable_15$MAE_mean)

write_csv(r_181_pgt15, "R2_acceptable181_pgt15.csv")


#######################################################################

