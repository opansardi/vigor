# Altruistic punishment in action: movement vigor in neuroeconomic choice -----
# Generalized linear mixed models analyses – Study1 & Study2 ------------------


# Clear Workspace -------------------------------------------------------------
rm(list = ls())              
set.seed(123)

# Libraries ----------------------------------------------------------------
library(readxl)   
library(dplyr)    
library(plyr)   
library(lme4)     
library(lmerTest) 
library(emmeans)  
library(datawizard) 
library(olsrr)   
library(moments) 
library(car)      
library(afex)    

# Set sum constrasts globally for all factors in the session ------------------
options(contrasts=c("contr.sum", "contr.poly"))



# STUDY1 – Ultimatum Game (Behavioural) ---------------------------------------

## 1A. Data preparation ------------------------------------------------------
# insert here the path to the data you downloaded
data_all  <- read_excel("C:/Users/.../data_vigor_Study1&Study2.xlsx", sheet = "Study1")

### Drop control trials ---------------------------------------------------------
data_clean <- data_all %>% 
  filter(OFFER != 0)               

### Convert to factors ---------------------------------------------------------
data_clean <- data_clean %>%
  mutate(
    Subject  = factor(Subject),
    DECISION = factor(DECISION, levels = c("2", "1"))
  )

## 1B. Centring predictor ----------------------------------------------------
offer_mean <- mean(data_clean$OFFER)
data_clean$OFFER <- data_clean$OFFER - offer_mean

## 1C. Model specification ---------------------------------------------------
optimizer <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6))
options(contrasts = c("contr.sum", "contr.poly"))
### Random effects structure selection (BIC)----------------------------------
m00 <- glmer(DECISION ~ OFFER + (1 | Subject),
             data   = data_clean,
             family = binomial(link = logit),
             control = optimizer)

m01 <- glm(DECISION ~ OFFER,
           data   = data_clean,
           family = binomial(link = logit))

bic_vals <- c(m00 = BIC(m00), m01 = BIC(m01))
print(bic_vals)
cat("Selected model (lowest BIC):", names(which.min(bic_vals)), "\n") #m00

### Likelihood‑ratio test -----------------------------------------------------
final_mod <- mixed(DECISION ~ OFFER + (1 | Subject),
                   data    = data_clean,
                   family  = binomial(link = logit),
                   control = optimizer,
                   method  = "LRT")
print(final_mod$anova_table)

### Summary of the model with Wald test --------------------------------------
sel_mod <- m00
summary(sel_mod)



# STUDY 1 – Reaction Time (RT) -----------------------------------------------

## 2A. Data preparation ------------------------------------------------------
# insert here the path to the data you downloaded
data_all  <- read_excel("C:/Users/.../data_vigor_Study1&Study2.xlsx", sheet = "Study1")
### Drop control trials ----------------------------------------------------
data_clean <- data_all %>%
  filter(OFFER != 0,                    
         DECISION != 0)               
        
### Drop rejections at OFFER = 5 (not a costly punishment)------------------
data_clean <- data_clean %>%
  filter(!(OFFER == 5 & DECISION == 2))

### Convert to factors --------------------------------------------------------------
data_clean <- data_clean %>%
  mutate(
    Subject   = factor(Subject),
    DECISION  = factor(DECISION, levels = c("1", "2"), labels = c("Acc", "Rej")),
    DIRECTION = factor(DIRECTION)
  )

## 2B. Subject‑level outlier removal (1.5 × IQR rule) ------------------------
feature = 'RT'
m = 1.5
data <- data_clean%>% group_by(Subject) %>% 
  filter(.data[[feature]] <= (quantile(.data[[feature]], .75) + m* (quantile(.data[[feature]], .75) - quantile(.data[[feature]], .25)) )) %>% 
  filter(.data[[feature]] >= (quantile(.data[[feature]], .25) - m* (quantile(.data[[feature]], .75) - quantile(.data[[feature]], .25)) )) %>%
  ungroup()

## 2C. Normality checks ------------------------------------------------------
print(shapiro.test(data$RT))
print(skewness(data$RT))

## 2D. Centring OFFER --------------------------------------------------------
offer_mean <- mean(data$OFFER)
data$OFFER <- data$OFFER - offer_mean

## 2E. Model specification ---------------------------------------------------
optimizer <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6))

### Gamma or gaussian? -------------------------------------------------------
m00 <- glmer(RT ~ DECISION * OFFER + DIRECTION + (1 | Subject),
             data   = data,
             family = Gamma(link = log),
             control = optimizer)

m01 <- lmer(RT ~ DECISION * OFFER + DIRECTION + (1 | Subject),
            data = data,
            control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)))

bic_vals <- c(m00 = BIC(m00), m01 = BIC(m01))
print(bic_vals)
cat("Selected model (lowest BIC):", names(which.min(bic_vals)), "\n")

### Random effects structure selection (BIC) ----------------------------------
m02 =  glm(RT ~ DECISION*OFFER + DIRECTION, 
           family = Gamma(link = log), data = data)
models = c('m00','m01')
bics = c(BIC(m00),BIC(m02))
bics
index = which.min(bics)
sprintf('Model with lowest BIC: %s (BIC=%.2f)', models[index], min(bics)) #m00

# Likelihood‑ratio test ------------------------------------------------------
final_mod <- mixed(RT ~ DECISION * OFFER + DIRECTION + (1 | Subject),
                   data    = data,
                   family  = Gamma(link = log),
                   control = optimizer,
                   method  = "LRT")
print(final_mod$anova_table)

# Summary of the model and Wald test
sel_mod <- m00
summary(sel_mod)

#Post-hoc comparison of offer slopes for accept vs. reject decisions ---------
emm  <- emtrends(sel_mod, ~ DECISION, var = "OFFER", adjust = "holm")
print(summary(emm, infer = c(TRUE, TRUE)))



# STUDY 1 – Peak Velocity (PV) -----------------------------------------------

## 3A. Data preparation ------------------------------------------------------
# insert here the path to the data you downloaded
data_all  <- read_excel("C:/Users/.../data_vigor_Study1&Study2.xlsx", sheet = "Study1")

### Drop control trials and not costly punishments --------------------------
data_clean <- data_all %>%
  filter(OFFER != 0, DECISION != 0) %>%
  filter(!(OFFER == 5 & DECISION == 2))

### Convert to factors -------------------------------------------------------
data_clean <- data_clean %>%
  mutate(
    Subject   = factor(Subject),
    DECISION  = factor(DECISION, levels = c("1", "2"), labels = c("Acc", "Rej")),
    DIRECTION = factor(DIRECTION)
  )

### Remove outliers (subject level) ------------------------------------------
feature = 'PV'
m = 1.5
data <- data_clean%>% group_by(Subject) %>% 
  filter(.data[[feature]] <= (quantile(.data[[feature]], .75) + m* (quantile(.data[[feature]], .75) - quantile(.data[[feature]], .25)) )) %>% 
  filter(.data[[feature]] >= (quantile(.data[[feature]], .25) - m* (quantile(.data[[feature]], .75) - quantile(.data[[feature]], .25)) )) %>%
  ungroup()

## 3B. Centring OFFER --------------------------------------------------------
offer_mean <- mean(data$OFFER)
data$OFFER <- data$OFFER - offer_mean

## 3C. Normality diagnostics -------------------------------------------------
print(shapiro.test(data$PV))
print(skewness(data$PV))

## 3D. Model specification ---------------------------------------------------
### Random effects structure selection ---------------------------------------
m00 <- lmer(PV ~ DECISION * OFFER + DIRECTION + (1 | Subject),
            data    = data,
            control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)))

m01 <- lm(PV ~ DECISION * OFFER + DIRECTION, data = data)

bic_vals <- c(m00 = BIC(m00), m01 = BIC(m01))
print(bic_vals)
cat("Selected model (lowest BIC):", names(which.min(bic_vals)), "\n") #m00

### LRT ----------------------------------------------------------------------
final_mod <- mixed(PV ~ DECISION * OFFER + DIRECTION + (1 | Subject),
                   data    = data,
                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)),
                   method  = "LRT")
print(final_mod$anova_table)

### Summary of the model and Wald tests --------------------------------------
sel_mod <- m00
summary(sel_mod)

# Post-hoc comparison of offer slopes for accept vs. reject decisions --------
emm  <- emtrends(sel_mod, ~ DECISION, var = "OFFER", adjust = "holm")
print(summary(emm, infer = c(TRUE, TRUE)))


# STUDY 2 – Behavioural------------------------------------------------------

## 4A. Data preparation ------------------------------------------------------
# insert here the path to the data you downloaded
data_all  <- read_excel("C:/Users/.../data_vigor_Study1&Study2.xlsx", sheet = "Study2")
### Drop practice trials and create EOP
data_clean <- data_all %>%
  filter(TrialType == 1) %>%
  mutate(EOP = OtherCost / SelfCost)

### Convert to factors ---------------------------------------------------------
data_clean <- data_clean %>%
  mutate(
    Subject   = factor(Subject),
    EOP       = factor(EOP, levels = c("5", "10")),
    selfcost  = factor(SelfCost),
    Response  = as.factor(Response)
  )

## 4B. Centring Block --------------------------------------------------------

block_mean <- mean(data_clean$Block)
data_clean$Block <- data_clean$Block - block_mean

## 4C. Model specification ---------------------------------------------------
optimizer <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6))
optimizer1 <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6), calc.derivs = FALSE)
### Random-effects structure selection (BIC) ---------------------------------
m00 <- glmer(Response ~ EOP * selfcost + (Block | Subject),
             data   = data_clean,
             family = binomial(link = logit),
             control = optimizer1)

m01 <- glmer(Response ~ EOP * selfcost + (1 | Subject),
             data   = data_clean,
             family = binomial(link = logit),
             control = optimizer)

m02 <- glm(Response ~ EOP * selfcost, data = data_clean, family = binomial(link = logit))

bic_vals <- c(m00 = BIC(m00), m01 = BIC(m01), m02 = BIC(m02))
print(bic_vals)
cat("Selected model (lowest BIC):", names(which.min(bic_vals)), "\n") #m01

### LRT ------------------------------------------------------------------------
final_mod <- mixed(Response ~ EOP * selfcost + (1 | Subject),
                   data    = data_clean,
                   family  = binomial(link = logit),
                   control = optimizer,
                   method  = "LRT")
print(final_mod$anova_table)

### Summary of the model and Wald tests ---------------------------------------
sel_mod <- m01
summary(sel_mod)

### Post‑hoc contrasts ---------------------------------------------------------
mod_emm <- emmeans(sel_mod, ~ EOP * selfcost)
print(contrast(regrid(mod_emm), "consec", simple = "each", combine = TRUE, adjust = "holm"))



# STUDY 2 – Reaction Time (RT) -----------------------------------------------

## 5A. Data preparation ------------------------------------------------------
# insert here the path to the data you downloaded
data_all  <- read_excel("C:/Users/.../data_vigor_Study1&Study2.xlsx", sheet = "Study2")

### Drop practice trials and not punishments, create EOP ----------------------
data_clean <- data_all %>%
  filter(TrialType == 1, Response == 1) %>%
  mutate(EOP = OtherCost / SelfCost)

### Convert to factors----------------------------------------------------------
data_clean <- data_clean %>%
  mutate(
    Subject   = factor(Subject),
    EOP       = factor(EOP),
    selfcost  = factor(SelfCost)
  )

## 5B. Outlier removal (subject level) ----------------------------------------
feature = 'RT_ms'
m = 1.5
data <- data_clean%>% group_by(Subject) %>% 
  filter(.data[[feature]] <= (quantile(.data[[feature]], .75) + m* (quantile(.data[[feature]], .75)-quantile(.data[[feature]], .25)) )) %>% 
  filter(.data[[feature]] >= (quantile(.data[[feature]], .25) - m* (quantile(.data[[feature]], .75)-quantile(.data[[feature]], .25)) )) %>%
  ungroup()

### Center Block --------------------------------------------------------------
block_mean <- mean(data$Block)
data$Block <- data$Block - block_mean

## 5C. Normality check---------------------------------------------------------
print(shapiro.test(data$RT_ms))
print(skewness(data$RT_ms))

## 5D. Model specification ---------------------------------------------------
optimizer <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6))

### Random-effects structure selection ---------------------------------------
m00 <- glmer(RT_ms ~ EOP * selfcost + (Block | Subject),
             data   = data,
             family = Gamma(link = log),
             control = optimizer)

m01 <- glmer(RT_ms ~ EOP * selfcost + (1 | Subject),
             data   = data,
             family = Gamma(link = log),
             control = optimizer)

m02 <- glm(RT_ms ~ EOP * selfcost, data = data, family = Gamma(link = log))

bic_vals <- c(m00 = BIC(m00), m01 = BIC(m01), m02 = BIC(m02))
print(bic_vals)
cat("Selected model (lowest BIC):", names(which.min(bic_vals)), "\n") #m00

### LRT ----------------------------------------------------------------------
final_mod <- mixed(RT_ms ~ EOP * selfcost + (Block | Subject),
                   data    = data,
                   family  = Gamma(link = log),
                   control = optimizer,
                   method  = "LRT")
print(final_mod$anova_table)

### Summary of the model and Wald test ---------------------------------------
sel_mod <- m00
summary(sel_mod)


# STUDY 2 – Peak Velocity (PV) -----------------------------------------------

## 6A. Data preparation ------------------------------------------------------
# insert here the path to the data you downloaded
data_all  <- read_excel("C:/Users/.../data_vigor_Study1&Study2.xlsx", sheet = "Study2")
### Drop practice trials, not punishments and create EOP ----------------------
data_clean <- data_all %>%
  filter(TrialType == 1, Response == "1") %>%
  mutate(EOP = OtherCost / SelfCost)

### Convert to factor  --------------------------------------------------------
data_clean <- data_clean %>%
  mutate(
    Subject   = factor(Subject),
    EOP       = factor(EOP),
    selfcost  = factor(SelfCost)
  )

## 6B. Remove outliers (subject-level) ----------------------------------------
feature = 'PV'
m = 1.5
data <- data_clean%>% group_by(Subject) %>% 
  filter(.data[[feature]] <= (quantile(.data[[feature]], .75) + m* (quantile(.data[[feature]], .75)-quantile(.data[[feature]], .25)) )) %>% 
  filter(.data[[feature]] >= (quantile(.data[[feature]], .25) - m* (quantile(.data[[feature]], .75)-quantile(.data[[feature]], .25)) )) %>%
  ungroup()

### Center block -------------------------------------------------------------
block_mean <- mean(data$Block)
data$Block <- data$Block - block_mean

## 6C. Normality Check -------------------------------------------------------
print(shapiro.test(data$PV))
print(skewness(data$PV))

## 6D. Model specification ---------------------------------------------------

### Random-effects structure selection (BIC)----------------------------------
m00 <- lmer(PV ~ EOP * selfcost + (Block | Subject),
            data    = data,
            control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)))

m01 <- lmer(PV ~ EOP * selfcost + (1 | Subject), data = data)

m02 <- lm(PV ~ EOP * selfcost, data = data)

bic_vals <- c(m00 = BIC(m00), m01 = BIC(m01), m02 = BIC(m02))
print(bic_vals)
cat("Selected model (lowest BIC):", names(which.min(bic_vals)), "\n") #m00

### LRT ----------------------------------------------------------------------
final_mod <- mixed(PV ~ EOP * selfcost + (Block | Subject),
                   data    = data,
                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)),
                   method  = "LRT")
print(final_mod$anova_table)

### Summary of the model and Wald test
sel_mod <- m00
summary(sel_mod)


# Study 2 - Modulation of the effect of EOP on RT and PV by desire to punish (q3) and by response to abuse of trust (qmean2) ----

# Study 2 - RT ~ EOP * selfcost * q3 (desire to punish)---------------------------------------
## 7A. Data preparation ------------------------------------------------------
# insert here the path to the data you downloaded
data_all  <- read_excel("C:/Users/.../data_vigor_Study1&Study2.xlsx", sheet = "Study2")
### Drop practice trials and not punishments, create EOP ----------------------
data_clean <- data_all %>%
  filter(TrialType == 1, Response == 1) %>%
  mutate(EOP = OtherCost / SelfCost)

### Convert to factors----------------------------------------------------------
data_clean <- data_clean %>%
  mutate(
    Subject   = factor(Subject),
    EOP       = factor(EOP),
    selfcost  = factor(SelfCost)
  )

## 7B. Outlier removal (subject level) ----------------------------------------
feature = 'RT_ms'
m = 1.5
data <- data_clean%>% group_by(Subject) %>% 
  filter(.data[[feature]] <= (quantile(.data[[feature]], .75) + m* (quantile(.data[[feature]], .75)-quantile(.data[[feature]], .25)) )) %>% 
  filter(.data[[feature]] >= (quantile(.data[[feature]], .25) - m* (quantile(.data[[feature]], .75)-quantile(.data[[feature]], .25)) )) %>%
  ungroup()

## 7C. Calculate the mean and standard deviation of Q3 ------------------------
q3_mean <- mean(data$Q3)
q3_sd <- sd(data$Q3)
# Centered q1
data$q3 <- (data$Q3 - q3_mean)

## 7D. Center Block ----------------------------------------------------------
block_mean <- mean(data$Block)
data$Block <- data$Block - block_mean

## 7D. Model specification ---------------------------------------------------
optimizer <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6))

### Fit the same model with the additional EOP * q3 int -----------------------
m00 <- glmer(RT_ms ~ EOP * selfcost + EOP * q3 + (Block | Subject),
             data   = data,
             family = Gamma(link = log),
             control = optimizer)


### LRT ----------------------------------------------------------------------
final_mod <- mixed(RT_ms ~ EOP * selfcost + EOP * q3 + (Block | Subject),
                   data    = data,
                   family  = Gamma(link = log),
                   control = optimizer,
                   method  = "LRT")
print(final_mod$anova_table)

### Summary of the model and Wald test ---------------------------------------
sel_mod <- m00
summary(sel_mod)

# Study 2 - RT ~ EOP * selfcost * qmean2 (response to abuse of trust)--------------------

## 8A. Center qmean2 ----------------------------------------------------------
qmean2 <- mean(data$qmean2)
qmean2_sd <- sd(data$qmean2)
data$qmean2 <- (data$qmean2 - qmean2)

## 8B. Fit the same model with the additional EOP * qmean2 int -------------------
m00 <- glmer(RT_ms ~ EOP * selfcost + EOP * qmean2 + (Block | Subject),
             data   = data,
             family = Gamma(link = log),
             control = optimizer)


## 8C. LRT ----------------------------------------------------------------------
final_mod <- mixed(RT_ms ~ EOP * selfcost + EOP * qmean2 + (Block | Subject),
                   data    = data,
                   family  = Gamma(link = log),
                   control = optimizer,
                   method  = "LRT")
print(final_mod$anova_table)

## 8D. Summary of the model and Wald test ---------------------------------------
sel_mod <- m00
summary(sel_mod)


# Study 2 - PV ~ EOP * selfcost * q3 (desire to punish)--------------------

## 9A. Data preparation ------------------------------------------------------
# insert here the path to the data you downloaded
data_all  <- read_excel("C:/Users/.../data_vigor_Study1&Study2.xlsx", sheet = "Study2")
### Drop practice trials, not punishments and create EOP ----------------------
data_clean <- data_all %>%
  filter(TrialType == 1, Response == "1") %>%
  mutate(EOP = OtherCost / SelfCost)

### Convert to factor  --------------------------------------------------------
data_clean <- data_clean %>%
  mutate(
    Subject   = factor(Subject),
    EOP       = factor(EOP),
    selfcost  = factor(SelfCost)
  )

## 9B. Remove outliers (subject-level) ----------------------------------------
feature = 'PV'
m = 1.5
data <- data_clean%>% group_by(Subject) %>% 
  filter(.data[[feature]] <= (quantile(.data[[feature]], .75) + m* (quantile(.data[[feature]], .75)-quantile(.data[[feature]], .25)) )) %>% 
  filter(.data[[feature]] >= (quantile(.data[[feature]], .25) - m* (quantile(.data[[feature]], .75)-quantile(.data[[feature]], .25)) )) %>%
  ungroup()

## 9C. Calculate the mean and standard deviation of Q3 ------------------------
q3_mean <- mean(data$Q3)
q3_sd <- sd(data$Q3)
# Centered q1
data$q3 <- (data$Q3 - q3_mean)


## 9D. Center block -------------------------------------------------------------
block_mean <- mean(data$Block)
data$Block <- data$Block - block_mean

## 9E. Fit the same model + EOP*q3 int ------------------------------------------
m00 <- lmer(PV ~ EOP * selfcost + EOP * q3 + (Block | Subject),
            data    = data,
            control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)))


## 9F. LRT ----------------------------------------------------------------------
final_mod <- mixed(PV ~ EOP * selfcost + EOP * q3 + (Block | Subject),
                   data    = data,
                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)),
                   method  = "LRT")
print(final_mod$anova_table)

## 9G. Summary of the model and Wald test ---------------------------------------
sel_mod <- m00
summary(sel_mod)


# Study 2 - PV ~ EOP * selfcost * qmean2 (response to abuse of trust) -------------------

## 10A. Calculate the mean and standard deviation of qmean2 -------------------
qmean2 <- mean(data$qmean2)
qmean2_sd <- sd(data$qmean2)
# Centered q1
data$qmean2 <- (data$qmean2 - qmean2)

## 10B. Fit the same model + EOP*qmean2 int ------------------------------------
m00 <- lmer(PV ~ EOP * selfcost + EOP * qmean2 + (Block | Subject),
            data    = data,
            control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)))


## 10F. LRT --------------------------------------------------------------------
final_mod <- mixed(PV ~ EOP * selfcost + EOP * qmean2 + (Block | Subject),
                   data    = data,
                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)),
                   method  = "LRT")
print(final_mod$anova_table)

## 10G. Summary of the model and Wald test --------------------------------------
sel_mod <- m00
summary(sel_mod)
