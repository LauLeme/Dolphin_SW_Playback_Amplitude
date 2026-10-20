# =====================================================================
# 02 - RELATIVE CHANGE IN ODBA
# =====================================================================
# --- QUESTION ---
#   (a) Do the two source-level categories (higher / lower SL) really
#       differ in the received level (RL) measured on the tagged animals?
#   (b) Does the relative change in ODBA (a proxy for energy expenditure)
#       depend on the mean received level, sex, stimulus type, group size
#       or age?
#
# --- DATA ---
#   data_ODBA: tagged subset, 40 trials on 23 individuals
#   (single animals or pairs; the two trials with > 2 individuals are removed).
#
# --- RESPONSE VARIABLE ---
#   relative_ODBA_change: relative change of ODBA computed for each trial over 30s before and after
#        = (mean ODBA 0-30s after playback - mean ODBA 30s before) / sd ODBA 30s before
#
# --- EXPLANATORY VARIABLES (reference level in brackets) ---
#   Mean_RL : mean received level of the trial (dB re 1 uPa), continuous
#   Sex : Male (reference) / Female
#   Stimulus_type : Familiar (reference) / Unfamiliar
#   Group_size : 1 = single (reference) / 2 = pair (factor)
#   Age :  age of the focal
#
# --- RETAINED MODEL ---
#   glm(relative_ODBA_change ~ Mean_RL + Sex, family = gaussian)
#
# --- VALUES TO LOOK FOR IN THE OUTPUT (manuscript, Results and Table 2) ---
#   > RL, whole dataset: 118 ± 9 dB re 1 uPa (range 100-138)
#   > RL: higher SL = 123 ± 9 vs. lower SL = 114 ± 7
#     Student's t-test: t = 3.405, p = 0.001
#   > summary(glm2.1_RCodba):
#     Mean_RL: estimate = 0.044, SE = 0.015, t = 2.835, p = 0.007
#     SexFemale: estimate = 0.886, SE 0.274, t =  3.230, p = 0.003
#   > Descriptive statistics by sex (Table S2), mean ± sd:
#     Female 1.09 ± 1.12 | Male 0.38 ± 0.71
# =====================================================================



# =====================================================================
# 0. PACKAGES
# =====================================================================
library(tidyverse)
library(MuMIn)    # dredge(), AICc()



# =====================================================================
# 1. RECEIVED LEVEL vs. SOURCE LEVEL
# =====================================================================
# ---------- Mean RL + range per SL category ----------
data_ODBA %>%
  group_by(SL) %>%
  summarise(mean = mean(Mean_RL),
            sd   = sd(Mean_RL),
            max  = max(Mean_RL),
            min  = min(Mean_RL))

# ---------- Conditions of the t-test ----------
# 1) Normality, for each category
data_ODBA %>% filter(SL == "Higher SL") %>% pull(Mean_RL) %>% shapiro.test()
data_ODBA %>% filter(SL == "Lower SL")  %>% pull(Mean_RL) %>% shapiro.test()

# 2) Homoscedasticity
var.test(Mean_RL ~ SL, data = data_ODBA)

# ---------- Student's t-test ----------
# Parametric test
t.test(Mean_RL ~ SL, data = data_ODBA, var.equal = TRUE)


# =====================================================================
# 2. FULL MODEL + EXPLORATORY SELECTION (dredge, no interactions)
# =====================================================================
glmfull_RCodba <- glm(relative_ODBA_change ~ Stimulus_type + Mean_RL + Group_size + Sex + Age,
                      data = data_ODBA, family = gaussian(link = "identity"))

# dredge() needs na.action = "na.fail"
options(na.action = "na.fail")
MuMIn::dredge(glmfull_RCodba)


# =====================================================================
# 3. CANDIDATE MODELS
# =====================================================================
# Note: Not all possible models may have been explored. Model development was
# guided by dredge and likelihood-ratio tests, which helped identify variables
# with weaker and stronger effects.

# ---------- Null model ----------
glm0_RCodba    <- glm(relative_ODBA_change ~ 1, data = data_ODBA, family = gaussian())

# ---------- 1 variable ----------
glm1.1_RCodba  <- glm(relative_ODBA_change ~ Mean_RL, data = data_ODBA, family = gaussian())
glm1.2_RCodba  <- glm(relative_ODBA_change ~ Sex, data = data_ODBA, family = gaussian())
glm1.3_RCodba  <- glm(relative_ODBA_change ~ Age, data = data_ODBA, family = gaussian())
glm1.4_RCodba  <- glm(relative_ODBA_change ~ Group_size, data = data_ODBA, family = gaussian())
glm1.5_RCodba  <- glm(relative_ODBA_change ~ Stimulus_type, data = data_ODBA, family = gaussian())

# ---------- 2 variables ----------
glm2.1_RCodba  <- glm(relative_ODBA_change ~ Mean_RL + Sex, data = data_ODBA, family = gaussian())  
glm2.2_RCodba  <- glm(relative_ODBA_change ~ Mean_RL + Age, data = data_ODBA, family = gaussian())
glm2.3_RCodba  <- glm(relative_ODBA_change ~ Mean_RL * Sex, data = data_ODBA, family = gaussian())
glm2.4_RCodba  <- glm(relative_ODBA_change ~ Sex + Group_size, data = data_ODBA, family = gaussian())
glm2.5_RCodba  <- glm(relative_ODBA_change ~ Mean_RL + Group_size, data = data_ODBA, family = gaussian())
glm2.6_RCodba  <- glm(relative_ODBA_change ~ Sex * Group_size, data = data_ODBA, family = gaussian())
glm2.7_RCodba  <- glm(relative_ODBA_change ~ Mean_RL + Stimulus_type, data = data_ODBA, family = gaussian())  
glm2.8_RCodba  <- glm(relative_ODBA_change ~ Sex + Stimulus_type, data = data_ODBA, family = gaussian())  
glm2.9_RCodba  <- glm(relative_ODBA_change ~ Group_size + Stimulus_type, data = data_ODBA, family = gaussian())  

# ---------- 3 variables ----------
glm3.1_RCodba  <- glm(relative_ODBA_change ~ Mean_RL + Sex + Group_size, data = data_ODBA, family = gaussian())
glm3.2_RCodba  <- glm(relative_ODBA_change ~ Mean_RL + Sex * Group_size, data = data_ODBA, family = gaussian())
glm3.3_RCodba  <- glm(relative_ODBA_change ~ Mean_RL + Sex + Stimulus_type, data = data_ODBA, family = gaussian())
glm3.4_RCodba  <- glm(relative_ODBA_change ~ Mean_RL + Sex + Age, data = data_ODBA, family = gaussian())
glm3.5_RCodba  <- glm(relative_ODBA_change ~ Mean_RL + Sex * Age, data = data_ODBA, family = gaussian())
glm3.6_RCodba  <- glm(relative_ODBA_change ~ Mean_RL + Sex * Stimulus_type, data = data_ODBA, family = gaussian())
glm3.7_RCodba  <- glm(relative_ODBA_change ~ Mean_RL * Stimulus_type + Sex, data = data_ODBA, family = gaussian())
glm3.8_RCodba <- glm(relative_ODBA_change ~ Mean_RL * Sex + Group_size, data = data_ODBA, family = gaussian())
glm3.9_RCodba <- glm(relative_ODBA_change ~ Mean_RL * Sex + Stimulus_type, data = data_ODBA, family = gaussian())

# ---------- 4 variables ----------
glm4.1_RCodba  <- glm(relative_ODBA_change ~ Mean_RL + Sex + Group_size * Stimulus_type, data = data_ODBA, family = gaussian())
glm4.2_RCodba  <- glm(relative_ODBA_change ~ Mean_RL + Sex + Group_size * Age, data = data_ODBA, family = gaussian())
glm4.3_RCodba  <- glm(relative_ODBA_change ~ Mean_RL + Sex + Group_size + Age, data = data_ODBA, family = gaussian())
glm4.4_RCodba  <- glm(relative_ODBA_change ~ Mean_RL + Sex * Group_size + Age, data = data_ODBA, family = gaussian())
glm4.5_RCodba  <- glm(relative_ODBA_change ~ Mean_RL + Sex + Age + Stimulus_type, data = data_ODBA, family = gaussian())
glm4.6_RCodba <- glm(relative_ODBA_change ~ Mean_RL * Sex + Age + Stimulus_type, data = data_ODBA, family = gaussian())
glm4.7_RCodba <- glm(relative_ODBA_change ~ Mean_RL * Age + Sex + Stimulus_type, data = data_ODBA, family = gaussian())


# =====================================================================
# 4. MODEL COMPARISON (AIC, BIC, AICc)
# =====================================================================
# ---------- AIC ----------
AIC(glmfull_RCodba, glm0_RCodba,
    glm1.1_RCodba, glm1.2_RCodba, glm1.3_RCodba, glm1.4_RCodba, glm1.5_RCodba,
    glm2.1_RCodba, glm2.2_RCodba, glm2.3_RCodba, glm2.4_RCodba, glm2.5_RCodba, glm2.6_RCodba,
    glm2.7_RCodba, glm2.8_RCodba, glm2.9_RCodba,
    glm3.1_RCodba, glm3.2_RCodba, glm3.3_RCodba, glm3.4_RCodba, glm3.5_RCodba, glm3.6_RCodba,
    glm3.7_RCodba, glm3.8_RCodba, glm3.9_RCodba,
    glm4.1_RCodba, glm4.2_RCodba, glm4.3_RCodba, glm4.4_RCodba, glm4.5_RCodba, glm4.6_RCodba,
    glm4.7_RCodba) %>%
  arrange(AIC)

# ---------- BIC ----------
BIC(glmfull_RCodba, glm0_RCodba,
    glm1.1_RCodba, glm1.2_RCodba, glm1.3_RCodba, glm1.4_RCodba, glm1.5_RCodba,
    glm2.1_RCodba, glm2.2_RCodba, glm2.3_RCodba, glm2.4_RCodba, glm2.5_RCodba, glm2.6_RCodba,
    glm2.7_RCodba, glm2.8_RCodba, glm2.9_RCodba,
    glm3.1_RCodba, glm3.2_RCodba, glm3.3_RCodba, glm3.4_RCodba, glm3.5_RCodba, glm3.6_RCodba,
    glm3.7_RCodba, glm3.8_RCodba, glm3.9_RCodba,
    glm4.1_RCodba, glm4.2_RCodba, glm4.3_RCodba, glm4.4_RCodba, glm4.5_RCodba, glm4.6_RCodba,
    glm4.7_RCodba) %>%
  arrange(BIC)

# ---------- AICc ----------
AICc(glmfull_RCodba, glm0_RCodba,
     glm1.1_RCodba, glm1.2_RCodba, glm1.3_RCodba, glm1.4_RCodba, glm1.5_RCodba,
     glm2.1_RCodba, glm2.2_RCodba, glm2.3_RCodba, glm2.4_RCodba, glm2.5_RCodba, glm2.6_RCodba,
     glm2.7_RCodba, glm2.8_RCodba, glm2.9_RCodba,
     glm3.1_RCodba, glm3.2_RCodba, glm3.3_RCodba, glm3.4_RCodba, glm3.5_RCodba, glm3.6_RCodba,
     glm3.7_RCodba, glm3.8_RCodba, glm3.9_RCodba,
     glm4.1_RCodba, glm4.2_RCodba, glm4.3_RCodba, glm4.4_RCodba, glm4.5_RCodba, glm4.6_RCodba,
     glm4.7_RCodba) %>%
  arrange(AICc)


# =====================================================================
# 5. LIKELIHOOD-RATIO TESTS (does adding a term improve the fit?)
# =====================================================================
# ---------- From the null model ----------
anova(glm0_RCodba, glm1.1_RCodba, test = "Chisq") # + Mean RL
anova(glm0_RCodba, glm1.2_RCodba, test = "Chisq") # + Sex 
anova(glm0_RCodba, glm1.3_RCodba, test = "Chisq") # + Age 
anova(glm0_RCodba, glm1.4_RCodba, test = "Chisq") # + Group_size
anova(glm0_RCodba, glm1.5_RCodba, test = "Chisq") # + Stimulus_type


# ---------- From Mean RL ----------
anova(glm1.1_RCodba, glm2.1_RCodba, test = "Chisq") # + Sex
anova(glm1.1_RCodba, glm2.3_RCodba, test = "Chisq") # * Sex
anova(glm1.1_RCodba, glm2.5_RCodba, test = "Chisq") # + Group_size
anova(glm1.1_RCodba, glm2.7_RCodba, test = "Chisq") # + Stimulus_type

# ---------- From Sex ----------
anova(glm1.2_RCodba, glm2.4_RCodba, test = "Chisq") # + Group_size 
anova(glm1.2_RCodba, glm2.8_RCodba, test = "Chisq") # + Stimulus_type
anova(glm1.2_RCodba, glm2.1_RCodba, test = "Chisq") # + Mean RL
anova(glm1.2_RCodba, glm2.3_RCodba, test = "Chisq") # * Mean RL

# ---------- From Group_size ----------
anova(glm1.4_RCodba, glm2.4_RCodba, test = "Chisq") # + Sex
anova(glm1.4_RCodba, glm2.9_RCodba, test = "Chisq") # + Stimulus_type 
anova(glm1.4_RCodba, glm2.5_RCodba, test = "Chisq") # + Mean RL

# ---------- From Mean RL + Sex ----------
anova(glm2.1_RCodba, glm3.1_RCodba, test = "Chisq") # + Group_size
anova(glm2.1_RCodba, glm3.2_RCodba, test = "Chisq") # + Group_size * Sex
anova(glm2.1_RCodba, glm3.3_RCodba, test = "Chisq") # + Stimulus_type
anova(glm2.1_RCodba, glm3.6_RCodba, test = "Chisq") # + Stimulus_type * Sex
anova(glm2.1_RCodba, glm3.4_RCodba, test = "Chisq") # + Age
anova(glm2.1_RCodba, glm3.5_RCodba, test = "Chisq") # + Age * Sex

# ---------- From Mean RL * Sex ----------
anova(glm2.3_RCodba, glm3.8_RCodba, test = "Chisq") # + Group_size
anova(glm2.3_RCodba, glm3.9_RCodba, test = "Chisq") # + Stimulus_type


# ---------- Best compromise among the shortlisted models ----------
AIC(glm1.4_RCodba, glm2.1_RCodba, glm2.3_RCodba, glm2.5_RCodba, glm3.1_RCodba) %>% arrange(AIC)
BIC(glm1.4_RCodba, glm2.1_RCodba, glm2.3_RCodba, glm2.5_RCodba, glm3.1_RCodba) %>% arrange(BIC)
AICc(glm1.4_RCodba, glm2.1_RCodba, glm2.3_RCodba, glm2.5_RCodba, glm3.1_RCodba) %>% arrange(AICc)


# =====================================================================
# 6. RETAINED MODEL
# =====================================================================
summary(glm2.1_RCodba)


# =====================================================================
# 7. DESCRIPTIVE STATISTICS BY SEX (Table S2)
# =====================================================================
data_ODBA %>%
  group_by(Sex) %>%
  summarise(mean = mean(relative_ODBA_change),
            sd = sd(relative_ODBA_change),
            median = median(relative_ODBA_change),
            IQR = IQR(relative_ODBA_change),
            min = min(relative_ODBA_change),
            max = max(relative_ODBA_change))

# =====================================================================
# 8. FIGURE 2A - RELATIVE CHANGE OF ODBA
# =====================================================================
data_ODBA %>% 
  ggplot(aes(x=Mean_RL, y = relative_ODBA_change, color = Sex)) +
  # Put in grey area where y < 0
  #annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0,
  #         fill = "grey20", alpha = 0.2)+
  geom_point(size = 2.5) +
  geom_smooth(method = "glm",
              method.args = list(family = gaussian(link = "identity")),
              se = TRUE,
              aes(group = 1),
              color = "grey40") +
  theme_bw() +
  scale_color_manual(values = c("Female"="#CC79A7", "Male"="#56B4E9")) + # from Okabe–Ito palette
  labs(y="ODBA (z-score)",
       x="Mean received level (dB re 1 µPa)",
       caption="n = 40 trials; 23 individuals") +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray50"),
    axis.title = element_text(face = "bold"),
    #axis.text = element_text(size=12),
    legend.title = element_text(face = "bold"),
    #legend.text = element_text(size=11)) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed"))



