# =====================================================================
# 03 - RESPONSE DURATION 
# =====================================================================
# --- QUESTION ---
#   Does the duration of the energetic response (return to baseline of
#   ODBA) depend on the received level, the stimulus type, the group
#   size, sex or age?
#
# --- DATA ---
#   data_ODBA: tagged subset, 40 trials on 23 individuals
#   (single animals or pairs; the two trials with > 2 individuals are removed).
#
# --- RESPONSE VARIABLE ---
#   resp_dur: First time after onset of playback where the relative ODBA decreased below 1 z-score
#     (= within 1 sd of the mean baseline).
#
#
# --- EXPLANATORY VARIABLES (reference level in brackets) ---
#   Mean_RL : mean received level of the trial (dB re 1 uPa), continuous
#   Sex : Male (reference) / Female
#   Stimulus_type : Familiar (reference) / Unfamiliar
#   Group_size : 1 = single (reference) / 2 = pair (factor)
#   Age :  age of the focal
#
# --- RETAINED MODEL ---
#   MASS::glm.nb(resp_dur ~ Group_size + Stimulus_type + Mean_RL)
#
# --- VALUES TO LOOK FOR IN THE OUTPUT (manuscript, Table 2) ---
#   > summary(glm3.11_RD):
#     Mean_RL : estimate = 0.025, SE 0.019, z =  1.306, p = 0.192
#     Group_size (pair) : estimate = 0.672, SE 0.331, z =  2.029, p = 0.042
#     Stimulus_type (unfamiliar) : estimate = 0.708, SE 0.328, z = 2.154, p = 0.031
#   > Descriptive statistics (Table S2), mean +/- sd [min; max]:
#     Single 3.2 ± 3.4 [0; 16] | Pair 8.2 ± 13.6 [1; 52]
#     Familiar 3.5 ± 4.4 [0; 19] | Unfamiliar 8.2 ± 13.6 [1; 52]
# =====================================================================


# =====================================================================
# 0. PACKAGES
# =====================================================================
library(MASS)        # glm.nb(); loaded BEFORE tidyverse so dplyr::select() is not masked
library(tidyverse)
library(MuMIn)       # dredge(), AICc()



# =====================================================================
# 1. FULL MODEL + EXPLORATORY SELECTION (dredge, no interactions)
# =====================================================================
glmfull_RD <- glm.nb(resp_dur ~ Stimulus_type + Mean_RL + Sex + Age + Group_size, data = data_ODBA)

# dredge() needs na.action = "na.fail"
options(na.action = "na.fail")
MuMIn::dredge(glmfull_RD)


# =====================================================================
# 3. CANDIDATE MODELS
# =====================================================================
# Note: Not all possible models may have been explored. Model development was
# guided by dredge and likelihood-ratio tests, which helped identify variables
# with weaker and stronger effects.

# ---------- Null model ----------
glm0_RD <- glm.nb(resp_dur ~ 1, data = data_ODBA)

# ---------- 1 variable ----------
glm1.1_RD <- glm.nb(resp_dur ~ Mean_RL, data = data_ODBA)
glm1.2_RD <- glm.nb(resp_dur ~ Sex, data = data_ODBA)
glm1.3_RD <- glm.nb(resp_dur ~ Group_size, data = data_ODBA)
glm1.4_RD <- glm.nb(resp_dur ~ Stimulus_type, data = data_ODBA)

# ---------- 2 variables ----------
glm2.1_RD <- glm.nb(resp_dur ~ Mean_RL + Sex, data = data_ODBA)
glm2.2_RD <- glm.nb(resp_dur ~ Mean_RL + Stimulus_type, data = data_ODBA)
glm2.3_RD <- glm.nb(resp_dur ~ Mean_RL + Group_size, data = data_ODBA)
glm2.4_RD <- glm.nb(resp_dur ~ Mean_RL + Age, data = data_ODBA)
glm2.5_RD <- glm.nb(resp_dur ~ Group_size + Sex, data = data_ODBA)
glm2.6_RD <- glm.nb(resp_dur ~ Group_size + Stimulus_type, data = data_ODBA)
glm2.7_RD <- glm.nb(resp_dur ~ Group_size * Stimulus_type, data = data_ODBA)
glm2.8_RD <- glm.nb(resp_dur ~ Group_size + Age, data = data_ODBA)

# ---------- 3 variables ----------
glm3.1_RD  <- glm.nb(resp_dur ~ Mean_RL + Sex + Stimulus_type, data = data_ODBA)
glm3.2_RD  <- glm.nb(resp_dur ~ Mean_RL + Sex * Stimulus_type, data = data_ODBA)
glm3.3_RD  <- glm.nb(resp_dur ~ Mean_RL + Sex + Age, data = data_ODBA)
glm3.4_RD  <- glm.nb(resp_dur ~ Mean_RL + Sex + Group_size, data = data_ODBA)
glm3.5_RD  <- glm.nb(resp_dur ~ Mean_RL + Sex * Group_size, data = data_ODBA)
glm3.6_RD  <- glm.nb(resp_dur ~ Mean_RL + Sex * Age, data = data_ODBA)
glm3.7_RD  <- glm.nb(resp_dur ~ Group_size + Stimulus_type + Sex, data = data_ODBA)
glm3.8_RD  <- glm.nb(resp_dur ~ Stimulus_type + Group_size * Sex, data = data_ODBA)
glm3.9_RD  <- glm.nb(resp_dur ~ Group_size + Stimulus_type + Age, data = data_ODBA)
glm3.10_RD <- glm.nb(resp_dur ~ Stimulus_type + Group_size * Age, data = data_ODBA)
glm3.11_RD <- glm.nb(resp_dur ~ Group_size + Stimulus_type + Mean_RL, data = data_ODBA)
glm3.12_RD <- glm.nb(resp_dur ~ Group_size * Stimulus_type + Mean_RL, data = data_ODBA)

# ---------- 4 variables ----------
glm4.1_RD <- glm.nb(resp_dur ~ Mean_RL + Sex + Stimulus_type + Age, data = data_ODBA)
glm4.2_RD <- glm.nb(resp_dur ~ Mean_RL + Sex + Stimulus_type * Age, data = data_ODBA)
glm4.3_RD <- glm.nb(resp_dur ~ Mean_RL + Sex + Group_size * Age, data = data_ODBA)
glm4.4_RD <- glm.nb(resp_dur ~ Mean_RL + Sex * Group_size + Age, data = data_ODBA)
glm4.5_RD <- glm.nb(resp_dur ~ Mean_RL + Sex + Stimulus_type + Group_size, data = data_ODBA)
glm4.6_RD <- glm.nb(resp_dur ~ Mean_RL + Sex + Stimulus_type * Group_size, data = data_ODBA)


# =====================================================================
# 4. MODEL COMPARISON (AIC, BIC, AICc)
# =====================================================================
# ---------- AIC ----------
AIC(glmfull_RD, glm0_RD,
    glm1.1_RD, glm1.2_RD, glm1.3_RD, glm1.4_RD,
    glm2.1_RD, glm2.2_RD, glm2.3_RD, glm2.4_RD, glm2.5_RD, glm2.6_RD, glm2.7_RD, glm2.8_RD,
    glm3.1_RD, glm3.2_RD, glm3.3_RD, glm3.4_RD, glm3.5_RD, glm3.6_RD,
    glm3.7_RD, glm3.8_RD, glm3.9_RD, glm3.10_RD, glm3.11_RD, glm3.12_RD,
    glm4.1_RD, glm4.2_RD, glm4.3_RD, glm4.4_RD, glm4.5_RD, glm4.6_RD) %>%
  arrange(AIC)

# ---------- BIC ----------
BIC(glmfull_RD, glm0_RD,
    glm1.1_RD, glm1.2_RD, glm1.3_RD, glm1.4_RD,
    glm2.1_RD, glm2.2_RD, glm2.3_RD, glm2.4_RD, glm2.5_RD, glm2.6_RD, glm2.7_RD, glm2.8_RD,
    glm3.1_RD, glm3.2_RD, glm3.3_RD, glm3.4_RD, glm3.5_RD, glm3.6_RD,
    glm3.7_RD, glm3.8_RD, glm3.9_RD, glm3.10_RD, glm3.11_RD, glm3.12_RD,
    glm4.1_RD, glm4.2_RD, glm4.3_RD, glm4.4_RD, glm4.5_RD, glm4.6_RD) %>%
  arrange(BIC)

# ---------- AICc ----------
AICc(glmfull_RD, glm0_RD,
     glm1.1_RD, glm1.2_RD, glm1.3_RD, glm1.4_RD,
     glm2.1_RD, glm2.2_RD, glm2.3_RD, glm2.4_RD, glm2.5_RD, glm2.6_RD, glm2.7_RD, glm2.8_RD,
     glm3.1_RD, glm3.2_RD, glm3.3_RD, glm3.4_RD, glm3.5_RD, glm3.6_RD,
     glm3.7_RD, glm3.8_RD, glm3.9_RD, glm3.10_RD, glm3.11_RD, glm3.12_RD,
     glm4.1_RD, glm4.2_RD, glm4.3_RD, glm4.4_RD, glm4.5_RD, glm4.6_RD) %>%
  arrange(AICc)



# =====================================================================
# 5. LIKELIHOOD-RATIO TESTS (does adding a term improve the fit?)
# =====================================================================
# ---------- From the null model ----------
anova(glm0_RD, glm1.1_RD, test = "Chisq") # + Mean RL
anova(glm0_RD, glm1.2_RD, test = "Chisq") # + Sex
anova(glm0_RD, glm1.3_RD, test = "Chisq") # + Group_size
anova(glm0_RD, glm1.4_RD, test = "Chisq") # + Stimulus_type

# ---------- From Mean RL ----------
anova(glm1.1_RD, glm2.1_RD, test = "Chisq") # + Sex
anova(glm1.1_RD, glm2.2_RD, test = "Chisq") # + Stimulus_type
anova(glm1.1_RD, glm2.3_RD, test = "Chisq") # + Group_size
anova(glm1.1_RD, glm2.4_RD, test = "Chisq") # + Age

# ---------- From Group_size ----------
anova(glm1.3_RD, glm2.5_RD, test = "Chisq") # + Sex
anova(glm1.3_RD, glm2.6_RD, test = "Chisq") # + Stimulus_type
anova(glm1.3_RD, glm2.7_RD, test = "Chisq") # * Stimulus_type
anova(glm1.3_RD, glm2.8_RD, test = "Chisq") # + Age
anova(glm1.3_RD, glm2.3_RD, test = "Chisq") # + Mean RL

# ---------- From Group_size + Stimulus_type ----------
anova(glm2.6_RD, glm2.7_RD, test = "Chisq") # interaction Group_size * Stimulus_type instead
anova(glm2.6_RD, glm3.7_RD, test = "Chisq") # + Sex
anova(glm2.6_RD, glm3.8_RD, test = "Chisq") # + Sex, interaction with Group_size
anova(glm2.6_RD, glm3.9_RD, test = "Chisq") # + Age
anova(glm2.6_RD, glm3.10_RD, test = "Chisq") # * Age, interaction with Group_size
anova(glm2.6_RD, glm3.11_RD, test = "Chisq") # + Mean RL

# ---------- From Group_size + Stimulus_type + Mean RL ----------
anova(glm3.11_RD, glm3.12_RD, test = "Chisq") # interaction Group_size * Stimulus_type instead

# ---------- From Mean RL + Sex ----------
anova(glm2.1_RD, glm3.1_RD, test = "Chisq") # + Stimulus_type
anova(glm2.1_RD, glm3.2_RD, test = "Chisq") # * Stimulus_type, interaction with Sex
anova(glm2.1_RD, glm3.3_RD, test = "Chisq") # + Age
anova(glm2.1_RD, glm3.4_RD, test = "Chisq") # + Group_size
anova(glm2.1_RD, glm3.5_RD, test = "Chisq") # * Group_size, interaction with Sex
anova(glm2.1_RD, glm3.6_RD, test = "Chisq") # * Age, interaction with Sex

# ---------- From Mean RL + Sex + Stimulus_type ----------
anova(glm3.1_RD, glm3.2_RD, test = "Chisq") # interaction Sex * Stimulus_type instead
anova(glm3.1_RD, glm4.1_RD, test = "Chisq") # + Age
anova(glm3.1_RD, glm4.2_RD, test = "Chisq") # * Age, interaction with Stimulus_type
anova(glm3.1_RD, glm4.5_RD, test = "Chisq") # + Group_size
anova(glm3.1_RD, glm4.6_RD, test = "Chisq") # * Group_size, interaction with Stimulus_type

# ---------- Best compromise among the shortlisted models ----------
AIC(glm1.1_RD, glm1.3_RD, glm2.1_RD, glm3.1_RD, glm2.6_RD, glm2.7_RD, glm3.11_RD) %>% arrange(AIC)
BIC(glm1.1_RD, glm1.3_RD, glm2.1_RD, glm3.1_RD, glm2.6_RD, glm2.7_RD, glm3.11_RD) %>% arrange(BIC)
AICc(glm1.1_RD, glm1.3_RD, glm2.1_RD, glm3.1_RD, glm2.6_RD, glm2.7_RD, glm3.11_RD) %>% arrange(AICc)


# =====================================================================
# 6. RETAINED MODEL
# =====================================================================
summary(glm3.11_RD)


# =====================================================================
# 7. DESCRIPTIVE STATISTICS (Table S2)
# =====================================================================
# ---------- By group size ----------
data_ODBA %>%
  group_by(Group_size) %>%
  summarise(mean = mean(resp_dur),
            sd = sd(resp_dur),
            median = median(resp_dur),
            IQR = IQR(resp_dur),
            min = min(resp_dur),
            max = max(resp_dur))

# ---------- By stimulus type ----------
data_ODBA %>%
  group_by(Stimulus_type) %>%
  summarise(mean = mean(resp_dur),
            sd = sd(resp_dur),
            median = median(resp_dur),
            IQR = IQR(resp_dur),
            min = min(resp_dur),
            max = max(resp_dur))


# =====================================================================
# 8. FIGURE 2B - RESPONSE DURATION
# =====================================================================
data_ODBA %>% 
  ggplot(aes(x = Mean_RL, y = resp_dur, shape = Group_size, color = Stimulus_type)) +
  geom_point(aes(shape=Group_size), size = 3) +
  geom_smooth(method = "glm",
              method.args = list(family = MASS::negative.binomial(theta = 1.184)), # theta from glm3.11_RD
              se = TRUE,
              aes(group = 1), # for 1 global glm
              color = "gray40") +
  labs(x = "Mean received level (dB re 1 µPa)",
       y = "Response duration (s)",
       color = "Stimulus type",
       shape = "Number of individuals",
       caption = "n = 40 trials; 23 individuals")+ 
  scale_color_manual(values = c("Familiar"="goldenrod2", "Unfamiliar"="darkorchid3")) +
  scale_shape_manual(values = c(1, 16),
                     labels = c("1" = "Single", "2" = "Pair"),
                     name = "Group size") +
  guides(color = guide_legend(order = 1, # Stimulus_type above
                              override.aes = list(shape = 8, # shape 
                                                  linetype = 0,
                                                  size = 2)),
         shape = guide_legend(order = 2)) +   # Group_size under
  # Remove the point for the sex on the legend
  theme_bw() +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "gray50"),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))
