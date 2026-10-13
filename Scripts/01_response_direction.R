# =====================================================================
# 01 - RESPONSE DIRECTION (toward / away from the speaker)
# =====================================================================
# --- QUESTION ---
#   Does the direction of the first response to signature whistle
#   playbacks depend on source level (SL) and/or individual characteristics?
#
# --- DATA ---
#   full dataset, 125 trials on 70 individuals
#   (first response from drone video or dead-reckoned tracks).
#   Trials without response are excluded (n = 10).
#
# --- RESPONSE VARIABLE ---
#   Response_direction: Toward / Away
#   -> the binomial GLM models the probability of an AWAY response.
#
# --- EXPLANATORY VARIABLES (reference level in brackets) ---
#   SL : Higher SL (reference) / Lower SL
#   Sex : Male (reference) / Female
#   Stimulus_type : Familiar (reference) / Unfamiliar
#   Group_size :  single / pair / >2
#   Age : numerical values
#
# --- RETAINED MODEL ---
#   glm(Response_direction ~ Stimulus_type * SL * Sex, family = binomial)
#
# --- VALUES TO LOOK FOR IN THE OUTPUT (manuscript, Table 2 and Table S1) ---
#   summary(model3.23), 3-way interaction:
#     SL(lower):Sex(female):Stimulus(unfamiliar)
#       estimate = 4.206, SE = 1.831, z = 2.297, p = 0.021
#
#   Tukey post hoc (Table S1):
#     Female Higher SL Familiar vs Female Lower SL Familiar:
#       estimate = 3.239, z = 3.665, p = 0.006
#     Male Higher SL Familiar vs Female Higher SL Familiar:
#       estimate = -3.178, z = -3.156, p = 0.034
#     Female Lower SL Familiar vs Female Higher SL Unfamiliar:
#       estimate = -2.746, z = -3.039, p = 0.049
# =====================================================================


# =====================================================================
# 0. PACKAGES
# =====================================================================
library(tidyverse)
library(MuMIn)       # dredge(), AICc()
library(emmeans)     # post hoc tests
library(flextable)   # Word table


# =====================================================================
# 1. FULL MODEL + EXPLORATORY SELECTION (dredge, no interactions)
# =====================================================================
full_model_SL <- glm(Response_direction ~ Stimulus_type + Group_size + Sex + Age + SL,
                     data = data_resp_dir, family = binomial)

# dredge() needs na.action = "na.fail"
options(na.action = "na.fail")
MuMIn::dredge(full_model_SL)


# =====================================================================
# 2. CANDIDATE MODELS
# =====================================================================
# Note: Not all possible models may have been explored. Model development was
# guided by dredge and likelihood-ratio tests, which helped identify variables
# with weaker and stronger effects.

# ---------- Null model ----------
model0_SL   <- glm(Response_direction ~ 1, data = data_resp_dir, family = binomial)

# ---------- 1 variable ----------
model1.1_SL <- glm(Response_direction ~ Stimulus_type, data = data_resp_dir, family = binomial)
model1.2_SL <- glm(Response_direction ~ Sex, data = data_resp_dir, family = binomial)
model1.3_SL <- glm(Response_direction ~ Group_size, data = data_resp_dir, family = binomial)
model1.4_SL <- glm(Response_direction ~ Age, data = data_resp_dir, family = binomial)
model1.5_SL <- glm(Response_direction ~ SL, data = data_resp_dir, family = binomial)

# ---------- 2 variables ----------
# ~ Stimulus_type
model2.1_SL  <- glm(Response_direction ~ Stimulus_type + Sex, data = data_resp_dir, family = binomial)
model2.2_SL  <- glm(Response_direction ~ Stimulus_type * Sex, data = data_resp_dir, family = binomial)
model2.3_SL  <- glm(Response_direction ~ Stimulus_type + Age, data = data_resp_dir, family = binomial)
model2.4_SL  <- glm(Response_direction ~ Stimulus_type * Age, data = data_resp_dir, family = binomial)
model2.5_SL  <- glm(Response_direction ~ Stimulus_type + Group_size, data = data_resp_dir, family = binomial)
model2.6_SL  <- glm(Response_direction ~ Stimulus_type * Group_size, data = data_resp_dir, family = binomial)

# ~ Sex
model2.7_SL  <- glm(Response_direction ~ Sex + Age, data = data_resp_dir, family = binomial)
model2.8_SL  <- glm(Response_direction ~ Sex * Age, data = data_resp_dir, family = binomial)
model2.9_SL  <- glm(Response_direction ~ Sex + Group_size, data = data_resp_dir, family = binomial)
model2.10_SL <- glm(Response_direction ~ Sex * Group_size, data = data_resp_dir, family = binomial)

# ~ Group_size
model2.11_SL <- glm(Response_direction ~ Group_size + Age, data = data_resp_dir, family = binomial)
model2.12_SL <- glm(Response_direction ~ Group_size * Age, data = data_resp_dir, family = binomial)

# ~ SL
model2.13_SL <- glm(Response_direction ~ Stimulus_type + SL, data = data_resp_dir, family = binomial)
model2.14_SL <- glm(Response_direction ~ Stimulus_type * SL, data = data_resp_dir, family = binomial)
model2.15_SL <- glm(Response_direction ~ Sex + SL, data = data_resp_dir, family = binomial)
model2.16_SL <- glm(Response_direction ~ Sex * SL, data = data_resp_dir, family = binomial)
model2.17_SL <- glm(Response_direction ~ Group_size + SL, data = data_resp_dir, family = binomial)
model2.18_SL <- glm(Response_direction ~ Group_size * SL, data = data_resp_dir, family = binomial)
model2.19_SL <- glm(Response_direction ~ Age + SL, data = data_resp_dir, family = binomial)
model2.20_SL <- glm(Response_direction ~ Age * SL, data = data_resp_dir, family = binomial)

# ---------- 3 variables ----------
# ~ Stimulus_type + Sex + Group_size
model3.1_SL  <- glm(Response_direction ~ Stimulus_type + Sex + Group_size, data = data_resp_dir, family = binomial)
model3.2_SL  <- glm(Response_direction ~ Stimulus_type * Sex + Group_size, data = data_resp_dir, family = binomial)
model3.3_SL  <- glm(Response_direction ~ Stimulus_type + Sex * Group_size, data = data_resp_dir, family = binomial)
model3.4_SL  <- glm(Response_direction ~ Stimulus_type * Group_size + Sex, data = data_resp_dir, family = binomial)

# ~ Stimulus_type + Sex + Age
model3.5_SL  <- glm(Response_direction ~ Stimulus_type + Sex + Age, data = data_resp_dir, family = binomial)
model3.6_SL  <- glm(Response_direction ~ Stimulus_type * Sex + Age, data = data_resp_dir, family = binomial)
model3.7_SL  <- glm(Response_direction ~ Stimulus_type + Sex * Age, data = data_resp_dir, family = binomial)
model3.8_SL  <- glm(Response_direction ~ Stimulus_type * Age + Sex, data = data_resp_dir, family = binomial)

# ~ Stimulus_type + Group_size + Age
model3.9_SL  <- glm(Response_direction ~ Stimulus_type + Group_size + Age, data = data_resp_dir, family = binomial)
model3.10_SL <- glm(Response_direction ~ Stimulus_type * Group_size + Age, data = data_resp_dir, family = binomial)
model3.11_SL <- glm(Response_direction ~ Stimulus_type + Group_size * Age, data = data_resp_dir, family = binomial)
model3.12_SL <- glm(Response_direction ~ Stimulus_type * Age + Group_size, data = data_resp_dir, family = binomial)

# ~ Sex + Group_size + Age
model3.13_SL <- glm(Response_direction ~ Sex + Age + Group_size, data = data_resp_dir, family = binomial)
model3.14_SL <- glm(Response_direction ~ Sex * Age + Group_size, data = data_resp_dir, family = binomial)
model3.15_SL <- glm(Response_direction ~ Sex + Age * Group_size, data = data_resp_dir, family = binomial)
model3.16_SL <- glm(Response_direction ~ Sex * Group_size + Age, data = data_resp_dir, family = binomial)

# ~ Stimulus_type + Sex + SL
model3.17_SL <- glm(Response_direction ~ Stimulus_type + Sex + SL, data = data_resp_dir, family = binomial)
model3.18_SL <- glm(Response_direction ~ Stimulus_type * Sex + SL, data = data_resp_dir, family = binomial)
model3.19_SL <- glm(Response_direction ~ Stimulus_type + Sex * SL, data = data_resp_dir, family = binomial)
model3.20_SL <- glm(Response_direction ~ Stimulus_type * SL + Sex, data = data_resp_dir, family = binomial)
model3.23_SL <- glm(Response_direction ~ Stimulus_type * SL * Sex, data = data_resp_dir, family = binomial)

# ~ SL * Sex + ?
model3.21_SL <- glm(Response_direction ~ SL * Sex + Group_size, data = data_resp_dir, family = binomial)
model3.22_SL <- glm(Response_direction ~ SL * Sex + Age, data = data_resp_dir, family = binomial)


# =====================================================================
# 3. MODEL COMPARISON (AIC, BIC, AICc)
# =====================================================================
# ---------- AIC ----------
AIC(full_model_SL, model0_SL,
    model1.1_SL, model1.2_SL, model1.3_SL, model1.4_SL, model1.5_SL,
    model2.1_SL, model2.2_SL, model2.3_SL, model2.4_SL, model2.5_SL, model2.6_SL,
    model2.7_SL, model2.8_SL, model2.9_SL, model2.10_SL, model2.11_SL, model2.12_SL,
    model2.13_SL, model2.14_SL, model2.15_SL, model2.16_SL, model2.17_SL, model2.18_SL,
    model2.19_SL, model2.20_SL,
    model3.1_SL, model3.2_SL, model3.3_SL, model3.4_SL, model3.5_SL, model3.6_SL,
    model3.7_SL, model3.8_SL, model3.9_SL, model3.10_SL, model3.11_SL, model3.12_SL,
    model3.13_SL, model3.14_SL, model3.15_SL, model3.16_SL, model3.17_SL, model3.18_SL,
    model3.19_SL, model3.20_SL, model3.21_SL, model3.22_SL, model3.23_SL) %>%
  arrange(AIC)

# ---------- BIC ----------
BIC(full_model_SL, model0_SL,
    model1.1_SL, model1.2_SL, model1.3_SL, model1.4_SL, model1.5_SL,
    model2.1_SL, model2.2_SL, model2.3_SL, model2.4_SL, model2.5_SL, model2.6_SL,
    model2.7_SL, model2.8_SL, model2.9_SL, model2.10_SL, model2.11_SL, model2.12_SL,
    model2.13_SL, model2.14_SL, model2.15_SL, model2.16_SL, model2.17_SL, model2.18_SL,
    model2.19_SL, model2.20_SL,
    model3.1_SL, model3.2_SL, model3.3_SL, model3.4_SL, model3.5_SL, model3.6_SL,
    model3.7_SL, model3.8_SL, model3.9_SL, model3.10_SL, model3.11_SL, model3.12_SL,
    model3.13_SL, model3.14_SL, model3.15_SL, model3.16_SL, model3.17_SL, model3.18_SL,
    model3.19_SL, model3.20_SL, model3.21_SL, model3.22_SL, model3.23_SL) %>%
  arrange(BIC)

# ---------- AICc ----------
MuMIn::AICc(full_model_SL, model0_SL,
            model1.1_SL, model1.2_SL, model1.3_SL, model1.4_SL, model1.5_SL,
            model2.1_SL, model2.2_SL, model2.3_SL, model2.4_SL, model2.5_SL, model2.6_SL,
            model2.7_SL, model2.8_SL, model2.9_SL, model2.10_SL, model2.11_SL, model2.12_SL,
            model2.13_SL, model2.14_SL, model2.15_SL, model2.16_SL, model2.17_SL, model2.18_SL,
            model2.19_SL, model2.20_SL,
            model3.1_SL, model3.2_SL, model3.3_SL, model3.4_SL, model3.5_SL, model3.6_SL,
            model3.7_SL, model3.8_SL, model3.9_SL, model3.10_SL, model3.11_SL, model3.12_SL,
            model3.13_SL, model3.14_SL, model3.15_SL, model3.16_SL, model3.17_SL, model3.18_SL,
            model3.19_SL, model3.20_SL, model3.21_SL, model3.22_SL, model3.23_SL) %>%
  arrange(AICc)


# =====================================================================
# 4. LIKELIHOOD-RATIO TESTS (does adding a term improve the fit?)
# =====================================================================
# ---------- From the null model ----------
anova(model0_SL, model1.1_SL, test = "Chisq")   # + Stimulus_type
anova(model0_SL, model1.2_SL, test = "Chisq")   # + Sex
anova(model0_SL, model1.3_SL, test = "Chisq")   # + Group_size
anova(model0_SL, model1.4_SL, test = "Chisq")   # + Age
anova(model0_SL, model1.5_SL, test = "Chisq")   # + SL

# ---------- From SL ----------
anova(model1.5_SL, model2.13_SL, test = "Chisq")   # + Stimulus_type
anova(model1.5_SL, model2.14_SL, test = "Chisq")   # * Stimulus_type
anova(model1.5_SL, model2.15_SL, test = "Chisq")   # + Sex
anova(model1.5_SL, model2.16_SL, test = "Chisq")   # * Sex
anova(model1.5_SL, model2.17_SL, test = "Chisq")   # + Group_size
anova(model1.5_SL, model2.18_SL, test = "Chisq")   # * Group_size
anova(model1.5_SL, model2.19_SL, test = "Chisq")   # + Age
anova(model1.5_SL, model2.20_SL, test = "Chisq")   # * Age

# ---------- From SL * Sex ----------
anova(model2.15_SL, model2.16_SL, test = "Chisq")  # interaction SL:Sex
anova(model2.16_SL, model3.19_SL, test = "Chisq")  # + Stimulus_type
anova(model2.16_SL, model3.23_SL, test = "Chisq")  # * Stimulus_type
anova(model2.16_SL, model3.21_SL, test = "Chisq")  # + Group_size
anova(model2.16_SL, model3.22_SL, test = "Chisq")  # + Age     

# ---------- Best compromise among the shortlisted models ----------
AIC(model2.16_SL, model3.19_SL, model1.5_SL, model3.21_SL, model3.23_SL) %>% arrange(AIC)
BIC(model2.16_SL, model3.19_SL, model1.5_SL, model3.21_SL, model3.23_SL) %>% arrange(BIC)
AICc(model2.16_SL, model3.19_SL, model1.5_SL, model3.21_SL, model3.23_SL) %>% arrange(AICc)


# =====================================================================
# 5. RETAINED MODEL: Stimulus_type * SL * Sex
# =====================================================================
levels(data_resp_dir$Response_direction)

# Probability of showing a negative response (away)
summary(model3.23_SL)


# =====================================================================
# 6. POST HOC TESTS
# =====================================================================
emm <- emmeans::emmeans(model3.23_SL, ~ Sex * SL * Stimulus_type)

# ---------- Between-cell comparisons (Tukey) ----------
pairs(emm, adjust = "tukey") %>%
  as.data.frame() %>%
  dplyr::filter(p.value < 0.1)   # keep p < 0.1 to also see marginal differences
# dplyr:: avoids a clash with other packages

# =====================================================================
# 7. TABLE FOR THE MANUSCRIPT (Tukey pairwise comparisons, Table S1)
# =====================================================================
emmeans_results <- emmeans(model3.23_SL, ~ Sex * SL * Stimulus_type) %>%
  pairs(adjust = "tukey") %>%
  as.data.frame()

# ---------- Format the table ----------
tab_emmeans <- emmeans_results %>%
  dplyr::select(contrast, estimate, SE, z.ratio, p.value) %>% 
  rename(Comparison = contrast, Estimate = estimate,
         `SE` = SE,
         `z-ratio` = z.ratio,
         `p-value` = p.value) %>%
  mutate(Estimate = round(Estimate, 3),
         SE = round(SE, 3),
         `z-ratio` = round(`z-ratio`, 3),
         `p-value` = ifelse(`p-value` < 0.001,
                            "<0.001", sprintf("%.3f", `p-value`)))

# ---------- Formatted Word table ----------
ft <- flextable(tab_emmeans) %>%
  autofit() %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  align(j = "Comparison", align = "left", part = "all")

ft

# ---------- Export to Word ----------
# Relative path -> adapt if needed
# officer::read_docx() %>%
#   body_add_flextable(ft) %>%
#   print(target = "emmeans_Tukey_all_comparisons.docx")