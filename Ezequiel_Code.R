################################################################################
################################################################################
####################             Cleaning 2022             #####################
################################################################################
################################################################################
# We downloaded the data from here
#https://www2.census.gov/programs-surveys/acs/data/pums/2022/1-Year/csv_pus.zip

if (!require("readr")) install.packages("readr", dependencies = TRUE)
library(readr)

# We append population a, b, c,d  data once
data_a <- read.csv("/Users/zequi/Desktop/Berkeley/230A/csv_pus/psam_pusa.csv")
data_b <- read.csv("/Users/zequi/Desktop/Berkeley/230A/csv_pus/psam_pusb.csv")
data <- rbind(data_a, data_b)

if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
library(dplyr)

data <- data %>%
  select(ST, DIVISION, AGEP, ESR, CIT, NATIVITY, CITWP, YOEP, COW, DIS, ENG, PAP, FER, MAR, SEX, SCH, SCHL, PINCP, WAGP, ANC, ANC1P, ANC2P, ESR, MIGSP, POBP, RAC1P, PERNP, HISP, HINS4, ADJINC)

write.csv(data, "psam_pus.csv", row.names = FALSE)
zip("psam_pus.zip", "psam_pus.csv")
file.remove("psam_pus.csv")

processed_data <- data %>%
  filter(AGEP >= 18, !ESR %in% c("b", 6)) %>%
  mutate(
    Citizen = ifelse(CIT %in% c(1, 2, 3, 4), 1, 0),
    Foreign_born = ifelse(NATIVITY == 2, 1, 0),
    High_School_Grad = ifelse(SCHL >= 16 & SCHL < 21, 1, 0),
    Bachelors_Degree = ifelse(SCHL >= 21, 1, 0),
    Unemployed = ifelse(ESR == 3, 1, 0),
    HISP_dummy = ifelse(HISP == 1, 0, 1),
    Years_in_US = 2022 - YOEP,
    Years_naturalized = 2022 - CITWP,
    SEX = as.numeric(SEX) - 1,
    HINS4 = as.numeric(HINS4) - 1,
    Asian = ifelse(RAC1P ==6, 1, 0)
  )

write.csv(processed_data, "processed_data.csv", row.names = FALSE)
zip("processed_data.zip", "processed_data.csv")
file.remove("processed_data.csv")

################################################################################
################################################################################
####################             Cleaning 2022             #####################
################################################################################
################################################################################
zip_file_path <- "/Users/zequi/Desktop/Berkeley/230A/stat230A/processed_data.zip"
csv_file_inside_zip <- "processed_data.csv"

processed_data <- read.csv(unzip(zip_file_path, files = csv_file_inside_zip))

#colnames(processed_data)
#correlation_matrix <- cor(processed_data)

library(tidyr)
library(dplyr)

if (!require(fastDummies)) install.packages("fastDummies")
library(fastDummies)

processed_data$ST <- as.factor(processed_data$ST)
processed_data <- dummy_cols(processed_data, select_columns = "ST", remove_first_dummy = TRUE)

processed_data$DIVISION <- as.factor(processed_data$DIVISION)
processed_data <- dummy_cols(processed_data, select_columns = "DIVISION", remove_first_dummy = TRUE)

processed_data <- processed_data %>%
  select(-ST, -DIVISION)


table(processed_data$SEX)
table(processed_data$Foreign_born)
chisq.test(processed_data$SEX, processed_data$Foreign_born)

table(processed_data$SEX)
table(processed_data$Citizen)
chisq.test(processed_data$SEX, processed_data$Citizen)

#*Logistics regression
#**First: Predict Employment status
employment_status_model <- glm(Unemployed ~ ST_2 + ST_4 + ST_5 + ST_6 + ST_8 + ST_9 + ST_10 + ST_11 + ST_12 + ST_13 + ST_15 + ST_16 + ST_17 + ST_18 + ST_19 + ST_20 + ST_21 + ST_22 + ST_23 + ST_24 + ST_25 + ST_26 + ST_27 + ST_28 + ST_29 + ST_30 + ST_31 + ST_32 + ST_33 + ST_34 + ST_35 + ST_36 + ST_37 + ST_38 + ST_39 + ST_40 + ST_41 + ST_42 + ST_44 + ST_45 + ST_46 + ST_47 + ST_48 + ST_49 + ST_50 + ST_51 + ST_53 + ST_54 + ST_55 + ST_56 + AGEP + DIS + ENG + PAP + FER + MAR + High_School_Grad + Bachelors_Degree + HISP_dummy + Asian + Years_in_US + Years_naturalized, data = processed_data, family = binomial())
#+ SEX + Citizen + Foreign_born
summary(employment_status_model)

#**Risk of poverty: 
#***Probability of PAP > 0 
processed_data$PAP_binary = as.numeric(processed_data$PAP > 0)
poverty_risk_model <- glm(PAP_binary ~ ST_2 + ST_4 + ST_5 + ST_6 + ST_8 + ST_9 + ST_10 + ST_11 + ST_12 + ST_13 + ST_15 + ST_16 + ST_17 + ST_18 + ST_19 + ST_20 + ST_21 + ST_22 + ST_23 + ST_24 + ST_25 + ST_26 + ST_27 + ST_28 + ST_29 + ST_30 + ST_31 + ST_32 + ST_33 + ST_34 + ST_35 + ST_36 + ST_37 + ST_38 + ST_39 + ST_40 + ST_41 + ST_42 + ST_44 + ST_45 + ST_46 + ST_47 + ST_48 + ST_49 + ST_50 + ST_51 + ST_53 + ST_54 + ST_55 + ST_56 + AGEP + DIS + ENG + Unemployed + FER + MAR + High_School_Grad + Bachelors_Degree + HISP_dummy + Asian + Years_in_US + Years_naturalized, family = binomial(), data = processed_data)
#+ SEX + Citizen + Foreign_born
summary(poverty_risk_model)


#***Probability of being on medicaid (HINS4)
medicaid_model <- glm(HINS4 ~ ST_2 + ST_4 + ST_5 + ST_6 + ST_8 + ST_9 + ST_10 + ST_11 + ST_12 + ST_13 + ST_15 + ST_16 + ST_17 + ST_18 + ST_19 + ST_20 + ST_21 + ST_22 + ST_23 + ST_24 + ST_25 + ST_26 + ST_27 + ST_28 + ST_29 + ST_30 + ST_31 + ST_32 + ST_33 + ST_34 + ST_35 + ST_36 + ST_37 + ST_38 + ST_39 + ST_40 + ST_41 + ST_42 + ST_44 + ST_45 + ST_46 + ST_47 + ST_48 + ST_49 + ST_50 + ST_51 + ST_53 + ST_54 + ST_55 + ST_56 + AGEP + DIS + ENG + Unemployed + FER + MAR + High_School_Grad + Bachelors_Degree + HISP_dummy + Asian + Years_in_US + Years_naturalized, family = binomial(), data = processed_data)
#+ SEX + Citizen + Foreign_born
summary(medicaid_model)



#*Blinder-Oaxaca
if (!require(oaxaca)) install.packages("oaxaca")
library(oaxaca)

#**Blinder Oxaca for the regressions, considering log(wage) and running z_i being Ethnic_interest (Asian, Latin=1 else 0)
oaxaca_data <- processed_data[processed_data$PERNP > 0, ]
oaxaca_data$Log_PERNP <- log(oaxaca_data$PERNP)

oaxaca_data$PERNP <- as.numeric(oaxaca_data$PERNP)
oaxaca_data$Ethnic_interest <- ifelse(oaxaca_data$HISP_dummy == 1 | oaxaca_data$Asian == 1, 1, 0)
oaxaca_data$Ethnic_interest <- as.factor(oaxaca_data$Ethnic_interest)

ethnic_oaxaca <- oaxaca(
  formula = log(PERNP) ~ ST_2 + ST_4 + ST_5 + ST_6 + ST_8 + ST_9 + ST_10 + ST_11 + ST_12 + ST_13 + ST_15 + ST_16 + ST_17 + ST_18 + ST_19 + ST_20 + ST_21 + ST_22 + ST_23 + ST_24 + ST_25 + ST_26 + ST_27 + ST_28 + ST_29 + ST_30 + ST_31 + ST_32 + ST_33 + ST_34 + ST_35 + ST_36 + ST_37 + ST_38 + ST_39 + ST_40 + ST_41 + ST_42 + ST_44 + ST_45 + ST_46 + ST_47 + ST_48 + ST_49 + ST_50 + ST_51 + ST_53 + ST_54 + ST_55 + ST_56 + AGEP + DIS + ENG + Unemployed + FER + MAR + High_School_Grad + Bachelors_Degree + Years_in_US + Years_naturalized | Ethnic_interest,
  data = oaxaca_data
)
#!!SEX, CIT, FOREING BORN
View(summary(ethnic_oaxaca))

#beta: Contains lists of estimated coefficients for different models:
#  beta.A and beta.B: The estimated coefficients for group A and group B, respectively. These coefficients represent the effect of each variable on the log-transformed personal income (log(PERNP)) for each group.
#beta a=1 (ethnic itnerest)
#beta.diff: The difference in coefficients between group A and group B. This indicates which variables contribute to the difference in income between the two groups and by how much.
#beta.R: The coefficients from a pooled regression across both groups.
#call: This is simply the function call that you made to perform the decomposition.
#n: Contains the number of observations in each group and overall.
#R: The number of bootstrap replications used to calculate the standard errors. In your case, this appears to be 100.
#reg: This contains lists with regression results for each group and the pooled regression.
#threefold: Contains the results of the threefold decomposition, if this option was chosen. It includes components such as the explained part (endowments), unexplained part (coefficients), and the interaction effect.
#twofold: Contains the results of the twofold decomposition, which includes the endowments and coefficients.
#x and y: These are the independent (x) and dependent (y) variables used in the models.


#**Compare immigrants and non-immigrants

colnames(oaxaca_data)

migrant_oaxaca <- oaxaca(
  formula = log(PERNP) ~ ST_2 + ST_4 + ST_5 + ST_6 + ST_8 + ST_9 + ST_10 + ST_11 + ST_12 + ST_13 + ST_15 + ST_16 + ST_17 + ST_18 + ST_19 + ST_20 + ST_21 + ST_22 + ST_23 + ST_24 + ST_25 + ST_26 + ST_27 + ST_28 + ST_29 + ST_30 + ST_31 + ST_32 + ST_33 + ST_34 + ST_35 + ST_36 + ST_37 + ST_38 + ST_39 + ST_40 + ST_41 + ST_42 + ST_44 + ST_45 + ST_46 + ST_47 + ST_48 + ST_49 + ST_50 + ST_51 + ST_53 + ST_54 + ST_55 + ST_56 + AGEP + DIS + ENG + Unemployed + FER + MAR + High_School_Grad + Bachelors_Degree | Foreign_born,
  data = oaxaca_data
)
#!!SEX, CIT, FOREING BORN
View(summary(migrant_oaxaca))