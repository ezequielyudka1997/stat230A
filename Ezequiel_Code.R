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
colnames(processed_data)

correlation_matrix <- cor(processed_data)

#*Logistics regression
#**First: Predict Employment status
!!fix
employment_status_model <- glm(Unemployed ~ ST + DIVISION + AGEP + DIS + ENG + PAP + FER + MAR + SEX + Citizen + Foreign_born + High_School_Grad + Bachelors_Degree + HISP_dummy + Years_in_US + Years_naturalized, data = processed_data, family = binomial())
summary(employment_status_model)

#**Risk of poverty: 
#***Probability of PAP > 0 
!!fix
processed_data$PAP_binary = as.numeric(processed_data$PAP > 0)
poverty_risk_model <- glm(PAP_binary ~ ST + DIVISION + AGEP + DIS + ENG + Unemployed + FER + MAR + SEX + Citizen + Foreign_born + High_School_Grad + Bachelors_Degree + HISP_dummy + Years_in_US + Years_naturalized, family = binomial(), data = processed_data)
summary(poverty_risk_model)


#***Probability of being on medicaid (HINS4)
!!fix
medicaid_model <- glm(HINS4 ~ ST + DIVISION + AGEP + DIS + ENG + Unemployed + FER + MAR + SEX + Citizen + Foreign_born + High_School_Grad + Bachelors_Degree + HISP_dummy + Years_in_US + Years_naturalized, family = binomial(), data = processed_data)
summary(medicaid_model)

#*Blinder-Oaxaca 
if (!require("oaxaca")) install.packages("oaxaca", dependencies = TRUE)
library(oaxaca)
processed_data$Ethnic_interest <- ifelse(processed_data$HISP_dummy == 1 | processed_data$Asian == 1, 1, 0)

ethnic_bo <- blinder_oaxaca(log_wage ~ X1 + X2 + ..., data = processed_data, group = Ethnic_interest)
summary(ethnic_bo)


**Blinder Oxaca for the regressions, considering log(wage) and running z_i being Ethnic_interest (Asian, Latin=1 else 0)
**Compare immigrants and non-immigrants






################################################################################
################################################################################
####################                 2019                  #####################
################################################################################
################################################################################
#https://www2.census.gov/programs-surveys/acs/data/pums/2019/1-Year/
#csv_pus.zip
#Note that for the National files, there are multiple files. 
#For PUMS 1-year data there is an “A” and “B” file. For Person-level data the 
#names are “PSAM_PUSA” and “PSAM_PUSB”. The Housing-level files are “PSAM_HUSA” 
#and “PSAM_HUSB”.

if (!require("readr")) install.packages("readr", dependencies = TRUE)
library(readr)

# We append a and b data once
#data_a <- read.csv("/Users/zequi/Desktop/Berkeley/230A/stat230A/csv_pus/psam_pusa.csv")
#data_b <- read.csv("/Users/zequi/Desktop/Berkeley/230A/stat230A/csv_pus/psam_pusb.csv")
#Below are instructions for concatenating the two 1-year “a” and “b” PUMS files 
#to create a single national file. The code is in italics and uses SAS 
#programming code. Concatenate the person-level files using the set statement:
#data population; set psam_pusa psam_pusb; run;
#data <- rbind(data_a, data_b)
#write.csv(data, "/Users/zequi/Desktop/Berkeley/230A/stat230A/psam_pus.csv", row.names = FALSE)
#zip("psam_pus.zip", "psam_pus.csv")
#file.remove("/Users/zequi/Desktop/Berkeley/230A/stat230A/psam_pus.csv")


library(dplyr)
library(oaxaca)

zip_file_path <- "/Users/zequi/Desktop/Berkeley/230A/psam_pus.zip"
csv_file_inside_zip <- "psam_pus.csv"

# Load data
data_2019 <- read.csv(unzip(zip_file_path, files = csv_file_inside_zip))

# Enhance data transformation
data_2019 <- data_2019 %>%
  mutate(
    HISP_dummy = ifelse(HISP == 1, 0, 1),
    RACWHT_NOT_HISP = as.integer(HISP_dummy == 0 & RACWHT == 1),
    RACBLK_NOT_HISP = as.integer(HISP_dummy == 0 & RACBLK == 1),
    SEX = as.numeric(SEX) - 1,  # Assuming SEX is 1 for male, 2 for female
    ethnic_group = case_when(
      RACASN == 1 ~ "Asian",
      RACBLK == 1 & HISP_dummy == 0 ~ "Non-Latin Black",
      RACWHT == 1 & HISP_dummy == 0 ~ "Non-Latin White",
      HISP_dummy == 1 ~ "Latin",
      TRUE ~ NA_character_
    ),
    Education_Level = case_when(
      SCHL %in% 1:15 ~ "School",
      SCHL %in% 16:17 ~ "High School",
      SCHL >= 18 ~ "College or More",
      TRUE ~ NA_character_
    ),
    Years_in_US = 2019 - YOEP
)

data_2019$SCHL <- as.factor(data_2019$SCHL)
data_2019$SEX <- as.numeric(data_2019$SEX)

# Filter non-Americans
non_americans_2019 <- data_2019 %>%
  filter(NATIVITY == 2, CIT != 3, AGEP >= 18, MIG %in% c(1, 3))

# Subset data
data_asian_2019 <- filter(non_americans_2019, ethnic_group == "Asian")
data_black_2019 <- filter(non_americans_2019, ethnic_group == "Non-Latin Black")
data_white_2019 <- filter(non_americans_2019, ethnic_group == "Non-Latin White")
data_latin_2019 <- filter(non_americans_2019, ethnic_group == "Latin")

# Summarize and check rows
counts <- sapply(list(data_asian_2019, data_black_2019, data_white_2019, data_latin_2019), nrow)
print(counts)

colnames(data_latin_2019)

library(lmtest)

#Interesting variables
#ENG: English-speaking ability.
#LANX: Language other than English spoken at home.
#YOEP: Year of entry to the U.S. for foreign-born.
#CITWP: Year of naturalization for citizens.

#age as proxy of experience, we should find something better
# Regression for each ethnic group
model_ethnic_group <- lm(PINCP ~ SCHL + AGEP + SEX + ENG + Years_in_US, data=data_latin_2019)

# Pooled regression
model_pooled <- lm(PINCP ~ SCHL + AGEP + SEX + ethnic_group + ENG + Years_in_US, data=data_2019)

model_advanced <- lm(PINCP ~ SCHL * SEX + AGEP + ENG + Years_in_US, data=data_2019)
summary(model_advanced)


#Probability of receiving public assistance
#risk of single parenthood
#risk of poverty


# Perform Blinder-Oaxaca decomposition
if (!require(oaxaca)) install.packages("oaxaca", dependencies = TRUE)
library(oaxaca)
model_formula <- PINCP ~ SCHL + AGEP + SEX | HISP_dummy
decomp_results <- oaxaca(formula = model_formula,
                         data = data_2019,
                         R = 100)
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
####not run
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
#Key variables
#Asian dummy
non_americans_2019$RACASN
#asian <- data_2019[data_2019$RACASN==1,]
#Asian detailed
non_americans_2019$RAC2P
#Hispanic dummy
data_2019$HISP_dummy
#Hispanic detailed
data_2019$HISP

# Oaxaca decomposition
model_formula <- PINCP ~ Education_Level + SEX | HISP_dummy
decomp_results <- oaxaca(formula = model_formula, data = data_latin_2019, data2 = data_white_2019, R = 100)
print(decomp_results)
barplot(decomp_results$decomposition$effects, main = "Blinder-Oaxaca Decomposition",
        names.arg = c("Endowments", "Coefficients", "Interaction"),
        col = c("skyblue", "orange", "grey"))


data$generation <- as.factor(data$generation)
data$family_structure <- as.factor(data$family_structure)  # 0 for two-parent, 1 for single-parent
data$poverty_status <- as.factor(data$poverty_status)      # 0 for not in poverty, 1 for in poverty
data$public_assistance <- as.factor(data$public_assistance)  # 0 for no, 1 for yes

df <- df %>%
  mutate(
    family_single_parent = ifelse(family_structure == "Single Parent", 1, 0),  # Binary outcome: 1 if single parent, 0 otherwise
    is_poor = ifelse(income < poverty_threshold, 1, 0)  # Assuming 'poverty_threshold' is defined
  )

if (!require(ROCR)) install.packages("ROCR")
library(ROCR)
library(stats)
library(broom)


model <- glm(family_single_parent ~ ethnic_group + generation + age_head + education_head, 
             data = df, family = binomial)
summary(model)
tidy(model)


# Predictive performance
prob_predictions <- predict(model, type = "response")
pred_obj <- prediction(prob_predictions, df$family_single_parent)
perf <- performance(pred_obj, measure = "tpr", x.measure = "fpr")

# Plot ROC curve
plot(perf)

# Check residuals and fitted values
plot(poverty_model)
