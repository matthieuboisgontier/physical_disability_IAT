# Load libraries
library(haven)
library(here)
library(dplyr)
library(tidyr)
library(stringr)
library(effects)
library(TOSTER)
library(ggplot2)
library(broom)
library(viridis)
library(patchwork)

# Load data
data_iat <- read_sav(here("Disability_IAT.public.2022-2024.sav"))

# recode country to geographic regions: Africa = 1, Asia = 2, Europe = 3, Northern America = 4, Oceania = 5, Latin America and Caribbeans= 6
# Recode countryres_num and countryres003_num for year = 2022
# Define the mapping of old values to new values
old_values <- 1:249
new_values <- c(4, 2, 3, 1, 5, 3, 1, 4, 4, 6, 6, 2, 6, 5, 3, 2, 6, 2, 2, 6, 3, 3, 6, 1, 4, 2, 6, 3, 1, 6, 6, 1, 2, 3, 1, 1, 2, 1, 4, 1, 6, 1, 1, 6, 2, 5, 5, 6, 1, 1, 1, 5, 6, 1, 3, 6, 2, 3, 3, 1, 6, 6, 2, 6, 1, 6, 1, 1, 3, 1, 6, 3, 5, 3, 3, 6, 5, 1, 1, 1, 2, 3, 1, 3, 3, 4, 4, 6, 5, 6, 1, 1, 6, 6, 5, 6, 2, 3, 3, 2, 2, 2, 2, 3, 2, 3, 6, 2, 2, 2, 1, 5, 2, 2, 2, 2, 2, 3, 2, 1, 1, 1, 3, 3, 3, 2, 3, 1, 1, 2, 2, 1, 3, 5, 6, 1, 1, 1, 6, 5, 3, 3, 2, 3, 6, 1, 1, 2, 1, 5, 2, 3, 6, 5, 5, 6, 1, 1, 5, 5, 5, 3, 2, 2, 5, 6, 5, 6, 6, 2, 5, 3, 3, 6, 2, 1, 3, 3, 1, 1, 6, 6, 4, 6, 5, 3, 1, 2, 1, 3, 1, 1, 2, 3, 3, 5, 1, 1, 6, 1, 3, 2, 1, 6, 3, 1, 3, 3, 2, 2, 2, 1, 2, 1, 5, 5, 6, 1, 2, 2, 6, 5, 1, 3, 2, 3, 5, 6, 2, 5, 3, 6, 2, 6, 6, 5, 2, 1, 1, 2, 6, 6, 5, 1, 2, 1, 1, NA, NA)

# Recode the variable using the mapping
data_iat$countryres_num <- new_values[match(data_iat$countryres_num, old_values)]
data_iat$countrycit_num <- new_values[match(data_iat$countrycit_num, old_values)]

data_iat$countryres003_num[data_iat$year == 2023] <- 
  recode(as.numeric(data_iat$countryres003_num[data_iat$year == 2023]), 
         !!!setNames(new_values, old_values))

data_iat$countrycit003_num[data_iat$year == 2023] <- 
  recode(as.numeric(data_iat$countrycit003_num[data_iat$year == 2023]), 
         !!!setNames(new_values, old_values))

# Old and new values for recoding
old_values_countryres003_num_2024 <- 1:249
new_values_countryres003_num_2024 <- c(4, 2, 3, 3, 1, 5, 3, 1, 4, 4, 6, 6, 2, 6, 5, 3, 2, 6, 2, 2, 6, 3, 3, 6, 1, 4, 2, 6, 6, 3, 1, 6, 6, 1, 2, 3, 1, 1, 2, 1, 4, 1, 6, 1, 1, 6, 2, 6, 1, 1, 1, 5, 6, 1, 3, 6, 6, 2, 3, 3, 1, 6, 6, 6, 1, 6, 1, 1, 3, 1, 1, 6, 3, 5, 3, 3, 6, 5, 1, 1, 1, 2, 3, 1, 3, 3, 4, 4, 6, 5, 6, 3, 1, 1, 6, 6, 5, 6, 2, 3, 3, 2, 2, 2, 2, 3, 3, 2, 3, 6, 2, 3, 2, 2, 1, 5, 2, 2, 2, 2, 2, 2, 3, 2, 1, 1, 1, 3, 3, 3, 2, 1, 1, 2, 2, 1, 3, 5, 6, 1, 1, 1, 6, 5, 3, 3, 2, 3, 6, 1, 1, 2, 1, 5, 2, 6, 5, 5, 6, 1, 1, 5, 5, 3, 5, 3, 2, 2, 5, 2, 6, 5, 6, 6, 2, 5, 3, 3, 6, 2, 1, 3, 3, 1, 6, 1, 6, 6, 6, 4, 6, 5, 3, 1, 2, 1, 3, 1, 1, 2, 6, 3, 3, 5, 1, 1, 6, 1, 3, 2, 1, 6, 3, 3, 3, 2, 2, 2, 1, 2, 2, 1, 5, 5, 6, 1, 2, 2, 6, 5, 1, 3, 2, 3, 5, 6, 2, 3, 6, 2, 6, 6, 5, 1, 2, 1, 1, NA, NA)

# Recode only for year == 2024
data_iat$countryres003_num[data_iat$year == 2024] <- 
  recode(as.numeric(data_iat$countryres003_num[data_iat$year == 2024]), 
         !!!setNames(new_values_countryres003_num_2024, old_values_countryres003_num_2024))

data_iat$countrycit003_num[data_iat$year == 2024] <- 
  recode(as.numeric(data_iat$countrycit003_num[data_iat$year == 2024]), 
         !!!setNames(new_values_countryres003_num_2024, old_values_countryres003_num_2024))

# Replace NA in Rcountryres003 with values in countrycit003
data_iat <- data_iat %>%
  mutate(countryres_num = coalesce(countryres_num, countryres003_num, countrycit_num, countrycit003_num)) %>%
  rename(geographic_region = countryres_num)

unique (data_iat$raceombmulti)
# Recode race variables and replace missing values in raceomb_002 with the corresponding values from raceomb_003
data_iat <- data_iat %>%
  mutate(
    # Recode raceomb_003 variables
    raceomb_003_asian = ifelse(raceomb_003_asian == 1, 1, raceomb_003_asian),
    raceomb_003_black = ifelse(raceomb_003_black == 1, 2, raceomb_003_black),
    raceomb_003_hispanic = ifelse(raceomb_003_hispanic == 1, 3, raceomb_003_hispanic),
    raceomb_003_middleeast = ifelse(raceomb_003_middleeast == 1, 4, raceomb_003_middleeast),
    raceomb_003_native = ifelse(raceomb_003_native == 1, 5, raceomb_003_native),
    raceomb_003_other = ifelse(raceomb_003_other == 1, 6, raceomb_003_other),
    raceomb_003_pacific = ifelse(raceomb_003_pacific == 1, 7, raceomb_003_pacific),
    raceomb_003_white = ifelse(raceomb_003_white == 1, 8, raceomb_003_white),

      # Recode raceombmulti variable
      raceombmulti = case_when(
        str_count(raceombmulti, ",") > 0 ~ 6,  # Assign 6 if there are multiple numbers
        raceombmulti == '1' ~ 5,  # Recoding as per initial logic
        raceombmulti == '5' ~ 2,  # Recoding as per initial logic
        raceombmulti == '6' ~ 8,  # Recoding as per initial logic
        raceombmulti == '7' ~ 1,  # Recoding as per initial logic
        raceombmulti == '[]' ~ NA_real_,  # Empty string should be NA
        is.na(raceombmulti) ~ NA_real_,  # NA remains NA
        TRUE ~ NA_real_  # Default to NA for any other case
      ),
    
    # Recode raceomb_002 variable
    raceomb_002 = case_when(
      raceomb_002 == 1 ~ 5,
      raceomb_002 == 2 ~ 1,
      raceomb_002 == 3 ~ 1,
      raceomb_002 == 4 ~ 7,
      raceomb_002 == 5 ~ 2,
      raceomb_002 == 6 ~ 8,
      raceomb_002 == 7 ~ 6,
      raceomb_002 == 8 ~ 6,
      TRUE ~ NA_real_  
    )
  )

# Replace NA values in raceomb_002 with the value from the raceomb_003 variables
data_iat <- data_iat %>% 
  mutate(
    raceomb_002 = ifelse(
      is.na(raceomb_002) | raceomb_002 == "", 
      coalesce(
        raceomb_003_asian, 
        raceomb_003_black, 
        raceomb_003_hispanic, 
        raceomb_003_middleeast, 
        raceomb_003_native, 
        raceomb_003_other, 
        raceomb_003_pacific, 
        raceomb_003_white, 
        raceombmulti  # Add this part to replace with raceombmulti if still NA
      ), 
      raceomb_002
    )
  )

# Data exploration
colnames(data_iat)
nrow(data_iat)

# Create age variable
data_iat$age <- data_iat$year - data_iat$birthyear

# Data filtering
data_iat <- data_iat %>%
  filter(pct_300 < 10, age >= 20, age <= 70)

# Recode occuSelfDetail and remove rows where occuSelfDetail is NA
data_iat <- data_iat %>%
  mutate(occuSelfDetail = case_when(
    occuSelfDetail == "29-1000" ~ "clin",
    occuSelfDetail == "31-2000" ~ "rehab",
    occuSelfDetail == "" ~ NA_character_,
    TRUE ~ "other"
  )) %>%
  filter(!is.na(occuSelfDetail))  

# Create the new variable occuSelfDetail_clinrehab merging clin and rehab values in clinrehab
data_iat$occuSelfDetail_clinrehab <- ifelse(data_iat$occuSelfDetail %in% c("clin", "rehab"), "clin_rehab", data_iat$occuSelfDetail)

# Group disabled variables
data_iat$disabled_newvar <- ifelse(
  data_iat$disabled_001 == 1 | data_iat$disabledfamilymem_001 == 1, 1L,
  ifelse(
    data_iat$disabled_001 == 2 & data_iat$disabledfamilymem_001 == 2, 2L,
    NA_integer_
  )
)

# map edu_14 to new 3-level classification edu_3
data_iat <- data_iat %>%
  mutate(edu_3 = case_when(
    edu_14 %in% 1:4  ~ "Primary & Secondary", 
    edu_14 %in% 5:7  ~ "College / Undergraduate", 
    edu_14 %in% 8:14 ~ "Graduate & Postgraduate", 
    TRUE ~ NA_character_
  ))

# birthSex: Recode genderidentity: keep 1 and 2, set others to NA
data_iat$genderIdentity_0002 <- ifelse(data_iat $genderIdentity_0002 %in% c(1, 2), 
                                               data_iat$genderIdentity_0002, 
                                               NA)
# birthSex: Recode genderidentity: keep 1 and 2, set others to NA
data_iat$genderIdentity <- ifelse(data_iat $genderIdentity %in% c(1, 2), 
                                       data_iat$genderIdentity, 
                                       NA)

## Convert birthSex to numeric, ensuring empty strings ("") become NA first
# Ensure birthSex is a character first
data_iat$birthSex <- as.character(data_iat $birthSex)

# Replace empty strings with NA
data_iat$birthSex[data_iat$birthSex == ""] <- NA

# Convert to numeric
data_iat$birthSex <- as.numeric(data_iat$birthSex)

# Ensure genderIdentity_0002 is numeric and only contains 1, 2, or NA
data_iat$genderIdentity_0002 <- ifelse(data_iat$genderIdentity_0002 %in% c(1, 2), 
                                               data_iat$genderIdentity_0002, 
                                               NA)
data_iat$genderIdentity <- ifelse(data_iat$genderIdentity %in% c(1, 2), 
                                       data_iat$genderIdentity, 
                                       NA)

# Replace birthSex with genderIdentity_0002 if birthSex is NA
data_iat <- data_iat %>%
  mutate(birthSex = coalesce(birthSex, genderIdentity_0002, genderIdentity))

# Convert relevant columns to factors
data_iat$occuSelfDetail <- as.factor(data_iat$occuSelfDetail)
data_iat$occuSelfDetail_clinrehab <- as.factor(data_iat$occuSelfDetail_clinrehab)
data_iat$genderIdentity <- as.factor(data_iat$genderIdentity)
data_iat$birthSex <- factor(ifelse(data_iat$birthSex == 2, 0, 1), levels = c(0, 1))  # 0 = Female, 1 = Male
data_iat$disabled_newvar  <- as.factor(data_iat$disabled_newvar) # 1 = yes; 2 = no
data_iat$geographic_region <- as.factor(data_iat$geographic_region)
data_iat$raceomb_002 <- as.factor(data_iat$raceomb_002)
data_iat$geographic_region <- as.factor(data_iat$geographic_region)
data_iat$edu_3 <- as.factor(data_iat$edu_3)

# Convert continuous variables to numeric
data_iat$age <- as.numeric(data_iat$age)
data_iat$att_7 <- as.numeric(data_iat$att_7) # 1 = strongly prefer disabled; 7 = strongly prefer abled
data_iat$year <- as.numeric(data_iat$year) 

# Select variables
selected_vars <- c("D_biep.PhysAbled_Good_all", "occuSelfDetail", "birthSex", "age", 
                   "att_7", "disabled_newvar", "geographic_region", "raceomb_002", "edu_3", "year")

# Select only the desired variables from the dataset
data_iat <- data_iat %>% 
  select(all_of(selected_vars))

### MISSING DATA

# Select only the variables used in the model
model_vars <- c("D_biep.PhysAbled_Good_all", "occuSelfDetail", "birthSex", "age", 
                "att_7", "disabled_newvar", "geographic_region", "raceomb_002", "edu_3", "year")

data_subset <- data_iat %>% select(all_of(model_vars))

# Compute number of missing values
missing_data <- data_subset %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "missing_count") %>%
  arrange(desc(missing_count))  # Sorting in descending order by missing_count

# Convert 'variable' to a factor with levels ordered by missing count (descending)
missing_data$variable <- factor(missing_data$variable, levels = missing_data$variable)

# Plot 
ggplot(missing_data, aes(y = variable, x = missing_count)) +
  geom_segment(aes(x = 0, xend = missing_count, y = variable, yend = variable), color = "black") +  # Horizontal line
  geom_point(aes(x = missing_count, y = variable), shape = 1, size = 3, color = "black") +  # Open circle
  labs(title = "Number of Missing Data in Model Variables",
       x = "Number of Missing Values",
       y = "Variable") +
  theme_minimal()

##Plot Missingness Patterns with a Heatmap
# Create a plot for missingness patterns for raceomb_002 (by count)
missing_data_by_raceomb_002_count <- data_subset %>%
  mutate(across(-c(raceomb_002, geographic_region), as.character)) %>%  # Convert all columns to character except raceomb_002 and geographic_region
  pivot_longer(cols = -c(raceomb_002, geographic_region), names_to = "variable", values_to = "value") %>%
  group_by(raceomb_002, variable) %>%
  summarise(missing_count = sum(is.na(value)), .groups = "drop")

ggplot(missing_data_by_raceomb_002_count, aes(x = raceomb_002, y = variable, fill = missing_count)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma", na.value = "gray") +
  labs(title = "Missing Data Patterns (Count) Across Variables and raceomb_002", 
       x = "raceomb_002", 
       y = "Variable", 
       fill = "Missing Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a plot for missingness patterns for geographic_region (by count)
missing_data_by_geographic_region_count <- data_subset %>%
  mutate(across(-c(geographic_region, raceomb_002), as.character)) %>%  # Convert all columns to character except geographic_region and raceomb_002
  pivot_longer(cols = -c(geographic_region, raceomb_002), names_to = "variable", values_to = "value") %>%
  group_by(geographic_region, variable) %>%
  summarise(missing_count = sum(is.na(value)), .groups = "drop")

ggplot(missing_data_by_geographic_region_count, aes(x = geographic_region, y = variable, fill = missing_count)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma", na.value = "gray") +
  labs(title = "Missing Data Patterns (Count) Across Variables and Geographic Region", 
       x = "Geographic Region", 
       y = "Variable", 
       fill = "Missing Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a plot for missingness patterns for age (by count)
missing_data_by_age_count <- data_subset %>%
  mutate(across(-c(age, geographic_region, raceomb_002), as.character)) %>%  # Convert all columns to character except key variables
  pivot_longer(cols = -c(age, geographic_region, raceomb_002), names_to = "variable", values_to = "value") %>%
  group_by(age, variable) %>%
  summarise(missing_count = sum(is.na(value)), .groups = "drop")

ggplot(missing_data_by_age_count, aes(x = age, y = variable, fill = missing_count)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma", na.value = "gray") +
  labs(title = "Missing Data Patterns (Count) Across Variables and Age", 
       x = "Age", 
       y = "Variable", 
       fill = "Missing Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Remove rows with NA
data_iat <- data_iat %>% drop_na()

# Standardize continuous variables for the entire dataset
data_iat$age_c <- scale(data_iat$age, center = TRUE, scale = TRUE)
data_iat$att_7_c <- scale(data_iat$att_7, center = TRUE, scale = TRUE)
data_iat$year_c <- scale(data_iat$year, center = TRUE, scale = TRUE)
data_iat$D_biep.PhysAbled_Good_all_c <- scale(data_iat$D_biep.PhysAbled_Good_all, center = TRUE, scale = TRUE)

# Ensure the new columns exist
data_iat$age_c_clin <- NA
data_iat$att_7_c_clin <- NA
data_iat$year_c_clin <- NA
data_iat$D_biep.PhysAbled_Good_all_c_clin <- NA

# create new columns
data_iat$age_c_rehab <- NA
data_iat$att_7_c_rehab <- NA
data_iat$year_c_rehab <- NA
data_iat$D_biep.PhysAbled_Good_all_c_rehab <- NA

# Standardize continuous variables for "clin" subset
data_iat$age_c_clin[data_iat$occuSelfDetail == "clin"] <- scale(data_iat$age[data_iat$occuSelfDetail == "clin"], center = TRUE, scale = TRUE)
data_iat$att_7_c_clin[data_iat$occuSelfDetail == "clin"] <- scale(data_iat$att_7[data_iat$occuSelfDetail == "clin"], center = TRUE, scale = TRUE)
data_iat$year_c_clin[data_iat$occuSelfDetail == "clin"] <- scale(data_iat$year[data_iat$occuSelfDetail == "clin"], center = TRUE, scale = TRUE)
data_iat$D_biep.PhysAbled_Good_all_c_clin[data_iat$occuSelfDetail == "clin"] <- scale(data_iat$D_biep.PhysAbled_Good_all[data_iat$occuSelfDetail == "clin"], center = TRUE, scale = TRUE)

# Standardize continuous variables for "rehab" subset
data_iat$age_c_rehab[data_iat$occuSelfDetail == "rehab"] <- scale(data_iat$age[data_iat$occuSelfDetail == "rehab"], center = TRUE, scale = TRUE)
data_iat$att_7_c_rehab[data_iat$occuSelfDetail == "rehab"] <- scale(data_iat$att_7[data_iat$occuSelfDetail == "rehab"], center = TRUE, scale = TRUE)
data_iat$year_c_rehab[data_iat$occuSelfDetail == "rehab"] <- scale(data_iat$year[data_iat$occuSelfDetail == "rehab"], center = TRUE, scale = TRUE)
data_iat$D_biep.PhysAbled_Good_all_c_rehab[data_iat$occuSelfDetail == "rehab"] <- scale(data_iat$D_biep.PhysAbled_Good_all[data_iat$occuSelfDetail == "rehab"], center = TRUE, scale = TRUE)

# Relevel factors
data_iat$occuSelfDetail <- relevel(data_iat$occuSelfDetail, ref = "other")
data_iat$raceomb_002 <- relevel(data_iat$raceomb_002, ref = "8") #white
data_iat$geographic_region <- relevel(data_iat$geographic_region, ref = "4")
data_iat$edu_3 <- relevel(data_iat$"edu_3", ref = "Primary & Secondary")

## Descriptive results
# Age and explicit attitudes as a function of occupation category
data_iat %>%
  group_by(occuSelfDetail) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE),
    
    mean_att_7 = mean(att_7, na.rm = TRUE),
    sd_att_7 = sd(att_7, na.rm = TRUE),
    min_att_7 = min(att_7, na.rm = TRUE),
    max_att_7 = max(att_7, na.rm = TRUE),
    median_att_7 = median(att_7, na.rm = TRUE),
    
    mean_D_biep.PhysAbled_Good_all = mean(D_biep.PhysAbled_Good_all, na.rm = TRUE),
    sd_D_biep.PhysAbled_Good_all = sd(D_biep.PhysAbled_Good_all, na.rm = TRUE),
    min_D_biep.PhysAbled_Good_all = min(D_biep.PhysAbled_Good_all, na.rm = TRUE),
    max_D_biep.PhysAbled_Good_all = max(D_biep.PhysAbled_Good_all, na.rm = TRUE),
    median_D_biep.PhysAbled_Good_all = median(D_biep.PhysAbled_Good_all, na.rm = TRUE),
    
n = n()  # Number of participants per occupation category
  ) %>%
  arrange(mean_age) %>%
  print(width = Inf)  # Forces full column display

# number of observations per value of the variables sex, disabled, geographic region, and race
# Function to count and calculate percentages
count_percentage <- function(data, group_var) {
  data %>%
    group_by(occuSelfDetail, {{ group_var }}) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(occuSelfDetail) %>%
    mutate(perc = round(100 * n / sum(n), 2)) %>%
    arrange(occuSelfDetail, desc(n))  # Optional: Order by occupation and count
}

# Count and percentage for each variable
birthSex_counts <- count_percentage(data_iat, birthSex)
disabled_counts <- count_percentage(data_iat, disabled_newvar)
geographic_region_counts <- count_percentage(data_iat, geographic_region)
race_counts <- count_percentage(data_iat, raceomb_002)
edu_3_counts <- count_percentage(data_iat, edu_3)

# Display results
print(birthSex_counts, n = Inf)
print(disabled_counts, n = Inf)
print(geographic_region_counts, n = Inf)
print(race_counts, n = Inf)
print(edu_3_counts, n = Inf)


### STATISTICAL MODELS ###
## Implicit attitudes
# Implicit: main model
lm1.implicit <- lm(D_biep.PhysAbled_Good_all ~ occuSelfDetail + birthSex + age_c + att_7_c + disabled_newvar + geographic_region + raceomb_002 + edu_3 + year_c, 
          data = data_iat, na.action=na.omit)
summary(lm1.implicit )
confint(lm1.implicit )

# For Figures and interpretation of continuous variables
lm1.implicit_fig <- lm(D_biep.PhysAbled_Good_all ~ occuSelfDetail + birthSex + age + att_7 + disabled_newvar + geographic_region + raceomb_002 + edu_3 + year, 
                       data = data_iat, na.action=na.omit)
summary(lm1.implicit_fig)
plot(allEffects(lm1.implicit_fig))

# Equivalence testing for lm1.implicit: Clinicians vs. other
tsum_TOST(
  m1 = 0.001736,               # Estimate for occuSelfDetailclin
  m2 = 0,                       # Reference group (Effect is 0 for reference)
  sd1 = 0.005490,               # Standard Error for occuSelfDetailclin
  sd2 = 0.4271,                 # Residual Standard Error for occuSelfDetailother_not-health
  n1 = 6445,                    # Sample size for occuSelfDetailclin
  n2 = 203264,                   # Sample size for occuSelfDetailother_not-health
  low_eqbound = -0.0194,        # Lower equivalence bound
  high_eqbound = 0.0194,        # Upper equivalence bound
  alpha = 0.05                  # Significance level
)

# Equivalence testing for lm1.implicit: Rehab assistants vs. other
tsum_TOST(
  m1 = 0.007488,               # Estimate for occuSelfDetailrehab
  m2 = 0,                       # Reference group (Effect is 0 for reference)
  sd1 = 0.007326,               # Standard Error for occuSelfDetailrehab
  sd2 = 0.4271,                 # Residual Standard Error for occuSelfDetailother_not-health
  n1 = 3482,                    # Sample size for occuSelfDetailrehab
  n2 = 203264,                   # Sample size for occuSelfDetailother_not-health
  low_eqbound = -0.0194,        # Lower equivalence bound
  high_eqbound = 0.0194,        # Upper equivalence bound
  alpha = 0.05                  # Significance level
)

# Subset clinicians
lm1_implicit.clin <- lm(D_biep.PhysAbled_Good_all ~ birthSex + age_c_clin + att_7_c_clin + disabled_newvar + geographic_region + raceomb_002 + edu_3 + year_c_clin, 
                        data = data_iat, na.action=na.omit,  
                        subset = (occuSelfDetail == "clin"))
summary(lm1_implicit.clin)
confint(lm1_implicit.clin)

# For Figures
lm1_implicit.clin_fig <- lm(D_biep.PhysAbled_Good_all ~ birthSex + age + att_7 + disabled_newvar + geographic_region + raceomb_002 + edu_3 + year, 
                        data = data_iat, na.action=na.omit,  
                        subset = (occuSelfDetail == "clin"))
summary(lm1_implicit.clin_fig)

# Subset rehab
lm1_implicit.rehab <- lm(D_biep.PhysAbled_Good_all ~ birthSex + age_c_rehab + att_7_c_rehab + disabled_newvar + geographic_region + raceomb_002 + edu_3 + year_c_rehab, 
                         data = data_iat, na.action=na.omit,  
                         subset = (occuSelfDetail == "rehab"))
summary(lm1_implicit.rehab)
confint(lm1_implicit.rehab)

# For Figures
lm1_implicit.rehab_fig <- lm(D_biep.PhysAbled_Good_all ~ birthSex + age + att_7 + disabled_newvar + geographic_region + raceomb_002 + edu_3 + year, 
                         data = data_iat, na.action=na.omit,  
                         subset = (occuSelfDetail == "rehab"))
summary(lm1_implicit.rehab)

## Explicit attitudes
# Explicit: main model
lm1.explicit <- lm(att_7 ~ occuSelfDetail + birthSex + age_c + D_biep.PhysAbled_Good_all_c + disabled_newvar + geographic_region + raceomb_002 + edu_3 + year_c, 
                   data = data_iat, na.action=na.omit)
summary(lm1.explicit)
confint(lm1.explicit)

# For Figures
lm1.explicit_fig <- lm(att_7 ~ occuSelfDetail + birthSex + age + D_biep.PhysAbled_Good_all + disabled_newvar + geographic_region + raceomb_002 + edu_3 + year, 
                       data = data_iat, na.action=na.omit)
summary(lm1.explicit_fig)

## subset clin
lm1_explicit.clin <- lm(att_7 ~ birthSex + age_c_clin + D_biep.PhysAbled_Good_all_c_clin + disabled_newvar + geographic_region + raceomb_002 + edu_3 + year_c_clin, 
                        data = data_iat, na.action=na.omit,  
                        subset = (occuSelfDetail == "clin"))
summary(lm1_explicit.clin)
confint(lm1_explicit.clin)

# For Figures
lm1_explicit.clin_fig <- lm(att_7 ~ birthSex + age + D_biep.PhysAbled_Good_all + disabled_newvar + geographic_region + raceomb_002 + edu_3 + year, 
                        data = data_iat, na.action=na.omit,  
                        subset = (occuSelfDetail == "clin"))
summary(lm1_explicit.clin_fig)

## Subset rehab
lm1_explicit.rehab <- lm(att_7 ~ birthSex + age_c_rehab + D_biep.PhysAbled_Good_all_c_rehab + disabled_newvar + geographic_region + raceomb_002 + edu_3 + year_c_rehab, 
                         data = data_iat, na.action=na.omit,  
                         subset = (occuSelfDetail == "rehab"))
summary(lm1_explicit.rehab)
confint(lm1_explicit.rehab)

# For Figures
lm1_explicit.rehab_fig <- lm(att_7 ~ birthSex + age + D_biep.PhysAbled_Good_all + disabled_newvar + geographic_region + raceomb_002 + edu_3 + year, 
                         data = data_iat, na.action=na.omit,  
                         subset = (occuSelfDetail == "rehab"))
summary(lm1_explicit.rehab_fig)

### FIGURES ###
## Figure 1A
# Compute effects
effects_list <- allEffects(lm1.implicit_fig)

# Convert to a data frame for ggplot
effects_df <- as.data.frame(effects_list$occuSelfDetail)  # Change variable name as needed

# Create plot
ggplot(effects_df, aes(x = occuSelfDetail, y = fit)) +
  geom_point(size = 3, color = "blue") +  # Effect estimates
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +  # Confidence intervals
  labs(title = "Effect of Occupation on D_biep.PhysAbled_Good_all",
       x = "Occupation",
       y = "Estimated Effect",
       caption = "Based on lm1 model") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

## Figure 1B
# Compute effects
effects_list <- allEffects(lm1.explicit_fig)

# Convert to a data frame for ggplot
effects_df <- as.data.frame(effects_list$occuSelfDetail)  # Change variable name as needed

# Create the plot
ggplot(effects_df, aes(x = occuSelfDetail, y = fit)) +
  geom_point(size = 3, color = "red") +  # Effect estimates
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +  # Confidence intervals
  labs(title = "Effect of Occupation on D_biep.PhysAbled_Good_all",
       x = "Occupation",
       y = "Estimated Effect",
       caption = "Based on lm1 model") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

## Figure 2A
# Extract coefficients with confidence intervals
coefs <- tidy(lm1.implicit_fig, conf.int = TRUE)  

# Exclude intercept and filter for significant effects (p < 0.05)
#coefs <- coefs[coefs$term != "(Intercept)" & coefs$p.value < 0.05, ]  
coefs_all <- coefs[coefs$term != "(Intercept)", ]

# Reorder terms by estimate value (smallest to largest)
#coefs$term <- reorder(coefs$term, coefs$estimate)
coefs_all$term <- reorder(coefs_all$term, coefs_all$estimate)

# Plot coefficients
ggplot(coefs_all, aes(x = term, y = estimate)) +
  geom_point(size = 3, color = "blue") +  # Regression coefficients
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  # 95% CI
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Reference line at 0
  labs(title = "Regression Coefficients for lm1 (Ordered)",
       x = "Explanatory Variables",
       y = "Estimated Coefficients",
       caption = "Error bars show 95% confidence intervals") +
  coord_flip() +  # Flip to horizontal for better readability
  theme_minimal()

## Figure 2B
# Extract coefficients with confidence intervals
coefs <- tidy(lm1.explicit_fig, conf.int = TRUE)  

# Exclude intercept and filter for significant effects (p < 0.05)
#coefs <- coefs[coefs$term != "(Intercept)" & coefs$p.value < 0.05, ]  
coefs_all <- coefs[coefs$term != "(Intercept)", ]

# Reorder terms by estimate value (smallest to largest)
#coefs$term <- reorder(coefs$term, coefs$estimate)
coefs_all$term <- reorder(coefs_all$term, coefs_all$estimate)

# Plot coefficients
ggplot(coefs_all, aes(x = term, y = estimate)) +
  geom_point(size = 3, color = "red") +  # Regression coefficients
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  # 95% CI
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Reference line at 0
  labs(title = "Regression Coefficients for lm1 (Ordered)",
       x = "Explanatory Variables",
       y = "Estimated Coefficients",
       caption = "Error bars show 95% confidence intervals") +
  coord_flip() +  # Flip to horizontal for better readability
  theme_minimal()

## Figure 3A: implicit & clinicians
# Extract coefficients with confidence intervals
coefs <- tidy(lm1_implicit.clin_fig, conf.int = TRUE)  

# Exclude intercept and filter for significant effects (p < 0.05)
coefs_sig <- coefs[coefs$term != "(Intercept)" & coefs$p.value < 0.05, ]

# Reorder terms by estimate value
coefs_sig$term <- reorder(coefs_sig$term, coefs_sig$estimate)

# Plot only significant coefficients
ggplot(coefs_sig, aes(x = term, y = estimate)) +
  geom_point(size = 3, color = "blue") +  # Regression coefficients
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  # 95% CI
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Reference line at 0
  labs(title = "Significant Regression Coefficients for lm1_implicit.clin",
       x = "Explanatory Variables",
       y = "Estimated Coefficients",
       caption = "Only significant effects (p < 0.05) are shown. Error bars represent 95% confidence intervals.") +
  coord_flip() +  # Flip to horizontal for readability
  theme_minimal()

## Figure 3B: explicit & clinicians
# Extract coefficients with confidence intervals
coefs <- tidy(lm1_explicit.clin_fig, conf.int = TRUE)  

# Exclude intercept and filter for significant effects (p < 0.05)
coefs_sig <- coefs[coefs$term != "(Intercept)" & coefs$p.value < 0.05, ]

# Reorder terms by estimate value
coefs_sig$term <- reorder(coefs_sig$term, coefs_sig$estimate)

# Plot only significant coefficients
ggplot(coefs_sig, aes(x = term, y = estimate)) +
  geom_point(size = 3, color = "red") +  # Regression coefficients
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  # 95% CI
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Reference line at 0
  labs(title = "Significant Regression Coefficients for lm1_explicit.clin",
       x = "Explanatory Variables",
       y = "Estimated Coefficients",
       caption = "Only significant effects (p < 0.05) are shown. Error bars represent 95% confidence intervals.") +
  coord_flip() +  # Flip to horizontal for readability
  theme_minimal()

## Figure 3C: implicit & rehabilitation assistants
# Extract coefficients with confidence intervals
coefs <- tidy(lm1_implicit.rehab_fig, conf.int = TRUE)  

# Exclude intercept and filter for significant effects (p < 0.05)
coefs_sig <- coefs[coefs$term != "(Intercept)" & coefs$p.value < 0.05, ]

# Reorder terms by estimate value
coefs_sig$term <- reorder(coefs_sig$term, coefs_sig$estimate)

# Plot only significant coefficients
ggplot(coefs_sig, aes(x = term, y = estimate)) +
  geom_point(size = 3, color = "blue") +  # Regression coefficients
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  # 95% CI
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Reference line at 0
  labs(title = "Significant Regression Coefficients for lm1_implicit.rehab",
       x = "Explanatory Variables",
       y = "Estimated Coefficients",
       caption = "Only significant effects (p < 0.05) are shown. Error bars represent 95% confidence intervals.") +
  coord_flip() +  # Flip to horizontal for readability
  theme_minimal()

## Figure 3D: explicit & rehabilitation assistants
# Extract coefficients with confidence intervals
coefs <- tidy(lm1_explicit.rehab_fig, conf.int = TRUE)  

# Exclude intercept and filter for significant effects (p < 0.05)
coefs_sig <- coefs[coefs$term != "(Intercept)" & coefs$p.value < 0.05, ]

# Reorder terms by estimate value
coefs_sig$term <- reorder(coefs_sig$term, coefs_sig$estimate)

# Plot only significant coefficients
ggplot(coefs_sig, aes(x = term, y = estimate)) +
  geom_point(size = 3, color = "red") +  # Regression coefficients
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  # 95% CI
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Reference line at 0
  labs(title = "Significant Regression Coefficients for lm1_explicit.rehab",
       x = "Explanatory Variables",
       y = "Estimated Coefficients",
       caption = "Only significant effects (p < 0.05) are shown. Error bars represent 95% confidence intervals.") +
  coord_flip() +  # Flip to horizontal for readability
  theme_minimal()

## Figure 4
# Extract the specific predictor effects as data frames
eff_clin_age <- as.data.frame(allEffects(lm1_implicit.clin_fig, select = 2)$age)  
eff_rehab_age <- as.data.frame(allEffects(lm1_implicit.rehab_fig, select = 2)$age)  
eff_clin_likert <- as.data.frame(allEffects(lm1_implicit.clin_fig, select = 3)$att_7)  
eff_rehab_likert <- as.data.frame(allEffects(lm1_implicit.rehab_fig, select = 3)$att_7)  

# Print structure to verify correct extraction
str(eff_clin_age)

# Define a function to plot effects
plot_effect <- function(data, title, color) {
  ggplot(data, aes(x = data[[1]], y = fit)) +  # First column = predictor
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = color, alpha = 0.3) +
    geom_line(color = color, size = 1.2) +
    labs(title = title, x = names(data)[1], y = "Effect Size") +
    theme_minimal()
}

# Generate plots
p1 <- plot_effect(eff_clin_age, "Implicit (Clinicians) - Age", viridis(4)[1])
p2 <- plot_effect(eff_rehab_age, "Implicit (Rehab) - Age", viridis(4)[2])
p3 <- plot_effect(eff_clin_likert, "Implicit (Clinicians) - Likert", viridis(4)[3])
p4 <- plot_effect(eff_rehab_likert, "Implicit (Rehab) - Likert", viridis(4)[4])

# Extract the specific predictor effects as data frames
eff_clin_age <- as.data.frame(allEffects(lm1_implicit.clin_fig, select = 2)$age)  
eff_rehab_age <- as.data.frame(allEffects(lm1_implicit.rehab_fig, select = 2)$age)  
eff_clin_likert <- as.data.frame(allEffects(lm1_implicit.clin_fig, select = 3)$att_7)  
eff_rehab_likert <- as.data.frame(allEffects(lm1_implicit.rehab_fig, select = 3)$att_7)  

# Add a column to differentiate the models
eff_clin_age$model <- "Clinicians"
eff_rehab_age$model <- "Rehab"
eff_clin_likert$model <- "Clinicians"
eff_rehab_likert$model <- "Rehab"

# Combine datasets
eff_age <- rbind(eff_clin_age, eff_rehab_age)
eff_likert <- rbind(eff_clin_likert, eff_rehab_likert)

# Extract different viridis colors for each set
scales::show_col(viridis(10, option = "viridis"))
custom_colors_p1_p2 <- c("#440154FF", "#9C179EFF")
custom_colors_p3_p4 <- c("#26828EFF", "#6DCD59FF")  

# Compute the actual min/max from CI to ensure full coverage
ymin_p1_p2 <- min(eff_age$lower, 0.4)  
ymax_p1_p2 <- max(eff_age$upper, 0.9)  
ymin_p3_p4 <- min(eff_likert$lower, 0.2)  
ymax_p3_p4 <- max(eff_likert$upper, 0.7)  

# Superimposed plot for Age effect
p1_p2 <- ggplot(eff_age, aes(x = age, y = fit, color = model, fill = model)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +  
  geom_line(size = 1.2) +  
  scale_color_manual(values = custom_colors_p1_p2, name = "Model") +  
  scale_fill_manual(values = custom_colors_p1_p2, name = "Model") +  
  scale_y_continuous(
    breaks = seq(0.4, 1.1, by = 0.2),  # Set fixed breaks
    limits = c(0.4,  1.1),  # Enforce range but allow expansion for CI
    expand = c(0, 0)
  ) +
  labs(title = "Implicit Attitudes - Age (Clinicians vs. Rehab)", x = "Age", y = "Effect Size") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

# Superimposed plot for Likert effect
p3_p4 <- ggplot(eff_likert, aes(x = att_7, y = fit, color = model, fill = model)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +  
  geom_line(size = 1.2) +  
  scale_color_manual(values = custom_colors_p3_p4, name = "Model") +  
  scale_fill_manual(values = custom_colors_p3_p4, name = "Model") +  
  scale_y_continuous(
    breaks = seq(0.2, 0.9, by = 0.2),
    limits = c(0.2, 0.9),
    expand = c(0, 0)
  ) +
  labs(title = "Implicit Attitudes - Likert (Clinicians vs. Rehab)", x = "Attitude", y = "Effect Size") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

# Arrange plots
final_plot <- p1_p2 / p3_p4
print(final_plot)
