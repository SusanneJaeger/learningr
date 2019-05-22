# Load the packages
library(tidyverse)
library(NHANES)

# Check column names
colnames(NHANES)

# Look at contents
str(NHANES)
glimpse(NHANES)

# See summary
summary(NHANES)

# Look over the dataset documentation
?NHANES

NHANES

NHANES_less <- NHANES %>%
  select(
  Age, Gender, SurveyYr, AgeDiabetesDiagnosis = DiabetesAge, BMI, BPDiaAve, BPSysAve,DrinksOfAlcoholInDay=AlcoholDay, PhysActiveDays, NumberBabies = nBabies,
  Poverty, TotalCholestrol=TotChol)

NHANES_less_1 <- NHANES_less %>%
mutate(MoreThan5DaysActive = if_else(PhysActiveDays >= 5,
                                     "yes",
                                     "no"))

NHANES_less_1 <- NHANES_less_1 %>%
  select(
    Age, Gender, SurveyYr, AgeDiabetesDiagnosis, BMI, BPDiaAve, BPSysAve,DrinksOfAlcoholInDay, NumberBabies,
    Poverty, TotalCholestrol)

NHANES_convert<- NHANES_less_1 %>%
  gather(Measure, Value, -SurveyYr, -Gender)


NHANES_convert_1 <- NHANES_convert %>%
  group_by (SurveyYr, Gender, Measure) %>%
  summarise(
    MeanValue = mean(Value, na.rm = TRUE)
  )






