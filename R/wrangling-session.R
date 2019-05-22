source(here::here("R/package-loading.R"))
glimpse(NHANES)
usethis::use_r("exercises-wrangling")

#control+shift+m --> %>%#
NHANES %>%
  colnames() %>%
  lenth()
length(colnames(NHANES))

NHANES %>%
  mutate(Height = Height/100,
         testing = "yes",
         HighlyActive = if_else(PhysActiveDays >= 5,
                                "yes",
                                "no"
        ))
#does not change the dataset unless you type that. its only on the screen#

NHANES_updated <- NHANES %>%
  mutate(UrineVolAverage = (UrineVol1 + UrineVol2) / 2)


# Exercise piping transforming and adding ---------------------------------


# Check the names of the variables
colnames(NHANES)

# Pipe the data into mutate function and:
NHANES_modified <- NHANES %>% # dataset
  mutate(
    # 1. Calculate average urine volume
    UrineVolAverage = UrineVol1 + UrineVol2 / 2,
    # 2. Modify Pulse variable
    Pulse = Pulse / 60,
    # 3. Create YoungChild variable using a condition
    YoungChild = if_else(Age < 6, TRUE, FALSE)
  )
NHANES_modified

head (NHANES_updated$UrineVolAverage)
summary(NHANES_updated$UrineVolAverage)


# Selecting variables -----------------------------------------------------

NHANES %>%
  select(Gender, BMI)

NHANES %>%
  select(-BMI)

NHANES %>%
  select(starts_with("Smoke"),
         contains("Vol"),
         matches("[123]"))  #find every variable with 1 or 2 or 3 --> regular expressions#

NHANES %>%
  rename(
    #NewName=OldName
    NumberBabies = nBabies,
    Sex = Gender
    )

NHANES %>%
  select(
    BMI, NumberBabies = nBabies,
    Gender, Height
    )


NHANES %>%
  filter(Gender == "female")

NHANES %>%
  filter(Gender != "female")

NHANES %>%
  filter(BMI == 25)

NHANES %>%
  filter(BMI >= 25 & Gender =="female")

NHANES %>%
  filter(BMI >= 25 | Gender =="female") #or#

NHANES %>%
  arrange(Age) %>%
  select(Age)

NHANES %>%
  arrange(desc(Age)) %>%
  select(Age)

NHANES %>%
  arrange(desc(Age), Gender) %>%
  select(Age, Gender)


# exercise ----------------------------------------------------------------

# To see values of categorical data
summary(NHANES)

# 1. BMI between 20 and 40 and who have diabetes
NHANES %>%
  # format: variable >= number
  filter(BMI >= 20 & BMI <= 40 & Diabetes == "Yes")

# 2. Working or renting, and not diabetes
NHANES %>%
  filter((Work == "Working" | HomeOwn == "Rent") & Diabetes == "No") %>%
  select(Age, Gender, Work, HomeOwn, Diabetes)

# 3. How old is person with most number of children.
NHANES %>%
  arrange(desc(nBabies)) %>%
  select(nBabies, Age)



# group by and summarize --------------------------------------------------

#na.rm = remove missing values#

NHANES %>%
  summarise(MaxAge = max(Age, na.rm = TRUE),

            MinBMI = min(BMI, na.rm = TRUE))


NHANES %>%
  group_by(Gender, Work) %>%
  summarise(
    MeanBMI = mean(BMI, na.rm = TRUE),
    MeanAge = mean(Age, na.rm = TRUE))


# Converting data ---------------------------------------------------------


table4b %>%
  gather(year, population, -country)

table4b %>%
  gather (year, population, `1999`, `2000`)

NHANES_simple <- NHANES %>%
  select(SurveyYr, Gender, Age, Weight, Height, BMI, BPSysAve)

NHANES_simple %>%
  gather(Measure, Value, -SurveyYr, -Gender)

NHANES_long <- NHANES_simple %>%
  gather(Measure, Value, -SurveyYr, -Gender)

NHANES_long %>%
  group_by (SurveyYr, Gender, Measure) %>%
  summarise(
    MeanValue = mean(Value, na.rm = TRUE)
  )

table2 %>%
  spread(type, count)

NHANES_summary <- NHANES_long %>%
  group_by(SurveyYr, Gender, Measure) %>%
  summarise(
    MeanValue = mean(Value, na.rm = TRUE)
  )

NHANES_summary %>%
  spread(SurveyYr, MeanValue)








































