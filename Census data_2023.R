library(tidyverse)
library(ggplot2)
library (ggthemes)
library(dplyr)
# Lets view the data, we are about to work with

#View(data_1)

# We have to tidy the data (Converting 0 to female and 1 to male), 
#also renaming the column"female" to ""Gender"

New_data <- data_1 |>
  mutate(Female = ifelse(Female == 0, "female", "male")) |>
  rename(Gender = Female,Highest_Ed = `Highest Ed`)


#View(New_data)

Tidy_data <- na.omit(New_data)

Mar_ed <- Tidy_data |>
  filter(Mar_Stat %in% c("Never married", "married"))

#View(Tidy_data)

#ggplot(Tidy_data, aes(x = Mar_Stat, y = Age,), Class = Highest_Ed ) +
 # geom_point(shape = 24, color = "pink", fill = "pink", size = 3)


df <- Tidy_data |>
  select(Highest_Ed, Mar_Stat, INC, Gender, Age)



df|>
  ggplot(aes(x = Age, y = INC)) +
  geom_bar(stat = "identity") 






ggplot(Mar_ed, aes(x = Age, y = INC, color = Gender)) + 
  geom_point() +
  geom_smooth(aes(linetype = Highest_Ed))


number_of_males <- sum(df$Gender == "male")

number_of_females <- sum(df$Gender == "female")

cat("Number of males:", number_of_males, "\n")


# Calculate the average incomes for males and females

mean_male_income <- df |>
  filter(Gender == "male") |>
  summarize(mean_income = mean(INC))

mean_Female_income <- df |>
  filter(Gender == "female") |>
  summarize(mean_income = mean(INC))


print(mean_male_income)
print(mean_Female_income)

if (mean_Female_income > mean_male_income){
  print("The average female make more money than the average male in this community.\n")
  }else if (avg_male_income < avg_female_income) {
    cat("The average female makes more money than the average male.\n")
}


#FINAL_VISUALISATION
  ggplot(df, aes(x = Gender, y = INC, fill = Gender)) +
    geom_boxplot() +
    labs(title = "Income Distribution Comparison", x = "Gender", y = "Income")
  
  
  ggplot(df, aes(x = Gender, y = INC, fill = Gender)) +
    geom_bar(stat = "identity") +
    labs(title = "Average Income Comparison", x = "Gender", y = "Income")
  
  
  
  ggplot(df, aes(x = Age, y = Highest_Ed, fill = Gender)) +
    geom_bar(stat = "identity") +
    labs(title = "Average Educational Comparison", x = "Educational status", y = "Age")



ggplot(aes(x = mean_female_age, mean_male_age ))





male_Education <- df |>
  filter(Gender == "male") |>
  summarize(male_Education = sum(
    df$Highest_Ed == "Bachelors Degree" | "Masters or higher"))

male_education_count <- df |>
  filter(Gender == "male") |>
  summarize(male_Education = sum(
    Highest_Ed == "Bachelors Degree" | Highest_Ed == "Masters or higher"))



print(male_education_count)



female_education_count <- df |>
  filter(Gender == "female") |>
  summarize(female_Education = sum(
    Highest_Ed == "Bachelors Degree" | Highest_Ed == "Masters or higher"))



print(female_education_count)



mean_male_age <- df |>
  filter(Gender == "male") |>
  summarize(mean_age = mean(Age))

print(mean_male_age)




mean_female_age <- df |>
  filter(Gender == "female") |>
  summarize(mean_age = mean(Age))

print(mean_female_age)



df <- df |>
  group_by(Gender) |>                  # Group by gender
  mutate(Mean_Age = mean(Age)) |>       # Calculate the mean age for each group
  ungroup() 
View(df)


ggplot(df, aes(x = Gender, y = Mean_Age, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Mean Age of Males and Females",
    x = "Gender",
    y = "Mean Age"
  )


ggplot(df, aes(x = Age, y = Highest_Ed, color = Gender)) +
  geom_point(size = 3) +
  labs(
    title = "Average Educational Comparison by Age and Gender",
    x = "Age",
    y = "Educational status"
  ) +
  theme_minimal()
