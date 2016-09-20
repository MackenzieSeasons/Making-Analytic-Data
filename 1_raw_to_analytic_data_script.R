library(tidyverse)

raw_data <- read_csv(file="rawData.csv")

str(raw_data)

View(raw_data)

raw_data <- read_csv(file="rawData.csv",na=c("","NA","-999","-888"))

categorical_variables <- select(raw_data, group, gender)

categorical_variables$group <- as.factor(categorical_variables$group)

categorical_variables$gender <- as.factor(categorical_variables$gender)
levels(categorical_variables$gender) <- list("Male"=1,"Female"=2)
#line 11 to line 16 make categorical variable, handling categorical variables to factors

affective_commitment_items <- select (raw_data, AC1, AC2, AC3, AC4, AC5)
agreeableness_items <- select (raw_data, A1, A2, A3, A4, A5)
extroversion_items <- select (raw_data, E1, E2, E3, E4, E5)

psych::describe(extroversion_items)
#checking min and max for extraversion
psych::describe(agreeableness_items)

agreeableness_items
## # A tibble: 10 x 5

is_bad_value <- agreeableness_items<1 | agreeableness_items>5

agreeableness_items[is_bad_value] <- NA

psych::describe(affective_commitment_items)
#just redoing the above process to check for out of range values for the affective committment scale

is_bad_value <- affective_commitment_items<1 | affective_commitment_items>7
affective_commitment_items[is_bad_value] <- NA
#done after this, we replaced all the items above 7 and below 1 with NA

agreeableness_items <- mutate(agreeableness_items,A5=6-A5)
#we reverse scaled A5 agreeableness item on a 1-5 point scale

#going to recode the AC items AC4 and AC5 below

affective_commitment_items <- mutate(affective_commitment_items,AC4=8-AC4)
affective_commitment_items <- mutate(affective_commitment_items,AC5=8-AC5)
#the above 2 lines recoded the 2 reverse items on the affective committment scale

#now we're obtaining a single scale score for each participant, i.e. a mean for each person brloe

agreeableness <- psych::alpha(as.data.frame(agreeableness_items),check.keys=FALSE)$scores
extroversion <- psych::alpha(as.data.frame(extroversion_items),check.keys=FALSE)$scores
affective_commitment <- psych::alpha(as.data.frame(affective_commitment_items),check.keys=FALSE)$scores

#now combining everything into analytic data

analytic_data <- cbind(categorical_variables,agreeableness,extroversion,affective_commitment)
