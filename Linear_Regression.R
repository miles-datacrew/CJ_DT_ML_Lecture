insurance <- read.csv('D:/insurance.csv', stringsAsFactors = TRUE)

str(insurance)

summary(insurance$charges)

hist(insurance$charges)

table(insurance$region)

cor(insurance[c("age", "bmi", "children", "charges")])

pairs(insurance[c("age", "bmi", "children", "charges")])

ins_model <- lm(charges ~ age + children + bmi + sex + smoker + region, data = insurance)

ins_model <- lm(charges ~ ., data = insurance)

ins_model

summary(ins_model)

insurance$age2 <- insurance$age^2

ins_model <- lm(charges ~ age + age2 + children + bmi + sex + smoker + region, data = insurance)

summary(ins_model)

insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

ins_model <- lm(charges ~ age + age2 + children + bmi + bmi30 + sex + smoker + region, data = insurance)

summary(ins_model)

ins_model <- lm(charges ~ age + age2 + children + bmi + sex + bmi30*smoker + region, data = insurance)
ins_model <- lm(charges ~ age + age2 + children + bmi + sex + bmi30 + smokeryes + bmi30:smokeryes + region, data = insurance)


bmi30*smoker = bmi30 + smokeryes + bmi30:smokeryes

summary(ins_model)







data <- read.csv('D:/edata.csv')
data$ideology_clean <- replace(data$ideology,which(data$ideology == 99), NA)
data$ideology_clean
data$party_clean <- replace(data$party,which(data$party == 9), NA)

data$party_clean[data$party_clean ==6] <-3
#ex) table( cut( rnorm(100), breaks=c(-Inf, -3:3, Inf) ) )

data$age_cat <- cut(data$age,breaks=c(-Inf, 10*(3:6), Inf) )
data$age_cat <- cut(data$age,breaks=c(-Inf, 10*(3:6), Inf), labels = 
                      c("20대이하", "30대","40대","50대","60대이상")
                    ,right = FALSE)
data$age_cat

result <- chisq.test(data$voting,data$age_cat)
observation <- result$observed
expectation <- result$expected

rowSums(observation)
colSums(observation)
sum(observation)
result$statistic
result$p.value

lm_ideology <- lm(formula = ideology_clean ~ age, data = data)
summary(lm_ideology)
