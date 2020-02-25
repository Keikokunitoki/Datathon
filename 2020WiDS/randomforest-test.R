train <- read.csv("C:/Users/keiko/Desktop/WiDS/training_v2.csv")
install.packages("randomForest")
library(randomForest)
library(dplyr)

head(train)
dim(train)

summary(train)
sapply(train, class)


sapply(train,class)
colSums(is.na(train))


train$pre_icu_los_days <- abs(train$pre_icu_los_days)

traind <-  select(train, - contains("h1_"))
traind <- select(traind, - contains("_apache"))
head(traind)
summary(traind)

traind$aids[is.na(traind$aids)] <- 0
traind$cirrhosis[is.na(traind$cirrhosis)] <- 0
traind$diabetes_mellitus[is.na(traind$diabetes_mellitus)] <- 0
traind$hepatic_failure[is.na(traind$hepatic_failure)] <- 0
traind$immunosuppression[is.na(traind$immunosuppression)] <- 0
traind$leukemia[is.na(traind$leukemia)] <- 0
traind$lymphoma[is.na(traind$lymphoma)] <- 0
traind$solid_tumor_with_metastasis[is.na(traind$solid_tumor_with_metastasis)] <- 0


traind$ethnicity[is.na(traind$ethnicity)] <- "Caucasian"
traind$apache_2_diagnosis[is.na(traind$apache_2_diagnosis)] <- 113
traind$apache_3j_diagnosis[is.na(traind$apache_3j_diagnosis)] <- 501.05

#test <- traind %>% filter(apache_4a_icu_death_prob>0)
#summary(test$apache_4a_icu_death_prob)

traind$apache_4a_hospital_death_prob[is.na(traind$apache_4a_hospital_death_prob)] <- 0.05
traind$apache_4a_icu_death_prob[is.na(traind$apache_4a_icu_death_prob)] <- 0.02

traind$apache_4a_hospital_death_prob <- lapply(traind$apache_4a_hospital_death_prob, gsub, pattern="-1", replacement = "0.05")
traind$apache_4a_icu_death_prob <- lapply(traind$apache_4a_icu_death_prob, gsub, pattern="-1", replacement = "0.02")

hist(traind$apache_4a_hospital_death_prob)

traind <- transform(
  traind,
  hospital_id=as.factor(hospital_id),
  hospital_death=as.factor(hospital_death),
  elective_surgery=as.factor(elective_surgery),
  icu_id=as.factor(icu_id),
  age=as.integer(age),
  gender=as.factor(gender),
  aids=as.factor(aids),
  cirrhosis=as.factor(cirrhosis),
  diabetes_mellitus=as.factor(diabetes_mellitus),
  hepatic_failure=as.factor(hepatic_failure),
  immunosuppression=as.factor(immunosuppression),
  leukemia=as.factor(leukemia),
  lymphoma=as.factor(lymphoma),
  apache_2_diagnosis=as.factor(apache_2_diagnosis),
  apache_3j_diagnosis=as.factor(apache_3j_diagnosis),
  apache_post_operative=as.factor(apache_post_operative),
  solid_tumor_with_metastasis=as.factor(solid_tumor_with_metastasis),
  apache_4a_icu_death_prob=as.numeric(apache_4a_icu_death_prob),
  apache_4a_hospital_death_prob=as.numeric(apache_4a_hospital_death_prob)
)

write.csv(traind,"C:/Users/keiko/Desktop/WiDS/traind.csv" )

### MICE ####
library(mice)
library(VIM)
library(lattice)
library(ggplot2)

md.pattern(train)

imp1 <- mice(traind , method = "cart")

##### Random Forest ###
rf <- randomForest(
  num ~ .,
  data=train
)

