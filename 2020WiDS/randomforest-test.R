
train <- read.csv("C:/Users/keiko/Desktop/WiDS/training_v2.csv")
install.packages("randomForest")
library(randomForest)

head(train)
dim(train)

summary(train)
sapply(train, class)

train <- transform(
  train,
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
  solid_tumor_with_metastasis=as.factor(solid_tumor_with_metastasis)
  )
sapply(train,class)
colSums(is.na(train))



rf <- randomForest(
  num ~ .,
  data=train
)
