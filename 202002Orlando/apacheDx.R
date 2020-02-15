delir <- read.csv("C:/Users/keiko/Desktop/datathon2002/delir.csv")
summary(delir$delirium)
summary(delir$delirium)
library(dplyr)
demo <- read.csv("C:/Users/keiko/Desktop/datathon2002/data1.csv")
data1 <- delir %>% left_join(demo, by = "patientunitstayid")
head(data1)
data1 <- data1 %>% filter(delirium != "NA")

myVars <- c("gender",                "age",                       "ethnicity"   ,             
 "hospitaladmitoffset" ,      "hospitaladmitsource"  ,  "hospitaldischargeoffset" , 
     "hospitaldischargelocation" ,"hospitaldischargestatus"  , "unittype"  ,  "unitadmitsource"   ,       
          "unitstaytype"  ,    "unitdischargeoffset" ,      "unitdischargelocation"  ,   "unitdischargestatus"     )               

tab <- CreateTableOne(vars = myVars, strata = "delirium" , data = data1)
tab

write.csv(tab, "C:/Users/keiko/Desktop/datathon2002/tableone.csv")
colnames(data)


data1$group_id <- data %>% group_indices(patientunitstayid) 
data1 <- data1 %>% select(-patientunitstayid)
head(data1)

data2 <- data1 %>% distinct(patientunitstayid, .keep_all = TRUE)
tab <- CreateTableOne(vars = myVars, strata = "delirium" , data = data2)
tab

tabMat <- print(tab,  exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tabMat, "C:/Users/keiko/Desktop/datathon2002/tableone.csv")

adDx <- CreateTableOne(apacheadmissiondx, strata = "delirium" , data = data2)

Dx<- c("apacheadmissiondx")
table <- CreateTableOne(vars = Dx, strata = "delirium" , data = data2)
table
tabMatD <- print(table,  exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tabMatD, "C:/Users/keiko/Desktop/datathon2002/adDx.csv")

######
#Parsing of APACHE Admit Diagnoses
#Parsing of APACHE admission diagnoses into organ system grouper with sepsis diagnoses in seperate category. 25 diagnoses were categorized as Undefined with 16 transplant diagnoses and 9 of various other non-specific nature.

data2 <- data2 %>% mutate(dialysis=as.factor(dialysis),aids=as.factor(aids),hepaticfailure=as.factor(hepaticfailure|cirrhosis),diabetes=as.factor(diabetes),immunosuppression=as.factor(immunosuppression),leukemia=as.factor(leukemia),lymphoma=as.factor(lymphoma),metastaticcancer=as.factor(metastaticcancer),thrombolytics=as.factor(thrombolytics))

ap <- read.csv("C:/Users/keiko/Desktop/datathon2002/apaDx.csv")

data2d0<- data2 %>% filter(delirium=="0")
data2d1<- data2 %>% filter(delirium=="1")

# apache diagnosis
parse_dx <- function(x) {
  sp <- str_split(as.character(x),"\\|")
  idx <- sapply(sp,length)
  out <- sapply(1:length(idx),function(v) { return(sp[[v]][idx[v]])})
  return(out)
}
ap <- ap %>% mutate(new_apdx =parse_dx(admitdxpath)) %>% group_by(new_apdx) %>% mutate(n=row_number()) %>% ungroup()
nrow(ap)
colnames(ap)
data2d0_j <- data2d0 %>% left_join(ap%>%filter(n==1)%>%select(-n),by=c("apacheadmissiondx"="new_apdx"))
nrow(data2d0_j)
if(nrow(data2d0_j)==nrow(data2d0)) {data2d0 <- data2d0_j;rm(data2d0_j)}
nrow(data2d0)
summary(data2d0$group, useNA = "ifany")
data2d0<-data2d0%>%mutate(group=droplevels(group))
summary(data2d0$group, useNA = "ifany")


#####
parse_dx <- function(x) {
  sp <- str_split(as.character(x),"\\|")
  idx <- sapply(sp,length)
  out <- sapply(1:length(idx),function(v) { return(sp[[v]][idx[v]])})
  return(out)
}
ap <- ap %>% mutate(new_apdx =parse_dx(admitdxpath)) %>% group_by(new_apdx) %>% mutate(n=row_number()) %>% ungroup()
nrow(ap)
colnames(ap)
data2d1_j <- data2d1 %>% left_join(ap%>%filter(n==1)%>%select(-n),by=c("apacheadmissiondx"="new_apdx"))
nrow(data2d1_j)
if(nrow(data2d1_j)==nrow(data2d1)) {data2d1 <- data2d1_j;rm(data2d1_j)}
nrow(data2d1)
summary(data2d1$group, useNA = "ifany")
data2d1<-data2d1%>%mutate(group=droplevels(group))
summary(data2d1$group, useNA = "ifany")

######


table(ssd$icu_admit_source, useNA = "ifany")
ssd <- ssd %>% mutate(icu_admit_source2=recode_factor(icu_admit_source,
                                                      `Acute Care/Floor`= "Floor",
                                                      `Chest Pain Center` = "OR/Proc Area",
                                                      `Direct Admit` = "Direct Admit",
                                                      `Emergency Department` = "Emergency Department",
                                                      `Floor` = "Floor",
                                                      `ICU` = "Other",
                                                      `ICU to SDU` = "Step-Down Unit",
                                                      `Observation` = "Other",
                                                      `Operating Room` = "OR/Proc Area",
                                                      `Other` = "Other",
                                                      `Other Hospital` = "Direct Admit",
                                                      `Other ICU` = "Other",
                                                      `PACU` = "OR/Proc Area",
                                                      `Recovery Room` = "OR/Proc Area",
                                                      `Step-Down Unit (SDU)`= "Step-Down Unit",
                                                      .default = "Other"))
table(ssd$icu_admit_source2,useNA = "ifany")

summary(data$apacheadmissiondx)


#####

parse_dx <- function(x) {
  sp <- str_split(as.character(x),"\\|")
  idx <- sapply(sp,length)
  out <- sapply(1:length(idx),function(v) { return(sp[[v]][idx[v]])})
  return(out)
}

ap <- read.csv("C:/Users/keiko/Desktop/datathon2002/delir.csv")

ap <- ap %>% mutate(new_apdx =parse_dx(admitdxpath)) %>% group_by(new_apdx) %>% mutate(n=row_number()) %>% ungroup()
nrow(ap)
colnames(ap)
data2_j <- data2 %>% left_join(ap%>%filter(n==1)%>%select(-n),by=c("apacheadmissiondx"="new_apdx"))
nrow(data2_j)
if(nrow(data2_j)==nrow(data2)) {data2 <- data2_j;rm(data2_j)}
nrow(data2)
summary(data2$group, useNA = "ifany")
data2<-data2%>%mutate(group=droplevels(group))
summary(data2$group, useNA = "ifany")
