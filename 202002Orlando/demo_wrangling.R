demo <- read.csv("C:/Users/keiko/Desktop/datathon2002/data1.csv")

# age #
demo$age_Ranges <- cut(demo$age, c(0, 25, 35, 45, 55, 65, 75, 85, 100))
table(demo$age_Ranges,useNA = "ifany")
demo%>%filter(is.na(age_Ranges))%>%select(age_Ranges, age)

#summary(mdga$hospital_los, useNA = "ifany")
#mdga$hospitalLOS_Ranges <- cut(mdga$hospital_los, c(0, 1, 3, 5, 10, 20, 30, 60, 90, 150, 999))
#table(mdga$hospitalLOS_Ranges,useNA = "ifany")
#summary(mdga$icu_los, useNA = "ifany")
#mdga$icuLOS_Ranges <- cut(mdga$icu_los, c(0, 1, 3, 5, 10, 20, 30, 60, 999))
#table(mdga$icuLOS_Ranges,useNA = "ifany")
summary(mdga$ethnicity, useNA = "ifany")

# ethnicity #
mdga <- mdga %>% mutate(ethnicity2=recode_factor(ethnicity,
                                                 `Caucasian` = "Caucasian",
                                                 `African American` = "African American",
                                                 `Hispanic`= "Hispanic",
                                                 `Asian` = "Asian",
                                                 `Native American` = "Native American",
                                                 `Other/Unknown` = "Other/Unknown",
                                                 .default = "Other/Unknown"))
summary(mdga$ethnicity2, useNA = "ifany")

#mdga<- mdga%>%mutate(hospital_mortality=as.factor(hospital_mortality), hospital_mortality_ultimate=as.factor(hospital_mortality_ultimate), icu_mortality=as.factor(icu_mortality))
#mdga <- mdga%>%mutate(hospital_region=as.factor(hospital_region))
#summary(mdga$hospital_region)
#mdga <- mdga %>% mutate(hospital_region2=recode_factor(hospital_region, 'Midwest' = "Midwest", 'Northeast' = "Northeast", 'South' = "South", 'West' = "West", .default = "Unknown"))
#summary(mdga$hospital_region2)


#mdga <- mdga%>%mutate(dialysis=as.factor(dialysis),aids=as.factor(aids),hepaticfailure=as.factor(hepaticfailure|cirrhosis),diabetes=as.factor(diabetes),immunosuppression=as.factor(immunosuppression),leukemia=as.factor(leukemia),lymphoma=as.factor(lymphoma),metastaticcancer=as.factor(metastaticcancer),thrombolytics=as.factor(thrombolytics),
# mdga <- mdga%>%mutate(hospitaldischargeyear=as.character(hospitaldischargeyear))
#summary(mdga$hospitaldischargeyear)
#mdga <- mdga%>%mutate(hospital_mortality=as.factor(hospital_mortality), hospital_mortality_ultimate=as.factor(hospital_mortality_ultimate), icu_mortality=as.factor(icu_mortality))


#Parsing of APACHE Admit Diagnoses
Parsing of APACHE admission diagnoses into organ system grouper with sepsis diagnoses in seperate category. 25 diagnoses were categorized as Undefined with 16 transplant diagnoses and 9 of various other non-specific nature.


# apache diagnosis
library(stringr)
parse_dx <- function(x) {
  sp <- str_split(as.character(x),"\\|")
  idx <- sapply(sp,length)
  out <- sapply(1:length(idx),function(v) { return(sp[[v]][idx[v]])})
  return(out)
}


ap <- ap %>% mutate(new_apdx =parse_dx(admitdxpath)) %>% group_by(new_apdx) %>% mutate(n=row_number()) %>% ungroup()

nrow(ap)
colnames(ap)
ssd_j <- ssd %>% left_join(ap%>%filter(n==1)%>%select(-n),by=c("apacheadmissiondx"="new_apdx"))
nrow(ssd_j)
if(nrow(ssd_j)==nrow(ssd)) {ssd <- ssd_j;rm(ssd_j)}
nrow(ssd)
summary(ssd$group, useNA = "ifany")
ssd<-ssd%>%mutate(group=droplevels(group))
summary(ssd$group, useNA = "ifany")


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
