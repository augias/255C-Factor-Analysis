# Data Cleanup file for Final factor analysis project
# Author: Fernando Mora
# Course: ED255C Introduction to factor analysis and Item Response theory
# Professor Mark Hansen


# Paths and Libraries ####
setwd("~/Documents/UCLA/Spring 2021/ED255C - Intro to Factor Analysis and IRT/TIMSS final proj")
library(tidyverse)
library(EdSurvey)
library(lavaan)
library(semTools)
library(semPlot)
library(ggcorrplot)
library(psych)
library(GPArotation)
options(scipen=999)
# rm(list=ls())

# downloadTIMSS(year=2019, root="~/Documents/UCLA/Spring 2021/ED255C - Intro to Factor Analysis and IRT/TIMSS final proj/data/")

countries = c('alb', 'arm', 'aus','aut','aze','bhr','bfl','bih','bgr','can','chl','twn','hrv','cyp','cze','dnk','egy','eng','fin','fra','geo','geo','deu',                                'hkg','hun','irn','irl','isr','ita','jpn','jor','kaz','kor','xkx','kwt','lva','lbn','ltu','mys','mlt','mne','mar','nld','nzl','mkd','nir','nor','omn','pak','phl','pol','prt','qat','rom','rus','sau','srb','sgp','svk','zaf','esp','swe','tur','are','usa') #No benchmarking cities


data <- readTIMSS("~/Documents/UCLA/Spring 2021/ED255C - Intro to Factor Analysis and IRT/TIMSS final proj/data/TIMSS/2019/", countries = countries, gradeLvl = "8", verbose = TRUE)
save(data,file="Rawdata.RData")
load(file = "Rawdata.RData")

# What are the context questionnaire questions I wanna look at?
timss.admin <- getData(data=data, 
                 varnames = c(
                   'idcntry',	#country id - numeric iso code
                   'idschool', #school id
                   # "school emphasis on academic success" - 5 points ####
                   # how would you characterize each of the following
                   # within your school?
                   # Admin questionnaire
                   "bcbg14a",
                   "bcbg14b",
                   "bcbg14c",
                   "bcbg14d",
                   "bcbg14e",
                   "bcbg14f",
                   "bcbg14g",
                   "bcbg14h",
                   "bcbg14i",
                   "bcbg14j",
                   "bcbg14k"
                   ), drop = TRUE, addAttributes = FALSE
                 )

timss.math <- getData(data=data, 
                       varnames = c(
                         'idcntry',	#country id - numeric iso code
                         'idschool',
                         'idteach',#school id
                         # "school emphasis on academic success" - 5 points ####
                         # how would you characterize each of the following
                         # within your school?
                         #And math teachers
                         'btbg06a.math',	#gen\characterize\tchs understanding
                         'btbg06b.math',	#gen\characterize\tchs degree of success
                         'btbg06c.math',	#gen\characterize\tchs expectations
                         'btbg06d.math',	#gen\characterize\tchs ability to inspire
                         'btbg06e.math',	#gen\characterize\parental involvement
                         'btbg06f.math',	#gen\characterize\parental commitment
                         'btbg06g.math',	#gen\characterize\parental expectations
                         'btbg06h.math',	#gen\characterize\parental support
                         'btbg06i.math',	#gen\characterize\students desire
                         'btbg06j.math',	#gen\characterize\ability to reach goals
                         'btbg06k.math' 	#gen\characterize\respect for classmates
                       ), drop = TRUE, addAttributes = FALSE
)

timss.sci <- getData(data=data, 
                      varnames = c(
                        'idcntry',	#country id - numeric iso code
                        'idschool',
                        'idteach',#school id
                        # "school emphasis on academic success" - 5 points ####
                        # how would you characterize each of the following
                        # within your school?
                        #And math teachers
                        'btbg06a.sci',	#gen\characterize\tchs understanding
                        'btbg06b.sci',	#gen\characterize\tchs degree of success
                        'btbg06c.sci',	#gen\characterize\tchs expectations
                        'btbg06d.sci',	#gen\characterize\tchs ability to inspire
                        'btbg06e.sci',	#gen\characterize\parental involvement
                        'btbg06f.sci',	#gen\characterize\parental commitment
                        'btbg06g.sci',	#gen\characterize\parental expectations
                        'btbg06h.sci',	#gen\characterize\parental support
                        'btbg06i.sci',	#gen\characterize\students desire
                        'btbg06j.sci',	#gen\characterize\ability to reach goals
                        'btbg06k.sci'   #gen\characterize\respect for classmates
                      ), drop = TRUE, addAttributes = FALSE
)

# The data frames for each group. Clean up first, then get to merging later?
head(timss.admin[[39]])
head(timss.math[[39]])
head(timss.sci[[39]])

# Admins Frame ####

timss.admin2<- as.data.frame(timss.admin[[1]])

for(i in 2:39){
  print(paste("Frame number ",i))
  timss.admin2<-rbind(timss.admin2, timss.admin[[i]])
} # Loop to bind all sub-lists in the weird data edsurvey data frame format

timss.admin2 <- unite(timss.admin2,"cntry_school",idcntry,idschool,sep="",remove=FALSE)
timss.admin2$group <- rep("admin",nrow(timss.admin2))

nrow(distinct(timss.admin2,cntry_school))
n_distinct(timss.admin2$cntry_school) # How many individual schools are there?
n_teacher_per_cntry <- timss.admin2 %>% group_by(idcntry) %>% summarise(n_distinct(cntry_teacher))
n_school_per_cntry <- timss.admin2 %>% group_by(idcntry) %>% summarise(n_distinct(cntry_school))

timss.admin3<-distinct(timss.admin2, cntry_school, .keep_all = TRUE) # Danger! Teacher ID codes repeat per country! before you remove duplicate rows, to keep 1 obs per teacher, we need to append the country code to prefix each id, that code is higher up.
n_teacher_per_cntry <- timss.admin3 %>% group_by(idcntry) %>% summarise(n_distinct(cntry_teacher))
n_school_per_cntry <- timss.admin3 %>% group_by(idcntry) %>% summarise(n_distinct(cntry_school))

for(i in 4:14){
  print(paste("Column ",i," NA = ", timss.admin3 %>% summarise(count = sum(is.na(timss.admin3[i])))))
  } #No NA values for any school regarding their context questionnaire responses. Yay!

admin <- timss.admin3
n_teacher_per_cntry <- admin %>% group_by(idcntry) %>% summarise(n_distinct(cntry_teacher))
n_school_per_cntry <- admin %>% group_by(idcntry) %>% summarise(n_distinct(cntry_school))

rownames(admin)<-rep(1:nrow(admin))

admin <- admin %>% relocate(c(idcntry,group), .before = cntry_school)
admin <- select(admin, - idschool)


rm(timss.admin, timss.admin2, timss.admin3) # Clean up the working frames. the code is still up there.

# Math Frame ####
# rm(timss.math2)
timss.math2<- as.data.frame(timss.math[[1]])

for(i in 2:39){
  print(paste("Frame number ",i))
  timss.math2<-rbind(timss.math2, timss.math[[i]])
} # Loop to bind all sub-lists in the weird data edsurvey data frame format

timss.math2$group <- rep("math",nrow(timss.math2))
timss.math2 <- unite(timss.math2,"cntry_teacher",idcntry,idteach,group,sep="",remove=FALSE)

nrow(distinct(timss.math2,cntry_teacher))
n_distinct(timss.math2$cntry_teacher) # How many individual schools are there?
n_school_per_cntry <- timss.math2 %>% group_by(idcntry) %>% summarise(n_distinct(cntry_school))

timss.math3<-distinct(timss.math2, cntry_teacher, .keep_all = TRUE) # Danger! Teacher ID codes repeat per country! before you remove duplicate rows, to keep 1 obs per teacher, we need to append the country code to prefix each id, that code is higher up.
n_teacher_per_cntry <- timss.math3 %>% group_by(idcntry) %>% summarise(n_distinct(cntry_teacher))

for(i in 4:14){
  print(paste("Column ",i," NA = ", timss.math3 %>% summarise(count = sum(is.na(timss.math3[i])))))
} #No NA values for any teacher regarding their context questionnaire responses. Yay!

math <- timss.math3

rownames(math)<-rep(1:nrow(math))

math <- math %>% relocate(group, .before = cntry_teacher)
math <- select(math, - c(idschool, idteach))

rm(timss.math, timss.math, timss.math3) # Clean up the working frames. the code is still up there.


# Science Frame ####
# rm(timss.sci2)
timss.sci2<- as.data.frame(timss.sci[[1]])

for(i in 2:39){
  print(paste("Frame number ",i))
  timss.sci2<-rbind(timss.sci2, timss.sci[[i]])
} # Loop to bind all sub-lists in the weird data edsurvey data frame format

timss.sci2$group <- rep("sci",nrow(timss.sci2))
timss.sci2 <- unite(timss.sci2,"cntry_teacher",idcntry,idteach,group,sep="",remove=FALSE)

nrow(distinct(timss.sci2,cntry_teacher))
n_distinct(timss.sci2$cntry_teacher) # How many individual teachers are there?
n_teacher_per_cntry <- timss.sci2 %>% group_by(idcntry) %>% summarise(n_distinct(cntry_teacher))

timss.sci3<-distinct(timss.sci2, cntry_teacher, .keep_all = TRUE) # Danger! Teacher ID codes repeat per country! before you remove duplicate rows, to keep 1 obs per teacher, we need to append the country code to prefix each id, that code is higher up.
n_teacher_per_cntry <- timss.sci3 %>% group_by(idcntry) %>% summarise(n_distinct(cntry_teacher))

for(i in 5:15){
  print(paste("Column ",i," NA = ", timss.sci3 %>% summarise(count = sum(is.na(timss.sci3[i])))))
} #No NA values for any teacher regarding their context questionnaire responses. Yay!

science <- timss.sci3

rownames(science)<-rep(1:nrow(science))

science <- science %>% relocate(c(idcntry, group), .before = cntry_teacher)
science <- select(science, - c(idschool, idteach))

rm(timss.sci, timss.sci2, timss.sci3) # Clean up the working frames. the code is still up there.

# Make all colnames the same so we can bind rows later
colnames(admin) <- colnames(math) <- colnames(science) <- c('country', 'group', 'id', 'bg06a', 'bg06b', 'bg06c', 'bg06d', 'bg06e', 'bg06f', 'bg06g', 'bg06h', 'bg06i', 'bg06j', 'bg06k')

# Save final data frames admin, math, and science ####
save(data, admin, math, science, file="Cleandata.RData")
load(file = "Cleandata.RData")

us.admin <- admin %>% filter(country==840)
us.math <- math %>% filter(country==840)
us.science <- science %>% filter(country==840)

us.df <-rbind(us.admin, us.math, us.science) # Bind all us observations grouped by admins, math, and science respondents.

df <- rbind(admin, math, science) # The whole shebang, all countries, grouped by admin, math, and science respondents. 

# Descriptives ####
n_per_cntry <- df %>% group_by(country) %>% summarise(n_distinct(id))
n_per_group <- df %>% group_by(group) %>% summarise(n_distinct(id))
# Setup and testing for EFA on test items ####
# First de-factor the variables and create a numeric frame for items
rm(df.numeric)
df.numeric <- df[,c(1:3)]

for(i in 4:14){
  df.numeric <- cbind(df.numeric, as.numeric(df[,i]))
}

for(i in 1:14){
  print(paste("Column ",i," NA = ", df.numeric %>% summarise(count = sum(is.na(df.numeric[,i])))))
} 

colnames(df.numeric) <- colnames(df)

save(data, df, df.numeric, us.df, admin, math, science, file="Cleandata.RData")

# Start here if sharing RData and analyses with anyone ####

load(file = "Cleandata.RData")


# Means and SDs by country do vary a lot. Definitely messy and perhaps better to pick a few or maybe just  two countries...
means <- df.numeric %>% group_by(country) %>% 
  summarise_at(vars(starts_with("bg")), funs(
    mean = mean(.,na.rm=TRUE),
    sd = sd(.,na.rm=TRUE),
    median=median(.,na.rm=TRUE),
    IQR=IQR(.,na.rm=TRUE)))


# Definitions: 
# data: data frame where factors have been removed and items are numeric
# df: Items are formatted as factors
# df2: Items are formatted as ordered factors 

df$bg06a #Are factors ordered for polychoric correlation? Nope
# For getting ordered levels...
df[,4:14]<-apply(df[,4:14],2,as.ordered)
df[,4:14]<-lapply(df[,4:14],as.ordered)

us.df[,4:14]<-apply(us.df[,4:14],2,as.ordered)
us.df[,4:14]<-lapply(us.df[,4:14],as.ordered)
us.df$bg06a #ok? ok

rus.df <- filter(df, country==643)
rus.df[,4:14]<-apply(rus.df[,4:14],2,as.ordered)
rus.df[,4:14]<-lapply(rus.df[,4:14],as.ordered)
rus.df$bg06a #ok? ok

all.admin <- filter(df, group == 'admin')
all.math <- filter(df, group == 'math')
all.science <- filter(df, group == 'sci')

# Pearson Correlations and covariances ####
cov <- round(cov(df.numeric[,4:14]),3)
corr <- round(cor(df.numeric[,4:14]),3)
p.mat<-cor_pmat(df.numeric[,4:14])
corr.plot<-ggcorrplot(corr, hc.order = FALSE, type = "upper")


# Polychoric Correlations ####
polycorr <- round(lavCor(df[,4:14], ordered=TRUE),3)
polycorr.plot <-ggcorrplot(polycorr, hc.order=FALSE, type="upper") #OK! The correlation matrix does look different than the pearson.

us.polycorr <- round(lavCor(us.df[,4:14], ordered = TRUE),3)
rus.polycorr <- round(lavCor(rus.df[,4:14], ordered = TRUE),3)
admin.polycorr <- round(lavCor(all.admin[,4:14], ordered = TRUE),3)
math.polycorr <- round(lavCor(all.math[,4:14], ordered = TRUE),3)
sci.polycorr <- round(lavCor(all.science[,4:14], ordered = TRUE),3)


correlations <- cbind(corr, NA, polycorr, NA, us.polycorr, NA, rus.polycorr, NA, admin.polycorr, NA, math.polycorr, NA, sci.polycorr)
write.csv(correlations,"correlation matrices.csv",na="")
ggsave(file = "corrplot.svg", plot=corr.plot, width=10, height=8)
ggsave(file = "polycorrplot.svg", plot=polycorr.plot, width=10, height=8)

save(data, df, df.numeric, file="Cleandata.RData")
