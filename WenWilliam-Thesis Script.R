library(tidyverse)
library(eiCompare)
library(readxl)


#Data files to use in order of tables presented
df1<- read_excel("2016 REPCO GEN.xlsx")
df2<- read_excel("2016 DEMCO GEN.xlsx")
df3<- read_excel("2018 DEMCO GEN.xlsx")
df4 <- read_excel("12 Trial.xlsx")
df5 <- read_excel("23 Trial.xlsx")
df6 <- read_excel("71 Trial.xlsx")
df7 <- read_excel("76 Trial.xlsx")
df8<- read_excel("2016 REPCO NOCOETH.xlsx")
df9<- read_excel("2016 REPCO YESCOETH.xlsx")
df10<- read_excel("2016 DEMCO NOCOETH.xlsx")
df11<- read_excel("2016 DEMCO YESCOETH.xlsx")
df12<- read_excel("2018 DEMCO NOCOETH.xlsx")
df13<- read_excel("2018 DEMCO YESCOETH.xlsx")
df14<- read_excel("VoteMail1.xlsx")
df15<- read_excel("VoteMail2.xlsx")
df16<- read_excel("12 Success Case.xlsx")
df17<- read_excel("14 Success Case.xlsx")
df18<- read_excel("76 Success Case.xlsx")
df19<- read_excel("27 Failure Case.xlsx")
df20<- read_excel("47 Failure Case.xlsx")

cands<- c("VOTERATE", "ROLLOFF")
form <- formula(cbind(VOTERATE, ROLLOFF) ~ cbind(DEM, REP, NPP, OTH))
table_names <- c("RxC: DEM", "RxC: REP", "RxC: NPP", "RxC: OTHER")
form2 <- formula(cbind(VOTERATE, ROLLOFF) ~ cbind(ASI,LAT,OTHR))
table_names2 <- c("RxC: ASI", "RxC: LAT", "RxC: OTHR")

#RxC for overall contests
ei_bayes <- ei.reg.bayes(form, data=df1, sample=10000, truncate=T)
ei_bayes_res<- bayes_table_make(ei_bayes, cand_vector=cands, table_names=table_names)
ei_bayes_res #Republican-Republican 2016

ei_bayes <- ei.reg.bayes(form, data=df2, sample=10000, truncate=T)
ei_bayes_res<- bayes_table_make(ei_bayes, cand_vector=cands, table_names=table_names)
ei_bayes_res #Democrat-Democrat 2016

ei_bayes <- ei.reg.bayes(form, data=df3, sample=10000, truncate=T)
ei_bayes_res<- bayes_table_make(ei_bayes, cand_vector=cands, table_names=table_names)
ei_bayes_res #Democrat-Democrat 2018

#RxC for 2016 Republican contests
ei_bayes <- ei.reg.bayes(form, data=df4, sample=10000, truncate=T)
ei_bayes_res<- bayes_table_make(ei_bayes, cand_vector=cands, table_names=table_names)
ei_bayes_res #AD12

ei_bayes <- ei.reg.bayes(form, data=df5, sample=10000, truncate=T)
ei_bayes_res<- bayes_table_make(ei_bayes, cand_vector=cands, table_names=table_names)
ei_bayes_res #AD23

ei_bayes <- ei.reg.bayes(form, data=df6, sample=10000, truncate=T)
ei_bayes_res<- bayes_table_make(ei_bayes, cand_vector=cands, table_names=table_names)
ei_bayes_res #AD71

ei_bayes <- ei.reg.bayes(form, data=df7, sample=10000, truncate=T)
ei_bayes_res<- bayes_table_make(ei_bayes, cand_vector=cands, table_names=table_names)
ei_bayes_res #AD76

#RxC for type of contest and separated by presence of nonwhite candidate
ei_bayes <- ei.reg.bayes(form2, data=df8, sample=10000, truncate=T)
ei_bayes_res<- bayes_table_make(ei_bayes, cand_vector=cands, table_names=table_names2)
ei_bayes_res #Republican-Republican 2016 No Minority Candidate

ei_bayes <- ei.reg.bayes(form2, data=df9, sample=10000, truncate=T)
ei_bayes_res<- bayes_table_make(ei_bayes, cand_vector=cands, table_names=table_names2)
ei_bayes_res #Republican-Republican 2016 at least one Minority Candidate

ei_bayes <- ei.reg.bayes(form2, data=df10, sample=10000, truncate=T)
ei_bayes_res<- bayes_table_make(ei_bayes, cand_vector=cands, table_names=table_names2)
ei_bayes_res #Democrat-Democrat 2016 No Minority Candidate

ei_bayes <- ei.reg.bayes(form2, data=df11, sample=10000, truncate=T)
ei_bayes_res<- bayes_table_make(ei_bayes, cand_vector=cands, table_names=table_names2)
ei_bayes_res #Democrat-Democrat 2016 at least one Minority Candidate

ei_bayes <- ei.reg.bayes(form2, data=df12, sample=10000, truncate=T)
ei_bayes_res<- bayes_table_make(ei_bayes, cand_vector=cands, table_names=table_names2)
ei_bayes_res #Democrat-Democrat 2018 No Minority Candidate

ei_bayes <- ei.reg.bayes(form2, data=df13, sample=10000, truncate=T)
ei_bayes_res<- bayes_table_make(ei_bayes, cand_vector=cands, table_names=table_names2)
ei_bayes_res #Democrat-Democrat 2018 at least one Minority Candidate

#RxC for all-mail county may take several minutes
ei_bayes <- ei.reg.bayes(form, data=df14, sample=10000, truncate=T)
ei_bayes_res<- bayes_table_make(ei_bayes, cand_vector=cands, table_names=table_names)
ei_bayes_res #2018 Sacramento County vote-by-mail only


ei_bayes <- ei.reg.bayes(form, data=df15, sample=10000, truncate=T)
ei_bayes_res<- bayes_table_make(ei_bayes, cand_vector=cands, table_names=table_names)
ei_bayes_res #2018 San Joaquin County not vote-by-mail only

#RxC for success-failure districts
ei_bayes <- ei.reg.bayes(form, data=df16, sample=10000, truncate=T)
ei_bayes_res<- bayes_table_make(ei_bayes, cand_vector=cands, table_names=table_names)
ei_bayes_res #AD12 2016 Republican-Republican Success

ei_bayes <- ei.reg.bayes(form, data=df17, sample=10000, truncate=T)
ei_bayes_res<- bayes_table_make(ei_bayes, cand_vector=cands, table_names=table_names)
ei_bayes_res #AD14 2016 Democrat-Democrat Success

ei_bayes <- ei.reg.bayes(form, data=df18, sample=10000, truncate=T)
ei_bayes_res<- bayes_table_make(ei_bayes, cand_vector=cands, table_names=table_names)
ei_bayes_res #AD76 2018 Democrat-Democrat Success

ei_bayes <- ei.reg.bayes(form, data=df19, sample=10000, truncate=T)
ei_bayes_res<- bayes_table_make(ei_bayes, cand_vector=cands, table_names=table_names)
ei_bayes_res #AD27 2016 Democrat-Democrat Failure

ei_bayes <- ei.reg.bayes(form, data=df20, sample=10000, truncate=T)
ei_bayes_res<- bayes_table_make(ei_bayes, cand_vector=cands, table_names=table_names)
ei_bayes_res #AD47 2016 Democrat-Democrat Failure


