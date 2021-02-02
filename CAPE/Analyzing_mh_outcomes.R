##################################
## This script analyzes data from the CAPE collaborative project
## It examines changes in MH symptoms and 
## its moderation by age, race, disease burden and government restrictions.
##
##################################


##################################
## 1. Load packages and data
##
##################################
library("dplyr")
library("tidyr")
library("lubridate")
library("ggplot2")
library("pals")
library("lme4")
library("readxl")
cape_dir ="C:/Users/marjo/Documents/postdoc/ACE/CAPE/"
options(scipen = 999)

ARC_mh <- read.csv(paste0(cape_dir,"Outcome_data/ARC_CAPE_outcomes.csv"),header = T)
BLP_mh <- read.csv(paste0(cape_dir,"Outcome_data/BLP_Outcome_data_20102020.csv"),header = T)
CAT_mh <- read_excel(paste0(cape_dir,"Outcome_data/CAT_Outcome_data_missingvalues.xlsx"))
EFC_mh <- read.csv(paste0(cape_dir,"Outcome_data/EFC_CAPE_Outcomes_Updated_09022020.csv"),header = T)
KLG_mh <- read.csv(paste0(cape_dir,"Outcome_data/KLG_CAPE Outcomes_08.28.2020.csv"),header = T) 
LIS_mh <- read.csv(paste0(cape_dir,"Outcome_data/LIS_MHOutcomeS_CAPE.csv"),header = T)
MFS_mh <- read.csv(paste0(cape_dir,"Outcome_data/MFS_outcomes_final.csv"),header = T)
NT_mh <- read.csv(paste0(cape_dir,"Outcome_data/NT_Outcomes_W1_8_11_1_2020.csv"),header = T)
SDS_mh <- read.csv(paste0(cape_dir,"Outcome_data/SDS_CAPE_Outcomes.csv"),header = T)
TAB_mh <- read.csv(paste0(cape_dir,"Outcome_data/TAB Outcomes.csv"),header = T)
TAG_mh <- read.csv(paste0(cape_dir,"Outcome_data/TAG_CAPE_Outcome_Data.csv"),header = T)
TGR_mh <- read.csv(paste0(cape_dir,"Outcome_data/TGR_Outcomes_Data_8.14.20.csv"),header = T)
TTP_mh <- read.csv(paste0(cape_dir,"Outcome_data/TTP_Outcomes_W1_5_11_1_2020.csv"),header = T)

####################################
## 2. Reformat and merge data 
## 
##
####################################

# Selecting the questionnaires that measure depressive or anxiety symptoms by self-report 
# and focus on measures that were completed both pre- and During-pandemic.

# ARC: Promis depression ; Promis anxiety
# BLP: none ; POMS tension (3 averages over 5 days for During-pandemic)
# CAT: CBCL withdrawn/depressed; SCARED 
# EFC: none ; MASC/CASPE emot
# KLG: CDI ; SCARED
# LIS: RCADS depressed mood ; RCADS anxiety
# MFS: CESD ; RCMAS
# NT: short MFQ ; none
# SDS: none ; RCADS_GeneralizedAnxiety
# TAB: CDI/none ; SCARED
# TAG: CESDC ; short SCARED-R/GAD7
# TGR: PHQ9 ; MASC2/GAD7
# TTP: short MFQ ; none

# For anxiety, EFC, TAG and TIGER had different measures pre versus During pandemic. These are still combined below


ARC_int <- pivot_longer(ARC_mh, cols=4:length(ARC_mh), names_to = c(".value","time"),
                         names_pattern = "(.*)_(.*)") %>%
            select(Study,ID,Sex,DOC,Age,PromisDepression_POMS,PromisAnxiety_POMS) %>%
            filter_at(vars(PromisDepression_POMS, PromisAnxiety_POMS), any_vars(!is.na(.))) %>%
            rename(Depression=PromisDepression_POMS,Anxiety=PromisAnxiety_POMS)

names(BLP_mh)[1] <- "Study"
BLP_int <- BLP_mh %>% mutate(Tension_POMS_time8=rowMeans(BLP_mh[c("Tension_POMS_time6","Tension_POMS_time7","Tension_POMS_time8",
                                                                  "Tension_POMS_time9","Tension_POMS_time10")],na.rm=T),
                              Tension_POMS_time13=rowMeans(BLP_mh[c("Tension_POMS_time11","Tension_POMS_time12","Tension_POMS_time13",
                                                           "Tension_POMS_time14","Tension_POMS_time15")],na.rm=T),
                              Tension_POMS_time18=rowMeans(BLP_mh[c("Tension_POMS_time16","Tension_POMS_time17","Tension_POMS_time18",
                                                          "Tension_POMS_time19","Tension_POMS_time20")],na.rm=T)) %>%
          select(Study,ID,Sex,DOC_time1,DOC_time2,DOC_time3,DOC_time4,DOC_time5,DOC_time8,DOC_time13,DOC_time18,
                 Age_time1,Age_time2,Age_time3,Age_time4,Age_time5,Age_time8,Age_time13,Age_time18,
                 Tension_POMS_time1,Tension_POMS_time2,Tension_POMS_time3,Tension_POMS_time4,Tension_POMS_time5,Tension_POMS_time8,Tension_POMS_time13,Tension_POMS_time18)
BLP_int <- pivot_longer(BLP_int, cols=4:length(BLP_int), names_to = c(".value","time"),
                        names_pattern = "(.*)_(.*)") %>%
          select(Study,ID,Sex,DOC,Age,Tension_POMS) %>%
          filter(!is.na(Tension_POMS)) %>%
          rename(Anxiety=Tension_POMS)  %>%
          mutate(Depression=NA) 

names(CAT_mh) <- sub("_pre", "", names(CAT_mh), fixed = TRUE)
names(CAT_mh) <- sub("_post", "", names(CAT_mh), fixed = TRUE)
names(CAT_mh) <- sub("/", ".", names(CAT_mh), fixed = TRUE)
CAT_mh$Age_time4[which(CAT_mh$ID=="RED_CMNT_157")] <- 14.0 #Fix data error
CAT_int <- pivot_longer(CAT_mh, cols=4:length(CAT_mh), names_to = c(".value","time"),
                        names_pattern = "(.*)_(.*)") %>%
  select(Study,ID,Sex,DOC,Age,SCARED_total,CBCL_Withdrawn.Depressed) %>%
  mutate(DOC=as.factor(DOC)) %>%
  filter_at(vars(SCARED_total,CBCL_Withdrawn.Depressed), any_vars(!is.na(.))) %>%
  rename(Depression=CBCL_Withdrawn.Depressed,Anxiety=SCARED_total)


EFC_int <- EFC_mh %>% rename(DOC_time1=child_PRECOVID_visit_date,DOC_time2=covidsurvey_time1,
                              Age_time1=child_PRECOVID_visit_age,Age_time2=age_COVID,
                              Anxiety_time1=MASC_child_selfreport_total,
                              Anxiety_time2=CASPE_mod_child_emotional_experience_time1) 
EFC_int  <- pivot_longer(EFC_int, cols=4:length(EFC_int), names_to = c(".value","time"),
                         names_pattern = "(.*)_(.*)") %>%
            select(Study,ID,Sex,DOC,Age,Anxiety) %>%
            filter(!is.na(Anxiety)) %>%
            mutate(Depression=NA,
                   DOC=ifelse(DOC=="05/18/0209","05/18/19",as.character(DOC))) 

KLG_mh <- KLG_mh %>% rename("ChildSelfReportChildrenDepressionInventory_time1"="ChildSelfReportChildrenDepressionInventoryII_time1")
KLG_int <- pivot_longer(KLG_mh, cols=4:length(KLG_mh), names_to = c(".value","time"),
                         names_pattern = "(.*)_(.*)") %>%
            select(Study,ID,ChildSex,DOC,ChildAge,ChildSelfReportChildrenDepressionInventory,ChildSelfReportSCARED) %>%
           filter(Study=="KLG") %>%
           filter_at(vars(ChildSelfReportChildrenDepressionInventory, ChildSelfReportSCARED), any_vars(!is.na(.))) %>%
           rename(Sex=ChildSex,Age=ChildAge,Depression=ChildSelfReportChildrenDepressionInventory,Anxiety=ChildSelfReportSCARED) %>%
           mutate(Sex="F")

names(LIS_mh)[1] <- "Study"
LIS_int <- pivot_longer(LIS_mh, cols=4:length(LIS_mh), names_to = c(".value","time"),
                         names_pattern = "(.*)_(.*)") %>%
            select(Study,LISID,Sex,DOC,Age,RCAa_DEM_POMS,RCAa_TotalAnxM_POMS) %>%
            filter_at(vars(RCAa_DEM_POMS, RCAa_TotalAnxM_POMS), any_vars(!is.na(.))) %>%
            rename(Depression=RCAa_DEM_POMS,Anxiety=RCAa_TotalAnxM_POMS,ID=LISID)

names(MFS_mh)[1] <- "Study"
MFS_mh <- MFS_mh %>% rename("CESD_Time1"="CESD_T1","CESD_Time2"="CESD_T2","RCMAS_Time1"="RCMAS_T1")  %>% 
          select(!c(BIRTHORDER,FAMID))
MFS_dem <- read.csv(paste0(cape_dir,"Demographics_data/MFS_demo_final.csv"),header = T)
MFS_mh <- merge(MFS_mh, MFS_dem[c("ID","Sex")], by="ID") %>%
          select(Study, ID, Sex, everything())
MFS_int <- pivot_longer(MFS_mh, cols=4:length(MFS_mh), names_to = c(".value","time"),
                         names_pattern = "(.*)_(.*)") %>%
            select(Study,ID,Sex,DOC,age,POMS_CESD,POMS_RCMAS) %>%
            filter_at(vars(POMS_CESD, POMS_RCMAS), any_vars(!is.na(.))) %>%
            rename(Depression=POMS_CESD,Anxiety=POMS_RCMAS,Age=age)

NT_int <- NT_mh %>% rename(Depression_time1=SMFQ_TTP_W1_POMS, Depression_time2=SMFQ_TTP_W2_POMS,Depression_time3=SMFQ_TTP_W3_POMS,Depression_time4=SMFQ_TTP_W4_POMS,
                            Depression_time11=SMFQ_NT_W1_POMS, Depression_time12=SMFQ_NT_W2_POMS,Depression_time13=SMFQ_NT_W3_POMS,Depression_time14=SMFQ_NT_W4_POMS,
                            Age_time1=NT_TTP_Age_Exact_T1,Age_time2=NT_TTP_Age_Exact_T2,Age_time3=NT_TTP_Age_Exact_T3,Age_time4=NT_TTP_Age_Exact_T4,
                            Age_time11=NT_Age_Exact_T1,Age_time12=NT_Age_Exact_T2,Age_time13=NT_Age_Exact_T3,Age_time14=NT_Age_Exact_T4,
                            DOC_time1=NT_TTP_DOC_T1,DOC_time2=NT_TTP_DOC_T2,DOC_time3=NT_TTP_DOC_T3,DOC_time4=NT_TTP_DOC_T4,
                            DOC_time11=NT_DOC_T1,DOC_time12=NT_DOC_T2,DOC_time13=NT_DOC_T3,DOC_time14=NT_DOC_T4) %>%
          mutate(ID=sprintf("NT%03d", ID),
                 Study="NT") 
NT_dem <- read.csv(paste0(cape_dir,"Demographics_data/NT_Dem_covid.csv"),header = T)
NT_int <- merge(NT_int, NT_dem[c("ID","Dem_BioSex_TTP_W1")], by="ID") %>%
          rename(Sex=Dem_BioSex_TTP_W1) %>%
          select(-IDN,NT_ID) %>% select(Study, ID, Sex, everything())
NT_int <- pivot_longer(NT_int, cols=4:length(NT_int), names_to = c(".value","time"),
                        names_pattern = "(.*)_(.*)") %>%
          select(Study,ID,Sex,DOC,Age,Depression) %>%
          filter(!is.na(Depression)) %>%
          mutate(Anxiety=NA,Sex=ifelse(Sex==0,"M",ifelse(Sex==1,"F",NA))) 

SDS_int <- SDS_mh %>% rename(POM_RCADS_GAD_time1=RCADS_GeneralizedAnxiety_Time1_POM,
                             POM_RCADS_GAD_time2=RCADS_GeneralizedAnxiety_Time2_POM) %>%
          pivot_longer(., cols=4:length(SDS_mh), names_to = c(".value","time"),
                        names_pattern = "(.*)_(.*)") %>%
          select(Study,ID,Sex,DOC,Age,POM_RCADS_GAD) %>%
          filter(!is.na(POM_RCADS_GAD)) %>%
          mutate(Depression=NA) %>%
          rename(Anxiety=POM_RCADS_GAD)

TAB_int <- TAB_mh %>% rename(Anxiety_time1=SCAREDC_Total_T1POMS, Anxiety_time2=SCAREDC_Total_T2POMS,
                            Depression_time1=CDI_TOT_R_T1POMS,Age_time1=ChildAge_Time1,Age_time2=ChildAge_Time2,
                            DOC_time1=DOC_Time1,DOC_time2=DOC_Time2)
TAB_int <- pivot_longer(TAB_int, cols=4:length(TAB_int), names_to = c(".value","time"),
                        names_pattern = "(.*)_(.*)") %>%
            mutate(ID=sprintf("TAB%03d", ID)) %>%
            select(Study,ID,Sex,DOC,Age,Depression,Anxiety) %>% 
            filter_at(vars(Depression,Anxiety), any_vars(!is.na(.))) 


TAG_mh <- TAG_mh %>% rename(Anxiety_time2=SCARED_POMS_time2, Anxiety_time4=SCARED_POMS_time4,
                            Anxiety_time6=SCARED_POMS_time6,Anxiety_time7=GAD7_POMS_time7) #Combining SCARED-R (pre-pandemic) and GAD7 (Duringpandemic)
TAG_mh$DOC_time7[which(TAG_mh$ID=="TAG247")] <- "4/24/20" #Fix impossible date and corresponding age
TAG_mh$Age_time7[which(TAG_mh$ID=="TAG247")] <- 13.0
TAG_int <- pivot_longer(TAG_mh, cols=4:length(TAG_mh), names_to = c(".value","time"),
                         names_pattern = "(.*)_(.*)") %>%
            select(Study,ID,Sex,DOC,Age,CESDC_POMS,Anxiety) %>%
            filter_at(vars(CESDC_POMS, Anxiety), any_vars(!is.na(.))) %>%
            mutate(Sex="F") %>%
            rename(Depression=CESDC_POMS) 

names(TGR_mh)[1] <- "ID"
TGR_mh$Study <- "TGR"        
TGR_mh <- TGR_mh %>% rename(MASC2_Total_timeX=GAD7_Total_timeX) #Combining MASC2 (pre-pandemic) and GAD7 (Duringpandemic)
TGR_int <- TGR_mh %>% select(Study, everything()) %>%
            mutate(ID=sprintf("TGR%03d", ID)) %>%
            pivot_longer(., cols=4:length(TGR_mh), names_to = c(".value","time"), 
                         names_pattern = "(.*)_(.*)") %>%
            select(Study,ID,ChildSex,DOC,Age,PHQ9_Total,MASC2_Total) %>%
            filter_at(vars(PHQ9_Total, MASC2_Total), any_vars(!is.na(.))) %>%
            rename(Depression=PHQ9_Total,Anxiety=MASC2_Total,Sex=ChildSex)

TTP_int <- TTP_mh %>% rename(Depression_time1=SMFQ_TTP_W1_POMS, Depression_time2=SMFQ_TTP_W2_POMS,
                            Depression_time3=SMFQ_TTP_W3_POMS,Depression_time4=SMFQ_TTP_W4_POMS,Depression_time5=SMFQ_TT_W5_POMS,
                            Age_time1=TTP_Age_Exact_T1,Age_time2=TTP_Age_Exact_T2,Age_time3=TTP_Age_Exact_T3,Age_time4=TTP_Age_Exact_T4,Age_time5=TTP_Age_Exact_T5,
                            DOC_time1=TTP_DOC_T1,DOC_time2=TTP_DOC_T2,DOC_time3=TTP_DOC_T3,DOC_time4=TTP_DOC_T4,DOC_time5=TTP_DOC_T5)
TTP_dem <- read.csv(paste0(cape_dir,"Demographics_data/TTP_Dem_covid.csv"),header = T)
TTP_int <- merge(TTP_int, TTP_dem[c("ID","Gender_T1")], by="ID") %>%
          rename(Sex=Gender_T1) %>%
          select(-ID_Num) %>% select(Study, ID, Sex, everything())
TTP_int <- pivot_longer(TTP_int, cols=4:length(TTP_int), names_to = c(".value","time"),
                        names_pattern = "(.*)_(.*)") %>%
           select(Study,ID,Sex,DOC,Age,Depression) %>%
           filter(!is.na(Depression)) %>%
           mutate(Anxiety=NA,Sex=ifelse(Sex==1,"M",ifelse(Sex==2,"F",NA))) 

#treat NT and TTP as one study
TTP_int$Study <- "NTTTP"
NT_int$Study <- "NTTTP"

#Merge
ALL_int_self <- rbind(ARC_int,BLP_int,CAT_int,EFC_int,KLG_int,LIS_int,MFS_int,NT_int,SDS_int,TAB_int,TAG_int,TGR_int,TTP_int)
ALL_int_self$DOC <- parse_date_time(ALL_int_self$DOC,
                                          orders = c("m/d/y","m/d/Y","Ymd"),
                                          locale = "eng")



# Remove date in the future
# and Make pre- vs post-pandemic variable (pre is before 11 March 2020, that is when WHO declared a pandemic)
ALL_int_self <- ALL_int_self %>% 
  filter(DOC<="2020-12-01") %>%
  mutate(Sex=ifelse(Sex=="M","Male",ifelse(Sex=="F","Female",NA)),
         PrePost=as.factor(ifelse(DOC<"2020-03-11","Pre",ifelse(DOC>"2020-03-10","During",NA))))

# create Age_2020 (Age in March 2020)
# Filter out participants younger than 9 or older than 18 
Age_2020 <- ALL_int_self %>%
  mutate(Age_2020=round(as.numeric(as.Date("2020-03-11")-as.Date(DOC))/365.25+Age,2)) %>%
  group_by(ID) %>%
  summarize(Age_2020=round(mean(Age_2020),1))
ALL_int_self <- merge(ALL_int_self, Age_2020,by="ID",all.x=T)
ALL_int_self <- ALL_int_self %>% 
  filter(Age_2020<18.0&Age_2020>9.0|is.na(Age_2020))

# Extra dataframes w/ only studies that used the same questionnaire pre- and During-pandemic
ALL_anx_self_same <- ALL_int_self %>% filter(Study!="TAG"&Study!="TGR"&Study!="EFC") %>%
  dplyr::select(!Depression)
ALL_anx_self_tagtgrefc_post <- ALL_int_self %>% filter(!(Study=="TAG"&PrePost=="Pre")) %>%
  filter(!(Study=="TGR"&PrePost=="Pre")) %>% filter(!(Study=="EFC"&PrePost=="Pre")) %>% dplyr::select(!Depression)
ALL_anx_self_tagtgrefc_pre <- ALL_int_self %>% filter(!(Study=="TAG"&PrePost=="During")) %>%
  filter(!(Study=="TGR"&PrePost=="During")) %>% filter(!(Study=="EFC"&PrePost=="During")) %>% dplyr::select(!Depression)





###########################
# 3. Modeling change over time and age moderation
#
###########################

ALL_int_self$PrePost <- factor(ALL_int_self$PrePost, levels=c("Pre","During"))

############## Depression 

#Mixed model testing with DOC
depr_base <- lmerTest::lmer(Depression ~ Sex + Age_2020 + (1|Study/ID), data=ALL_int_self)
#depr_doc <- lmerTest::lmer(Depression ~ as.Date(DOC,origin="2016-01-01") + Sex + Age_2020 + (1|Study/ID), data=ALL_int_self)
#depr_docexp <- lmerTest::lmer(log(Depression+1) ~ as.Date(DOC,origin="2016-01-01") + Sex + Age_2020 + (1|Study/ID), data=ALL_int_self)
#depr_doc_age <- lmerTest::lmer(Depression ~ scale(as.Date(DOC,origin="2016-01-01"))*scale(Age_2020) + Sex + (1|Study/ID), data=ALL_int_self)

#anova(depr_base,depr_doc)
#anova(depr_doc,depr_doc_age)

# Based on the plots, continuous DOC might not be the best predictor variable 
# (there is variation pre-pandemic because of recruitment strategies and different lengths of studies)
# Therefore, testing a binary pre- vs During-pandemic variable, with the cutoff at 2020-03-11

#Mixed model testing with Pre vs Post
depr_doc_2 <- lmerTest::lmer(Depression ~ PrePost + Sex + Age_2020 + (1|Study/ID), data=ALL_int_self)
anova(depr_base,depr_doc_2)
summary(depr_doc_2)

#Age moderation
depr_doc_2a <- lmerTest::lmer(Depression ~ PrePost + Sex + scale(Age_2020) + (1|Study/ID), data=ALL_int_self)
depr_doc_age_2 <- lmerTest::lmer(Depression ~ PrePost*scale(Age_2020) + Sex + (1|Study/ID), data=ALL_int_self)

anova(depr_doc_2a,depr_doc_age_2)



############## Anxiety

#Mixed model testing with DOC
anx_base <- lmerTest::lmer(Anxiety ~ Sex + Age_2020 + (1|Study/ID), data=ALL_int_self)
#anx_doc <- lmerTest::lmer(Anxiety ~ as.Date(DOC,origin="2016-01-01") + Sex + Age_2020 + (1|Study/ID), data=ALL_int_self)
#anx_docexp <- lmerTest::lmer(log(Anxiety+1) ~ as.Date(DOC,origin="2016-01-01") + Sex + Age_2020 + (1|Study/ID), data=ALL_int_self)
#anx_doc_age <- lmerTest::lmer(Anxiety ~ scale(as.Date(DOC,origin="2016-01-01"))*scale(Age_2020) + Sex + (1|Study/ID), data=ALL_int_self)

#anova(anx_base,anx_doc)
#anova(anx_doc,anx_doc_age)

#Mixed model testing with Pre vs Post
anx_doc_2 <- lmerTest::lmer(Anxiety ~ PrePost + Sex + Age_2020 + (1|Study/ID), data=ALL_int_self)
anova(anx_base,anx_doc_2)

#Age moderation
anx_doc_2a <- lmerTest::lmer(Anxiety ~ PrePost + Sex + scale(Age_2020) + (1|Study/ID), data=ALL_int_self)
anx_doc_age_2 <- lmerTest::lmer(Anxiety ~ PrePost*scale(Age_2020) + Sex + (1|Study/ID), data=ALL_int_self)

anova(anx_doc_2a,anx_doc_age_2)
summary(anx_doc_age_2)

#Mixed model testing with Pre vs Post REMOVING TAG AND TIGER AND EFC  
anx_base_s <- lmerTest::lmer(Anxiety ~ Sex + Age_2020 + (1|Study/ID), data=ALL_anx_self_same)
anx_doc_s <- lmerTest::lmer(Anxiety ~ PrePost + Sex + Age_2020 + (1|Study/ID), data=ALL_anx_self_same)
anova(anx_base_s,anx_doc_s)

anx_doc_sa <- lmerTest::lmer(Anxiety ~ PrePost + Sex + scale(Age_2020) + (1|Study/ID), data=ALL_anx_self_same)
anx_doc_age_s <- lmerTest::lmer(Anxiety ~ PrePost*scale(Age_2020) + Sex + (1|Study/ID), data=ALL_anx_self_same)
anova(anx_doc_sa,anx_doc_age_s)


###########################
# 3a. Plotting
#
###########################

############## Depression 

#DOC
ggplot(ALL_int_self,aes(x=DOC,y=Depression)) + 
  geom_point(aes(color=Study)) + 
  geom_line(aes(group=ID)) + 
  geom_smooth(method="loess") + 
  theme_minimal()+theme(legend.position = "none") + 
  labs(x="Date of self-report", y="Depression symptoms")

ggplot(ALL_int_self,aes(x=as.Date(DOC),y=Depression)) + 
  geom_point(aes(color=Study)) + 
  geom_smooth(method="loess",aes(color=Study),se=F) + 
  theme_minimal() + 
  labs(x="Date of self-report", y="Depression symptoms")

#Pre vs During
ggplot(ALL_int_self,aes(x=PrePost,y=Depression)) + 
  geom_dotplot(binaxis = "y", stackdir = "center",dotsize=0.25,aes(color=Study,fill=Study),position=position_jitter(width=0,height=0.01)) + 
  geom_violin(fill="none") +
  theme_minimal(base_size = 16)+  scale_fill_manual(values=pals::cols25(12)) +  scale_color_manual(values=pals::cols25(12)) +
  labs(x="Prior to versus during the pandemic", y="Depression symptoms") + 
  stat_summary(fun=mean,aes(color=Study), geom="point", size=4) + 
  stat_smooth(aes(group = Study,color=Study),method="lm",se=F)

#Age
ggplot(ALL_int_self,aes(x=Age_2020,y=Depression)) + 
  geom_point(aes(color=PrePost,shape=PrePost)) + 
  geom_smooth(method="loess",color='black',aes(fill=PrePost,linetype=PrePost)) +
  scale_fill_manual(values=c("#00BFC4","#F8766D")) +
  scale_color_manual(values=c("#00BFC4","#F8766D")) +
  theme_minimal(base_size = 14) +
  theme(legend.title = element_blank()) +
  labs(x="Age", y="Depression symptoms") 


############## Anxiety 


#DOC
ggplot(ALL_int_self,aes(x=DOC,y=Anxiety)) + 
  geom_point(aes(color=Study)) + 
  geom_line(aes(group=ID)) + 
  geom_smooth(method="loess") + 
  theme_minimal()+theme(legend.position = "none") + 
  labs(x="Date of self-report", y="Anxiety symptoms")

ggplot(ALL_int_self,aes(x=as.Date(DOC),y=Anxiety)) + 
  geom_point(aes(color=Study)) + 
  geom_smooth(method="loess",aes(color=Study),se=F) + 
  theme_minimal() + 
  labs(x="Date of self-report", y="Anxiety symptoms")

#Pre vs During
ggplot(ALL_int_self,aes(x=PrePost,y=Anxiety)) + 
  geom_dotplot(binaxis = "y", stackdir = "center",dotsize=0.25,aes(color=Study,fill=Study) ,position=position_jitter(width=0,height=0.01)) +
  geom_violin(fill="none") +
  theme_minimal(base_size = 16)+ scale_fill_manual(values=pals::cols25(12)) +  scale_color_manual(values=pals::cols25(12)) +
  labs(x="Prior to versus during pandemic", y="Anxiety symptoms") +
  stat_summary(fun=mean,aes(color=Study), geom="point", size=4) + 
  stat_smooth(aes(group = Study,color=Study),method="lm",se=F)

#Age
ggplot(ALL_int_self,aes(x=Age_2020,y=Anxiety)) + 
  geom_point(aes(color=PrePost,shape=PrePost)) + 
  geom_smooth(method="loess",color='black',aes(fill=PrePost,linetype=PrePost)) +
  scale_fill_manual(values=c("#00BFC4","#F8766D")) +
  scale_color_manual(values=c("#00BFC4","#F8766D")) +
  theme_minimal(base_size = 14) +
  theme(legend.title = element_blank()) +
  labs(x="Age", y="Anxiety symptoms")  


#####################
# 3b. control variable timespan from first to last assessment
#
#####################

timespan <- ALL_int_self %>%
  group_by(ID,PrePost) %>%
  summarize(latestdate=max(DOC),
            earliestdate=min(DOC),
            latestage=max(Age),
            earliestage=min(Age))
timespan <- pivot_wider(timespan,names_from = PrePost,
                               values_from = c("earliestdate","latestdate","earliestage","latestage")) %>%
  mutate(totalspan=difftime(latestdate_During, earliestdate_Pre, units = "weeks")/52)
ALL_int_self_span <- merge(ALL_int_self,timespan[c("ID","totalspan")])

depr_span <- lmerTest::lmer(Depression ~ PrePost + Sex + Age_2020 + totalspan + (1|Study/ID), data=ALL_int_self_span)
anx_span <- lmerTest::lmer(Anxiety ~ PrePost + Sex + Age_2020 + totalspan + (1|Study/ID), data=ALL_int_self_span)
depr_span_age <- lmerTest::lmer(Depression ~ PrePost*scale(Age_2020) + Sex + totalspan + (1|Study/ID), data=ALL_int_self_span)
anx_span_age <- lmerTest::lmer(Anxiety ~ PrePost*scale(Age_2020) + Sex + totalspan + (1|Study/ID), data=ALL_int_self_span)

ggplot(ALL_int_self_span,aes(x=totalspan,y=Depression)) + 
  geom_point(aes(color=PrePost,shape=PrePost)) + 
  geom_smooth(method="loess",color='black',aes(fill=PrePost,linetype=PrePost)) +
  scale_fill_manual(values=c("#00BFC4","#F8766D")) +
  scale_color_manual(values=c("#00BFC4","#F8766D")) +
  theme_minimal(base_size = 14) +
  theme(legend.title = element_blank()) +
  labs(x="Timespan", y="Depression symptoms")  


#####################
# 3c. Exploratory analyses of anxiety subscales 
#
####################

######## NOT USED IN PAPER
# Only includes a small subset of the studies

KLG_genanx <- pivot_longer(KLG_mh, cols=4:length(KLG_mh), names_to = c(".value","time"),
                        names_pattern = "(.*)_(.*)") %>%
  select(Study,ID,ChildSex,DOC,ChildAge,ChildSelfReportSCARED_GeneralizedAnxietyDisorder) %>%
  filter(!is.na(ChildSelfReportSCARED_GeneralizedAnxietyDisorder),Study=="KLG") %>%
  rename(Sex=ChildSex,Age=ChildAge,Generalized_Anxiety=ChildSelfReportSCARED_GeneralizedAnxietyDisorder) %>%
  mutate(Sex="F",Generalized_Anxiety=Generalized_Anxiety/18)

LIS_genanx <- pivot_longer(LIS_mh, cols=4:length(LIS_mh), names_to = c(".value","time"),
                        names_pattern = "(.*)_(.*)") %>%
  select(Study,LISID,Sex,DOC,Age,RCAa_GAM_POMS) %>%
  filter(!is.na(RCAa_GAM_POMS)) %>%
  rename(Generalized_Anxiety=RCAa_GAM_POMS,ID=LISID)

SDS_genanx <- SDS_int %>% rename(Generalized_Anxiety=Anxiety) %>% dplyr::select(!Depression)
ALL_genanx_self <- rbind(KLG_genanx,LIS_genanx,SDS_genanx)

ALL_genanx_self$DOC <- parse_date_time(ALL_genanx_self$DOC,
                                    orders = c("m/d/y","m/d/Y"),
                                    locale = "eng")
ALL_genanx_self <- ALL_genanx_self %>% 
  filter(DOC<="2020-11-01") %>%
  mutate(Sex=ifelse(Sex=="M","Male",ifelse(Sex=="F","Female",NA)),
         PrePost=as.factor(ifelse(DOC<"2020-03-11","Pre",ifelse(DOC>"2020-03-10","During",NA))))

ALL_genanx_self <- merge(ALL_genanx_self, Age_2020,by="ID")
ALL_genanx_self <- ALL_genanx_self %>% 
  filter(Age_2020<18.0&Age_2020>8.99)

anxg_base <- lmerTest::lmer(Generalized_Anxiety ~ Sex + Age_2020 + (1|Study/ID), data=ALL_genanx_self)
anxg_doc <- lmerTest::lmer(Generalized_Anxiety ~ PrePost + Sex + Age_2020 + (1|Study/ID), data=ALL_genanx_self)
anxg_doc_age <- lmerTest::lmer(Generalized_Anxiety ~ PrePost*scale(Age_2020) + Sex + (1|Study/ID), data=ALL_genanx_self)

anova(anxg_base,anxg_doc)
anova(anxg_doc,anxg_doc_age)

ALL_genanx_self$PrePost <- factor(ALL_genanx_self$PrePost, levels=c("Pre","During"))
ALL_genanx_self$agegrp <- cut(ALL_genanx_self$Age_2020, c(seq(9, 18, 1)))
ggplot(ALL_genanx_self,aes(x=PrePost,y=Generalized_Anxiety)) + 
  geom_dotplot(binaxis = "y", stackdir = "center",binwidth=0.01,aes(fill=agegrp)) + 
  scale_fill_brewer(palette = "Oranges") +
  geom_violin(fill="none") +
  theme_minimal()+ 
  labs(x="Prior to versus during pandemic", y="Generalized Anxiety symptoms")


###########################
# 4. Moderation by race/ethnicity
#
###########################

ALL_race_ethn <- read.csv(paste0(cape_dir,"Processed_data/raceethnicity.csv"),header = T) %>%
  mutate(ID=as.character(ID)) %>% select(-Race_raw,-Ethnicity_raw)
names(ALL_race_ethn)[1] <- "Study"

levels(ALL_race_ethn$RaceEthnicity)[1] <- "Other" #putting native Am into other category because of low numbers

ALL_int_self_raceethn <- left_join(ALL_int_self,ALL_race_ethn[c("ID","RaceEthnicity")],by=c("ID")) %>%
  mutate(RaceEthnicity = relevel(RaceEthnicity, ref="White"))
ALL_int_self_raceethn_full <- ALL_int_self_raceethn %>%
    filter(!is.na(RaceEthnicity)) 

#Descriptives
ALL_int_self_raceethn %>%
  distinct(., ID, .keep_all=T) %>%
  group_by(RaceEthnicity) %>%
  summarize(n_raceethn = n())

############## Depression 

#Mixed model testing with Pre vs Post
depr_doc_r <- lmerTest::lmer(Depression ~ PrePost + Sex + Age_2020 + (1|Study/ID), data=ALL_int_self_raceethn_full)
depr_doc_race <- lmerTest::lmer(Depression ~ PrePost*RaceEthnicity + Sex + Age_2020 + (1|Study/ID), data=ALL_int_self_raceethn_full)

anova(depr_doc_r,depr_doc_race)
summary(depr_doc_race)


############## Anxiety 

#Mixed model testing with Pre vs Post
anx_doc_r <- lmerTest::lmer(Anxiety ~ PrePost + Sex + Age_2020 + (1|Study/ID), data=ALL_int_self_raceethn_full)
anx_doc_race <- lmerTest::lmer(Anxiety ~ PrePost*RaceEthnicity + Sex + Age_2020 + (1|Study/ID), data=ALL_int_self_raceethn_full)

anova(anx_doc_r,anx_doc_race)
summary(anx_doc_race)

#Retesting without studies that used different questionnaires pre vs During
ALL_anx_self_same_raceethn <- ALL_int_self_raceethn_full %>% filter(Study!="TAG"&Study!="TGR"&Study!="EFC") %>%
  dplyr::select(!Depression)
anx_doc_r_same <- lmerTest::lmer(Anxiety ~ PrePost + Sex + Age_2020 + (1|Study/ID), data=ALL_anx_self_same_raceethn)
anx_doc_race_same <- lmerTest::lmer(Anxiety ~ PrePost*RaceEthnicity + Sex + Age_2020 + (1|Study/ID), data=ALL_anx_self_same_raceethn)
anova(anx_doc_r_same,anx_doc_race_same)
summary(anx_doc_race_same)

#################### 
# 4a. Plotting 
#
####################

ALL_int_self_raceethn$PrePost <- factor(ALL_int_self_raceethn$PrePost, levels=c("Pre","During"))

ggplot(ALL_int_self_raceethn,aes(x=PrePost,y=Depression)) + 
  geom_violin(aes(fill=RaceEthnicity),position=position_dodge(0.9)) +
  geom_dotplot(binaxis = "y",stackdir='center', position=position_dodge(0.9),dotsize=0.25,stackratio=0.5) + 
  facet_grid(. ~ RaceEthnicity) +
  theme_minimal(base_size = 14)+ theme(legend.position = "bottom") +
  labs(x="Prior to versus during pandemic", y="Depression symptoms") + 
  stat_summary(fun=median,aes(shape=RaceEthnicity), geom="point", size=3,position=position_dodge(0.9))

ggplot(ALL_int_self_raceethn,aes(x=PrePost,y=Anxiety)) + 
  geom_violin(aes(fill=RaceEthnicity),position=position_dodge(0.9)) +
  geom_dotplot(binaxis = "y",stackdir='center', position=position_dodge(0.9),dotsize=0.25,stackratio=0.5) + 
  facet_grid(. ~ RaceEthnicity) +
  theme_minimal(base_size = 14)+ theme(legend.position = "bottom") +
  labs(x="Prior to versus during pandemic", y="Anxiety symptoms") + 
  stat_summary(fun=median,aes(shape=RaceEthnicity), geom="point", size=3,position=position_dodge(0.9))




###########################
# 5. Moderation by disease burden
# 
#
###########################

ALL_cases <- read.csv(paste0(cape_dir,"Processed_data/cases_per_mil.csv"),header = T) %>%
   select(Study, ID, DOC, cases_per_1M_day,cases_per_1M_week,cases_per_1M_cumul) %>%
  mutate(ID=ifelse(ID=="BLP",as.character(Study),as.character(ID))) %>%
  mutate(Study=ifelse(substr(Study, 1, 3) == 'BLP',"BLP",as.character(Study)))
ALL_deaths <- read.csv(paste0(cape_dir,"Processed_data/death_per_mil.csv"),header = T) %>%
  mutate(ID=as.character(ID)) %>% select(Study, ID, DOC, deaths_per_1M_day,deaths_per_1M_week,deaths_per_1M_cumul)
ALL_burden <- merge(ALL_cases,ALL_deaths,by=c("ID","Study","DOC"),all=T)


#calculating mean case/death rate within participant so that it can be used in the long format mixed model
mean_burden <- ALL_burden %>%
  group_by(ID) %>%
  summarize(mean_cases=round(mean(cases_per_1M_week, na.rm=T),2),
            mean_deaths=round(mean(deaths_per_1M_week, na.rm=T),2))
ALL_int_self_burden <- merge(ALL_int_self, mean_burden,by="ID",all=T) %>%
  mutate(mean_cases=ifelse(mean_cases<0,NA,mean_cases),
         mean_deaths=ifelse(mean_deaths<0,NA,mean_deaths))

hist(ALL_int_self_burden$mean_cases)
hist(ALL_int_self_burden$mean_deaths)

############## Depression 

ALL_int_self_burden_full <- ALL_int_self_burden %>%
  select(ID, Study, Sex, Age_2020, PrePost, Depression, mean_cases, mean_deaths) %>%
  na.omit(.)
ALL_int_self_burden_full_notab <- ALL_int_self_burden_full %>%
  filter(Study!="TAB")
ALL_int_self_burden_full$PrePost <- factor(ALL_int_self_burden_full$PrePost, levels=c("Pre","During"))
ALL_int_self_burden_full_notab$PrePost <- factor(ALL_int_self_burden_full_notab$PrePost, levels=c("Pre","During"))

#Baseline
depr_doc_db <- lmerTest::lmer(Depression ~ PrePost + Sex + Age_2020 + (1|Study/ID), data=ALL_int_self_burden_full)

#Case rates
depr_doc_cases <- lmerTest::lmer(Depression ~ PrePost*scale(sqrt(mean_cases)) + Sex + Age_2020 + (1|Study/ID), data=ALL_int_self_burden_full)
anova(depr_doc_db,depr_doc_cases)
summary(depr_doc_cases)

#Death rates
depr_doc_deaths <- lmerTest::lmer(Depression ~ PrePost*scale(sqrt(mean_deaths)) + Sex + Age_2020 + (1|Study/ID), data=ALL_int_self_burden_full)
anova(depr_doc_db,depr_doc_deaths)

#without the TAB study
depr_doc_db_notab <- lmerTest::lmer(Depression ~ PrePost + Sex + Age_2020 + (1|Study/ID), data=ALL_int_self_burden_full_notab)
depr_doc_deaths_notab <- lmerTest::lmer(Depression ~ PrePost*sqrt(mean_deaths) + Sex + Age_2020 + (1|Study/ID), data=ALL_int_self_burden_full_notab)
anova(depr_doc_db_notab,depr_doc_deaths_notab)


#Mixed model testing cases and death within DURING-PANDEMIC DATA ####Extra NOT USED#####
ALL_int_self_burden_mid <- ALL_int_self_burden %>%
  filter(PrePost=="During") %>% filter(!is.na(cases_per_1M))

depr_doc_db2 <- lmerTest::lmer(Depression ~  Sex + (1|Study/ID), data=ALL_int_self_burden_mid)
depr_doc_cases_mid <- lmerTest::lmer(Depression ~ cases_per_1M + Sex + (1|Study/ID), data=ALL_int_self_burden_mid)

anova(depr_doc_db2,depr_doc_cases_mid)

depr_doc_deaths_mid <- lmerTest::lmer(Depression ~ deaths_per_1M + Sex + (1|Study/ID), data=ALL_int_self_burden_mid)

anova(depr_doc_db2,depr_doc_deaths_mid)



############## Anxiety 

ALL_int_self_burden_afull <- ALL_int_self_burden %>%
  select(ID, Study, Sex, Age_2020, PrePost, Anxiety, mean_cases, mean_deaths) %>%
  na.omit(.)
ALL_int_self_burden_afull_notab <- ALL_int_self_burden_afull %>%
  filter(Study!="TAB")
ALL_int_self_burden_afull$PrePost <- factor(ALL_int_self_burden_afull$PrePost, levels=c("Pre","During"))
ALL_int_self_burden_afull_notab$PrePost <- factor(ALL_int_self_burden_afull_notab$PrePost, levels=c("Pre","During"))

#Mixed model testing PRE-POST WITH MEAN CASE/DEATH RATE WITHIN PP

#baseline
anx_doc_db <- lmerTest::lmer(Anxiety ~ PrePost + Sex + Age_2020 + (1|Study/ID), data=ALL_int_self_burden_afull)

#case rate
anx_doc_cases <- lmerTest::lmer(Anxiety ~ PrePost*scale(sqrt(mean_cases)) + Sex + Age_2020 + (1|Study/ID), data=ALL_int_self_burden_afull)
anova(anx_doc_db,anx_doc_cases)
summary(anx_doc_cases)

#death rate
anx_doc_deaths <- lmerTest::lmer(Anxiety ~ PrePost*scale(sqrt(mean_deaths)) + Sex + Age_2020 + (1|Study/ID), data=ALL_int_self_burden_afull)
anova(anx_doc_db,anx_doc_deaths)

#wihout the TAB study
anx_doc_db_notab <- lmerTest::lmer(Anxiety ~ PrePost + Sex + Age_2020 + (1|Study/ID), data=ALL_int_self_burden_afull_notab)
anx_doc_deaths_notab <- lmerTest::lmer(Anxiety ~ PrePost*sqrt(mean_deaths) + Sex + Age_2020 + (1|Study/ID), data=ALL_int_self_burden_afull_notab)
anova(anx_doc_db_notab,anx_doc_deaths_notab)


#Mixed model testing CASES AND DEATHS WITHIN During-PANDEMIC DATA ####Extra NOT USED#####
anx_doc_db2 <- lmerTest::lmer(Anxiety ~  Sex + (1|Study/ID), data=ALL_int_self_burden_mid)
anx_doc_cases_mid <- lmerTest::lmer(Anxiety ~ cases_per_1M + Sex + (1|Study/ID), data=ALL_int_self_burden_mid)

anova(anx_doc_db2,anx_doc_cases_mid)

anx_doc_deaths_mid <- lmerTest::lmer(Anxiety ~ deaths_per_1M + Sex + (1|Study/ID), data=ALL_int_self_burden_mid)

anova(anx_doc_db2,anx_doc_deaths_mid)



########################
# 5a. plotting
#
########################

ALL_int_self_burden$PrePost <- factor(ALL_int_self_burden$PrePost, levels=c("Pre","During"))

#Depression
ggplot(ALL_int_self_burden,aes(x=mean_cases,y=Depression)) + 
  geom_point(aes(color=PrePost,shape=PrePost)) + 
  geom_smooth(formula=y~sqrt(x),aes(group=PrePost,fill=PrePost,linetype=PrePost)) +
  theme_minimal()+   
  scale_fill_manual(values=c("#00CCCC","#FF3300")) +
  scale_color_manual(values=c("#00CCCC","#FF3300")) +
  labs(x="Cases per 1M people in county/region", y="Depressive symptoms")

ggplot(ALL_int_self_burden_full_notab,aes(x=mean_deaths,y=Depression)) + 
  geom_point(aes(color=PrePost,shape=PrePost)) + 
  geom_smooth(formula=y~sqrt(x),aes(group=PrePost,fill=PrePost,linetype=PrePost)) +
  scale_fill_manual(values=c("#00CCCC","#FF3300")) +
  scale_color_manual(values=c("#00CCCC","#FF3300")) +
  theme_minimal()+ 
  labs(x="Deaths per 1M people in county/region", y="Depressive symptoms")

#Anxiety
ALL_int_self_burden$PrePost <- factor(ALL_int_self_burden$PrePost, levels=c("Pre","During"))
ggplot(ALL_int_self_burden,aes(x=mean_cases,y=Anxiety)) + 
  geom_point(aes(color=PrePost,shape=PrePost)) + 
  geom_smooth(formula=y~sqrt(x),aes(group=PrePost,fill=PrePost,linetype=PrePost)) +
  scale_fill_manual(values=c("#00CCCC","#FF3300")) +
  scale_color_manual(values=c("#00CCCC","#FF3300")) +
  theme_minimal()+ 
  labs(x="Cases per 1M people in county/region", y="Anxiety symptoms")

ggplot(ALL_int_self_burden_afull_notab,aes(x=mean_deaths,y=Anxiety)) + 
  geom_point(aes(color=PrePost,shape=PrePost)) + 
  geom_smooth(formula=y~sqrt(x),aes(group=PrePost,fill=PrePost,linetype=PrePost)) +
  scale_fill_manual(values=c("#00CCCC","#FF3300")) +
  scale_color_manual(values=c("#00CCCC","#FF3300")) +
  theme_minimal()+ 
  labs(x="Deaths per 1M people in county/region", y="Anxiety symptoms")




###########################
# 6. Moderation by govenrment restrictions
#
###########################

ALL_gov_restric <- read.csv(paste0(cape_dir,"Processed_data/dates_loc_restrictions.csv"),header = T) %>%
  mutate(ID=as.character(ID)) 
ALL_gov_restric$DOC <- parse_date_time(ALL_gov_restric$DOC, orders = c("m/d/y","m/d/Y"),
                                    locale = "eng")
ALL_int_self_restric <- left_join(ALL_int_self, ALL_gov_restric[c("ID","DOC","Strictness")],
                                  by=c("ID","DOC"))
median_restric <- ALL_int_self_restric %>%
  filter(PrePost=="During") %>%
  group_by(ID) %>%
  summarize(median_Strictness=median(Strictness))

ALL_int_self_medianrestric <- left_join(ALL_int_self, median_restric,by="ID") %>% 
  filter(!is.na(median_Strictness))

#Mixed model testing strictness PRE VERSUS POST
depr_doc_s1 <- lmerTest::lmer(Depression ~  PrePost + Sex + Age_2020 + (1|Study/ID), data=ALL_int_self_medianrestric)
depr_doc_strictness <- lmerTest::lmer(Depression ~ PrePost*scale(median_Strictness) + Sex + Age_2020 + (1|Study/ID), data=ALL_int_self_medianrestric)

anova(depr_doc_s1,depr_doc_strictness)
summary(depr_doc_strictness)

anx_doc_s1 <- lmerTest::lmer(Anxiety ~ PrePost + Sex + Age_2020 + (1|Study/ID), data=ALL_int_self_medianrestric)
anx_doc_strictness <- lmerTest::lmer(Anxiety ~ PrePost*scale(median_Strictness) + Sex + Age_2020 + (1|Study/ID), data=ALL_int_self_medianrestric)

anova(anx_doc_s1,anx_doc_strictness)
summary(anx_doc_strictness)


#Mixed model testing strictness WITHIN During-PANDEMIC DATA ###NOT USED
ALL_int_self_restric_mid <- ALL_int_self_restric %>%
  filter(PrePost=="During") %>% filter(!is.na(Strictness))
ALL_int_self_restric_mid$Strictness_fac <- factor(ALL_int_self_restric_mid$Strictness, levels=c("5","4","3","2"))

anx_doc_s <- lmerTest::lmer(Anxiety ~  Sex + (1|Study/ID), data=ALL_int_self_restric_mid)
anx_doc_strictness_mid <- lmerTest::lmer(Anxiety ~ Strictness_fac + Sex + (1|Study/ID), data=ALL_int_self_restric_mid)

anova(anx_doc_s,anx_doc_strictness_mid)

depr_doc_s <- lmerTest::lmer(Depression ~  Sex + (1|Study/ID), data=ALL_int_self_restric_mid)
depr_doc_strictness_mid <- lmerTest::lmer(Depression ~ Strictness_fac + Sex + (1|Study/ID), data=ALL_int_self_restric_mid)

anova(depr_doc_s,depr_doc_strictness_mid)

################ 
# 6a. Plotting 
#
#################

ALL_int_self_medianrestric$PrePost <- factor(ALL_int_self_medianrestric$PrePost, levels=c("Pre","During"))

ggplot(ALL_int_self_medianrestric,aes(x=median_Strictness,y=Depression)) + 
  geom_point(aes(color=PrePost,shape=PrePost),position=position_jitter(width=0.02,height=0)) + 
  geom_smooth(method="lm",aes(group=PrePost,fill=PrePost,linetype=PrePost)) +
  scale_fill_manual(values=c("#00CCCC","#FF3300")) +
  scale_color_manual(values=c("#00CCCC","#FF3300")) +
  theme_minimal()+ 
  labs(x="Level of government restrictions", y="Depressive symptoms")

ggplot(ALL_int_self_medianrestric,aes(x=PrePost,y=Depression)) + 
  geom_violin(aes(color=PrePost)) + 
  geom_dotplot(aes(color=PrePost,fill=PrePost),binaxis = "y",stackdir='center', dotsize=0.25) +
  facet_grid(. ~ median_Strictness) +
  theme_minimal()+ 
  scale_color_manual(values=c("#00CCCC","#FF3300"),name="") +
  scale_fill_manual(values=c("#00CCCC","#FF3300"),name="") +
  labs(x="", y="Depressive symptoms") +
  stat_summary(fun=median,geom="point", size=3)

ggplot(ALL_int_self_medianrestric,aes(x=median_Strictness,y=Anxiety)) + 
  geom_point(aes(color=PrePost,shape=PrePost),position=position_jitter(width=0.02,height=0)) + 
  geom_smooth(method="lm",aes(group=PrePost,fill=PrePost,linetype=PrePost)) +
  scale_fill_manual(values=c("#00CCCC","#FF3300")) +
  scale_color_manual(values=c("#00CCCC","#FF3300")) +
  theme_minimal() + 
  labs(x="Level of government restrictions", y="Anxiety symptoms")

ggplot(ALL_int_self_medianrestric,aes(x=PrePost,y=Anxiety)) + 
  geom_violin(aes(color=PrePost)) + 
  geom_dotplot(aes(color=PrePost,fill=PrePost),binaxis = "y",stackdir='center', dotsize=0.25,stackratio=0.5) +
  facet_grid(. ~ median_Strictness) +
  theme_minimal()+ 
  scale_color_manual(values=c("#00CCCC","#FF3300"),name="") +
  scale_fill_manual(values=c("#00CCCC","#FF3300"),name="") +
  labs(x="", y="Anxiety symptoms") +
  stat_summary(fun=median,geom="point", size=3)


#Plotting restriction levels During-pandemic data only #NOT USED
ggplot(ALL_int_self_restric_mid,aes(x=Strictness,y=Anxiety)) + 
  geom_point(aes(color=Study)) + 
  geom_line(aes(group=ID)) + 
  geom_smooth(method="loess") + 
  theme_minimal()+theme(legend.position = "none") + 
  labs(x="Level of government restrictions", y="Anxiety symptoms")

ggplot(ALL_int_self_restric_mid,aes(x=Strictness,y=Anxiety)) + 
  geom_point(aes(color=Study)) + 
  geom_smooth(method="loess") + 
  theme_minimal()+ 
  labs(x="Level of government restrictions", y="Anxiety symptoms")



###########################
# 7. Effect sizes and descriptives
#
###########################

#effect sizes
ALL_pp_summaries <- ALL_int_self %>%
  group_by(ID,PrePost) %>%
  summarize(depression_intra_mean = mean(Depression,na.rm=T),
            anxiety_intra_mean = mean(Anxiety,na.rm=T))

ALL_pp_summaries %>%
  group_by(PrePost) %>%
  summarize(depression_median = median(depression_intra_mean,na.rm=T),
            anxiety_median = median(anxiety_intra_mean,na.rm=T),
            depression_mean = mean(depression_intra_mean,na.rm=T),
            anxiety_mean = mean(anxiety_intra_mean,na.rm=T))  

#n and age and sex and DOC total sample
length(unique(ALL_int_self$ID))

ALL_int_self %>%
  group_by(PrePost) %>%
  summarize(n_prepost = n(),
            medianDOC=median(DOC))

ALL_int_self %>%
  summarize(mean_age = mean(Age,na.rm=T),
            sd_age = sd(Age,na.rm = T)) 
ALL_int_self %>%
  distinct(., ID, .keep_all=T) %>%
  group_by(Sex) %>%
  summarize(malefemale = n())

num_obs <- as.data.frame(table(ALL_int_self$ID) ) %>% 
  filter(Freq>0)
describe(num_obs$Freq)

earliest_latest <- ALL_int_self %>%
       group_by(ID,PrePost) %>%
      summarize(latestdate=max(DOC),
      earliestdate=min(DOC),
      latestage=max(Age),
      earliestage=min(Age))
earliest_latest <- pivot_wider(earliest_latest,names_from = PrePost,
                               values_from = c("earliestdate","latestdate","earliestage","latestage")) %>%
  mutate(interval=difftime(earliestdate_During, latestdate_Pre, units = "days"))
describe(as.numeric(earliest_latest$interval))
describe(earliest_latest$earliestage_Pre)
describe(earliest_latest$latestage_During)

#Descriptives by study
ALL_int_self %>%
  group_by(Study) %>%
  summarize(min_age = min(Age,na.rm=T),
            max_age = max(Age,na.rm=T),
            startdate = min(DOC,na.rm=T),
            enddate = max(DOC,na.rm=T)) 

ALL_int_self %>%
  distinct(., ID, .keep_all=T) %>%
  group_by(Study,Sex) %>%
  summarize(malefemale = n()) %>% 
  print(n=100)

ALL_int_self_raceethn %>%
  distinct(., ID, .keep_all=T) %>%
  group_by(Study,RaceEthnicity) %>%
  summarize(n_raceethn = n()) %>% 
  print(n=100)

#effect sizes dependent on disease burden
lowcases_by_pp <- ALL_int_self_burden %>%
  filter(mean_cases<100) %>%
  group_by(ID,PrePost) %>%
  summarize(depression_intra_mean = mean(Depression,na.rm=T),
            anxiety_intra_mean = mean(Anxiety,na.rm=T))
lowcases_by_pp_short <- lowcases_by_pp %>%
  pivot_wider(names_from = "PrePost",values_from = c("depression_intra_mean","anxiety_intra_mean"))
lowcases_by_pp_short <- lowcases_by_pp_short %>% 
  mutate(depression_ch=depression_intra_mean_During - depression_intra_mean_Pre,
         anxiety_ch=anxiety_intra_mean_During - anxiety_intra_mean_Pre)
summary(lowcases_by_pp_short$depression_ch)
summary(lowcases_by_pp_short$anxiety_ch)


highcases_by_pp <- ALL_int_self_burden %>%
  filter(mean_cases>200) %>%
  group_by(ID,PrePost) %>%
  summarize(depression_intra_mean = mean(Depression,na.rm=T),
            anxiety_intra_mean = mean(Anxiety,na.rm=T))
highcases_by_pp_short <- highcases_by_pp %>%
  pivot_wider(names_from = "PrePost",values_from = c("depression_intra_mean","anxiety_intra_mean"))
highcases_by_pp_short <- highcases_by_pp_short %>% 
  mutate(depression_ch=depression_intra_mean_During - depression_intra_mean_Pre,
         anxiety_ch=anxiety_intra_mean_During - anxiety_intra_mean_Pre)
summary(highcases_by_pp_short$depression_ch)
summary(highcases_by_pp_short$anxiety_ch)


lowdeaths_by_pp <- ALL_int_self_burden %>%
  filter(mean_deaths<1) %>%
  group_by(ID,PrePost) %>%
  summarize(depression_intra_mean = mean(Depression,na.rm=T),
            anxiety_intra_mean = mean(Anxiety,na.rm=T))
lowdeaths_by_pp_short <- lowdeaths_by_pp %>%
  pivot_wider(names_from = "PrePost",values_from = c("depression_intra_mean","anxiety_intra_mean"))
lowdeaths_by_pp_short <- lowdeaths_by_pp_short %>% 
  mutate(depression_ch=depression_intra_mean_During - depression_intra_mean_Pre,
         anxiety_ch=anxiety_intra_mean_During - anxiety_intra_mean_Pre)
summary(lowdeaths_by_pp_short$depression_ch)
summary(lowdeaths_by_pp_short$anxiety_ch)


highdeaths_by_pp <- ALL_int_self_burden_afull_notab %>%
  filter(mean_deaths>5) %>%
  group_by(ID,PrePost) %>%
  summarize(depression_intra_mean = mean(Depression,na.rm=T),
            anxiety_intra_mean = mean(Anxiety,na.rm=T))
highdeaths_by_pp_short <- highdeaths_by_pp %>%
  pivot_wider(names_from = "PrePost",values_from = c("depression_intra_mean","anxiety_intra_mean"))
highdeaths_by_pp_short <- highdeaths_by_pp_short %>% 
  mutate(depression_ch=depression_intra_mean_During - depression_intra_mean_Pre,
         anxiety_ch=anxiety_intra_mean_During - anxiety_intra_mean_Pre)
summary(highdeaths_by_pp_short$depression_ch)
summary(highdeaths_by_pp_short$anxiety_ch)

### Correlation restriction and disease burden
burden_restric <- merge(mean_burden, median_restric, by="ID", all=T) 
cor(burden_restric[c("median_Strictness", "mean_cases", "mean_deaths")],use="pairwise.complete.obs", method = "spearman")



###############################
# 8. Leave one out analyses
#
#
###############################

list_studies <- as.vector(unique(ALL_int_self$Study))
rightrow <- 0
pvals_main <- data.frame(depr=as.integer(99),anx=as.integer(99)) 
pvals_age <- data.frame(depr=as.integer(99),anx=as.integer(99)) 
pvals_race <- data.frame(depr=as.integer(99),anx=as.integer(99)) 
pvals_case <- data.frame(depr=as.integer(99),anx=as.integer(99)) 
pvals_death<- data.frame(depr=as.integer(99),anx=as.integer(99)) 
pvals_restr <- data.frame(depr=as.integer(99),anx=as.integer(99)) 

for (leavemeout in list_studies)
{
  ALL_int_self_minusone <- ALL_int_self %>% filter(Study!=leavemeout)
  ALL_int_self_minusone_race <- ALL_int_self_raceethn_full %>% filter(Study!=leavemeout)
  ALL_int_self_minusone_burdend <- ALL_int_self_burden_full %>% filter(Study!=leavemeout)
  ALL_int_self_minusone_burdena <- ALL_int_self_burden_afull %>% filter(Study!=leavemeout)
  ALL_int_self_minusone_restr <- ALL_int_self_medianrestric %>% filter(Study!=leavemeout)
  rightrow <- rightrow+1 
  
  depr_main_m1 <- lmerTest::lmer(Depression ~ PrePost + Sex + Age_2020 + (1|Study/ID), data=ALL_int_self_minusone)
  anx_main_m1 <- lmerTest::lmer(Anxiety ~ PrePost + Sex + Age_2020 + (1|Study/ID), data=ALL_int_self_minusone)
  pvals_main[rightrow,1] <- summary(depr_main_m1)$coefficients["PrePostDuring",5]
  pvals_main[rightrow,2] <- summary(anx_main_m1)$coefficients["PrePostDuring",5]
  
  depr_age_m1 <- lmerTest::lmer(Depression ~ PrePost*scale(Age_2020) + Sex + (1|Study/ID), data=ALL_int_self_minusone)
  anx_age_m1 <- lmerTest::lmer(Anxiety ~ PrePost*scale(Age_2020) + Sex + (1|Study/ID), data=ALL_int_self_minusone)
  pvals_age[rightrow,1] <- summary(depr_age_m1)$coefficients["PrePostDuring:scale(Age_2020)",5]
  pvals_age[rightrow,2] <- summary(anx_age_m1)$coefficients["PrePostDuring:scale(Age_2020)",5]
  
  depr_r_m1 <- lmerTest::lmer(Depression ~ PrePost + Sex + Age_2020 + (1|Study/ID), data=ALL_int_self_minusone_race)
  depr_race_m1 <- lmerTest::lmer(Depression ~ PrePost*RaceEthnicity + Sex + Age_2020 + (1|Study/ID), data=ALL_int_self_minusone_race)
  depr_loglike <- anova(depr_r_m1,depr_race_m1)
  pvals_race[rightrow,1] <- depr_loglike$`Pr(>Chisq)`[2]
  anx_r_m1 <- lmerTest::lmer(Anxiety ~ PrePost + Sex + Age_2020 + (1|Study/ID), data=ALL_int_self_minusone_race)
  anx_race_m1 <- lmerTest::lmer(Anxiety ~ PrePost*RaceEthnicity + Sex + Age_2020 + (1|Study/ID), data=ALL_int_self_minusone_race)
  anx_loglike <- anova(anx_r_m1,anx_race_m1)
  pvals_race[rightrow,2] <- anx_loglike$`Pr(>Chisq)`[2]
  
  depr_case_m1 <- lmerTest::lmer(Depression ~ PrePost*mean_cases + Age_2020 + Sex + (1|Study/ID), data=ALL_int_self_minusone_burdend)
  anx_case_m1 <- lmerTest::lmer(Anxiety ~ PrePost*mean_cases + Age_2020 + Sex + (1|Study/ID), data=ALL_int_self_minusone_burdena)
  pvals_case[rightrow,1] <- summary(depr_case_m1)$coefficients["PrePostDuring:mean_cases",5]
  pvals_case[rightrow,2] <- summary(anx_case_m1)$coefficients["PrePostDuring:mean_cases",5]

  depr_death_m1 <- lmerTest::lmer(Depression ~ PrePost*mean_deaths + Age_2020 + Sex + (1|Study/ID), data=ALL_int_self_minusone_burdend)
  anx_death_m1 <- lmerTest::lmer(Anxiety ~ PrePost*mean_deaths + Age_2020 + Sex + (1|Study/ID), data=ALL_int_self_minusone_burdena)
  pvals_death[rightrow,1] <- summary(depr_death_m1)$coefficients["PrePostDuring:mean_deaths",5]
  pvals_death[rightrow,2] <- summary(anx_death_m1)$coefficients["PrePostDuring:mean_deaths",5]
  
  depr_restr_m1 <- lmerTest::lmer(Depression ~ PrePost*median_Strictness + Age_2020 + Sex + (1|Study/ID), data=ALL_int_self_minusone_restr)
  anx_restr_m1 <- lmerTest::lmer(Anxiety ~ PrePost*median_Strictness + Age_2020 + Sex + (1|Study/ID), data=ALL_int_self_minusone_restr)
  pvals_restr[rightrow,1] <- summary(depr_restr_m1)$coefficients["PrePostDuring:median_Strictness",5]
  pvals_restr[rightrow,2] <- summary(anx_restr_m1)$coefficients["PrePostDuring:median_Strictness",5]
  
}

