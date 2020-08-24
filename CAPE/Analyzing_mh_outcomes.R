##################################
## This script analyzing data from the CAPE collaborative project
## It examines changes in MH symptoms and 
## its moderation by age, race, disease burden and government restrictions.
##
##################################


# Figure out why the early TIGER assessments had such high symptoms: older adolescents were recruited earlier
# Do a 'most recent pre-' versus 'mid-' repeated meas t-test? Would lose a lot of data.. OR the comparing mean/median within person pre and post?
# Do checks because of skewness of symptom variables
# Test Anxiety subscales: 
#          KLG: self SCARED (1,6), parent SCARED (1,2,6), subscales social,panic,separation,general,school
#          LIS: RCADS subscales social,panic,separation,general,obses-comp
#          TGR: pre: MASC2_Total subscales social,sepration,harm av,phys s
#          SDS: RCADS general

##################################
## 1. Load packages and data
##
##################################
library("dplyr")
library("tidyr")
library("ggplot2")
library("lme4")
cape_dir ="C:/Users/marjo/Documents/postdoc/ACE/CAPE/"
ARC_mh <- read.csv(paste0(cape_dir,"Outcome_data/ARC_CAPE_outcomes.csv"),header = T)
KLG_mh <- read.csv(paste0(cape_dir,"Outcome_data/KLG_CAPE_Outcomes.csv"),header = T) #### THIS IS NOT IN POMS YET!!
LIS_mh <- read.csv(paste0(cape_dir,"Outcome_data/LIS_MHOutcomeS_CAPE.csv"),header = T)
MFS_mh <- read.csv(paste0(cape_dir,"Outcome_data/MFS_outcomes_final.csv"),header = T)
SDS_mh <- read.csv(paste0(cape_dir,"Outcome_data/SDS_CAPE_Outcomes.csv"),header = T)
TAG_mh <- read.csv(paste0(cape_dir,"Outcome_data/TAG_CAPE_Outcome_Data.csv"),header = T)
TGR_mh <- read.csv(paste0(cape_dir,"Outcome_data/TGR_Outcomes_Data_8.14.20.csv"),header = T)

####################################
## 2. Reformat and merge data 
## 
##
####################################

#Selecting the questionnaires that measure depressive or anxiety symptoms by self-report 
# and focus on measures that were completed both pre- and mid-pandemic
# For anxiety, TAG and TIGER had different measures pre versus mid pandemic (TAG:SCARED abd GAD7, TIGER MASC2 and GAD7)
# These are still combined below


ARC_int <- pivot_longer(ARC_mh, cols=4:length(ARC_mh), names_to = c(".value","time"),
                         names_pattern = "(.*)_(.*)") %>%
            select(Study,ID,Sex,DOC,Age,PromisDepression_POMS,PromisAnxiety_POMS) %>%
            filter(!is.na(PromisDepression_POMS & PromisAnxiety_POMS)) %>%
            rename(Depression=PromisDepression_POMS,Anxiety=PromisAnxiety_POMS)


KLG_mh <- KLG_mh %>% rename("ChildSelfReportChildrenDepressionInventory_time1"="ChildSelfReportChildrenDepressionInventoryII_time1")
KLG_int <- pivot_longer(KLG_mh, cols=4:length(KLG_mh), names_to = c(".value","time"),
                         names_pattern = "(.*)_(.*)") %>%
            select(Study,ID,ChildSex,DOC,ChildAge,ChildSelfReportChildrenDepressionInventory,ChildSelfReportSCARED) %>%
           filter(!is.na(ChildSelfReportChildrenDepressionInventory & ChildSelfReportSCARED),Study=="KLG") %>%
           rename(Sex=ChildSex,Age=ChildAge,Depression=ChildSelfReportChildrenDepressionInventory,Anxiety=ChildSelfReportSCARED) %>%
           mutate(Sex="F",Depression=Depression/24,Anxiety=Anxiety/82)

names(LIS_mh)[1] <- "Study"
LIS_int <- pivot_longer(LIS_mh, cols=4:length(LIS_mh), names_to = c(".value","time"),
                         names_pattern = "(.*)_(.*)") %>%
            select(Study,LISID,Sex,DOC,Age,RCAa_DEM_POMS,RCAa_TotalAnxM_POMS) %>%
            filter(!is.na(RCAa_DEM_POMS & RCAa_TotalAnxM_POMS)) %>%
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
            filter(!is.na(POMS_CESD & POMS_RCMAS)) %>%
            rename(Depression=POMS_CESD,Anxiety=POMS_RCMAS,Age=age)

SDS_int <- SDS_mh %>% rename(POM_RCADS_GAD_time1=RCADS_GeneralizedAnxiety_Time1_POM,
                             POM_RCADS_GAD_time2=RCADS_GeneralizedAnxiety_Time2_POM) %>%
          pivot_longer(., cols=4:length(SDS_mh), names_to = c(".value","time"),
                        names_pattern = "(.*)_(.*)") %>%
          select(Study,ID,Sex,DOC,Age,POM_RCADS_GAD) %>%
          filter(!is.na(POM_RCADS_GAD)) %>%
          mutate(Depression=NA) %>%
          rename(Anxiety=POM_RCADS_GAD)

TAG_mh <- TAG_mh %>% rename(Anxiety_time2=SCARED_POMS_time2, Anxiety_time4=SCARED_POMS_time4,
                            Anxiety_time6=SCARED_POMS_time6,Anxiety_time7=GAD7_POMS_time7)
TAG_int <- pivot_longer(TAG_mh, cols=4:length(TAG_mh), names_to = c(".value","time"),
                         names_pattern = "(.*)_(.*)") %>%
            select(Study,ID,Sex,DOC,Age,CESDC_POMS,Anxiety) %>%
            filter(!is.na(CESDC_POMS)) %>%
            mutate(Sex="F") %>%
            rename(Depression=CESDC_POMS)

names(TGR_mh)[1] <- "ID"
TGR_mh$Study <- "TGR"        
TGR_mh <- TGR_mh %>% rename(MASC2_Total_timeX=GAD7_Total_timeX) #Combining MASC2 (pre-pandemic) and GAD7 (midpandemic)
TGR_int <- TGR_mh %>% select(Study, everything()) %>%
            mutate(ID=sprintf("TGR%03d", ID)) %>%
            pivot_longer(., cols=4:length(TGR_mh), names_to = c(".value","time"), 
                         names_pattern = "(.*)_(.*)") %>%
            select(Study,ID,ChildSex,DOC,Age,PHQ9_Total,MASC2_Total) %>%
            filter(!is.na(PHQ9_Total)) %>%
            rename(Depression=PHQ9_Total,Anxiety=MASC2_Total,Sex=ChildSex)

#Merge
ALL_int_self <- rbind(ARC_int,KLG_int,LIS_int,MFS_int,SDS_int,TAG_int,TGR_int)
ALL_int_self$DOC <- parse_date_time(ALL_int_self$DOC,
                                          orders = c("m/d/y","m/d/Y"),
                                          locale = "eng")



# Remove date in the future
# and Make pre- vs post-pandemic variable (pre is before 11 March 2020, that is when WHO declared a pandemic)
ALL_int_self <- ALL_int_self %>% 
  filter(DOC<="2020-11-01") %>%
  mutate(Sex=ifelse(Sex=="M","Male",ifelse(Sex=="F","Female",NA)),
         PrePost=as.factor(ifelse(DOC<"2020-03-11","Pre",ifelse(DOC>"2020-03-10","Mid",NA))))

# create Age_2020 (Age in March 2020)
# Filter out participants younger than 9 or older than 18 
Age_2020 <- ALL_int_self %>%
  mutate(Age_2020=round(as.numeric(as.Date("2020-03-11")-as.Date(DOC))/365.25+Age,2)) %>%
  group_by(ID) %>%
  summarize(Age_2020=round(mean(Age_2020),1))
ALL_int_self <- merge(ALL_int_self, Age_2020,by="ID")
ALL_int_self <- ALL_int_self %>% 
  filter(Age_2020<18.0&Age_2020>7.99)

data_for_plot <- ALL_int_self %>% mutate(Age_2gr=case_when(Age_2020 > 14.99 ~ "later",
                                                                Age_2020 < 15.0 ~ "earlier"))

# Only studies that used the same questionnaire pre- and mid-pandemic
ALL_anx_self_same <- ALL_int_self %>% filter(Study!="TAG"&Study!="TGR") %>%
  dplyr::select(!Depression)
ALL_anx_self_tagtiger_post <- ALL_int_self %>% filter(!(Study=="TAG"&PrePost=="Pre")) %>%
  filter(!(Study=="TGR"&PrePost=="Pre")) %>% dplyr::select(!Depression)
ALL_anx_self_tagtiger_pre <- ALL_int_self %>% filter(!(Study=="TAG"&PrePost=="Mid")) %>%
  filter(!(Study=="TGR"&PrePost=="Mid")) %>% dplyr::select(!Depression)

###########################
# 3. Plotting
#
###########################

############## Depression 

ggplot(ALL_int_self,aes(x=DOC,y=Depression)) + 
  geom_point(aes(color=Study)) + 
  geom_line(aes(group=ID)) + 
  geom_smooth(method="loess") + 
  theme_minimal()+theme(legend.position = "none") + 
  labs(x="Date of self-report", y="Depressive symptoms")

ggplot(ALL_int_self,aes(x=as.Date(DOC),y=Depression)) + 
  geom_point(aes(color=Study)) + 
  geom_smooth(method="loess",aes(color=Study),se=F) + 
  theme_minimal() + 
  labs(x="Date of self-report", y="Depressive symptoms")

ALL_int_self$PrePost <- factor(ALL_int_self$PrePost, levels=c("Pre","Mid"))
ggplot(ALL_int_self,aes(x=PrePost,y=Depression)) + 
  geom_dotplot(binaxis = "y", stackdir = "center",binwidth=0.01,aes(color=Study)) + 
  geom_violin(fill="none") +
  theme_minimal()+ 
  labs(x="Pre- versus mid-pandemic", y="Depressive symptoms")

ggplot(data_for_plot,aes(x=as.Date(DOC),y=Depression)) + 
  geom_point(aes(color=Age_2020)) + 
  scale_color_gradient(low="yellow",high="red") +
  geom_smooth(method="loess",aes(group=Age_2gr,fill=Age_2gr)) +
  scale_fill_manual(values=c("yellow","red")) +
  theme_minimal() +
  labs(x="Date of self-report", y="Depressive symptoms") 

############## Anxiety 

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

ALL_int_self$PrePost <- factor(ALL_int_self$PrePost, levels=c("Pre","Mid"))
ggplot(ALL_int_self,aes(x=PrePost,y=Anxiety)) + 
  geom_dotplot(binaxis = "y", stackdir = "center",binwidth=0.01,aes(color=Study)) + 
  geom_violin(fill="none") +
  theme_minimal()+ 
  labs(x="Pre- versus mid-pandemic", y="Anxiety symptoms")

ggplot(data_for_plot,aes(x=as.Date(DOC),y=Anxiety)) + 
  geom_point(aes(color=Age_2020)) + 
  scale_color_gradient(low="yellow",high="red") +
  geom_smooth(method="loess",aes(group=Age_2gr,fill=Age_2gr)) +
  scale_fill_manual(values=c("yellow","red")) +
  theme_minimal() +
  labs(x="Date of self-report", y="Anxiety symptoms") 
  

###########################
# 4. Modeling change over time and age moderation
#
###########################

############## Depression 

#Mixed model testing with DOC
depr_base <- lmerTest::lmer(Depression ~ Sex + (1|Study/ID), data=ALL_int_self)
depr_doc <- lmerTest::lmer(Depression ~ as.Date(DOC,origin="2016-01-01") + Sex + (1|Study/ID), data=ALL_int_self)
depr_docexp <- lmerTest::lmer(log(Depression+1) ~ as.Date(DOC,origin="2016-01-01") + Sex + (1|Study/ID), data=ALL_int_self)
depr_doc_age <- lmerTest::lmer(Depression ~ scale(as.Date(DOC,origin="2016-01-01"))*scale(Age_2020) + Sex + (1|Study/ID), data=ALL_int_self)

anova(depr_base,depr_doc)
anova(depr_doc,depr_doc_age)

# Based on the plots, continuous DOC might not be the best predictor variable 
# (there is variation pre-pandemic because of recruitment strategies and different lengths of studies)
# Therefore, testing a binary pre- vs mid-pandemic variable, with the cutoff at 2020-03-11

#Mixed model testing with Pre vs Post
depr_doc_2 <- lmerTest::lmer(Depression ~ PrePost + Sex + (1|Study/ID), data=ALL_int_self)
depr_doc_age_2 <- lmerTest::lmer(Depression ~ PrePost*scale(Age_2020) + Sex + (1|Study/ID), data=ALL_int_self)

anova(depr_base,depr_doc_2)
anova(depr_doc_2,depr_doc_age_2)

############## Anxiety

#Mixed model testing with DOC
anx_base <- lmerTest::lmer(Anxiety ~ Sex + (1|Study/ID), data=ALL_int_self)
anx_doc <- lmerTest::lmer(Anxiety ~ as.Date(DOC,origin="2016-01-01") + Sex + (1|Study/ID), data=ALL_int_self)
anx_docexp <- lmerTest::lmer(log(Anxiety+1) ~ as.Date(DOC,origin="2016-01-01") + Sex + (1|Study/ID), data=ALL_int_self)
anx_doc_age <- lmerTest::lmer(Anxiety ~ scale(as.Date(DOC,origin="2016-01-01"))*scale(Age_2020) + Sex + (1|Study/ID), data=ALL_int_self)

anova(anx_base,anx_doc)
anova(anx_doc,anx_doc_age)

#Mixed model testing with Pre vs Post
anx_doc_2 <- lmerTest::lmer(Anxiety ~ PrePost + Sex + (1|Study/ID), data=ALL_int_self)
anx_doc_age_2 <- lmerTest::lmer(Anxiety ~ PrePost*scale(Age_2020) + Sex + (1|Study/ID), data=ALL_int_self)

anova(anx_base,anx_doc_2)
anova(anx_doc_2,anx_doc_age_2)

#Mixed model testing with Pre vs Post REMOVING TAG AND TIGER
anx_base_s <- lmerTest::lmer(Anxiety ~ Sex + (1|Study/ID), data=ALL_anx_self_same)
anx_doc_s <- lmerTest::lmer(Anxiety ~ PrePost + Sex + (1|Study/ID), data=ALL_anx_self_same)
anx_doc_age_s <- lmerTest::lmer(Anxiety ~ PrePost*scale(Age_2020) + Sex + (1|Study/ID), data=ALL_anx_self_same)

anova(anx_base_s,anx_doc_s)
anova(anx_doc_s,anx_doc_age_s)

###########################
# 5. Effect sizes
#
###########################

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



