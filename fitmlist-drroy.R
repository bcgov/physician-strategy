library(hsiaR)
library(dplyr)
library(dtplyr)
library(readxl)
library(stringr)
library(dtplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(dendextend)
library(ggdendro)
library(ggsci)
library(mclust)
library(dbscan)



# LTC
# Longtitidanal
# Maternity
# Inpatient
# Anesthesia
# ED
# Sports med?
# General ?

con = hiConnect(use_dialog_box=T)
myschema = con@info[["username"]]

#fitm list using encounters definition, and core_can table
fitm_list2=hiQuery(use_local_tables=F,query="SELECT distinct
  servloc, msp.fitm, fitmdesc
FROM ahip.AR_MSPCLM_CORE_CAN msp
  left join MSEA_TEAM_LVL2.LAB_PROVIDER lab on
    msp.payenum=LPAD(lab.payenum, 5, '0')
  left join ahip.cb_dtl_dm_srv_date_vw dt on
    msp.servdt = dt.srv_date
  left join ahip.CB_DTL_FT_PRACRSTFYR_VWD pr on
    dt.SRV_FISC_YR=pr.FISC_YR and
    pracnum=PRAC_BLLG_NUM
  left join ahip.fitmds fitm on
    msp.fitm=fitm.fitm
where
/* FPs only*/
  (case when FUNC_SPTY_cd='UNKNOWN' then rcnt_bllg_spty_cd_1 else FUNC_SPTY_cd end) in ('00','50','76') and
  pracnum is not null and
  (clmtp in ('MM','MA','MB','MH','MN','MS','PM') or (servcd=13 and clmtp = 'PB')) AND
  msp.fitm not in (15501,15601) and
  ( ( PAYESTAT IN ('Y','F') AND (ENCTR_CLM_MSPD IS NOT NULL OR (ENCTR_CLM_MSPD IS NULL AND PAIDSERV >0) )) OR (PAYESTAT NOT IN ('Y','F') AND PAIDSERV >0) ) AND
  to_number(to_char(servdt,'YYYY'))>2009
/* Remove lab services of lab providers*/
   AND
  not (lab.payenum is not null and ((servcd = 93 and payestat in ('C', 'H', 'L')) or (servcd = 94 and msp.fitm <> 90665) or (servcd = 94 and msp.fitm = 90665 and payestat in ('C', 'H', 'L')) or (servcd = 98 and msp.fitm in (00012,90000) and payestat in ('C', 'H', 'L'))) )
/* Remove registration and 15min codes*/
   AND
  NOT( (fitmdesc like '%LFP%' and fitmdesc like '%15 min%') or fitmdesc like '%INCENTIVE%' or fitmdesc like '%PCN PANEL%' or fitmdesc like '%PRIMARY CARE PANE%' or fitmdesc like '%PCN ATTACH%' or fitmdesc like '%PCN DETACH%' or fitmdesc like '%PCN PANEL%' or fitmdesc like '%MANAGEMENT FEE%' or fitmdesc like '%CARE TIME-PER 15 MIN%' or fitmdesc like '%ENROLMENT CODE%' or fitmdesc like '%REGISTRATION CODE%' OR fitmdesc like '%TRANSITION CODE%' OR fitmdesc like '%PREMIUM%' )
/* Remove opioid management*/
   AND
  msp.fitm<>39 AND
  NOT servcd in (12,17) AND
  msp.fitm not in (98111,98112)

")%>%rename_with(tolower)


#MENTAL HEALTH
MHfitm=unique((fitm_list2%>%
          filter(str_detect(fitmdesc, "MENTAL")|
                 str_detect(fitmdesc, "OPIOID")|
                 str_detect(fitmdesc, "DRUG"))
        )$fitmdesc)
#EMERGENCY DEPARTMENT
EDfitm1=unique((fitm_list2%>%filter(str_detect(fitmdesc, "EMERGENCY CARE")|
                                   str_detect(fitmdesc, "EMERGENCY MEDICINE")|
                                   str_detect(fitmdesc,"VISIT, EMERGENCY")))$fitmdesc)
#ANESTHESIA
ANfitm1=unique((fitm_list2%>%filter(str_detect(fitmdesc, "ANESTHESIA")|
                                   str_detect(fitmdesc, "EPIDURAL")|
                                   str_detect(fitmdesc, "ANAES") ,
                                   !str_detect(fitmdesc, " NO ANAESTHESIA")
                                   #,!str_detect(fitmdesc, "UNDER ANESTHESIA")
                                  ))$fitmdesc)
#should "Under anaesthesia" be included or not?"

#Pain management
PMfitm=unique((fitm_list2%>%filter(str_detect(fitmdesc, "TRIGGER POINT INJECTION")|
                                  str_detect(fitmdesc, "PERIPHERAL NERVE BLOCK")|
                                  str_detect(fitmdesc, "NEUROTOMY")
))$fitmdesc)

#SURGICAL ASSIST
SURGfitm1=unique((fitm_list2%>%filter(str_detect(fitmdesc, "SURGICAL ASSIST")))$fitmdesc)
#ADVICE
ADVfitm=unique((fitm_list2%>%filter(str_detect(fitmdesc, "ADVICE")|
                                   str_detect(fitmdesc, "CONSULT EXP")|
                                   str_detect(fitmdesc, "CONFERENCE")|
                                   str_detect(fitmdesc, "FP BRIEF CLIN. CONF W/ ALL. CARE PROV. AND/OR PHYS"))  )$fitmdesc)
#Not a Visit
NaVfitm=unique((fitm_list2%>%filter(str_detect(fitmdesc, "INCENTIVE")|
                                   str_detect(fitmdesc, "PCN PANEL RPT: GENERAL")|
                                   str_detect(fitmdesc, "PRIMARY CARE PANEL REPORT")|
                                   str_detect(fitmdesc, "PCN ATTACH RPT")|
                                   str_detect(fitmdesc, "PCN DETACH RPT")|
                                   str_detect(fitmdesc, "CARE TIME-PER 15 MIN")|
                                   str_detect(fitmdesc, "MANAGEMENT FEE")|
                                   str_detect(fitmdesc, "REGISTRATION CODE")|
                                   #(str_detect(fitmdesc, 'CONTRACTED CLINICAL SHIFT') &str_detect(fitmdesc, 'PER 15 MIN'))|
                                   (str_detect(fitmdesc, 'LFP') &str_detect(fitmdesc, '15 MIN'))|
                                   str_detect(fitmdesc, "ENROLMENT CODE")
                                     )  )$fitm)
NaVfitm=c(NaVfitm,98001,98002,98003,98004,98006)

#Dermatology
Dermfitm=unique((fitm_list2%>%filter(str_detect(fitmdesc, "WARTS")|
                                     str_detect(fitmdesc, "ULTRA VIOLET B TREATMENT")|
                                     str_detect(fitmdesc, "EXCISION TUMOR OF SKIN")|
                                    str_detect(fitmdesc, "EXCISION ADDITIONAL TUMOR OF SKIN")
)  )$fitmdesc)


#unique((fitm_list%>%filter(str_detect(fitmdesc, "PHYSICAL")))$fitmdesc)

#REFERRAL
REFfitm=unique((fitm_list2%>%filter(str_detect(fitmdesc, "REFERRAL")))$fitmdesc)
#TRAY
TRAYfitm=unique((fitm_list2%>%filter(str_detect(fitmdesc, "TRAY")))$fitmdesc)

# see ones fitm unique to a location
df_unique <- fitm_list2 %>%
  distinct(servloc, fitm, fitmdesc) %>%         # Remove duplicates
  group_by(fitm, fitmdesc) %>%
  summarise(
    n_servloc = n_distinct(servloc),
    .groups = 'drop'
  ) %>%
  filter(n_servloc == 1) %>%
  left_join(fitm_list2, by = c("fitm", "fitmdesc"))     # Join back to get full rows

# View the results for ED
EDfitm2=(df_unique%>%filter(servloc=='E'))
Inpfitm2=(df_unique%>%filter(servloc=='I'))




#fitm list related to MATERNITY
#add LFP maternity for future years!
fitm_mat1=hiQuery(use_local_tables=F,query="select fitm from youmin_ding_lvl2.REF_MAT_FEE_CODE_PHSA_MOH  a
left join ahip.fitmds b on a.fee_item=b.fitm")%>%rename_with(tolower)
fitm_mat2=unique((fitm_list2%>%filter(fitm>98000,str_detect(fitmdesc, "BIRTH")|
                                                            str_detect(fitmdesc, "PREG")
)  )$fitm)


fitm_mat=c(fitm_mat1$fitm,fitm_mat2,119,790)

#fitm from LTC or inpatient
fitm_inpLTC=hiQuery(use_local_tables=F,query="select servloc,a.fitm as fitm,setting as setting2 from JIANLONG_SUN_LVL2.FITM_DOBC_1129 a
left join ahip.fitmds b on a.fitm=b.fitm")%>%rename_with(tolower)%>%filter(setting2!='Maternal')


fitm_inpLTC_All=fitm_inpLTC%>%filter(servloc=="All"|servloc=="All-C")%>%select(-servloc)%>%
rbind(.,
  fitm_list2%>%filter(str_detect(fitmdesc, "ON CALL, ON SITE")|str_detect(fitmdesc,"LFP INPATIENT")|str_detect(fitmdesc,"LFP LOCUM INPATIENT"))%>%
    mutate(setting2="Inpatient")%>%select(fitm,setting2)%>%unique())%>%rbind(.,
          fitm_list2%>%filter(str_detect(fitmdesc, "LFP LTC")|str_detect(fitmdesc, "LFP LOCUM LTC"))%>%
            mutate(setting2="LTC")%>%select(fitm,setting2)%>%unique()
)

fitm_inpLTC_C=fitm_inpLTC%>%filter(servloc=="C")
fitm_inpLTC_I=fitm_inpLTC%>%filter(servloc=="I")



#fitm from PRIMARY CARE
fitm_pc=read_excel("../data/Copy of Primary Care Services Fee Items.xlsx")%>%
  mutate(fitm=as.numeric(FITM_CD))
#add contraceptive
fitm_pc=c(fitm_pc$fitm,14542,14540)

#merge fitms
# add All to each group to account for blank servloc later
fitm_list_all <- fitm_list2 %>% mutate(servloc=ifelse(is.na(servloc),'Blank',servloc))

merged=fitm_list_all%>%
  #LTC, Inpatient
  merge(fitm_inpLTC_All,by=c("fitm"), all.x = TRUE)%>%
  merge(fitm_inpLTC_C,by=c("fitm","servloc"), all.x = TRUE)%>%
  merge(fitm_inpLTC_I,by=c("fitm","servloc"), all.x = TRUE)%>%
  mutate(setting2=coalesce(setting2.y,setting2.x,setting2))%>%
  select(fitm,servloc,fitmdesc, setting2)%>%
  mutate(settingMat=ifelse(fitm %in% fitm_mat,"Maternity",NA),
         settingPC=ifelse(fitm %in% fitm_pc,"primary",NA),
         settingMH=ifelse(fitmdesc %in% MHfitm,"Mental Health",NA),
         settingED=ifelse(fitmdesc %in% c(EDfitm1,EDfitm2$fitmdesc),"Emergency Department", NA),
         settingANAS=ifelse(fitmdesc %in% ANfitm1,"Anasthesia", NA),
         settingSURG=ifelse(fitmdesc %in% SURGfitm1,"Surgical", NA),
         settingREF=ifelse(fitmdesc %in% REFfitm,"Referral", NA),
         settingADV=ifelse(fitmdesc %in% ADVfitm,"Advice", NA),
         settingTRAY=ifelse(fitmdesc %in% TRAYfitm,"Tray", NA),
         settingNaV=ifelse(fitm %in% NaVfitm,"Not a Visit", NA),
         settingPM=ifelse(fitmdesc %in% PMfitm,"Pain Management", NA),
         settingDerm=ifelse(fitmdesc %in% Dermfitm,"Dermatology", NA)
  )%>%
  mutate(settingEND=coalesce(settingMat,settingSURG,settingMH,settingANAS,settingED,setting2,settingPC,settingNaV,settingPM,settingDerm,settingTRAY,settingADV,settingREF))%>%
  mutate(settingEND=case_when(settingEND=="Inpatient"~"I",
                              settingEND=="Mental Health"~"Me",
                              settingEND=="primary"~"PC",
                              settingEND=="LTC"~"L",
                              settingEND=="Maternity"~"Ma",
                              settingEND=="Emergency Department"~"ED",
                              settingEND=="Surgical"~"S",
                              settingEND=="Anasthesia"~"An",
                              settingEND=="Pain Management"~"PM",
                              settingEND=="Dermatology"~"DERM",
                              settingEND=="Advice"~"Ad",
                              settingEND=="Referral"~"R",
                              settingEND=="Tray"~"T",
                              settingEND=="Not a Visit"~"NaV"))

hiWriteTables(names ="FitmSrvLoc",values = as.data.frame(merged),schema = myschema,overwrite = TRUE)
hiWriteTables(names ="FitmSrvLoc",values = as.data.frame(merged),team_schema = "msea",overwrite = TRUE)

##############################################
##ASSIGN ALL VISITS
##############################################
#with encounter definition
allAssign2=hiQuery(use_local_tables=F,query="
                   with AllData as (

SELECT pracnum,
case when FUNC_SPTY_cd='UNKNOWN' then rcnt_bllg_spty_cd_1 else FUNC_SPTY_cd end as funcspec, servdt,SRV_FISC_YR_CD, clnt_label,
listagg(settingend,', ') within group (order by settingend) as all_settings
FROM ahip.AR_MSPCLM_CORE_CAN msp
  left join MSEA_TEAM_LVL2.LAB_PROVIDER lab on
    msp.payenum=LPAD(lab.payenum, 5, '0')
  left join ahip.cb_dtl_dm_srv_date_vw dt on
    msp.servdt = dt.srv_date
  left join ahip.CB_DTL_FT_PRACRSTFYR_VWD pr on
    dt.SRV_FISC_YR=pr.FISC_YR and
    pracnum=PRAC_BLLG_NUM
    left join sandra_roy_lvl2.FitmSrvLoc fisr on
    fisr.fitm=msp.fitm and
    fisr.servloc=coalesce(msp.servloc,'Blank')
where
/* FPs only*/
  (case when FUNC_SPTY_cd='UNKNOWN' then rcnt_bllg_spty_cd_1 else FUNC_SPTY_cd end) in ('00','50','76') and
  pracnum is not null and
  (clmtp in ('MM','MA','MB','MH','MN','MS','PM') or (servcd=13 and clmtp = 'PB')) AND
  msp.fitm not in (15501,15601) and
  ( ( PAYESTAT IN ('Y','F') AND (ENCTR_CLM_MSPD IS NOT NULL OR (ENCTR_CLM_MSPD IS NULL AND PAIDSERV >0) )) OR (PAYESTAT NOT IN ('Y','F') AND PAIDSERV >0) ) AND
  to_number(to_char(servdt,'YYYY'))>2009
/* Remove lab services of lab providers*/
   AND
  not (lab.payenum is not null and ((servcd = 93 and payestat in ('C', 'H', 'L')) or (servcd = 94 and msp.fitm <> 90665) or (servcd = 94 and msp.fitm = 90665 and payestat in ('C', 'H', 'L')) or (servcd = 98 and msp.fitm in (00012,90000) and payestat in ('C', 'H', 'L'))) )
/* Remove registration and 15min codes*/
   AND
  NOT( (fitmdesc like '%LFP%' and fitmdesc like '%15 min%') or fitmdesc like '%INCENTIVE%' or fitmdesc like '%PCN PANEL%' or fitmdesc like '%PRIMARY CARE PANE%' or fitmdesc like '%PCN ATTACH%' or fitmdesc like '%PCN DETACH%' or fitmdesc like '%PCN PANEL%' or fitmdesc like '%MANAGEMENT FEE%' or fitmdesc like '%CARE TIME-PER 15 MIN%' or fitmdesc like '%ENROLMENT CODE%' or fitmdesc like '%REGISTRATION CODE%' OR fitmdesc like '%TRANSITION CODE%' OR fitmdesc like '%PREMIUM%' )
/* Remove opioid management*/
   AND
  msp.fitm<>39 AND
  NOT servcd in (12,17) AND
  msp.fitm not in (98111,98112)
group by
  pracnum, case when FUNC_SPTY_cd='UNKNOWN' then rcnt_bllg_spty_cd_1 else FUNC_SPTY_cd end, servdt,SRV_FISC_YR_CD, clnt_label
  )


  select pracnum,funcspec,SRV_FISC_YR_CD,count(*) as visits,
case
when ALL_SETTINGS like '%Ma%' then 'Maternity'
when ALL_SETTINGS like '%S%' then 'Surgery'
when ALL_SETTINGS like '%Me%' then 'Mental_Health'
when ALL_SETTINGS like '%An%' then 'Anasthesia'
when ALL_SETTINGS like '%ED%' then 'Emergency'
when ALL_SETTINGS like '%I%' then 'Inpatient'
when ALL_SETTINGS like '%L%' then 'Long_Term_Care'
when ALL_SETTINGS like '%PC%' then 'Primary_Care'
when ALL_SETTINGS like '%PM%' then 'Pain_Management'
when ALL_SETTINGS like '%DERM%' then 'Dermatology'
when ALL_SETTINGS like '%Ad%' then 'Advice'
when ALL_SETTINGS like '%R%' then 'Referral'
when ALL_SETTINGS like '%NaV%' then 'Not_a_Visit'
when ALL_SETTINGS like '%T%' then 'Tray'
else 'Unknown'
end as Setting
from AllData
group by pracnum,funcspec,SRV_FISC_YR_CD,
case
when ALL_SETTINGS like '%Ma%' then 'Maternity'
when ALL_SETTINGS like '%S%' then 'Surgery'
when ALL_SETTINGS like '%Me%' then 'Mental_Health'
when ALL_SETTINGS like '%An%' then 'Anasthesia'
when ALL_SETTINGS like '%ED%' then 'Emergency'
when ALL_SETTINGS like '%I%' then 'Inpatient'
when ALL_SETTINGS like '%L%' then 'Long_Term_Care'
when ALL_SETTINGS like '%PC%' then 'Primary_Care'
when ALL_SETTINGS like '%PM%' then 'Pain_Management'
when ALL_SETTINGS like '%DERM%' then 'Dermatology'
when ALL_SETTINGS like '%Ad%' then 'Advice'
when ALL_SETTINGS like '%R%' then 'Referral'
when ALL_SETTINGS like '%NaV%' then 'Not_a_Visit'
when ALL_SETTINGS like '%T%' then 'Tray'
else 'Unknown'
end

                   ")%>%rename_with(tolower)





############# Make table of %percentage #####################
# First, calculate % of services by setting per prac
#############################################################
df_percent <- allAssign2 %>%rename(fiscal=srv_fisc_yr_cd)%>%
  group_by(pracnum,fiscal,funcspec) %>%
  mutate(total_services = sum(visits)) %>%
  ungroup() %>%
  mutate(pct_services = visits / total_services * 100,
         pct_services=ifelse(pct_services==0,NA,pct_services))

#see majority
View(df_percent%>%
  filter(pct_services>50)%>%
  group_by(fiscal,setting)%>%
  summarize(ncount=n_distinct(pracnum))%>%
  pivot_wider(names_from=fiscal,values_from = ncount))

df_percent%>%
  group_by(fiscal)%>%
  summarize(ncount=n_distinct(pracnum))

# Now plot the histogram distribution of % of services for 1 year
ggplot(df_percent%>%
         filter(fiscal=='2020.2021'), aes(x = setting, y = pct_services,color=funcspec)) +
  geom_jitter(width = 0.2, alpha = 0.2) +
  labs(title = "Scatter Plot of % Services by Setting per Practitioner",
       y = "% of Services", x = "Setting") +
  theme_minimal()

# Plot for all years. but combine some groups

df_filtered <- df_percent %>%
  group_by(pracnum, fiscal) %>%
  slice_max(order_by = pct_services, n = 1, with_ties = FALSE) %>%
  ungroup()

df_summary <- df_filtered %>%
  group_by(fiscal, setting) %>%
  summarise(num_prac = n_distinct(pracnum), .groups = "drop") %>%
  group_by(fiscal) %>%
  mutate(pct_prac = num_prac / sum(num_prac) * 100)

ggplot(df_summary, aes(x = fiscal, y = pct_prac, fill = setting)) +
  geom_col(position = "stack") +
  scale_fill_viridis_d(option = "turbo") +
  labs(
    x = "Fiscal Year",
    y = "Percentage of Practitioners",
    fill = "Setting",
    title = "Percentage of Practitioners by Most used Setting and Fiscal Year"
  ) +
  theme_minimal()

#Total services setting
df_summary <- df_percent %>%
group_by(fiscal, setting) %>%
  summarise(sum_visit = sum(visits), .groups = "drop") %>%
  group_by(fiscal) %>%
  mutate(pct_visit = sum_visit / sum(sum_visit) * 100)

ggplot(df_summary, aes(x = fiscal, y = pct_visit, fill = setting)) +
  geom_col(position = "stack") +
  scale_fill_viridis_d(option = "turbo") +
  labs(
    x = "Fiscal Year",
    y = "Percentage of visits",
    fill = "Setting",
    title = "Type of visits used"
  ) +
  theme_minimal()



###############try another plot#################

# ---- 1. Physicians % (based on most-used setting) ----
physicians_pct <- df_percent %>%
  group_by(pracnum, fiscal) %>%
  slice_max(order_by = pct_services, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  group_by(fiscal, setting) %>%
  summarise(num_prac = n_distinct(pracnum), .groups = "drop") %>%
  group_by(fiscal) %>%
  mutate(percentage = num_prac / sum(num_prac) * 100,
         metric = "Physicians")

# ---- 2. Visits % (all visits by setting) ----
visits_pct <- df_percent %>%
  group_by(fiscal, setting) %>%
  summarise(total_visits = sum(visits), .groups = "drop") %>%
  group_by(fiscal) %>%
  mutate(percentage = total_visits / sum(total_visits) * 100,
         metric = "Visits")

# ---- 3. Combine ----
df_plot <- physicians_pct %>%
  select(fiscal, setting, percentage, metric) %>%
  bind_rows(
    visits_pct %>% select(fiscal, setting, percentage, metric)
  )
# ---- 4. Plot ----
ggplot(df_plot, aes(x = fiscal, y = percentage,
                    color = setting,
                    linetype = metric,
                    group = interaction(setting, metric))) +
  geom_line(size = 1) +
  labs(
    x = "Fiscal Year",
    y = "Percentage",
    color = "Setting",
    linetype = "Metric",
    title = "Physicians main setting vs total Visits Distribution by Setting"
  ) +
  scale_linetype_manual(values = c("Physicians" = "solid", "Visits" = "dashed")) +
  theme_minimal()+ylim(0,10)



#--This will show the number of fitms with no attached status, by fitms
noassignment=hiQuery(use_local_tables=F,query="
SELECT
  msp.servloc, msp.fitm, fitm.fitmdesc,count (distinct msp.pracnum||msp.servdt||msp.clnt_label) AS VISITS
FROM ahip.AR_MSPCLM_CORE_CAN msp
  left join MSEA_TEAM_LVL2.LAB_PROVIDER lab on
    msp.payenum=LPAD(lab.payenum, 5, '0')
  left join ahip.cb_dtl_dm_srv_date_vw dt on
    msp.servdt = dt.srv_date
  left join ahip.CB_DTL_FT_PRACRSTFYR_VWD pr on
    dt.SRV_FISC_YR=pr.FISC_YR and
    pracnum=PRAC_BLLG_NUM
  left join ahip.fitmds fitm on
    msp.fitm=fitm.fitm
  left join sandra_roy_lvl2.FitmSrvLoc fisr on
    fisr.fitm=msp.fitm and
    fisr.servloc=coalesce(msp.servloc,'Blank')
where
/* FPs only*/
  (case when FUNC_SPTY_cd='UNKNOWN' then rcnt_bllg_spty_cd_1 else FUNC_SPTY_cd end) in ('00','50','76') and
  pracnum is not null and
  (clmtp in ('MM','MA','MB','MH','MN','MS','PM') or (servcd=13 and clmtp = 'PB')) AND
  msp.fitm not in (15501,15601) and
  ( ( PAYESTAT IN ('Y','F') AND (ENCTR_CLM_MSPD IS NOT NULL OR (ENCTR_CLM_MSPD IS NULL AND PAIDSERV >0) )) OR (PAYESTAT NOT IN ('Y','F') AND PAIDSERV >0) ) AND
  to_number(to_char(servdt,'YYYY'))>2009
/* Remove lab services of lab providers*/
   AND
  not (lab.payenum is not null and ((servcd = 93 and payestat in ('C', 'H', 'L')) or (servcd = 94 and msp.fitm <> 90665) or (servcd = 94 and msp.fitm = 90665 and payestat in ('C', 'H', 'L')) or (servcd = 98 and msp.fitm in (00012,90000) and payestat in ('C', 'H', 'L'))) )
/* Remove registration and 15min codes*/
   AND
  NOT( (fitm.fitmdesc like '%LFP%' and fitm.fitmdesc like '%15 min%') or fitm.fitmdesc like '%INCENTIVE%' or fitm.fitmdesc like '%PCN PANEL%' or fitm.fitmdesc like '%PRIMARY CARE PANE%' or fitm.fitmdesc like '%PCN ATTACH%' or fitm.fitmdesc like '%PCN DETACH%' or fitm.fitmdesc like '%PCN PANEL%' or fitm.fitmdesc like '%MANAGEMENT FEE%' or fitm.fitmdesc like '%CARE TIME-PER 15 MIN%' or fitm.fitmdesc like '%ENROLMENT CODE%' or fitm.fitmdesc like '%REGISTRATION CODE%' OR fitm.fitmdesc like '%TRANSITION CODE%' OR fitm.fitmdesc like '%PREMIUM%' )
/* Remove opioid management*/
   AND
  msp.fitm<>39 AND
  NOT servcd in (12,17) AND
  msp.fitm not in (98111,98112)
  and settingend is null

  group by msp.servloc, msp.fitm, fitm.fitmdesc


                   ")%>%rename_with(tolower)

# Most common unassigned fitms
noassignment%>%arrange(desc(visits))%>%
  slice_head(n=20)



ggplot(df_bar, aes(x = cluster, y = mean_pct, fill = setting)) +
  geom_col(width = 0.7) +
  geom_text(
    data = cluster_counts,
    aes(x = cluster, y = 102, label = paste0("n = ", n_prac)),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 110), expand = c(0, 0)) +
  scale_fill_d3(palette = "category20") +
  labs(
    title = "Average % Services by Setting per Cluster (DBSCAN)",
    x = "Cluster", y = "% of Services", fill = "Setting"
  ) +
  theme_minimal()
