---
title: "Data cleaning"
author: "Mengfan Luo"
date: "11/11/2021"
output: github_document
---




## Data loading and crude cleaning

We select year 2014-2016 and variables relating to smoking and insurance. 


```r
chs16 = read_sas("data/chs2016_public.sas7bdat")

chs16_filter = chs16 %>% 
  select(agegroup,generalhealth,insuredgateway16,insure16,insured,insure5,sickadvice16,sickplace,didntgetcare16,smoker,everyday,numberperdaya,cost20cigarettes,imputed_povertygroup ,bmi,child,sex) %>% 
  mutate(year = 2016) %>% 
  rename(insuredgateway = insuredgateway16, insure = insure16,sickadvice = sickadvice16,didntgetcare = didntgetcare16)


chs15 = read_sas("data/chs2015_public.sas7bdat")

chs15_filter = chs15 %>% 
  select(agegroup,generalhealth,insuredgateway15,insure15,insured,insure5,sickadvice15,sickplace,didntgetcare15,smoker,everyday,numberperdaya,cost20cigarettes,imputed_povertygroup ,bmi,child,sex) %>% 
  mutate(year = 2015) %>% 
  rename(insuredgateway = insuredgateway15, insure = insure15,sickadvice = sickadvice15,didntgetcare = didntgetcare15)


chs14 = read_sas("data/chs2014_public.sas7bdat")

chs14_filter = chs14 %>% 
  select(agegroup,generalhealth,insuredgateway14,insure14,insured,insure5,sickadvice14,sickplace,didntgetcare14,smoker,everyday,numberperdaya,cost20cigarettes,imputed_povertygroup ,bmi,child,sex) %>% 
  mutate(year = 2014) %>% 
  rename(insuredgateway = insuredgateway14, insure = insure14,sickadvice = sickadvice14,didntgetcare = didntgetcare14)


chs_14_16 = bind_rows(chs14_filter,chs15_filter,chs16_filter)
```



```r
dataset_basic = chs_14_16 %>% 
  select(agegroup,insure,smoker,everyday,numberperdaya,cost20cigarettes,generalhealth,imputed_povertygroup,bmi,child,sex) %>% 
  mutate(numberperdaya = round(numberperdaya,1),
         imputed_povertygroup = factor(imputed_povertygroup),
         child = factor(child,levels = c(1,2)),
         sex = factor(sex),
         insure = factor(insure,levels = c(1,2,3,4,5,6,7)),
         generalhealth = factor(generalhealth,levels = c(5,4,3,2,1)),
         smoker = factor(smoker,levels = c(1,2,3)),
         everyday = factor(everyday),
         agegroup = factor(agegroup,ordered = TRUE),
         imputed_povertygroup = factor(imputed_povertygroup,levels = c(1,2,3,4,5))
         )
#write_csv(dataset_basic,"data/dataset_basic.csv")
```







