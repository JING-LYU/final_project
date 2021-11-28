Data cleaning
================
Mengfan Luo
11/11/2021

### Sample code given with the source data

    rm(list=ls(all=TRUE))
    library("haven")
    library("survey")
    library(dplyr)
    require(data.table)
    chs17<-read_sas("data/chs2017_public.sas7bdat")

    #city-wide estimates
    chs<-transform(chs17,strata=as.character(strata),all=as.factor(survey))

    #define the survey
    chs.dsgn<-svydesign(ids = ~1,strata = ~strata,weights=~wt18_dual,data = chs,nest = TRUE,na.rm=TRUE )
    #age adjusted survey
    pop.agecat4=c(0.128810, 0.401725, 0.299194, 0.170271)
    chs.stdes<-svystandardize(subset(chs.dsgn,diabetes17>0 ),by=~agegroup,over=~all,population=pop.agecat4,excluding.missing =~ agegroup+ ~all)

    #weighted N
    aggregate(chs17$wt18_dual, by=list(Category=chs17$diabetes17), FUN=sum)

    #crude prevalance estimates
    svyby(~diabetes17==1,~all,subset(chs.dsgn,diabetes17>0),svyciprop,vartype = "ci",method="xlogit",df=degf(chs.dsgn))
    svyby(~diabetes17==2,~all,subset(chs.dsgn,diabetes17>0),svyciprop,vartype = "ci",method="xlogit",df=degf(chs.dsgn))

    #age adjusted prevalance estimates

    svyby(~diabetes17==1,~all,chs.stdes,svyciprop,vartype = "ci",method="xlogit",df=degf(chs.dsgn))
    svyby(~diabetes17==2,~all,chs.stdes,svyciprop,vartype = "ci",method="xlogit",df=degf(chs.dsgn))

    #estimate by sex
    chs<-transform(chs17,strata=as.character(strata),allsex2=as.factor(sex))

    #define the survey
    chs.dsgn<-svydesign(ids = ~1,strata = ~strata,weights=~wt18_dual,data = chs,nest = TRUE,na.rm=TRUE )
    #age adjusted survey
    pop.agecat4=c(0.128810, 0.401725, 0.299194, 0.170271)
    chs.stdes<-svystandardize(subset(chs.dsgn,diabetes17>0 ),by=~agegroup,over=~allsex2,population=pop.agecat4,excluding.missing =~ agegroup+ ~allsex2)

    #crude prevalance estimates
    svyby(~diabetes17==1,~allsex2,subset(chs.dsgn,diabetes17>0),svyciprop,vartype = "ci",method="xlogit",df=degf(chs.dsgn))
    svyby(~diabetes17==2,~allsex2,subset(chs.dsgn,diabetes17>0),svyciprop,vartype = "ci",method="xlogit",df=degf(chs.dsgn))


    #age adjusted prevalance estimates

    svyby(~diabetes17==1,~allsex2,chs.stdes,svyciprop,vartype = "ci",method="xlogit",df=degf(chs.dsgn))
    svyby(~diabetes17==2,~allsex2,chs.stdes,svyciprop,vartype = "ci",method="xlogit",df=degf(chs.dsgn))

## Data loading and crude cleaning

We select year 2014-2016 and variables relating to smoking and
insurance.

``` r
chs16 = read_sas("data/chs2016_public.sas7bdat")

chs16_filter = chs16 %>% 
  select(agegroup,generalhealth,insuredgateway16,insure16,insured,insure5,sickadvice16,sickplace,didntgetcare16,smoker,everyday,numberperdaya,cost20cigarettes,imputed_povertygroup,fluvaccineshot) %>% 
  mutate(year = 2016) %>% 
  rename(insuredgateway = insuredgateway16, insure = insure16,sickadvice = sickadvice16,didntgetcare = didntgetcare16)


chs15 = read_sas("data/chs2015_public.sas7bdat")

chs15_filter = chs15 %>% 
  select(agegroup,generalhealth,insuredgateway15,insure15,insured,insure5,sickadvice15,sickplace,didntgetcare15,smoker,everyday,numberperdaya,cost20cigarettes,imputed_povertygroup,fluvaccineshot) %>% 
  mutate(year = 2015) %>% 
  rename(insuredgateway = insuredgateway15, insure = insure15,sickadvice = sickadvice15,didntgetcare = didntgetcare15)


chs14 = read_sas("data/chs2014_public.sas7bdat")

chs14_filter = chs14 %>% 
  select(agegroup,generalhealth,insuredgateway14,insure14,insured,insure5,sickadvice14,sickplace,didntgetcare14,smoker,everyday,numberperdaya,cost20cigarettes,imputed_povertygroup,fluvaccineshot) %>% 
  mutate(year = 2014) %>% 
  rename(insuredgateway = insuredgateway14, insure = insure14,sickadvice = sickadvice14,didntgetcare = didntgetcare14)


chs_14_16 = bind_rows(chs14_filter,chs15_filter,chs16_filter)
```

``` r
dataset_basic = chs_14_16 %>% 
  select(agegroup,insuredgateway,insure,sickadvice,sickadvice,didntgetcare,smoker,everyday,numberperdaya,cost20cigarettes,generalhealth,fluvaccineshot,imputed_povertygroup) %>% 
  mutate(numberperdaya = round(numberperdaya,1))
```

``` r
summary(dataset_basic)
```

    ##     agegroup    insuredgateway      insure        sickadvice     didntgetcare  
    ##  Min.   :1.00   Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :1.000  
    ##  1st Qu.:2.00   1st Qu.:1.000   1st Qu.:1.000   1st Qu.:1.000   1st Qu.:2.000  
    ##  Median :3.00   Median :1.000   Median :3.000   Median :1.000   Median :2.000  
    ##  Mean   :2.79   Mean   :1.095   Mean   :2.867   Mean   :1.762   Mean   :1.904  
    ##  3rd Qu.:3.00   3rd Qu.:1.000   3rd Qu.:4.000   3rd Qu.:2.000   3rd Qu.:2.000  
    ##  Max.   :4.00   Max.   :2.000   Max.   :7.000   Max.   :9.000   Max.   :2.000  
    ##  NA's   :62     NA's   :129     NA's   :510     NA's   :190     NA's   :158    
    ##      smoker         everyday     numberperdaya     cost20cigarettes
    ##  Min.   :1.000   Min.   :1.000   Min.   :  0.000   Min.   : 0.10   
    ##  1st Qu.:1.000   1st Qu.:1.000   1st Qu.:  1.600   1st Qu.: 8.00   
    ##  Median :1.000   Median :1.000   Median :  5.000   Median :11.00   
    ##  Mean   :1.561   Mean   :1.384   Mean   :  7.705   Mean   :10.29   
    ##  3rd Qu.:2.000   3rd Qu.:2.000   3rd Qu.: 10.000   3rd Qu.:12.50   
    ##  Max.   :3.000   Max.   :2.000   Max.   :200.000   Max.   :40.00   
    ##  NA's   :161     NA's   :25078   NA's   :25078     NA's   :25678   
    ##  generalhealth   fluvaccineshot  imputed_povertygroup
    ##  Min.   :1.000   Min.   :1.000   Min.   :1.000       
    ##  1st Qu.:2.000   1st Qu.:1.000   1st Qu.:1.000       
    ##  Median :3.000   Median :2.000   Median :3.000       
    ##  Mean   :2.727   Mean   :1.522   Mean   :2.762       
    ##  3rd Qu.:3.000   3rd Qu.:2.000   3rd Qu.:4.000       
    ##  Max.   :5.000   Max.   :2.000   Max.   :5.000       
    ##  NA's   :219     NA's   :162

1.  
