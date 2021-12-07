Data cleaning
================
Mengfan Luo
11/11/2021

## Data loading and crude cleaning

We select year 2014-2016 and variables relating to smoking and
insurance.

``` r
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

``` r
dataset_basic = chs_14_16 %>% 
  select(agegroup,insure,smoker,everyday,numberperdaya,cost20cigarettes,generalhealth,imputed_povertygroup,bmi,child,sex,insuredgateway) %>% 
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
write_csv(dataset_basic,"data/dataset_basic.csv")
```

For **Community Health Survey Public Use Data**, we select year
2014-2016 and variables relating to smoking, insurance, and basic health
and economic status. The resulting dataset, available **here**, consists
of roughly 28000 participants and includes the following 12 variables:

**Insurance related**

-   `insuredgateway`: Whether having health insurance (1=Yes, 2=No)

-   `insure`: Type of health insurance coverage

(1=Employer, 2=Self-purchase,3=Medicare,4=Medicaid/Family
Health+,5=Milit/CHAMPUS/Tricare,6=COBRA/Other,7=Uninsured)

**Smoking related**

-   `smoker`: Smoking status (1= Never, 2= Current, 3= Former)

-   `everyday`: Smoking frequency (1 = everyday smoker, 2 = casual
    smoker)

-   `numberperdaya`: Average cigarettes smoked per day

-   `cost20cigarettes`: Cost of 20 cigarettes (one pack)

**Health and economic status**

-   `agegroup`: Age groups (1=18-24yrs,2=25-44 yrs,3=45-64 yrs,4=65+
    yrs)

-   `generalhealth`: Self-evaluated general health status

(1=Excellent,2=Very good,3=Good,4=Fair,5=Poor)

-   `sex`: Participant’s sex (1= male, 2= female)

-   `bmi`: Participant’s BMI (*k**g*/*m*<sup>2</sup>)

-   `child`: Number of children the participant have: (1= yes, 0= no)

-   `imputed_povertygroup`: Federal poverty level(FPL) for the household

(1= &lt;100% FPL, 2=100 - &lt;200% FPL, 3=200 - &lt;400% FPL, 4=400 -
&lt;600% FPL, 5= &gt;600% FPL)
