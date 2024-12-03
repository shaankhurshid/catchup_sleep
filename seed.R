library(data.table)
library(stringr)
library(plyr)

# Load data
data <- fread('/Volumes/medpop_afib/skhurshid/sedentary/complete_time_sed_with_all_activities_secondary_vars_091624.csv')

# Load censor data
censor_data <- fread(file='/Volumes/medpop_afib/skhurshid/phenotypes/2021_06/censor_202106.csv')

### Adequate sleep defined as 7 hours per day. Regular defined as no day < 6.5 hours ####
data[,sleep_group7 := ifelse(sleep_daily_total < 2940,"Inadequate",
                            ifelse(c(sleep1 < 390 | sleep2 < 390 | sleep3 < 390 | sleep4 < 390 | sleep5 < 390 | sleep6 < 390 | sleep7 < 390),'Catchup','Regular'))]

### Adequate sleep defined as 7 hours per day. Regular defined as no day < 6.5 hours ####
data[,sleep_group8 := ifelse(sleep_daily_total < 3360,"Inadequate",
                            ifelse(c(sleep1 < 450 | sleep2 < 450 | sleep3 < 450 | sleep4 < 450 | sleep5 < 450 | sleep6 < 450 | sleep7 < 450),'Catchup','Regular'))]

# Merges
setkey(data,sample_id); setkey(censor_data,sample_id)
censor_data[data,':='(sleep_group7 = i.sleep_group7, sleep_group8 = i.sleep_group8,
                    accel_age = i.age_accel, sex = i.sex, race = i.race_category_adjust, tob = i.tob,
                    etoh = i.etoh_grams, tdi = i.tdi, employment_status = i.employment_status, 
                    self_health = i.self_health, diet = i.diet, qual_ea = i.qual_ea, accel_date = i.end_date,
                    mvpa_daily_total = i.mvpa_daily_total,sleep_daily_total = i.sleep_daily_total)]

# Load withdrawals
withdrawals <- fread(file='/Volumes/medpop_afib/skhurshid/phenotypes/withdrawals/w7089_20230821.csv')

#### Fix censor data in censor file
# Load center categories
center <- fread(file='/Volumes/medpop_afib/skhurshid/phenotypes/center0.csv')
center_lookup <- fread(file='/Volumes/medpop_afib/skhurshid/phenotypes/enrollment_correspondences.csv')

# Add center value to dataset
setkey(censor_data,sample_id); setkey(center,sample_id)
censor_data[center,':='(center_code = i.value)]

setkey(censor_data,center_code); setkey(center_lookup,Code)
censor_data[center_lookup,':='(center_location = i.Region)]

# Now correct censor dates based on location
censor_data[,':='(phenotype_censor_date = as.Date(ifelse(center_location=='England',phenotype_censor_date,
                                                         ifelse(center_location=='Scotland',pmin(phenotype_censor_date,as.Date('2021-03-31',format='%Y-%m-%d')),
                                                                pmin(phenotype_censor_date,as.Date('2018-02-28',format='%Y-%m-%d')))),origin='1970-01-01'))]

# And set censor date to date of death for those who died
censor_data[,':='(phenotype_censor_date = as.Date(ifelse(!is.na(death_date),pmin(death_date,phenotype_censor_date),phenotype_censor_date),origin='1970-01-01'))]

# Remove missing exposure data
censor_data <- censor_data[!is.na(sleep_group7)] #502485 - 412912 = 89573

# Remove withdrawals
censor_data <- censor_data[!(sample_id %in% withdrawals$V1)] #89573 - 0 = 89573

# Scope columns
seed <- censor_data[,c('sample_id','accel_age','accel_date',
                     'sex','race','tob','etoh','tdi','employment_status',
                     'self_health','diet','qual_ea','phenotype_censor_date',
                     'sleep_group7','sleep_group8','mvpa_daily_total','sleep_daily_total')]

# Write out 
write.csv(seed,file='/Volumes/medpop_afib/skhurshid/catchup_sleep/catchup_sleep_seed_120324.csv',row.names = F)
