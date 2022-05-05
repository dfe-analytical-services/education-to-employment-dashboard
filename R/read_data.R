
datalist <- readRDS("datasets_random.rds")

# first page --------------------------------------------------------------

stat_subs <- datalist$stat_subs %>%
  filter(median_income != "u" &
           perc_subs != "c" &
           number_students_subs != 'low') %>%
  mutate_at(vars(matches("_")), as.numeric)

stat_subs_sub <- datalist$stat_subs_sub %>%
  filter(median_income != "u" &
           perc != "c" &
           number_students_sub != 'low') %>%
  mutate_at(vars(c("median_income", "perc", "number_students_sub")), as.numeric)

stat_hq <- datalist$stat_hq %>%
  filter(median_income != "u" &
           perc_hq != "c" &
           number_students_hq != 'low') %>%
  mutate_at(vars(c("median_income", "perc_hq", "number_students_hq")), as.numeric)
  
stat_hq_sub <- datalist$stat_hq_sub %>%
  filter(median_income != "u" &
           perc != "c" &
           number_students_qual != 'low') %>%
  mutate_at(vars(c("median_income", "perc", "number_students_qual")), as.numeric)

kpis <- datalist$kpis %>%
  filter(median_income_sector != "u" &
           perc_students_sector != "c") %>%
  mutate_at(vars(matches("_")), as.numeric)
  
wf <- datalist$wf

sectors_v <- unique(stat_subs$Sector)
regions_v <- unique(stat_subs$Region)

subsector_v <- unique(stat_subs_sub$Subsector)
levels_v <- unique(stat_hq_sub$Level_order)

# vector for relabel level of qualification

levelsRelabelled <- c("Below level 2 (GCSE grades 1-3)", 
                      "Level 2 (GCSE grades 4-9)",
                      "Level 3 (GCE A level)",
                      "Level 4/5 (HNC or HND)",
                      "Level 6 (Degree)",
                      "Level 7 (Master's degree)")

# second page -------------------------------------------------------------

qualifications <- datalist$qualifications %>%
  filter(Links != "low") %>%
  mutate_at(vars(matches("Links")), as.numeric)

students_in_work <- datalist$students_in_work %>%
  filter(perc != "c") %>%
  mutate_at(vars(matches("perc")), as.numeric)

# vector for relabel level of qualification
sector_v2 <- unique(qualifications$IndustrySector)
region_v2 <- unique(qualifications$Region)
level_v2 <- c("Level 2", "Level 3", "Level 4/5", "Level 6", "Level 7+")

# download data -----------------------------------------------------------

subsectors_table <- datalist$stat_subs %>%
  rename( 
         'Median Earnings' = median_income, 
         'Proportion' = perc_subs,
         'Volume employees' = number_students_subs)

highest_qualification_table <- datalist$stat_hq %>%
  rename('Qualification level' = Level_order, 
         'Median Earnings' = median_income, 
         'Proportion' = perc_hq,
         'Volume employees' = number_students_hq)

qualifications_titles_table <- datalist$stat_subs_sub %>%
  rename('Qualification level' = Level_order, 
         'Median Earnings' = median_income, 
         'Proportion' = perc,
         'Volume employees' = number_students_sub)

subjects_table <- datalist$stat_hq_sub %>%
  select(Region, 
         Sector, 
         Subsector, 
         Qualification, 
         'Qualification level' = Level_order_UI, 
         'Subject studied' = Subject, 
         'Median Earnings' = median_income,
         'Proportion' = perc,
         'Volume employees' = number_students_qual)

income_proportions_table <- datalist$kpis %>%
  rename('Median Earnings' = median_income_sector, 
         'Proportion' = perc_students_sector)

working_futures_table <- datalist$wf %>%
  rename('Region code' = RegionCode,
         'Sector' = IndustrySector,
         'Subsector' = Sector,
         'Period 2022-2027' = Years2022.2027)

qualifications_pathways_table <- datalist$qualifications %>%
  select(Region, 
         Sector = IndustrySector,
         'Qualification' = Qual,
         'Qualification level' = Level,
         'Next qualification' = NextQual,
         'Next qualification level' = LevelNextQual,
         'Employees' = Links)

progression_to_work_by_level_table <- datalist$students_in_work %>%
  select(Region, Sector, 
         'Qualification level' = Level_order, 
         'Proportion' = perc)
