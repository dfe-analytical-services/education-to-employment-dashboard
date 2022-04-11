
datalist <- readRDS("datasets_random.rds")

# first page --------------------------------------------------------------

stat_subs<- datalist$stat_subs
stat_subs_sub <- datalist$stat_subs_sub
stat_hq <- datalist$stat_hq 
stat_hq_sub <- datalist$stat_hq_sub 
kpis <- datalist$kpis
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

qualifications <- datalist$qualifications
students_in_work <- datalist$students_in_work
t3_subjects <- datalist$subjects_t3

# vector for relabel level of qualification
sector_v2 <- unique(qualifications$IndustrySector)
region_v2 <- unique(qualifications$Region)
level_v2 <- c("Level 2", "Level 3", "Level 4/5", "Level 6", "Level 7+")
subject_v2 <- unique(qualifications$Subject)




