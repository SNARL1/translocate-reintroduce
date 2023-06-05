if (!require(librarian)){
  install.packages("librarian")
}
librarian::shelf(RPostgres, DBI, rstudioapi, dplyr, tidyr, readr, ggplot2, lubridate, magrittr, here, patchwork) # remove ggplot when plotting moved to different script

# Retrieve data from database
source("code/db_connect.R") 
relocate <- tbl(con, "relocate") %>% 
  filter(release_siteid1 %in% c(70611, 70413, 70279) & release_date > "2021-01-01") %>% 
  rename(relocate_id = id,
         relocate_comment = comment) %>% 
  inner_join(tbl(con, "relocate_frog"), by = "relocate_id") %>% 
  rename(relocate_frog_comment = comment) %>% 
  collect()

cmr_captures <- tbl(con, "visit") %>% 
  filter(site_id %in% c(70611, 70114, 70175, 70279, 70413, 71968, 72008, 72093, 72264, 72390, 72442) & visit_date > "2021-01-01") %>% 
  rename(visit_id = id,
         visit_comment = comment) %>% 
  inner_join(tbl(con, "survey"), by = "visit_id") %>% 
  rename(survey_id = id,
         survey_comment = comment) %>% 
  left_join(tbl(con, "capture_survey"), by = "survey_id") %>% 
  filter(visit_status == "suitable" & 
           survey_type == "cmr" & 
           (species == "ramu" | is.na(species) == TRUE) &
           (capture_animal_state != "dead" | is.na(capture_animal_state) == TRUE)) %>% 
  left_join(tbl(con, "bd_load"), by = c("swab_id" = "sample_id")) %>% 
  select(site_id, visit_date, pit_tag_ref, swab_id, bd_load) %>% 
  collect()
source("code/db_disconnect.R")

cmr_captures <- cmr_captures %>% 
  mutate(site_id = as.character(site_id),
         pit_tag_ref = as.character(pit_tag_ref)) 

write_csv(cmr_captures, here("data", "clean", "cmr_captures.csv"))

# Retrieve unappended data from "pCloudDrive/MLRG/code/ErrorChecking/2021/cleaned_data/relocate_semiclean_220121" -  not appended yet due to missing zoo data for some sites
relocate_csv <- read_csv(here("data", "raw", "relocate.csv")) 
relocate_frog_csv <- read_csv(here("data", "raw", "relocate_frog.csv"))

relocate_csv <- relocate_csv %>%   
  filter(release_siteid1 %in% c(70611, 70413, 70279) & release_date > "2021-01-01") %>% 
  rename(relocate_id = id,
         relocate_comment = comment) %>% 
  inner_join(relocate_frog_csv, by = "relocate_id") %>% 
  rename(relocate_frog_comment = comment) %>% 
  mutate(collect_siteid = as.character(collect_siteid),
         release_siteid1 = as.integer(release_siteid1),
         release_siteid2 = as.integer(release_siteid2),
         pit_tag_ref = as.character(pit_tag_ref),
         weight = as.integer(weight),
         length = as.integer(length),
         bd_treatment = as.character(bd_treatment))
relocate_all <- bind_rows(relocate, relocate_csv) %>% 
  rename(site_id = release_siteid1) %>% 
  replace_na(list(bd_exposure = "unexposed")) %>% # frogs were not subjected to immune_priming
  mutate(site_id = as.character(site_id),
         collect_siteid = as.character(collect_siteid),
         pit_tag_ref = as.character(pit_tag_ref)) %>% 
  select(site_id, release_date, collect_siteid, collect_date, collect_stage, zoo, type, pit_tag_ref, sex, weight, length, bd_exposure, swab_id) 

write_csv(relocate_all, here("data", "clean", "relocate_all.csv"))

# Identify frogs that were reintroduced versus translocated and captured versus not recaptured
cmr_captures1 <- cmr_captures %>% # Use cmr_captures.csv as necessary
  distinct(pit_tag_ref) %>% 
  mutate(recaptured = 1) %>% 
  right_join(relocate_all, by = "pit_tag_ref") %>% # Use relocate_all.csv as necessary
  replace_na(list(recaptured = 0)) %>% 
  select(site_id, pit_tag_ref, recaptured, type, release_date)

# Create plot of recapture probability for translocated versus reintroduced frogs - result mostly unchanged when 2022 releases are excluded
cmr_captures1 %>% 
  filter(site_id != "70279") %>% # excluded because this site only received reintroduced frogs (no translocated frogs)
  ggplot(aes(x = type, fill = factor(recaptured))) +
  geom_bar(position = "fill") +
  labs(x = "Release type", y = "Proportion of frogs recaptured", fill = "recaptured") +
  scale_fill_manual(values = c("blue4", "green3"), labels = c("No", "Yes")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8, vjust = 0.7),
        axis.title.x = element_text(vjust = -1)) +
  facet_wrap(~site_id)
ggsave(here("out", "plot_prop_recapture.png"))

# Create plot of Bd loads of translocated versus reintroduced frogs following release
# NOTE: bd_load data from 2022 is currently missing from amphibians database. Code below adds these data from preliminary CSV files
cmr_captures2 <- relocate_all %>% 
  filter(site_id != 70279 &
           release_date < "2022-01-01") %>% # restrict data to relocations conducted in 2021
  distinct(pit_tag_ref) %>% 
  inner_join(cmr_captures, by = "pit_tag_ref") %>% # restrict captures to those of frog relocated in 2021
  mutate(site_id = as.character(site_id), 
         pit_tag_ref = as.character(pit_tag_ref),
         site_id = case_match(site_id, "70279" ~ "70413", "72093" ~ "70413", .default = site_id))

# Assign periods/labels to survey dates  
cmr_periods <- cmr_captures2 %>% 
  distinct(site_id, visit_date) %>% 
  arrange(site_id, visit_date) %>% 
  group_by(site_id) %>% 
  mutate(period = cumsum(c(1L, diff(visit_date) > 2))) %>% 
  ungroup() 
cmr_periods <- cmr_periods %>% 
  group_by(site_id, period) %>% 
  mutate(date_label = min(visit_date)) %>% 
  ungroup() %>% 
  select(-period) %>% 
  inner_join(cmr_periods, by = c("site_id", "visit_date"))

# Join periods to capture history, add preliminary 2022 swab data, remove records without associated bd loads
bdload_2022 <- read_csv(here("data", "raw", "bdload_2022.csv")) %>% 
  mutate(bd_load = round(bd_load, 3))

cmr_captures3 <- cmr_captures2 %>% 
  left_join(cmr_periods, by = c("site_id", "visit_date")) %>% 
  rows_patch(bdload_2022, by = "swab_id", unmatched = "ignore") %>% # update rows in which bd_load = NA with bd_load in bdload_2022 tibble
  filter(!is.na(bd_load)) %>% # include only records of frogs from which swab was collected
  left_join(relocate_all %>% select(pit_tag_ref, type), by = "pit_tag_ref") %>% 
  select(site_id, visit_date, date_label, period, pit_tag_ref, type, swab_id, bd_load) %>% 
  arrange(site_id, period, type)

# Create plot of bd_load by site, time, and relocate type
bdload_70413 <- cmr_captures3 %>% 
  filter(site_id == "70413") %>% 
  mutate(date_label = as.character(date_label)) %>% 
  ggplot(aes(x = date_label, y = log10(bd_load + 1), fill = type)) +
  geom_boxplot() + 
  scale_fill_manual(values = c("royalblue1", "green3")) +
  labs(x = "Date", y = (expression(Bd~load~(log["10"]~copies+1))), fill = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8, vjust = 0.7),
        legend.position = "none") +
  facet_wrap(~site_id)

bdload_70611 <- cmr_captures3 %>% 
  filter(site_id == "70611") %>% 
  mutate(date_label = as.character(date_label)) %>% 
  ggplot(aes(x = date_label, y = log10(bd_load + 1), fill = type)) +
  geom_boxplot() + 
  scale_fill_manual(values = c("royalblue1", "green3")) +
  labs(x = "Date", y = (expression(Bd~load~(log["10"]~copies+1))), fill = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8, vjust = 0.7)) +
  facet_wrap(~site_id)

bdload_70413 + bdload_70611 
ggsave(here("out", "bdload_by_type.png"), width = 6, height = 5)

# Create table of translocation and reintroduction details
relocate_all %>% 
  mutate(collect_siteid = recode(collect_siteid, 
                                 "70284" = "70567", 
                                 "72458" = "70567",
                                 "72878" = "70567"),
         collect_year = year(collect_date),
         release_year = year(release_date)) %>% 
    count(site_id, collect_siteid, collect_year, release_year, type) %>% 
write_csv(here("data", "clean", "tbl_relocate.csv"))


