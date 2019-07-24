#!/usr/bin/env Rscript --vanilla
#
# Authors:     PB
# Maintainers: PB
# Copyright:   2018, HRDAG, GPL v2 or later
# ============================================
# PH/import/src/import-manila.R
#
library(pacman)
p_load(dplyr, janitor, tibble, readxl, argparse, assertr)

parser <- ArgumentParser()
parser$add_argument("--input",
                    default = "input/Manila_Complete_Sources.xlsx")
parser$add_argument("--output=", default = "output/manila.rds")
args <- parser$parse_args()

expected_cols <- c(
   "id", "date", "location",
   "police_district",
   "police_station",
   "police",
   "news",
   "commission_on_human_rights",
   "police_reports_submitted_to_the_house_of_representatives",
   "task_force_detainees_of_the_philippines",
   "philippine_human_rights_information_center",
   "rise_up_for_life_and_rights",
   "cct_drk_research",
   "parishes")

src_names <- c(
   source_police = "police",
   source_news = "news",
   source_chr = "commission_on_human_rights",
   source_house = "police_reports_submitted_to_the_house_of_representatives",
   source_tfdp = "task_force_detainees_of_the_philippines",
   source_philr = "philippine_human_rights_information_center",
   source_riseup = "rise_up_for_life_and_rights",
   source_academics = "cct_drk_research",
   source_parish = "parishes")

# these ids found in David's email 2019-07-02
ids_to_drop <- c(632)

read_sheet <- function(args, sheetid) {
    perp <- ifelse(sheetid == "police", "police", "unidentified")
    sheetname <- paste0(sheetid, " killings")
    cw <- read_xlsx(args$input, sheet = sheetname) %>%
            clean_names() %>%
            glimpse() %>%
            verify(colnames(.) == expected_cols) %>%
            rename(date_of_killing = date) %>%
            rename(!!! src_names) %>%
            filter(! id %in% ids_to_drop) %>%
            mutate(id = paste0("Manila-", id))  %>%
            mutate(perp = perp) %>%
            mutate_at(vars(starts_with("source_")), .funs = as.character) %>%
            mutate_at(vars(starts_with("source_")),
                      .funs = funs(ifelse(. == "TRUE", TRUE, FALSE))) %>%
            glimpse() %>%
            select(-source_house) %>%
            select(id, date_of_killing, location, perp, starts_with("source_"))
        return(cw)
}

cw <- bind_rows(read_sheet(args, "police"),
                read_sheet(args, "unidentified assailant"))

saveRDS(cw, args$output)

# done.
