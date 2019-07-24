#!/usr/bin/env Rscript --vanilla
#
# Authors:     PB
# Maintainers: PB
# Copyright:   2018, HRDAG, GPL v2 or later
# ============================================
# PH/import/src/import-quezon.R
#
library(pacman)
p_load(dplyr, janitor, tibble, readxl, argparse, assertr)

parser <- ArgumentParser()
parser$add_argument("--input",
                    default = "input/QuezonCity_Complete_Sources.xlsx")
parser$add_argument("--output=", default = "output/quezon.rds")
args <- parser$parse_args()

expected_cols <- c("id", "date", "location",
                   "police_district", "police_station",
                   "police", "news", "commission_on_human_rights",
                   "philippine_human_rights_information_center",
                   "task_for_detainees_of_the_philippines",
                   "rise_up_for_life_and_rights",
                   "parishes",
                   "cct_drk_research",
                   "barangay_holy_spirit_public_safety_office")

# these ids found in David's email 2019-07-02
ids_to_drop <- c(663, 695, 696)

src_names <- c(source_police = "police",
               source_news = "news",
               source_chr = "commission_on_human_rights",
               source_philr = "philippine_human_rights_information_center",
               source_tfdp = "task_for_detainees_of_the_philippines",
               source_riseup = "rise_up_for_life_and_rights",
               source_parish = "parishes",
               source_academics = "cct_drk_research",
               source_bpso = "barangay_holy_spirit_public_safety_office")

read_sheet <- function(args, sheetid) {
    sheetname <- paste0(sheetid, " killings")
    perpcat <- ifelse(sheetid == "police", "police", "unidentified")
    cw <- read_xlsx(args$input, sheet = sheetname) %>%
            clean_names() %>%
            glimpse() %>%
            verify(colnames(.) == expected_cols) %>%
            rename(date_of_killing = date) %>%
            rename(!!! src_names) %>%
            filter(! id %in% ids_to_drop) %>%
            mutate(id = paste0("Quezon-", id))  %>%
            mutate(perp = perpcat) %>%
            mutate_at(vars(starts_with("source_")), .funs = as.character) %>%
            mutate_at(vars(starts_with("source_")),
                .funs = recode,
                "TRUE" = TRUE, "FALSE" = FALSE,
                "N/A" = NA, default = NA) %>%
            glimpse() %>%
            select(id, date_of_killing, location, perp, starts_with("source_"))
        return(cw)
}


#---main-----
cw <- bind_rows(read_sheet(args, "police"),
                read_sheet(args, "unidentified assailant"))

saveRDS(cw, args$output)

# done.
