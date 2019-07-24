#!/usr/bin/env Rscript --vanilla
#
# Authors:     PB
# Maintainers: PB
# Copyright:   2018, HRDAG, GPL v2 or later
# ============================================
# PH/import/src/import-caloocan.R
#
library(pacman)
p_load(dplyr, janitor, tibble, readxl, argparse, assertr)

parser <- ArgumentParser()
parser$add_argument("--input",
                    default = "input/Caloocan_Complete_Sources.xlsx")
parser$add_argument("--output=", default = "output/caloocan.rds")
args <- parser$parse_args()

expected_cols <- c("id", "date", "location",
                   "police_district", "police_station",
                   "police", "news", "journalists_notes",
                   "task_force_detainees_of_the_philippines",
                   "rise_up_for_life_and_rights",
                   "balay_rehabilitation_center",
                   "cct_drk_research",
                   "redemptorist_social_mission",
                   "funeral_parlor",
                   "bishop_of_caloocan",
                   "philippine_human_rights_information_center",
                   "commission_on_human_rights")

read_sheet <- function(args, sheetname) {
    src_names <- c(source_police = "police",
                   source_news = "news",
                   source_photographer = "journalists_notes",
                   source_tfdp = "task_force_detainees_of_the_philippines",
                   source_riseup = "rise_up_for_life_and_rights",
                   source_balay = "balay_rehabilitation_center",
                   source_academic = "cct_drk_research",
                   source_priest = "redemptorist_social_mission",
                   source_funeral = "funeral_parlor",
                   source_bishop = "bishop_of_caloocan",
                   source_philr = "philippine_human_rights_information_center",
                   source_chr =  "commission_on_human_rights")
    coltypes=c("numeric", "date", "text", "text", "text",
                "logical", "logical", "logical", "logical",
                "logical", "logical", "logical", "logical",
                "logical", "logical", "logical", "logical")

    is_police = grepl("police", sheetname)
    cw <- read_xlsx(args$input, sheet = sheetname, col_types=coltypes) %>%
            clean_names() %>%
            glimpse() %>%
            verify(colnames(.) == expected_cols) %>%
            rename(date_of_killing = date) %>%
            rename(!!! src_names) %>%
            mutate(id = paste0("Caloocan-", id))  %>%
            mutate(perp2 = sheetname) %>%
            mutate(perp = if_else(is_police, "police", "unidentified")) %>%
            mutate_at(vars(starts_with("source_")), .funs = as.character) %>%
            mutate_at(vars(starts_with("source_")),
                .funs = recode,
                "TRUE" = TRUE, "FALSE" = FALSE,
                "N/A" = NA, default = NA) %>%
            select(-source_balay) %>%
            glimpse() %>%
            select(id, date_of_killing, location, perp, perp2,
                   starts_with("source_"))

        return(cw)
}

cw <- bind_rows(read_sheet(args, "police killings"),
                read_sheet(args, "unidentified assailant killings"))

saveRDS(cw, args$output)

# done.
