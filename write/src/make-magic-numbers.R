#  PH/write/src/make-magic-numbers.R


require(pacman)
p_load(here, assertr, yaml, readr, tidyverse, stringr)

files <- list(ests = here::here("write/output/PH-estimates.csv.gz"),
              results_csv = here::here("write/output/PH-results.csv.gz"),
              results_yaml = here::here("write/output/PH-results.yaml"),
              man_pol_cnts = here::here("write/output/man_pol_cnts.rds")
)

ests <- tibble(
   man_uni = readRDS(here::here("lcmcr/output/manila-unidentified-lcmcr.rds")),
   man_pol = readRDS(here::here("lcmcr/output/manila-police-lcmcr.rds")),
   que_uni = readRDS(here::here("lcmcr/output/quezon-unidentified-lcmcr.rds")),
   que_pol = readRDS(here::here("lcmcr/output/quezon-police-lcmcr.rds")),
   cal_uni = readRDS(here::here("lcmcr/output/caloocan-unidentified-lcmcr.rds")),
   cal_pol = readRDS(here::here("lcmcr/output/caloocan-police-lcmcr.rds"))
)
write_delim(ests, files$ests, delim="|")

CIs <- function(pcat=NULL) {
  if (is.null(pcat)) {
    flds <- colnames(ests)
  } else {
    flds <- grep(pcat, colnames(ests), value=TRUE)
  }
  rcnt <- ests %>% select(!!flds) %>% rowSums(.)
  as.integer(round(c(
    quantile(rcnt, probs=c(0.025)),
    mean(rcnt),
    quantile(rcnt, probs=c(0.975))), 0))
}

total_ci <- CIs()
total_pol_ci <- CIs('pol')
total_uni_ci <- CIs('uni')
rm(ests)

tb <- tibble(region=character(), pcat=character(),
             recs=numeric(),
             lo=numeric(), med=numeric(), hi=numeric(),
             undocpct=numeric(), polrep=numeric(), polmult=numeric())

total_recs <- 0
police_recs <- 0
police_est <- 0
total_est_loop <- 0
doc_nonpolice <- 0

for (region in c("manila", "quezon", "caloocan")) {
   for (pcat in c("police", "unidentified")) {
        fname <- here::here(paste0("lcmcr/output/", region, "-", pcat, ".yaml"))
        d <- yaml::yaml.load_file(fname)
        total_recs <- total_recs + d$n_records
        police_recs <- police_recs + d$pol_cnt
        regname <- ifelse(region=="quezon", "Quezon City", str_to_title(region))
        pct <- paste0(round((d$CI_med - d$n_records)/d$CI_med * 100, 0), "%")

	# From David's 2019.06.05 email "Re: latest draft"
	# killings reported by police
        polrep <- case_when(
	    region=="manila" & pcat=="police" ~ 313,
	    region=="manila" & pcat=="unidentified" ~ 158,
	    region=="quezon" & pcat=="police" ~ 245,
	    region=="quezon" & pcat=="unidentified" ~ 24,
	    region=="caloocan" & pcat=="police" ~ 206,
	    region=="caloocan" & pcat=="unidentified" ~ 19
	)
	polmult <- round(d$CI_med/polrep, 1)

        tb <- add_row(tb,
                    region=regname, pcat=pcat,
                    recs=d$n_records, undocpct=pct,
                    lo=d$CI_lo, med=d$CI_med, hi=d$CI_hi,
		    polrep=polrep, polmult=polmult)

        total_est_loop <- total_est_loop + d$CI_med
        rm(fname, d)
    }
}
write_delim(tb, files$results_csv, delim="|")

total_ci[2] <- tb %>%
    summarize(xsum = sum(med)) %>%
    as.integer

total_pol_ci[2] <- tb %>%
    filter(pcat=="police") %>%
    summarize(xsum = sum(med)) %>%
    as.integer

total_uni_ci[2] <- tb %>%
    filter(pcat=="unidentified") %>%
    summarize(xsum = sum(med)) %>%
    as.integer

stopifnot(abs(total_est_loop - total_ci[2]) <= 1)

yaml::write_yaml(list(
        total_recs=as.integer(total_recs),
        doc_nonpolice=doc_nonpolice,
        total_ci_lo=total_ci[1],
        total_ci_med=total_ci[2],
        total_ci_hi=total_ci[3],
        total_pol_ci_lo=total_pol_ci[1],
        total_pol_ci_med=total_pol_ci[2],
        total_pol_ci_hi=total_pol_ci[3],
        total_uni_ci_lo=total_uni_ci[1],
        total_uni_ci_med=total_uni_ci[2],
        total_uni_ci_hi=total_uni_ci[3],
        police_recs=as.integer(police_recs)),
    file=files$results_yaml)

print("make-magic-numbers done.")
# done
