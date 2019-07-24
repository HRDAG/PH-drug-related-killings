#
# Authors:     PB
# Maintainers: PB
# Copyright:   2019, HRDAG, GPL v2 or later
# ============================================
# PH/MSE/src/lcmcr.R

require(pacman)
p_load(tidyr, dplyr, purrr, stringr, LCMCR, argparse, yaml, UpSetR,
       bayesplot, ggplot2, here)
# this is to check if RStudio is breaking here()
stopifnot(here::here() > Sys.getenv("HOME"))

parser <- ArgumentParser()
parser$add_argument("--region", default='manila')
parser$add_argument("--perp", default="police")
args <- parser$parse_args()
set.seed(19481210)

args[['input']] <- here(paste0('import/output/', args$region, '.rds'))
stub <- paste0('lcmcr/output/', args$region, '-', args$perp)
args[['yaml']] <- here(paste0(stub, '.yaml'))
args[['lcmcr']] <- here(paste0(stub, '-lcmcr.rds'))
args[['upsetr']] <- here(paste0(stub, '-upsetr.pdf'))
args[['density']] <- here(paste0(stub, '-density.pdf'))
args[['trace']] <- here(paste0(stub, '-trace.pdf'))
args[['omitted']] <- here(paste0(stub, '-omitted.pdf'))
print(args)

graph_ht <- 5
graph_wd <- 7

getmwn <- function(args, perpcat, minsrc=0) {
# omitted date filter bc Mariel hand-cleaned out-of-range dates
# filter('2016-07-01' <= date_of_killing & date_of_killing <= '2017-12-31')

  mwn <- readRDS(args$input)  %>%
    filter(perp == perpcat) %>%
    select(starts_with("source_")) %>%  # verify all are logical
    mutate_all(funs(as.numeric(.))) %>%
    replace(is.na(.), 0) %>%
    mutate(rsum = rowSums(.)) %>%
    filter(rsum > 0) %>%
    as.data.frame

  str(mwn)
  colcnts <- mwn %>% colSums(.)
  validcols <- names(colcnts[colcnts > minsrc])
  mwn <- mwn %>%
    select(one_of(validcols)) %>%
    select(starts_with("source_")) %>%
    as.data.frame

  print("pre-upsetr")

  mwn0 <- mwn %>%
      rename_all(str_sub, 8, 30) %>%
      as.data.frame
  pdf(file=args$upsetr, width=graph_wd, height=graph_ht, onefile=FALSE)
  print(UpSetR::upset(mwn0, nsets=ncol(mwn0), empty.intersections=NULL))
  dev.off()
  rm(mwn0)

  mw <- mwn %>%
    mutate_all(funs(as.factor(.))) %>%
    as.data.frame

  if (args$region == "caloocan") {
    if (args$perp == "police") {
      stopifnot(nrow(mw) == 286)
    } else if (args$perp == "unidentified") {
      stopifnot(nrow(mw) == 599)
    }
  }

  print("data prep complete.")
  return(mw)
}


make_mse <- function(mw, runsize=1) {
    options(warn=-1)
    sampler <- lcmCR(captures=mw, tabular=FALSE, seed=19481210,
                     buffer_size=10000, thinning=10000,
                     in_list_label="1", not_in_list_label="0")
    N <- lcmCR_PostSampl(sampler, samples=100000*runsize)
    options(warn=0)
    N <- N[seq(1, length(N), 100*runsize)] # thin again
    return(N)
}


#---main-----
print("entering main")
mw <- getmwn(args, perpcat=args$perp)
lo <- nrow(mw) - 5

N <- make_mse(mw, runsize=3)
saveRDS(N, file=args$lcmcr)

mwn <- mw %>%
  mutate_all(as.character) %>%
  mutate_all(as.numeric)

pol <- mwn %>%
  select(source_police)  %>%
  sum() %>%
  as.integer()

news <- mwn %>%
  select(source_news)  %>%
  sum() %>%
  as.integer()

oth <- mwn %>%
  select(-source_news, -source_police)  %>%
  mutate(pm = do.call(pmax, (.))) %>%
  select(pm) %>%
  sum() %>%
  as.integer()

CIs <- as.integer(round(c(
          quantile(N, probs=c(0.025)),
          mean(N),
          quantile(N, probs=c(0.975))), 0))

yamllist <- list(n_records = nrow(mw),
                 pol_cnt = pol, news_cnt = news, oth_cnt = oth,
                 CI_lo = CIs[1], CI_med = CIs[2], CI_hi = CIs[3])
rm(mwn, pol, news, oth)

write_yaml(yamllist, args$yaml)
hi <- CIs[3]
results <- tibble(omitted="none", smpld=N)
N <- N[yamllist$CI_lo <= N & N <= hi]
N <- as.data.frame(N)
print(str(N))

p <- N %>% ggplot(aes(x = N)) +
  scale_x_continuous(limits=c((nrow(mw)- 5), (hi + 5))) +
  geom_density(trim=TRUE) +
  xlab("estimated total killings (95% CI)") + ylab("probability") +
  theme(text = element_text(size=14), axis.ticks.y=element_blank()) +
  geom_vline(xintercept=yamllist$n_records, color="red")
ggsave(args$density, plot=p, width=graph_wd, height=graph_ht, units="in")

p <- mcmc_trace(N)
ggsave(args$trace, plot=p, width=graph_wd, height=graph_ht, units="in")
rm(N, p, CIs)

#---test removal of each data source-----
results <- NULL
mw_colnames <- colnames(mw)
print(paste0("colnames are ", paste0(mw_colnames, collapse=",")))
for (xcol in mw_colnames) {
  print(paste('omitting', xcol))
  mw0 <- mw %>% select(-one_of(c(xcol)))
  N <- make_mse(mw0)
  CIs <- as.integer(quantile(N, probs=c(0.025, 0.5, 0.975)))
  hi <- ifelse(CIs[3] > hi, CIs[3], hi)
  lo <- ifelse(CIs[1] < lo, CIs[1], lo)
  xcolnm <- str_sub(xcol, 8, 30)
  tb <- tibble(omitted = xcolnm, smpld=N)
  results <- bind_rows(tb, results)
  rm(mw0, N, CIs)
}

p <- ggplot(results, aes(smpld, fill=omitted, color=omitted)) +
  geom_density(alpha=0.1) + xlim(lo, hi) +
  theme(text = element_text(size=14), axis.ticks.y=element_blank()) +
  xlab("estimated total killings") + ylab("probability") +
  geom_vline(xintercept=yamllist$CI_med, color="blue")
ggsave(args$omitted, plot=, width=graph_wd, height=graph_ht, units="in")

# done.
