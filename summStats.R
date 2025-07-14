# get summaries of data

library(gtsummary)

summ <- readRDS("Data/espac3clean.rds")

characteristics <- tbl_summary(summ,
                     include = c(LymphN, ResecM, Diff_Status, PostOpCA199),
                     statistic = list(all_categorical()~ "{n} / {N} ({p}%)",
                                      all_continuous() ~ "{median} ({p25},{p75} )"))
