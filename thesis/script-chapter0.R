#-----------------------------------------------------------------------
# Packages
## ---- chap0-packages

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(forcats)
library(lattice)
library(latticeExtra)

source("script-helper0.R")

#-----------------------------------------------------------------------
# Descriptive analysis of the cotton bools data set
## ---- chap0-plot-cotton

cotton <- read_delim("data/cotton.txt", delim = "\t") %>%
    mutate(est = fct_inorder(factor(est)))
cotton

cotton_summary <-
    cotton %>%
    group_by(est, des) %>%
    summarise(me = mean(ncap),
              va = var(ncap),
              di = va/me) %>%
    ungroup()
cotton_summary

xy1 <-
    xyplot(ncap ~ des | est,
           layout = c(3, NA),
           as.table = TRUE,
           grid = TRUE,
           type = c("p", "smooth"),
           xlab = "Artificial defoliation level",
           ylab = "Number of bolls produced",
           sub = "(a)",
           spread = 0.03,
           scales = list(
               x = list(at = unique(cotton$des))
           ),
           panel = panel.beeswarm,
           data = cotton)

lim <- extendrange(unlist(cotton_summary[, c("me", "va")]))
xy2 <-
    xyplot(va ~ me,
           xlim = lim,
           ylim = lim,
           type = c("g", "p", "r"),
           sub = "(b)",
           xlab = "Sample mean",
           ylab = "Sample variance",
           panel = function(...) {
               panel.xyplot(...)
               panel.abline(a = 0, b = 1, lty = 2)
           },
           data = cotton_summary)

print(xy1, position = c(0, 0, 0.6, 1), more = TRUE)
print(xy2, position = c(0.6, 0, 1, 0.98), more = FALSE)

#-----------------------------------------------------------------------
# Descriptive analysis of the soybeans data set
## ---- chap0-plot-soy

# Load data
soybean <- read_delim("data/soybean.txt", delim = "\t") %>%
    mutate(umid = factor(umid)) %>%
    slice(-74)
soybean

soybean_summary <-
    soybean %>%
    gather(key = "var", value = "count", ngra, nvag) %>%
    group_by(K, umid, var) %>%
    summarise(me = mean(count),
              va = var(count),
              di = va/me) %>%
    ungroup()
soybean_summary

fvls <- paste0("Soil moisture: ", levels(soybean$umid), "%")
xy1 <-
    xyplot(ngra + nvag ~ K | umid,
           xlab = "Potassium fertilization level",
           ylab = "Count",
           sub = "(a)",
           type = c("p", "g", "smooth"),
           jitter.x = TRUE,
           layout = c(NA, 2),
           as.table = TRUE,
           scales = list(
               x = list(at = unique(soybean$K))
           ),
           strip = strip.custom(
               factor.levels = fvls
           ),
           key = list(
               corner = c(1.05, 0.2),
               between = 0.8,
               size = 3,
               padding.text = 3,
               type = "b",
               divide = 1,
               cex = 0.97,
               lines = list(pch = pchs[1:2],
                            lty = ltys[1:2],
                            cex = 0.8),
               text = list(c("Number of bean seeds",
                             "Number of pods"))),
           data = soybean)

# lim <- extendrange(unlist(soybean_summary[, c("me", "va")]))
xy2 <-
    xyplot(va ~ me | var,
           # xlim = lim,
           # ylim = lim,
           groups = var,
           type = c("g", "p"),
           sub = "(b)",
           xlab = "Sample mean",
           ylab = "Sample variance",
           layout = c(NA, 2),
           scales = "free",
           strip = strip.custom(
               factor.levels = c("Number of bean seeds",
                                 "Number of pods")
           ),
           panel = function(subscripts, x, y, ...) {
               panel.xyplot(subscripts = subscripts, x, y, ...)
               panel.lmline(x, y)
               panel.abline(a = 0, b = 1, lty = 2)
           },
           prepanel = function(x, y, subscripts) {
               rr <- extendrange(c(x, y))
               list(xlim = rr, ylim= rr)
           },
           data = soybean_summary)

print(xy1, position = c(0, 0, 0.63, 1), more = TRUE)
print(xy2, position = c(0.65, 0, .98, 0.98), more = FALSE)

#-----------------------------------------------------------------------
# Descriptive analysis of the nitrofen data set
## ---- chap0-plot-nitrofen

# Load data
nitrofen <- read_delim("data/nitrofen.txt", delim = "\t") %>%
    mutate(dose = dose/100)
nitrofen

nitrofen_summary <-
    nitrofen %>%
    group_by(dose) %>%
    summarise(me = mean(novos),
              va = var(novos),
              di = va/me) %>%
    ungroup()
nitrofen_summary

# Scatter plot
xy1 <-
    xyplot(novos ~ dose,
           xlab = "Nitrofen concentration level",
           ylab = "Number of live offspring",
           sub = "(a)",
           jitter.x = TRUE,
           type = c("p", "g", "smooth"),
           scales = list(
               x = list(at = unique(nitrofen$dose))
           ),
           data = nitrofen)

lim <- extendrange(unlist(nitrofen_summary[, c("me", "va")]))
xy2 <-
    xyplot(va ~ me,
           xlim = lim,
           ylim = lim,
           type = c("g", "p"),
           sub = "(b)",
           xlab = "Sample mean",
           ylab = "Sample variance",
           panel = function(x, y, ...) {
               panel.xyplot(x, y, ...)
               lab <- sprintf("(%.2f)", nitrofen_summary$dose)
               panel.text(x, y, lab,
                          pos = 4, cex = 0.9)
               panel.abline(a = 0, b = 1, lty = 2)
           },
           data = nitrofen_summary)

print(xy1, position = c(0, 0, 0.55, 1), more = TRUE)
print(xy2, position = c(0.55, 0, 1, 1), more = FALSE)

## ---- chap0-table-nitrofen

# Load data
nitrofen <- read_delim("data/nitrofen.txt", delim = "\t") %>%
    mutate(dose = dose/100)
# nitrofen

nitrofen_summary <-
    nitrofen %>%
    group_by(dose) %>%
    summarise(ob = length(novos),
              me = mean(novos),
              va = var(novos),
              di = va/me) %>%
    ungroup()
# nitrofen_summary

# Table
cap <-
    "Sample means, sample variances and sample dispersion indexes of the ten replicates for each concentration level in the nitrofen study ($\\text{DI} = \\bar{x}/s^2$)."
nitrofen_summary %>%
    setNames(c("Dose",
               "N. obs.",
               "Sample mean",
               "Sample variance",
               "Sample DI")) %>%
    xtable(caption = cap,
           label = "tab:chap0-table-nitrofen",
           digits = c(0, 2, 0, 2, 4, 4),
           align = "cccccc") %>%
    print.xtable(size = "normalsize")

#-----------------------------------------------------------------------
# Descriptive analysis of the sitophilus data set
## ---- chap0-plot-sitophilus

# Load data
sitophilus <- read_delim("data/sitophilus.txt", delim = "\t") %>%
    mutate(extract = factor(extract, levels = c("Control", "Leaf",
                                                "Branch", "Seed")))
sitophilus

sitophilus_summary <-
    sitophilus %>%
    group_by(extract) %>%
    summarise(me = mean(ninsect),
              va = var(ninsect),
              di = va/me) %>%
    ungroup()
sitophilus_summary

xy1 <-
    xyplot(ninsect ~ extract,
           type = c("p", "g"),
           spread = 0.08,
           xlab = "Extract",
           ylab = "Number of emerged insects",
           panel = panel.beeswarm,
           data = sitophilus)

lim <- extendrange(unlist(sitophilus_summary[, c("me", "va")]))
xy2 <-
    xyplot(va ~ me,
           xlim = lim,
           ylim = lim,
           type = c("g", "p", "r"),
           sub = "(b)",
           xlab = "Sample mean",
           ylab = "Sample variance",
           panel = function(x, y, ...) {
               panel.xyplot(x, y, ...)
               lab <- sprintf("(%s)", sitophilus_summary$extract)
               panel.text(x, y, lab,
                          pos = 4, cex = 0.9)
               panel.abline(a = 0, b = 1, lty = 2)
           },
           data = sitophilus_summary)

print(xy1, position = c(0, 0, 0.55, 1), more = TRUE)
print(xy2, position = c(0.55, 0, 1, 1), more = FALSE)

## ---- chap0-table-sitophilus

# Load data
sitophilus <- read_delim("data/sitophilus.txt", delim = "\t") %>%
    mutate(extract = factor(extract, levels = c("Control", "Leaf",
                                                "Branch", "Seed")))
# sitophilus

sitophilus_summary <-
    sitophilus %>%
    group_by(extract) %>%
    summarise(ob = length(ninsect),
              me = mean(ninsect),
              va = var(ninsect),
              di = va/me) %>%
    ungroup()
# sitophilus_summary

# Table
cap <-
    "Sample means, sample variances and sample dispersion indexes of the ten replicates for each treatment in the maize pest study ($\\text{DI} = \\bar{x}/s^2$)."
levels(sitophilus_summary$extract) <-
    c("Control", "Leaf extract", "Branch extract", "Seed extract")
sitophilus_summary %>%
    setNames(c("Treatment",
               "N. obs.",
               "Sample mean",
               "Sample variance",
               "Sample DI")) %>%
    xtable(caption = cap,
           label = "tab:chap0-table-sitophilus",
           digits = c(0, 2, 0, 2, 4, 4),
           align = "clcccc") %>%
    print.xtable(size = "normalsize",
                 table.placement = "htb")

#-----------------------------------------------------------------------
# Descriptive analysis of the nitrofen data set
## ---- chap0-plot-bromelia

bromelia <- read_delim("data/bromelia.txt", delim = "\t") %>%
    mutate(treat = fct_relevel(factor(treat), "Xaxim"))
bromelia

bromelia_summary <-
    bromelia %>%
    group_by(treat, time) %>%
    summarise(me = mean(nleaves),
              va = var(nleaves),
              di = va/me) %>%
    ungroup()
bromelia_summary

xy1 <-
    xyplot(nleaves ~ time | treat,
           layout = c(3, NA),
           as.table = TRUE,
           grid = TRUE,
           type = c("p", "smooth"),
           xlab = "Days after planting",
           ylab = "Number of leaves",
           sub = "(a)",
           spread = 10,
           scales = list(
               x = list(at = unique(bromelia$time),
                        rot = 30)
           ),
           panel = panel.beeswarm,
           data = bromelia)

lim <-
    bromelia_summary %>%
    dplyr::select(me, va) %>%
    filter(va > 0) %>%
    unlist() %>%
    log() %>%
    extendrange()

xy2 <-
    bromelia_summary %>%
    filter(va > 0) %>%
    xyplot(log(va) ~ log(me),
           xlim = lim,
           ylim = lim,
           jitter.x = TRUE,
           jitter.y = TRUE,
           factor = 2,
           type = c("g", "p", "r"),
           sub = "(b)",
           xlab = "Logarithm of sample mean",
           ylab = "Logarithm of sample variance",
           panel = function(...) {
               panel.xyplot(...)
               panel.abline(a = 0, b = 1, lty = 2)
           },
           data = .)

print(xy1, position = c(0, 0, 0.6, 1), more = TRUE)
print(xy2, position = c(0.6, 0, 1, 0.96), more = FALSE)
