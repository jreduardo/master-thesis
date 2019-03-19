#' % Chapter: Motivating datasets
#' % Eduardo E. Ribeiro Jr (<jreduardo@usp.br>),
#' % `r format(Sys.Date(), "updated on %B %d, %Y")`.
#'

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# For data manipulation
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(purrr)

# For data visualization
library(ggplot2)
library(ggbeeswarm)
library(cowplot, warn.conflicts = FALSE)

# For get the data
library(cmpreg, warn.conflicts = FALSE)
library(flexcm, warn.conflicts = FALSE)

#+ include=FALSE
source("./helper01_ggplot-theme.R")
library(knitr)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "figures/chapter0-",
  out.width = "100%",
  dpi = 300
)

#' ## Artificial defoliation in cotton phenology
#+ sec2.1, include=TRUE, fig.height=4
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Artificial defoliation in cotton phenology (Section 2.1)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Compute sample mean and variances
cotton_summary <-
  cotton %>%
  group_by(stage, defol) %>%
  summarise(me = mean(bolls),
            va = var(bolls),
            di = va/me) %>%
  ungroup()
cotton_summary

# To define the axis limits in ggplot
lim <-
  cotton_summary %>%
  with(tibble(x = range(me, va)))

# To display a least-square-line
lsl <-
  lm(va ~ me, data = cotton_summary) %>%
  coef()

# Scatter plot
gg1 <-
  cotton %>%
  ggplot(aes(defol, bolls)) +
  geom_beeswarm() +
  geom_smooth() +
  facet_wrap(vars(stage)) +
  labs(y = "Number of bolls produced",
       x = "Artificial defoliation level",
       tag = "(a)")

# Observed mean-variance relationship
gg2 <-
  cotton_summary %>%
  ggplot(aes(me, va)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  geom_abline(intercept = lsl[1], slope = lsl[2]) +
  geom_blank(aes(x = x, y = x), data = lim) +
  labs(y = "Sample variance",
       x = "Sample mean",
       tag = "(b)")

# Combine plots
plot_grid(gg1, gg2, rel_widths = c(.6, .4))

#' ## Soil moisture and potassium fertilization on soyaben
#+ sec2.2, include=TRUE, fig.height=5
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Soil moisture and potassium fertilization on soyaben (Section 2.2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Compute sample mean and variances
soybean_summary <-
  soybean %>%
  select(-vpods) %>%
  gather("variable", "count", seeds:tpods) %>%
  group_by(K, water, variable) %>%
  summarise(me = mean(count),
            va = var(count),
            di = va/me) %>%
  ungroup()
soybean_summary

# To define the axis limits in ggplot
lim <-
  soybean_summary %>%
  split(.$variable) %>%
  map_dfr(.id = "variable",
          ~tibble(x = range(.x$me, .x$va)))

# To display a leat-square-line
lsl <-
  soybean_summary %>%
  group_by(variable) %>%
  nest(variable, me, va) %>%
  mutate(lsl = map(data, ~lm(va ~ me, data = .)),
         b0  = map_dbl(lsl, ~coef(.)[1]),
         b1  = map_dbl(lsl, ~coef(.)[2]))

# Strip names
nam <- c("seeds" = "Number of bean seeds",
         "tpods" = "Number of total pods")

# Scatter plot
gg1 <-
  soybean %>%
  select(-vpods) %>%
  gather("variable", "count", seeds:tpods) %>%
  ggplot(aes(K, count, shape = variable)) +
  geom_point() +
  geom_smooth(aes(linetype = variable)) +
  facet_wrap(vars(water),
             labeller = as_labeller(function(x)
               sprintf("Soil moisture: %s%%", x)),
             ncol = 2) +
  guides(linetype = guide_legend(title = NULL),
         shape = guide_legend(title = NULL, keywidth = 2.5)) +
  scale_shape_manual(values = 1:2, labels = nam) +
  scale_linetype_manual(values = 1:2, labels = nam) +
  # scale_linetype_manual(name = "", labels = nam) +
  theme(legend.position = c(.75, .25),
        legend.text = element_text(size = 12)) +
  labs(x = "Potassium fertilization dose",
       y = "Count variable",
       tag = "(a)")

# Observed mean-variance relationship
gg2 <-
  soybean_summary %>%
  ggplot(aes(me, va, shape = variable)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  geom_abline(aes(intercept = b0, slope = b1),
              data = lsl) +
  facet_wrap(vars(variable),
             scales = "free",
             ncol = 1,
             labeller = as_labeller(nam)) +
  geom_blank(aes(x = x, y = x), data = lim) +
  guides(shape = FALSE) +
    labs(y = "Sample variance",
       x = "Sample mean",
       tag = "(b)")

# Combine plots
plot_grid(gg1, gg2, rel_widths = c(.6, .4))

#' ## Toxicity of nitrofen in aquatic systems
#+ sec2.3, include=TRUE, fig.height=4
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Toxicity of nitrofen in aquatic systems (Section 2.3)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Compute sample mean and variances
nitrofen_summary <-
  nitrofen %>%
  group_by(dose) %>%
  summarise(me = mean(noffs),
            va = var(noffs),
            di = va/me) %>%
  ungroup()
nitrofen_summary

# To define the axis limits in ggplot
lim <-
  nitrofen_summary %>%
  with(tibble(x = range(me, va)))

# To display a leat-square-line
lsl <-
  lm(va ~ me, data = cotton_summary) %>%
  coef()


# Scatter plot
gg1 <-
  nitrofen %>%
  ggplot(aes(dose, noffs)) +
  geom_beeswarm() +
  geom_smooth() +
  labs(x = "Nitrofen concentration level",
       y = "Number of live offspring",
       tag = "(a)")

# Observed mean-variance relationship
gg2 <-
  nitrofen_summary %>%
  ggplot(aes(me, va, label = sprintf("(%.2f)", dose))) +
  geom_point() +
  geom_text(hjust = -0.3) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  geom_abline(intercept = lsl[1], slope = lsl[2]) +
  geom_blank(aes(x = x, y = x), data = lim,
             inherit.aes = FALSE) +
  labs(y = "Sample variance",
       x = "Sample mean",
       tag = "(b)")

# Combine plots
plot_grid(gg1, gg2, rel_widths = c(.55, .45))

#' ## Annona mucosa in control of stored maize peast
#+ sec2.4, include=TRUE, fig.height=4
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Annona mucosa in control of stored maize peast (Section 2.4)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Compute sample mean and variances
sitophilus_summary <-
  sitophilus %>%
  group_by(extract) %>%
  summarise(me = mean(ninsect),
            va = var(ninsect),
            di = va/me) %>%
  ungroup()
sitophilus_summary

# To define the axis limits in ggplot
lim <-
  sitophilus_summary %>%
  with(tibble(x = range(me, va)))

# To display a leat-square-line
lsl <-
  lm(va ~ me, data = sitophilus_summary) %>%
  coef()

# Scatter plot
gg1 <-
  sitophilus %>%
  ggplot(aes(extract, ninsect)) +
  geom_beeswarm() +
  labs(x = "Treatment",
       y = "Number of emerged insects",
       tag = "(a)")

# Observed mean-variance relationship
gg2 <-
  sitophilus_summary %>%
  ggplot(aes(me, va, label = sprintf("(%s)", extract))) +
  geom_point() +
  geom_text(hjust = -0.3) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  geom_abline(intercept = lsl[1], slope = lsl[2]) +
  geom_blank(aes(x = x, y = x), data = lim,
             inherit.aes = FALSE) +
  labs(y = "Sample variance",
       x = "Sample mean",
       tag = "(b)")

# Combine plots
plot_grid(gg1, gg2, rel_widths = c(.55, .45))

#' ## Session info
#+ info, include=TRUE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Session info
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
devtools::session_info()
