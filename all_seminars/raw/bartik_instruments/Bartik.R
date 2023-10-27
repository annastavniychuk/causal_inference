# Источник: https://github.com/paulgp/bartik-weight/blob/master/R-code/example_ADH.R
# Статья, к которой код: http://paulgp.github.io/papers/bartik_gpss.pdf 

# install.packages("devtools")
devtools::install_github("paulgp/bartik-weight/R-code/pkg")
library("bartik.weight", lib.loc="~/R/win-library/4.0")

library(tidyverse)
## ── Attaching packages ───────────────────────────────────────── tidyverse 1.2.1 ──

## ✔ ggplot2 2.2.1.9000     ✔ purrr   0.2.4     
## ✔ tibble  1.4.2          ✔ dplyr   0.7.4     
## ✔ tidyr   0.8.0          ✔ stringr 1.3.0     
## ✔ readr   1.1.1.9000     ✔ forcats 0.3.0

## ── Conflicts ──────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ✖ dplyr::vars()   masks ggplot2::vars()

ADH_local %>%
  mutate(ind = str_glue("t{year}_sh_ind_{ind}")) %>%
  spread(ind, sh_ind_, fill = 0) %>%
  print() -> ADH_local2


# Prepare variables in the master tibble
y = "d_sh_empl_mfg"
x = "d_tradeusch_pw"
controls = c("reg_midatl", "reg_encen", "reg_wncen", "reg_satl",
             "reg_escen", "reg_wscen", "reg_mount", "reg_pacif", "l_sh_popedu_c",
             "l_sh_popfborn", "l_sh_empl_f", "l_sh_routine33", "l_task_outsource",
             "t2", "l_shind_manuf_cbp")
weight = "timepwt48"

# Prepare variables in the local tibble
Z = setdiff(names(ADH_local_wide), c("czone", "year"))

# Prepare variables in the global tibble
G = "trade_"

# Estimate the weight (alpha) and the IV estimates (beta)
bw = bw(ADH_master, y, x, controls, weight, ADH_local2, Z, ADH_global, G)
bw

bw %>%
  top_n(5, alpha) %>%
  arrange(desc(alpha)) %>%
  mutate(ind = case_when(
    ind == "3571" ~ "Electronic Computers",
    ind == "3944" ~ "Games and Toys",
    ind == "3651" ~ "Household Audio and Video",
    ind == "3661" ~ "Telephone Apparatus",
    ind == "3577" ~ "Computer Equipment"
  )) %>%
  rename(g = trade_) %>%
  knitr::kable(digits = 3, caption = "Top five Rotemberg weight industries")
