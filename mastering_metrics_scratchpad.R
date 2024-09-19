library(tidyverse)
library(haven)
library(rlang)
library(broom)
library(clubSandwich)
library(multiwayvcov)
library(lmtest)
library(fs)


# https://jrnold.github.io/masteringmetrics/mlda-difference-in-difference.html

# note the masteringmetrics package wouldn't install from github, so manually downloaded deaths.rda file
# devtools::install_github("jrnold/masteringmetrics", subdir = "masteringmetrics")


# remotes::install_github("jjchern/mmdata")

# note the masteringmetrics package wouldn't install from github, so manually downloaded deaths.rda file
# load deaths
# data("deaths", package = "masteringmetrics")

setwd("C:/Users/Stephen/Desktop/R/mastering_metrics")


#///////////////////////////////////////////////////////////////////////////////////////


dir_ls()
load(file = "deaths.rda")
deaths <- deaths %>% mutate(year_fct = factor(year))

# inspect
deaths
deaths %>% glimpse()
deaths %>% skim()

deaths %>% count(year)
deaths %>% count(state)
# dtype refers to the mortality rate (mrate), and what death sources it includes
deaths %>% count(dtype)

# note records are unique at the year/state/dtype/agegr level
deaths %>% count(year, state, dtype) %>% arrange(desc(n))
deaths %>% count(year, state, dtype, agegr) %>% arrange(desc(n))


#///////////////////////////////////////////////////////////////////////////////////////


# dtype refers to the mortality rate (mrate), and what death sources it includes
dtypes <- c("all" = "All deaths",
            "MVA" = "Motor vehicle accidents",
            "suicide" = "Suicide",
            "internal" = "All internal causes")
dtypes


#///////////////////////////////////////////////////////////////////////////////////////


# get data
data <- filter(deaths, year <= 1983, agegr == "18-20 yrs", dtype == "all")


#//////////////////////


# inspect
data 
data %>% glimpse()


#///////////////////////////////////////////////////////////////////////////////////////


# run ols model

model_1 <- lm(mrate ~ 0 + legal + state + year_fct, data = data)

model_2 <- lm(mrate ~ 0 + legal + state + year_fct + state:year, data = data)


# with state trends and weighted by population
# note that coef_test errors on model_4 (and its commented out in run_mlda_dd() posted on website)
model_4 <- lm(mrate ~ 0 + legal + state + year_fct + state:year, data = data, weights = pop)


#//////////////////////


# inspect
model_1
model_1 %>% summary()
model_1 %>% tidy()

model_2
model_2 %>% summary()
model_2 %>% tidy()

model_4
model_4 %>% summary()
model_4 %>% tidy()


#///////////////////////////////////////////////////////////////////////////////////////


# get cluster-robust coefficients using clubsandwich::vcovCR()

vcov_model_1 <- vcovCR(model_1, cluster = data[["state"]], type = "CR2")
vcov_model_1

vcov_model_2 <- vcovCR(model_2, cluster = data[["state"]], type = "CR2")
vcov_model_2

# note that coef_test errors on model_4 (and its commented out in run_mlda_dd() posted on website)
vcov_model_4 <- vcovCR(model_4, cluster = data[["state"]], type = "CR2")
vcov_model_4


#////////////////////////


# alternate cluster-robust coefficients using multiwayvcov::cluster.vcov()
# note these alternate test match more exactly the results in the book

vcov_model_1_alternate <- cluster.vcov(model_1, data$state)
vcov_model_1_alternate

vcov_model_2_alternate <- cluster.vcov(model_2, data$state)
vcov_model_2_alternate

vcov_model_4_alternate <- cluster.vcov(model_4, data$state)
vcov_model_4_alternate


#///////////////////////////////////////////////////////////////////////////////////////


# get coefficient table

coef_test(obj = model_1, vcov = vcov_model_1)

coef_test(obj = model_2, vcov = vcov_model_2)

# note that coef_test errors on model_4 (and its commented out in run_mlda_dd() posted on website)
coef_test(obj = model_4, vcov = vcov_model_4)


#/////////////////////////


# alternate coefficient test using lmtest:coeftest()
# note these alternate test match more exactly the results in the book

coeftest(x = model_1, vcov = vcov_model_1_alternate)

coeftest(x = model_2, vcov = vcov_model_2_alternate)

coeftest(x = model_4, vcov = vcov_model_4_alternate)


#///////////////////////////////////////////////////////////////////////////////////////


# this is for easier mapping across different models, but gives same output as manual code above
# create cluster_se() to get clustered standard errors
cluster_se <- function(mod, cluster, type = "CR2") {
        vcov <- vcovCR(mod, cluster = cluster, type = "CR2")
        coef_test(mod, vcov = vcov) %>%
                rownames_to_column(var = "term") %>%
                as_tibble() %>%
                select(term, estimate = beta, std.error = SE)
}


# create run_mlda_dd()
# this will run different models and nicely package output, but is same output as manual code above
run_mlda_dd <- function(i) {
        data <- filter(deaths, year <= 1983, agegr == "18-20 yrs", dtype == i) # nolint
        mods <- tribble(
                ~ name, ~ model,
                "No trends, no weights",
                lm(mrate ~ 0 + legal + state + year_fct, data = data),
                "Time trends, no weights",
                lm(mrate ~ 0 + legal + year_fct + state + state:year, data = data),
                "No trends, weights",
                lm(mrate ~ 0 + legal + year_fct + state, data = data, weights = pop),
                # nolint start
                # "Time trends, weights",
                #   lm(mrate ~ 0 + legal + year_fct + state + state:year,
                #      data = data, weights = pop)
                # nolint end
        ) %>%
                mutate(coefs = map(model, ~ cluster_se(.x, cluster = data[["state"]],
                                                       type = "CR2"))) %>%
                unnest(coefs) %>%
                filter(term == "legal") %>%
                mutate(response = i) %>%
                select(name, response, estimate, std.error)
}


#///////////////////////////////////////////////////////////////////////////////////////


# get output with mlda_dd

mlda_dd <- map_df(names(dtypes), run_mlda_dd)
mlda_dd






