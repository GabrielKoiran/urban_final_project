#Libraries

library(fixest)
library(dplyr)
library(readr)
library(haven)
library(mgcv)
library(stargazer)
library(ggplot2)
library(gratia)
library(patchwork)

rm(list = ls())

#Load data, create observation identifier from population and year
#And keep relevant variables
#Merge different tables to have all otucomes together

tabA6 <- read_stata("Appendix_Table6.dta") %>%
    mutate(obs = paste(epop, year, sep = "_")) %>%
    select(c(obs, dpop, d_bus_dom2, year, fe_group, epop, fips_state)) %>%
    rename(c("instrument" = "d_bus_dom2", "population_change" = "dpop"))

tabA7 <- read_stata("Appendix_Table7.dta") %>%
    mutate(obs = paste(epop, year, sep = "_")) %>%
    select(c(obs, dadjlwage)) %>%
    rename(c("wage_change" = "dadjlwage"))

tabA8 <- read_stata("Appendix_Table8.dta") %>%
    mutate(obs = paste(epop, year, sep = "_")) %>%
    select(c(obs, dadjlrent)) %>%
    rename(c("rent_change" = "dadjlrent"))

tabA9 <- read_stata("Appendix_Table9.dta") %>%
    mutate(obs = paste(epop, year, sep = "_")) %>%
    select(c(obs, demp)) %>%
    rename(c("employment_change" = "demp"))

figA6 <- read_stata("Appendix_Figure6.dta") %>%
    mutate(state_year = paste(fips_state, year, sep = "_")) %>%
    select(c(state_year, d_corporate_rate)) %>%
    distinct() 

main_table <- tabA6 %>%
    left_join(tabA7, by = "obs") %>%
    left_join(tabA8, by = "obs") %>%
    left_join(tabA9, by = "obs") %>%
    mutate(state_year = paste(fips_state, year, sep = "_")) %>%
    left_join(figA6, by = "state_year")

#Replicate paper's models

model_population <- feols(population_change ~ instrument + factor(year) + factor(fe_group), data = main_table, weights = ~epop, cluster = ~fips_state)
summary(model_population)

model_employment <- feols(employment_change ~ instrument + factor(year) + factor(fe_group), data = main_table, weights = ~epop, cluster = ~fips_state)
summary(model_employment)

model_wage <- feols(wage_change ~ instrument + factor(year) + factor(fe_group), data = main_table, weights = ~epop, cluster = ~fips_state)
summary(model_wage)

model_rent <- feols(rent_change ~ instrument + factor(year) + factor(fe_group), data = main_table, weights = ~epop, cluster = ~fips_state)
summary(model_rent)

#Replicate paper's models without 2010

model_population_no2010 <- feols(population_change ~ instrument + factor(year) + factor(fe_group), data = main_table %>% filter(year!=2010), weights = ~epop, cluster = ~fips_state)
summary(model_population_no2010)

model_employment_no2010 <- feols(employment_change ~ instrument + factor(year) + factor(fe_group), data = main_table %>% filter(year!=2010), weights = ~epop, cluster = ~fips_state)
summary(model_employment_no2010)

model_wage_no2010 <- feols(wage_change ~ instrument + factor(year) + factor(fe_group), data = main_table %>% filter(year!=2010), weights = ~epop, cluster = ~fips_state)
summary(model_wage_no2010)

model_rent_no2010 <- feols(rent_change ~ instrument + factor(year) + factor(fe_group), data = main_table %>% filter(year!=2010), weights = ~epop, cluster = ~fips_state)
summary(model_rent_no2010)

#Replicate paper's models without Midwest FE

model_population_nogroupFE <- feols(population_change ~ instrument + factor(year), data = main_table, weights = ~epop, cluster = ~fips_state)
summary(model_population_nogroupFE)

model_employment_nogroupFE <- feols(employment_change ~ instrument + factor(year), data = main_table, weights = ~epop, cluster = ~fips_state)
summary(model_employment_nogroupFE)

model_wage_nogroupFE <- feols(wage_change ~ instrument + factor(year), data = main_table, weights = ~epop, cluster = ~fips_state)
summary(model_wage_nogroupFE)

model_rent_nogroupFE <- feols(rent_change ~ instrument + factor(year), data = main_table, weights = ~epop, cluster = ~fips_state)
summary(model_rent_nogroupFE)

#Quadratic models

model_population_quadratic <- feols(population_change ~ poly(instrument, 2) + factor(year) + factor(fe_group), data = main_table, weights = ~epop, cluster = ~fips_state)
summary(model_population_quadratic)

model_employment_quadratic <- feols(employment_change ~ poly(instrument, 2) + factor(year) + factor(fe_group), data = main_table, weights = ~epop, cluster = ~fips_state)
summary(model_employment_quadratic)

model_wage_quadratic <- feols(wage_change ~ poly(instrument, 2) + factor(year) + factor(fe_group), data = main_table, weights = ~epop, cluster = ~fips_state)
summary(model_wage_quadratic)

model_rent_quadratic <- feols(rent_change ~ poly(instrument, 2) + factor(year) + factor(fe_group), data = main_table, weights = ~epop, cluster = ~fips_state)
summary(model_rent_quadratic)

#Quadratic models without 2010

model_population_quadratic_no2010 <- feols(population_change ~ poly(instrument, 2) + factor(year) + factor(fe_group), data = main_table %>% filter(year != 2010), weights = ~epop, cluster = ~fips_state)
summary(model_population_quadratic_no2010)

model_employment_quadratic_no2010 <- feols(employment_change ~ poly(instrument, 2) + factor(year) + factor(fe_group), data = main_table %>% filter(year != 2010), weights = ~epop, cluster = ~fips_state)
summary(model_employment_quadratic_no2010)

model_wage_quadratic_no2010 <- feols(wage_change ~ poly(instrument, 2) + factor(year) + factor(fe_group), data = main_table %>% filter(year != 2010), weights = ~epop, cluster = ~fips_state)
summary(model_wage_quadratic_no2010)

model_rent_quadratic_no2010 <- feols(rent_change ~ poly(instrument, 2) + factor(year) + factor(fe_group), data = main_table %>% filter(year != 2010), weights = ~epop, cluster = ~fips_state)
summary(model_rent_quadratic_no2010)

#Quadratic models without Midwest FE

model_population_quadratic_nogroupFE <- feols(population_change ~ poly(instrument, 2) + factor(year), data = main_table, weights = ~epop, cluster = ~fips_state)
summary(model_population_quadratic_nogroupFE)

model_employment_quadratic_nogroupFE <- feols(employment_change ~ poly(instrument, 2) + factor(year), data = main_table, weights = ~epop, cluster = ~fips_state)
summary(model_employment_quadratic_nogroupFE)

model_wage_quadratic_nogroupFE <- feols(wage_change ~ poly(instrument, 2) + factor(year), data = main_table, weights = ~epop, cluster = ~fips_state)
summary(model_wage_quadratic_nogroupFE)

model_rent_quadratic_nogroupFE <- feols(rent_change ~ poly(instrument, 2) + factor(year), data = main_table, weights = ~epop, cluster = ~fips_state)
summary(model_rent_quadratic_nogroupFE)

#Smoothed non-parametric regressions

model_population_nonparam <- gam(population_change ~ s(instrument, k = 5) + factor(year) + factor(fe_group), data = main_table)
plot_population <- draw(model_population_nonparam, select = "s(instrument)") +
    ggtitle("A. Population")

model_employment_nonparam <- gam(employment_change ~ s(instrument, k = 5) + factor(year) + factor(fe_group), data = main_table)
plot_employment <- draw(model_employment_nonparam, select = "s(instrument)") +
    ggtitle("B. Employment")

model_population_nonparam_k10 <- gam(population_change ~ s(instrument, k = 10) + factor(year) + factor(fe_group), data = main_table)
plot_population_k10 <- draw(model_population_nonparam_k10, select = "s(instrument)") +
    ggtitle("D. Population (k=10)")

model_employment_nonparam_k10 <- gam(employment_change ~ s(instrument, k = 10) + factor(year) + factor(fe_group), data = main_table)
plot_employment_k10 <- draw(model_employment_nonparam_k10, select = "s(instrument)") +
    ggtitle("E. Employment (k=10)")

model_wage_nonparam <- gam(wage_change ~ s(instrument, k = 5) + factor(year) + factor(fe_group), data = main_table)
plot_wage <- draw(model_wage_nonparam, select = "s(instrument)") +
    ggtitle("C. Wages")

model_rent_nonparam <- gam(rent_change ~ s(instrument, k = 5) + factor(year) + factor(fe_group), data = main_table)
plot_rent <- draw(model_rent_nonparam, select = "s(instrument)") +
    ggtitle("F. Rental Cost")

#Smoothed non-parametric regressions with raw corporate rate change

model_population_nonparam_raw <- gam(population_change ~ s(d_corporate_rate, k = 5) + factor(year) + factor(fe_group), data = main_table)
plot_population_raw <- draw(model_population_nonparam_raw, select = "s(d_corporate_rate)") +
    ggtitle("A. Population")

model_employment_nonparam_raw <- gam(employment_change ~ s(d_corporate_rate, k = 5) + factor(year) + factor(fe_group), data = main_table)
plot_employment_raw <- draw(model_employment_nonparam_raw, select = "s(d_corporate_rate)") +
    ggtitle("B. Employment")

model_wage_nonparam_raw <- gam(wage_change ~ s(d_corporate_rate, k = 5) + factor(year) + factor(fe_group), data = main_table)
plot_wages_raw <- draw(model_wage_nonparam_raw, select = "s(d_corporate_rate)") +
    ggtitle("C. Wages")

model_rent_nonparam_raw <- gam(rent_change ~ s(d_corporate_rate, k = 5) + factor(year) + factor(fe_group), data = main_table)
plot_rent_raw <- draw(model_rent_nonparam_raw, select = "s(d_corporate_rate)") +
    ggtitle("D. Rental Cost")

#Smoothed non-parametric regressions without 2010

model_population_nonparam_no2010 <- gam(population_change ~ s(instrument, k = 5) + factor(year), data = main_table %>% filter(year != 2010))
plot_population_no2010 <- draw(model_population_nonparam_no2010, select = "s(instrument)") +
    ggtitle("A. Population")

model_employment_nonparam_no2010 <- gam(employment_change ~ s(instrument, k = 5) + factor(year), data = main_table %>% filter(year != 2010))
plot_employment_no2010 <- draw(model_employment_nonparam_no2010, select = "s(instrument)") +
    ggtitle("B. Employment")

model_wage_nonparam_no2010 <- gam(wage_change ~ s(instrument, k = 5) + factor(year), data = main_table %>% filter(year != 2010))
plot_wage_no2010 <- draw(model_wage_nonparam_no2010, select = "s(instrument)") +
    ggtitle("C. Wages")

model_rent_nonparam_no2010 <- gam(rent_change ~ s(instrument, k = 5) + factor(year), data = main_table %>% filter(year != 2010))
plot_rent_no2010 <- draw(model_rent_nonparam_no2010, select = "s(instrument)") +
    ggtitle("D. Rental Costs")

#Smoothed non-parametric regressions without midwest FE

model_population_nonparam_nogroupFE <- gam(population_change ~ s(instrument, k = 5) + factor(year), data = main_table)
plot_population_nogroupFE <- draw(model_population_nonparam_nogroupFE, select = "s(instrument)") +
    ggtitle("A. Population")

model_employment_nonparam_nogroupFE <- gam(employment_change ~ s(instrument, k = 5) + factor(year), data = main_table)
plot_employment_nogroupFE <- draw(model_employment_nonparam_nogroupFE, select = "s(instrument)") +
    ggtitle("B. Employment")

model_wage_nonparam_nogroupFE <- gam(wage_change ~ s(instrument, k = 5) + factor(year), data = main_table)
plot_wage_nogroupFE <- draw(model_wage_nonparam_nogroupFE, select = "s(instrument)") +
    ggtitle("C. Wages")

model_rent_nonparam_nogroupFE <- gam(rent_change ~ s(instrument, k = 5) + factor(year), data = main_table)
plot_rent_nogroupFE <- draw(model_rent_nonparam_nogroupFE, select = "s(instrument)") +
    ggtitle("D. Rental Costs")

#Focus near the center of the distribution

main_table_restricted <- main_table %>%
    filter(instrument > -0.5 & instrument < 0.5)

model_population_nonparam_rest <- gam(population_change ~ s(instrument, k =5) + factor(year) + factor(fe_group), data = main_table_restricted)
plot_population_rest <- draw(model_population_nonparam_rest, select = "s(instrument)") +
    ggtitle("A. Population")

model_employment_nonparam_rest <- gam(employment_change ~ s(instrument, k =5) + factor(year) + factor(fe_group), data = main_table_restricted)
plot_employment_rest <- draw(model_employment_nonparam_rest, select = "s(instrument)") +
    ggtitle("B. Employment")

model_wage_nonparam_rest <- gam(wage_change ~ s(instrument, k =5) + factor(year) + factor(fe_group), data = main_table_restricted)
plot_wages_rest <- draw(model_wage_nonparam_rest, select = "s(instrument)") +
    ggtitle("C. Wages")

model_rent_nonparam_rest <- gam(rent_change ~ s(instrument, k =5) + factor(year) + factor(fe_group), data = main_table_restricted)
plot_rent_rest <- draw(model_rent_nonparam_rest, select = "s(instrument)") +
    ggtitle("D. Rental costs")

#Descriptive

hist(main_table$instrument, breaks = 20, main = "Histogram of Instrument", xlab = "Instrument", ylab = "Frequency")

#Stargazer tables

etable(model_population, model_employment, model_wage, model_rent,
       dict = c("instrument" = "Log tax change",
                "population_change" = "Population change",
                "employment_change" = "Employment change",
                "wage_change" = "Wage change",
                "rent_change" = "Rent change",
                "fips_state" = "State"),
       keep = "Log tax change",
       tex = TRUE)

etable(model_population_nogroupFE, model_employment_nogroupFE, model_wage_nogroupFE, model_rent_nogroupFE,
       dict = c("instrument" = "Log tax change",
                "population_change" = "Population change",
                "employment_change" = "Employment change",
                "wage_change" = "Wage change",
                "rent_change" = "Rent change",
                "fips_state" = "State"),
       keep = "Log tax change",
       tex = TRUE)

etable(model_population_no2010, model_employment_no2010, model_wage_no2010, model_rent_no2010,
       dict = c("instrument" = "Log tax change",
                "population_change" = "Population change",
                "employment_change" = "Employment change",
                "wage_change" = "Wage change",
                "rent_change" = "Rent change",
                "fips_state" = "State"),
       keep = "Log tax change",
       tex = TRUE)

etable(model_population_quadratic, model_employment_quadratic, model_wage_quadratic, model_rent_quadratic,
       dict = c("poly(instrument, 2)1" = "Log tax change",
                "poly(instrument, 2)2" = "Log tax change squared",
                "population_change" = "Population change",
                "employment_change" = "Employment change",
                "wage_change" = "Wage change",
                "rent_change" = "Rent change"),
       keep = c("Log tax change", "Log tax change squared"),
       tex = TRUE)

etable(model_population_quadratic_no2010, model_employment_quadratic_no2010, model_wage_quadratic_no2010, model_rent_quadratic_no2010,
       dict = c("poly(instrument, 2)1" = "Log tax change",
                "poly(instrument, 2)2" = "Log tax change squared",
                "population_change" = "Population change",
                "employment_change" = "Employment change",
                "wage_change" = "Wage change",
                "rent_change" = "Rent change"),
       keep = c("Log tax change", "Log tax change squared"),
       tex = TRUE)

etable(model_population_quadratic_nogroupFE, model_employment_quadratic_nogroupFE, model_wage_quadratic_nogroupFE, model_rent_quadratic_nogroupFE,
       dict = c("poly(instrument, 2)1" = "Log tax change",
                "poly(instrument, 2)2" = "Log tax change squared",
                "population_change" = "Population change",
                "employment_change" = "Employment change",
                "wage_change" = "Wage change",
                "rent_change" = "Rent change"),
       keep = c("Log tax change", "Log tax change squared"),
       tex = TRUE)

#Final plots

main_plot <- (
    wrap_elements(plot_population) +
        wrap_elements(plot_employment) +
        wrap_elements(plot_wage)
) / (
    wrap_elements(plot_population_k10) +
        wrap_elements(plot_employment_k10) +
        wrap_elements(plot_rent)
)
print(main_plot)

main_plot_nogroupFE <- (
        wrap_elements(plot_population_nogroupFE) +
        wrap_elements(plot_employment_nogroupFE)
) / (
        wrap_elements(plot_wage_nogroupFE) +
        wrap_elements(plot_rent_nogroupFE)
)
print(main_plot_nogroupFE)

main_plot_no2010 <- (
    wrap_elements(plot_population_no2010) +
        wrap_elements(plot_employment_no2010)
) / (
    wrap_elements(plot_wage_no2010) +
        wrap_elements(plot_rent_no2010)
)
print(main_plot_no2010)


main_plot_center_distribution <- (
    wrap_elements(plot_population_rest) +
        wrap_elements(plot_employment_rest)
) / (
    wrap_elements(plot_wages_rest) +
        wrap_elements(plot_rent_rest)
)
print(main_plot_center_distribution)

main_plot_raw <- (
    wrap_elements(plot_population_raw) +
        wrap_elements(plot_employment_raw)
) / (
    wrap_elements(plot_wages_raw) +
        wrap_elements(plot_rent_raw)
)
print(main_plot_raw)
