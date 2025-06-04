## list of all packages that we are using
library(tidyverse)
library(naniar)
library(superheat)
library(patchwork)

source("functions/imputeFeature.R")
source("functions/prepareOrganData.R")

# load the data set

organs_original <- read_csv("data/global-organ-donation_2018.csv",
                      col_types = list(`Utilized DBD` = col_number(),
                                     `DD Lung Tx` = col_number(),
                                     `Total Utilized DD` = col_number(),
                                     `LD Lung Tx` = col_number()))

organs_preprocessed <- prepareOrganData(organs_original,
                                        .impute_method = "average")


organs_preprocessed <- organs_preprocessed |>
  mutate(total_deceased_donors_per_mil = total_deceased_donors/ (population_imputed + 1)*1000000, 
         total_deceased_donors_imputed_per_mil = total_deceased_donors_imputed/ (population_imputed + 1)*1000000)

organs_preprocessed |>
  filter(year == 2017) |>
  select(population, total_deceased_donors, total_deceased_donors_per_mil) |>
  pivot_longer(everything()) |>
  ggplot() +
  geom_histogram(aes(x = value), color = "white") +
  facet_wrap(~name, scales = "free")

organs_preprocessed |>
  group_by(year) |>
  summarize(total_donations = sum(total_deceased_donors_imputed)) |>
  ungroup() |>
  ggplot() +
  geom_line(aes(x = year, y= total_donations))
  
total_2017 <- organs_preprocessed |>
  filter(year == 2017) |>
  summarise(total = sum(total_deceased_donors_imputed)) |>
  pull(total)

total_2000 <- organs_preprocessed |>
  filter(year == 2000) |>
  summarise(total = sum(total_deceased_donors_imputed)) |>
  pull(total)

# is there a difference between imputation techniques?

organs_preprocessed |>
  mutate(total_deceased_donors_imputed_previous = 
           imputeFeature(organs_preprocessed, 
                         .feature = total_deceased_donors,
                         .group = country,
                         .impute_method = "previous")) |>
  group_by(year) |>
  summarise(None = sum(total_deceased_donors, na.rm = T),
            Average = sum(total_deceased_donors_imputed),
            Previous = sum(total_deceased_donors_imputed_previous)) |>
  ungroup() |>
  pivot_longer(c("None", "Average", "Previous"),
               names_to = "Imputation method",
               values_to = "Donor count") |>
  ggplot() +
  geom_line(aes(x = year, y = `Donor count`, color = `Imputation method`), 
            alpha = 0.8)
  
countries_top_20_2017 <- organs_preprocessed |>
  filter(year == 2017) |>
  arrange(desc(total_deceased_donors_imputed)) |>
  head(20) |>
  select(country, total_deceased_donors_imputed)

countries_top_20_2017_per_mil <- organs_preprocessed |>
  filter(year == 2017) |>
  arrange(desc(total_deceased_donors_imputed_per_mil)) |>
  head(20) |>
  select(country, total_deceased_donors_imputed_per_mil)
countries_top_20_2017_per_mil 
