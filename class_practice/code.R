## list of all packages that we are using
library(tidyverse)
library(naniar)
library(superheat)
library(patchwork)

source("functions/imputeFeature.R")
source("functions/prepareOrganData.R")

# load the data set

organs_original <- read.csv("~/Documents/stat155/class_practice/data/global-organ-donation_2018.csv", 
                            )

correct_names <- c(names(organs_original))

organs_preprocessed <- prepareOrganData(organs_original, .impute_method = "average")

head(organs_preprocessed)
glimpse(organs_preprocessed)

summary(organs_preprocessed$population_imputed)


hist(organs_preprocessed$deceased_donors_brain_death)


organs_preprocessed |>
  filter(year == 2017) |>
  select(population, total_deceased_donors, 
         total_deceased_donors_imputed) |>
  pivot_longer(everything()) |>
  ggplot() +
  geom_histogram(aes(x = value), color="white") +
  facet_wrap(~name, scales = "free")




