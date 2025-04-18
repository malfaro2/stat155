
# load the libraries we will need in this document
library(tidyverse)
library(naniar) # this is a nice package for visualizing missing data
library(superheat)
library(patchwork)

# load in the function that will allow us to clean the organ data
source("functions/imputeFeature.R")
source("functions/prepareOrganData.R")

# load in the original data
organs_original <- read_csv("data/global-organ-donation_2018.csv", 
     col_types = list(`Utilized DBD` = col_number(),
                      `DD Lung Tx` = col_number(),
                      `Total Utilized DD` = col_number(),
                      `LD Lung Tx` = col_number())) 

# clean and pre-process the data
organs_preprocessed <- prepareOrganData(organs_original, 
                                        .impute_method = "average")

# add a donors_per_mil column for each type of imputed donors col
organs_preprocessed <- organs_preprocessed |>
  # note that we use `population_imputed + 1` in the denominator because there 
  # are some countries with a reported population of 0.
  mutate(total_deceased_donors_per_mil = total_deceased_donors / (population_imputed + 1) * 1000000,
         total_deceased_donors_imputed_per_mil = total_deceased_donors_imputed / (population_imputed + 1) * 1000000) 

# high level summary
organs_preprocessed |>
  filter(year == 2017) |>
  select(population, total_deceased_donors, total_deceased_donors_per_mil) |>
  pivot_longer(everything()) |>
  ggplot() +
  geom_histogram(aes(x = value), color = "white") +
  # scales = "free" allows each plot to have its own x-axis 
  facet_wrap(~name, scales = "free")

## Are global organ donations are increasing over time*?

organs_preprocessed |>
  # for each year, add up the (imputed) number of organ donations
  group_by(year) |>
  summarise(total_donations = sum(total_deceased_donors_imputed)) |>
  ungroup() |>
  # plot the number of donations using a line plot
  ggplot() +
  geom_line(aes(x = year, y = total_donations)) 

# compute the number of organ donations in 2017
total_2017 <- organs_preprocessed |> 
  filter(year == 2017) |>
  summarise(total = sum(total_deceased_donors_imputed)) |>
  pull(total)

# compute the number of organ donations in 2000
total_2000 <- organs_preprocessed |> 
  filter(year == 2000) |>
  summarise(total = sum(total_deceased_donors_imputed)) |>
  pull(total)

### PCS evaluation

#### Stability to a cleaning and pre-processing judgment call

# "The global organ donation trend according to different imputation techniques"
organs_preprocessed |>
  # create the "previous" imputed donor count
  mutate(total_deceased_donors_imputed_previous = 
           imputeFeature(organs_preprocessed,
                         .feature = total_deceased_donors, 
                         .group = country, 
                         .impute_method = "previous")) |>
  group_by(year) |>
  # add up the total donor counts for each year based on the imputation options
  summarise(None = sum(total_deceased_donors, na.rm = T),
            Average = sum(total_deceased_donors_imputed),
            Previous = sum(total_deceased_donors_imputed_previous)) |>
  ungroup() |> 
  pivot_longer(c("None", "Average", "Previous"), 
               names_to = "Imputation method", 
               values_to = "Donor count") |>
  ggplot() +
  geom_line(aes(x = year, y = `Donor count`, color = `Imputation method`), alpha = 0.8) 

## The US has the *most donors*, but Spain has the *highest donor rate*

countries_top_20_2017 <- organs_preprocessed |>
  # filter to 2017
  filter(year == 2017) |>
  # arrange in descending order of donor count
  arrange(desc(total_deceased_donors_imputed)) |>
  # keep just the top 20 rows
  head(20) |>
  select(country, total_deceased_donors_imputed)
countries_top_20_2017

# "A bar chart showing the number of 2017 organ donations for the 20 countries with the highest number of donations"

organs_preprocessed |>
  # filter to the top 20 countries in 2017
  filter(year == 2017, country %in% countries_top_20_2017$country) |>
  # arrange in descending order of donor count
  arrange(desc(total_deceased_donors_imputed)) |>
  # force the countries to appear in the order of donors
  mutate(country = fct_inorder(country)) |>
  # create a bar plot
  ggplot() +
  geom_col(aes(x = country, y = total_deceased_donors_imputed)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

countries_top_20_2017_per_mil <- organs_preprocessed |>
  # filter to 2017
  filter(year == 2017) |>
  # arrange in descending order of donor count
  arrange(desc(total_deceased_donors_imputed_per_mil)) |>
  # keep just the top 20 rows
  head(20) |>
  select(country, total_deceased_donors_imputed_per_mil)
countries_top_20_2017_per_mil

# "A bar chart showing the number of 2017 organ donations per million for the 20 countries with the highest number of donations per million"

organs_preprocessed |>
  # filter to the top 20 countries in 2017
  filter(year == 2017, country %in% countries_top_20_2017_per_mil$country) |>
  # arrange in descending order of donor count per mil
  arrange(desc(total_deceased_donors_imputed_per_mil)) |>
  # force the countries to appear in the order of donors per mil
  mutate(country = fct_inorder(country)) |>
  # create a bar plot
  ggplot() +
  geom_col(aes(x = country, y = total_deceased_donors_imputed_per_mil)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

### PCS evaluation

#### Predictability
# "A bar chart showing the number of 2016 organ donations per million for the 20 countries with the highest number of donations per million"

countries_top_20_2016 <- organs_preprocessed |>
  filter(year == 2016) |>
  arrange(desc(total_deceased_donors_imputed)) |>
  head(20) 
countries_top_20_2016_per_mil <- organs_preprocessed |>
  filter(year == 2016) |>
  arrange(desc(total_deceased_donors_imputed_per_mil)) |>
  head(20) 

gg_donors_2016 <- organs_preprocessed |>
  filter(year == 2016, country %in% countries_top_20_2016$country) |>
  # force the countries to appear in the order of donors per mil
  arrange(desc(total_deceased_donors_imputed)) |>
  mutate(country = fct_inorder(country)) |>
  ggplot() +
  geom_col(aes(x = country, y = total_deceased_donors_imputed)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

gg_donors_2016_per_mil <- organs_preprocessed |>
  filter(year == 2016, country %in% countries_top_20_2016_per_mil$country) |>
  # force the countries to appear in the order of donors per mil
  arrange(desc(total_deceased_donors_imputed_per_mil)) |>
  mutate(country = fct_inorder(country)) |>
  ggplot() +
  geom_col(aes(x = country, y = total_deceased_donors_imputed_per_mil)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

gg_donors_2016 + gg_donors_2016_per_mil

#### Stability to a data visualization judgment call

# "A heatmap of the donor rate for each country/year"

organs_preprocessed |>
  filter(country %in% countries_top_20_2017_per_mil$country) |>
  # force the countries to appear in the order of donors per mil
  arrange(desc(year), total_deceased_donors_imputed_per_mil) |>
  mutate(country = fct_inorder(country)) |>
  ggplot() +
  geom_tile(aes(x = year, y = country, 
                fill = total_deceased_donors_imputed_per_mil),
            col = "white") +
  scale_fill_gradient(low = "white", high = "black") +
  scale_x_continuous(expand = c(0, 0)) +
  theme(legend.position = "top")

### Creating an explanatory figure

organs_preprocessed |>
  # filter to the top 20 countries in 2017
  filter(year == 2017, country %in% countries_top_20_2017_per_mil$country) |>
  # arrange in descending order of donor count per mil
  arrange(desc(total_deceased_donors_imputed_per_mil)) |>
  # force the countries to appear in the order of donors per mil
  mutate(country = fct_inorder(country)) |>
  # create a bar plot
  ggplot() +
  geom_col(aes(x = country, y = total_deceased_donors_imputed_per_mil, 
               fill = country == "Spain")) +
  scale_fill_manual(values = c("grey50", "Orange")) +
  theme_classic() +
  labs(x = NULL, y = "Organ donations per million", 
       title = "Organ donation rates per million in 2017",
       subtitle = "For the top 20 countries") +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "none",
        axis.line = element_blank(),
        axis.ticks.x = element_blank(), 
        panel.grid.major.y = element_line(color = "grey90")) 

## Visualizing the donor rates over time for each country

# "Trend lines for each country's donor rates"
organs_preprocessed |>
  #dplyr::filter(year < 2013) |>
  mutate(highlight = if_else(country %in% c("Spain", "Croatia", "Belgium", "Malta", "United States of America"), as.character(country), "other")) |>
  ggplot() +
  geom_line(aes(x = year, y = total_deceased_donors_per_mil, col = highlight, group = country), alpha = 0.5) +
  scale_color_manual(values = c("#84ACCE", "#F6AE2D", "#589D6F", "grey60", "#CEA1C3", "#E68992"))

### PCS evaluations
#### Stability to data perturbations

set.seed(4395)
perturbed_organs_data <- organs_preprocessed |>
  # for each country, compute its standard deviation and record it in a column
  group_by(country) |>
  mutate(sd = sd(total_deceased_donors_imputed)) |>
  ungroup() |>
  # for each row in the data:
  rowwise() |>
  # compute four different perturbed versions of the total_deceased_donors variable
  # this involves adding a random Normal number to 30% of the values. 
  # Note that we don't add noise to 0 counts. 
  mutate(donors_pert1 = if_else((total_deceased_donors != 0) & (rbinom(1, 1, 0.3) == 1), 
                                true = total_deceased_donors + rnorm(1, 0, sd),
                                false = total_deceased_donors),
         donors_pert2 = if_else((total_deceased_donors != 0) & (rbinom(1, 1, 0.3) == 1), 
                                true = total_deceased_donors + rnorm(1, 0, sd),
                                false = total_deceased_donors),
         donors_pert3 = if_else((total_deceased_donors != 0) & (rbinom(1, 1, 0.3) == 1), 
                                true = total_deceased_donors + rnorm(1, 0, sd),
                                false = total_deceased_donors),
         donors_pert4 = if_else((total_deceased_donors != 0) & (rbinom(1, 1, 0.3) == 1), 
                                true = total_deceased_donors + rnorm(1, 0, sd),
                                false = total_deceased_donors)) |>
  # filter to countries with at least 500 total donors
  group_by(country) |>
  mutate(total_donors = max(total_deceased_donors_imputed)) |>
  ungroup() |>
  filter(total_donors > 500) 

# plot the perturbed trendlines and the original trendline
perturbed_organs_data |> 
  # add a column that identifies which rows correspond to Spain
  mutate(Spain = country == "Spain") |>
  ggplot() +
  # add the original donor trend lines for each country, and color Spain
  geom_line(aes(x = year, y = 1000000 * (total_deceased_donors / population), 
                group = country, col = Spain, alpha = Spain)) +
  # add the first set of perturbed trend lines for each country, and color Spain
  geom_line(aes(x = year, y = 1000000 * (donors_pert1 / population), 
                group = country, col = Spain, alpha = Spain), 
            linetype = "dashed") +
  # add the second set of perturbed trend lines for each country, and color Spain
  geom_line(aes(x = year, y = 1000000 * (donors_pert2 / population), 
                group = country, col = Spain, alpha = Spain), 
            linetype = "dashed") +
  # add the third set of perturbed trend lines for each country, and color Spain
  geom_line(aes(x = year, y = 1000000 * (donors_pert3 / population), 
                group = country, col = Spain, alpha = Spain), 
            linetype = "dashed") +
  # add the fourth set of perturbed trend lines for each country, and color Spain
  geom_line(aes(x = year, y = 1000000 * (donors_pert4 / population), 
                group = country, col = Spain, alpha = Spain), 
            linetype = "dashed") +
  scale_color_manual("Country", 
                     values = c("grey50", "#9C528B"),
                     labels = c("Other", "Spain")) +
  labs(y = "Organ donation rates per million") +
  scale_alpha_manual(values = c(0.3, 1), guide = "none") +
  theme_classic() 

#### Stability to a data visualization judgment call

# "A series of multi-line plots displaying the organ donation rate per million over time based on four alternative filtering judgment calls (a) all countries, (b) the top 20 countries in 2017, (c) countries that reported at least 500 donors for at least one year, and (d) countries that reported at least 20 donors per million for at least one year."
# create a function that we can re-use to create each plot
plotLines <- function(data) {
  gg <- data |> 
    ggplot() +
    geom_line(aes(x = year, y = (total_deceased_donors / population) * 1000000, 
                  col = Spain, alpha = Spain, group = country)) +
    theme_classic() +
    scale_color_manual(values = c("grey50", "#9C528B")) +
    scale_alpha_manual(values = c(0.5, 1)) +
    labs(y = "Organ donations per million",
         x = NULL)
  return(gg)
}

# No filtering
gg_no_filter <- organs_preprocessed |>
  # add a column that specifies whether the current row corresponds to Spain
  mutate(Spain = (country == "Spain")) |>
  plotLines() +
  ggtitle("(a) No filtering")


# filter to the 20 countries with the highest organ donation rates in 2017
gg_2017 <- organs_preprocessed |>
  filter(country %in% countries_top_20_2017$country) |>
  # add a column that specifies whether the current row corresponds to Spain
  mutate(Spain = (country == "Spain")) |>
  plotLines() +
  ggtitle("(b) Top 20 countries in 2017")


# Filter to countries with at least 500 donors
gg_500donors <- organs_preprocessed |>
  # add a column that specifies whether the current row corresponds to Spain
  mutate(Spain = (country == "Spain")) |>
  # compute the largest donor entry for each country and record it in a column
  group_by(country) |>
  mutate(total_donors = max(total_deceased_donors_imputed)) |>
  ungroup() |>
  # filter to the countries with at least 500 donors in at least one year
  filter(total_donors > 500) |>
  # create the figure
  plotLines() +
  ggtitle("(c) At least 500 donors")

# Filter to countries with at least 20 donors per million
gg_20donors_per_mil <- organs_preprocessed |>
  # add a column that specifies whether the current row corresponds to Spain
  mutate(Spain = (country == "Spain")) |>
  # identify the largest donor rate entry for each country and record it in a column
  group_by(country) |>
  mutate(total_donors_per_mil = max(total_deceased_donors_imputed / population_imputed) * 1000000) |>
  ungroup() |>
  # filter to countries with at least one donor rate entry above 20
  filter(total_donors_per_mil >= 20) |>
  plotLines() +
  ggtitle("(d) At least 20 donors per million")

# combine the plots using patchwork syntax
(gg_no_filter + gg_2017) / (gg_500donors + gg_20donors_per_mil)

### Creating an explanatory figure

# "A cleaner line plot of (imputed) donor trends over time"

# create a temporary version of the organs clean dataset that just contains
# the top 20 countries in 2017 and adds a variable that contains the country
# name only for the countries of interest and "other" for all other countries
organs_preprocessed_top_20 <- organs_preprocessed |>
  filter(country %in% countries_top_20_2017_per_mil$country) |>
  mutate(highlight = if_else(country %in% c("Spain", "Croatia",
                                            "United States of America"),
                             as.character(country), "Other"))


organs_preprocessed_top_20 |>
  ggplot() +
  # add "Other" the line layers
  geom_line(aes(x = year, y = total_deceased_donors_imputed_per_mil,
                group = country),
            data = filter(organs_preprocessed_top_20, highlight == "Other"),
            alpha = 0.3, color = "grey50", linewidth = 0.5) +
  # add the Spain, Croatia, and US line layers
  geom_line(aes(x = year, y = total_deceased_donors_imputed_per_mil,
                group = country,
                col = highlight),
            data = filter(organs_preprocessed_top_20, highlight != "Other"), linewidth = 1.5) +
  # add the country name annotation 
  geom_text(aes(x = year, y = total_deceased_donors_imputed_per_mil,
                label = country),
            data = filter(organs_preprocessed_top_20,
                          highlight != "Other", year == 2017),
            hjust = 0, nudge_x = 0.3, col = "grey30") +
  # Choose the colors
  scale_color_manual("Country", values = c("#84ACCE", "#F6AE2D", "#589D6F"), guide = NULL) +
  scale_x_continuous(limits = c(2000, 2020), breaks = seq(2000, 2015, 5)) +
  # remove grid
  theme_classic() +
  labs(x = "Year", y = "Donations per million")

# "This plot shows a grid of the unimputed donation counts per million over time for each of the top 15 countries in 2017"

organs_preprocessed |>
  # filter to the top 15 countries in 2017
  filter(country %in% countries_top_20_2017_per_mil$country[1:15]) |>
  # make sure that the countries are arranged in 2017 donor rate order
  arrange(desc(year), desc(total_deceased_donors_imputed_per_mil)) |>
  mutate(country = fct_inorder(country)) |>
  # create line plots
  ggplot() +
  geom_line(aes(x = year, y = total_deceased_donors_per_mil)) +
  # split line plots by country
  facet_wrap(~country, ncol = 3) +
  # Clean up the the plot
  scale_y_continuous("Organ donations per million") +
  scale_x_continuous("Year",
                     breaks = seq(2000, 2017, 5)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.line = element_blank()) +
  ggtitle("Number of organ donations per million from 2000 to 2017",
          subtitle = "For the top 20 countries in 2017")

## The relationship between population and number of donors

cor_pop_donor <- organs_preprocessed |>
  filter(year == 2017) |>
  summarise(cor = cor(population_imputed, total_deceased_donors_imputed)) |>
  pull(cor)
cor_pop_donor

# "A scatterplot of imputed donor rates and population for 2017"
organs_preprocessed |>
  filter(year == 2017) |>
  ggplot() + 
  geom_point(aes(x = population_imputed, y = total_deceased_donors_imputed),
             alpha = 0.5) +
  geom_text(aes(x = population_imputed, y = total_deceased_donors_imputed,
                label = country),
            alpha = 0.5, check_overlap = TRUE, hjust = 0, nudge_x = 10000000)

# "A log-scale scatterplot of imputed donor rates and population for 2017"
organs_preprocessed |>
  filter(year == 2017, total_deceased_donors_imputed < 2500, population_imputed < 500000000) |>
  ggplot() + 
  geom_point(aes(x = population_imputed, y = total_deceased_donors_imputed),
             alpha = 0.5) +
  geom_text(aes(x = population_imputed, y = total_deceased_donors_imputed,
                label = country),
            alpha = 0.5, check_overlap = TRUE, hjust = 0, nudge_x = 10000000)

organs_preprocessed |>
  filter(year == 2017) |>
  ggplot() + 
  geom_point(aes(x = population_imputed, y = total_deceased_donors_imputed),
             alpha = 0.5) +
  geom_text(aes(x = population_imputed, y = total_deceased_donors_imputed,
                label = country),
            alpha = 0.5, check_overlap = TRUE, hjust = 0, nudge_x = 0.05) +
  geom_smooth(aes(x = population_imputed, y = total_deceased_donors_imputed),
              method = "lm", se = FALSE, col = "grey") +
  scale_x_log10() +
  scale_y_log10()

## \[Exercise: to complete\] Is there a difference in deceased donor type (i.e., whether the organs come from brain death or circulatory death donors) across different countries?

# Conduct your own analysis to answer this question. The relevant variables in the pre-processed data (`organs_preprocessed`) will be `deceased_donors_brain_death`, `deceased_donors_circulatory_death`, and `country`.

## \[Exercise: to complete\] Create a dot plot comparing the organ donation rates for the US and Spain


organs_preprocessed %>%
  filter(country %in% c("Spain", "United States of America"), 
         year == 2017) %>%
  transmute(country, 
            kidney = total_kidney_tx / population * 1000000,
            liver = total_liver_tx / population * 1000000, 
            heart = total_heart_tx / population * 1000000, 
            lung = total_lung_tx / population * 1000000) %>%
  pivot_longer(c("kidney", "liver", "heart", "lung"), 
               names_to = "organ", values_to = "donation_rate") %>%
  arrange(donation_rate) %>%
  mutate(organ = fct_inorder(organ))

