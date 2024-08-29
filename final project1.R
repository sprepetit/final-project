library(gtsummary)
library(tidyverse)
here::here ()
library(tidytuesdayR)
# Get the Data
# Read in with tidytuesdayR package
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2022-01-18')
chocolate <- tuesdata$chocolate
library(dplyr)
library(forcats)
# 1 Create a {gtsummary} table of descriptive statistics about your data (1 pt)

#grouping my categories of interest


chocolate2 <- chocolate|>
  mutate(company_location2 = fct_lump(company_location, n = 3),
  			 bean_origin2 = fct_lump(country_of_bean_origin, n = 5),
  			 cocoa_pct2 = fct_lump(cocoa_percent, n = 5),
				 ingredients2 = fct_lump(ingredients, n = 3)
	)


table(chocolate$company_location)
table(chocolate2$company_location2)

chocolate3 <- filter(chocolate, company_location == "U.S.A." |
										 company_location == "Canada" | company_location == "France")

1136+176+177

table(chocolate3$company_location)
  #making my table
  tbl_summary(
  	chocolate2,
  	by=company_location2,
  	include = c( bean_origin2, cocoa_pct2, ingredients2),
  	missing_text = "missing")  |>

  	add_overall()|>
    bold_labels()

# 2 Fit a regression and present well-formatted results from the regression
  tbl_uvregression(
  	chocolate2,
  	y = rating,
  	include = c(bean_origin2, cocoa_pct2,ingredients2),
  	method = glm,
  	method.args=list(family = gaussian()))

  lm(rating ~ bean_origin2, data = chocolate2)


# 3 Create a figure
hist(chocolate2$rating)

# 4 Write and use a function that does something with the data
 new_mean <- function(x) {
	n <- length(x)
	mean_val <- sum(x) / n
	return(mean_val)
 }

# 5 Create and render a quarto document
# 6 Use the {here} package every time you refer to file paths when reading in data and saving any files
