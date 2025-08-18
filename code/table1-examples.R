library(tidyverse)
library(gtsummary)

# Load and clean data
nlsy_cols <- c(
  "glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
  "id", "nsibs", "samp", "race_eth", "sex", "region",
  "income", "res_1980", "res_2002", "age_bir"
)
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
  na = c("-1", "-2", "-3", "-4", "-5", "-998"),
  skip = 1, col_names = nlsy_cols
) |>
  mutate(
    region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
    sex_cat = factor(sex, labels = c("Male", "Female")),
    race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
    eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
    glasses_cat = factor(glasses, labels = c("No", "Yes"))
  )


# simple table
tbl_summary(
  nlsy,
  by = sex_cat,
  include = c(
    sex_cat, race_eth_cat, region_cat,
    eyesight_cat, glasses, age_bir
  )
)

# add labels for the variables and for the "missing" category
tbl_summary(
  nlsy,
  by = sex_cat,
  include = c(
    sex_cat, race_eth_cat, region_cat,
    eyesight_cat, glasses, age_bir
  ),
  label = list(
    race_eth_cat ~ "Race/ethnicity",
    region_cat ~ "Region",
    eyesight_cat ~ "Eyesight",
    glasses ~ "Wears glasses",
    age_bir ~ "Age at first birth"
  ),
  missing_text = "Missing"
)

# add p-values, a total column, bold labels, and remove the footnote
tbl_summary(
  nlsy,
  by = sex_cat,
  include = c(
    sex_cat, race_eth_cat,
    eyesight_cat, glasses, age_bir, region_cat
  ),
  label = list(
    race_eth_cat ~ "Race/ethnicity",
    eyesight_cat ~ "Eyesight",
    glasses ~ "Wears glasses",
    age_bir ~ "Age at first birth", region_cat ~ "Region"
  ),
  missing_text = "Missing"
) |>
  # change the test used to compare sex_cat groups
  add_p(test = list(
    all_continuous() ~ "t.test",
    all_categorical() ~ "chisq.test"
  )) |>
  # add a total column with the number of observations
  add_overall(col_label = "**Total** N = {N}") |>
  bold_labels() |>
  # remove the default footnotes
  modify_footnote(update = everything() ~ NA) |>
  # replace the column headers and make them bold
  modify_header(label = "**Variable**", p.value = "**P**")


##############################################################333


# add p-values, a total column, bold labels, and remove the footnote
tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(
		starts_with("sleep"),
		race_eth_cat,
		region_cat,
		income
	),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		region_cat ~ "Region",
		income ~ "Income",
		sleep_wkdy ~ "Sleeps on Weekday",
		sleep_wknd ~ "Sleeps on weekend"
	),
	missing_text = "Missing",

	statistic = list(income ~ "10% = {p10}; 90% = {p90}",
									 sleep_wkdy ~ "min = {min}; max = {max}",
									 sleep_wknd ~ "min = {min}; max = {max}"
									 ),

	digits = list(income ~ c(3, 3),
								sleep_wkdy ~ c(1,1),
								sleep_wknd ~ c(1,1)
								)
	)|>

	# change the test used to compare sex_cat groups
	add_p(test = list(
		all_continuous() ~ "t.test",
		all_categorical() ~ "chisq.test"
	)) |>
	# add a total column with the number of observations
	add_overall(col_label = "**Total** N = {N}") |>
	bold_labels() |>
	# remove the default footnotes
	modify_footnote(update = everything() ~ NA) |>
	# replace the column headers and make them bold
	modify_header(label = "**Variable**", p.value = "**P**")

