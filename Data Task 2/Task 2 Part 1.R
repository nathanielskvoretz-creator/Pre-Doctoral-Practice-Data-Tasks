library(tidyverse)

sci_raw <- read.csv("data/Part1/county_county_sci.tsv", sep = "\t") 
dist <- read.csv("data/Part1/sf12010countydistancemiles.csv")
counties <- read.csv("data/Part1/county_description.csv")
county_dem <- read.csv("data/Part1/county_demographics.csv") |> 
  pivot_wider(
    names_from = "measure" ,
    values_from = "value"
  )

sci <- sci_raw |> 
  left_join(counties, by = join_by("user_loc" == "county_fips")) |> 
  left_join(counties, by = join_by("fr_loc" == "county_fips"), suffix = c("_user", "_fr"))

sci_dist <- sci |> 
  left_join(dist, by = join_by("user_loc" == "county1", "fr_loc" == "county2"))

sci_dist$mi_to_county[is.na(sci_dist$mi_to_county)] <- 0

##Part1 

#a
sci |> 
  filter(county_name_user == "Washtenaw") |> 
  ggplot(aes(x = scaled_sci)) + 
  geom_density()

sci |> 
  filter(county_name_user == "Washtenaw") |> 
  ggplot(aes(x = log(scaled_sci))) + 
  geom_density()

#b
sci |> 
  filter(county_name_user == "Washtenaw") |> 
  arrange(desc(scaled_sci)) |> 
  select(county_name_fr, state_name_fr) |> 
  as_tibble()

# The most connected counties to Washtenaw by SCI are Washtenaw, Lenawee, Livingston, Jackson, Monroe, Wayne, Oakland, Ingham, Leelanau, and Hillsdale

#c
sci_dist |> 
  sample_n(50000) |> 
  filter(mi_to_county>0, scaled_sci>0) |> 
  ggplot(aes(x = mi_to_county, y = scaled_sci)) +
  geom_point(alpha = 0.1) + 
  scale_y_continuous(
    labels = function(y) paste0(y / 1000, "k")
  ) +
  coord_cartesian(ylim = c(0, 1000000)) 

# Clear negative relationship between SCI and distance. Likely an exponential relationship

sci_dist_model <- lm(data = sci_dist, scaled_sci ~ mi_to_county) 
summary(sci_dist_model)

#d
county_net_con <- sci_dist |> 
  group_by(county_name_user, user_loc, state_fips_user, state_abrev_user, state_name_user) |> 
  summarise(
    net_con = sum(scaled_sci[mi_to_county < 50 ])/sum(scaled_sci)
  )
# I choose to look at the portion of summed SCI for a county that came from counties within 50 mikes. 

#e
county_dem_net_con <- county_net_con |> 
  left_join(county_dem, by = join_by("user_loc" == "county_fips"))

#measure 1
county_dem_net_con |> 
  ggplot(aes(x = no_highschool_share, y = net_con)) +
  geom_point(alpha = 0.5) +
  geom_smooth()

# The slight positive relationship could be explained by limited social mobility.
# Lower social mobility could then lead to lower physical mobility. 
# Thus is friendship networks are relatively consistent across social groups, there would be higher network concentration for counties with higher relative non highschool education

#measure 2
county_dem_net_con |> 
  ggplot(aes(x = median_hh_income, y = net_con)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")

# Similar to the last one, but the opposite effect. Higher incomes indicate higher social and thus physical mobility.
# Thus friendship networks will be more spread-out 

#measure 3
county_dem_net_con |> 
  ggplot(aes(x = e_rank_b, y = net_con)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")

# Interestingly upward social mobility does not have an impact. 