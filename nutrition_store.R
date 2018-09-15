library(readr)
library(tidyverse)
library(naniar)

db <- read_delim("local_data/en.openfoodfacts.org.products.csv", 
           "\t", escape_double = FALSE, trim_ws = TRUE)

db_sample <- db %>% sample_n(3000)

vis_miss(db_sample, warn_large_data = FALSE) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

keep_features <- db %>% 
  summarise_all(funs(sum(is.na(.)))) %>%
  gather(feature, n_na) %>% 
  mutate(frac_na = n_na/nrow(db)) %>% 
  arrange(frac_na) %>% 
  
  filter(frac_na < 0.3) %>% 
  filter(! feature %in% c("creator", "url", "created_t", "last_modified_t", 
                          "last_modified_datetime", "created_datetime", "states", "states_tags", "countries")) %>% 
  .$feature
  
keep_brands <- db %>% 
  select(keep_features) %>% 
  filter(countries_en == "United States") %>% 
  group_by(brands_tags) %>% 
  summarise(energy = mean(energy_100g, na.rm = TRUE),
            sat_fat = mean(`saturated-fat_100g`, na.rm = TRUE),
            n = n(),
            n_na = sum(is.na(.))) %>% 
  arrange(desc(n))

keep_brands %>% ggplot(aes(n)) +
  geom_histogram() + 
  scale_y_log10()  

keep_brands %>% 
  filter(n > 50) %>%
  ggplot(aes(n, sat_fat)) +
  geom_point()+ 
  #scale_y_log10() + 
  scale_x_log10()
