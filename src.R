library(magrittr)
library(tidyverse)

employed_raw <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/employed.csv')

## Wrangling

women_employed_by_industry  <- employed_raw %>%
  filter(!is.na(employ_n), race_gender == 'Women') %>%
  group_by(industry, year) %>%
  summarise(employ_n = sum(employ_n))

total_employed_by_industry <- employed_raw %>%
  filter(!is.na(employ_n), race_gender == 'TOTAL') %>%
  group_by(industry,year) %>%
  summarise(employ_n = sum(employ_n)) 

women_employed_by_industry %<>%
  left_join(total_employed_by_industry, by = c('industry','year'), suffix = c('_women', '_total')) %>%
  mutate(employ_prop_women = employ_n_women / employ_n_total)

## Visualizing 

p<-women_employed_by_industry %>%
  mutate(employ_prop_women_status = if_else(employ_prop_women >= 0.55,
                                            'majority',
                                            if_else(employ_prop_women <= .45,
                                                    'minority', 'equality'))) %>%
  ggplot(aes(x = year, y = industry, size = employ_prop_women,
             color = employ_prop_women_status)) +
  geom_point(alpha=0.7) +
  scale_size( range = c(0, 10), breaks=c(.25, .5, .75), labels=c('25%', '50%', '75%')) +
  scale_color_manual(breaks = c('minority', 'equality', 'majority'),
                     values=c('#eb9486','#7e7f9a' , '#f3de8a')) +
  guides(size = guide_legend(override.aes = list(colour = '#f9f8f8', shape=21, alpha=1)),
         color = guide_legend(override.aes = list(shape=15))) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = '#272838'),
        plot.title = element_text(size=16),
        axis.text.x =element_text(color = '#F9F8F8'),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y =element_text(color = '#F9F8F8', face = 'bold'),
        axis.line.x = element_line(size=1, color = '#F9F8F8'),
        text = element_text(family= 'sans', color = '#F9F8F8')) +
  labs(color = 'Women employed', size = 'Proportion (>=)',
       caption = 'Source: US Bureau of Labor Statistics | Author: Victor Navarro')
p

ggsave('tidytuesday_plot.png', width = 22.2, height = 18.8,dpi=700, limitsize = FALSE, unit = 'cm')
