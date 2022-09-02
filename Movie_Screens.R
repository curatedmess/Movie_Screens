# National Cinema Day | September 3, 2022 | Movie Theater Screens in America
# Data source is from https://www.natoonline.org/

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(scales)
library(rvest)
library(janitor)

# add font ----------------------------------------------------------------
font_add_google(name = "Red Hat Display", family = "Red Hat Display")
font_add_google(name = "Red Hat Text", family = "Red Hat Text")

# turn on showtext --------------------------------------------------------
showtext_auto()

# create font variable ----------------------------------------------------
font1 <- "Red Hat Display"
font2 <- "Red Hat Text"

# WEB SCRAPE -----------------------------------------------------

# get data using rvest for screen scraping html ---------------------------

# url
url <- "https://www.natoonline.org/data/us-movie-screens/"

web_data <- read_html(url)

# get data and create df --------------------------------------------------
cinema_screens <- web_data %>%
  html_nodes(xpath = '//*[@id="tablepress-5"]') %>%
  html_table()

df_temp <- data.frame(cinema_screens) %>%
  clean_names()

# wrangle data for plot ---------------------------------------------------
df <- df_temp %>%
  select(!total) %>% 
  pivot_longer(cols = c("indoor", "drive_in"),
               names_to = "screens", 
               values_to = "number") %>% 
  mutate(number = as.numeric(gsub(",", "", number))) %>% 
  group_by(screens) %>% 
  arrange(year, .by_group = TRUE) %>% 
  mutate(pct_change = round((number/lag(number) - 1), 2)) %>% 
  ungroup() %>% 
  filter(year >= "1990")

color <- ifelse(df$pct_change < 0, "#ef8354", "#4f5d75")
df$screens <- factor(df$screens, labels = c("Drive-In", "Indoor"))

# create plot -------------------------------------------------------------
df %>% 
  ggplot(aes(x = year, y = pct_change)) +
  geom_bar(stat = "identity", fill = color) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  facet_wrap(~screens) +
  theme_minimal() +
  theme(text = element_text(size = 10, family = font2, color = "#000000"),
        plot.title = element_text(family = font1, size = 24, hjust = 0.5, face="bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font2, size = 10, hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, size = 9),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 8, family = font2, color = "#000000"),
        axis.title.y = element_text(size = 8, family = font2, color = "#000000"),
        legend.position = "top",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "#000000", size = 0.25, linetype = "dashed"),
        axis.line.x.bottom = element_line(color = "#000000", size = 0.4),
        panel.spacing = unit(1, "cm"),
        strip.text = element_text(size = 9, family = font2, color = "#000000", face="bold"),
        plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm"),
        plot.background = element_rect(color = "#F2F2F2", fill = "#F2F2F2")) +
  labs(title = "Movie Theater Screens",
       subtitle = "Annual percentage change in the number of movie screens in the U.S. by type from 1990 to 2020\n\n",
       caption = "\n\n#NationalCinemaDay | Data: www.natoonline.org | Design: Ryan Hart",
       y = "Change in Number of Movie Screens\n")

# save plot ---------------------------------------------------------------
ggsave(paste0("Movie_Screens_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 6)




