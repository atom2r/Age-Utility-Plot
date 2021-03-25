library(tidyverse)
library(rvest)
library(ggrepel)
library(scales)
library(glue)
library(extrafont)
library(lubridate)
library(magick)
library(grid)
library(showtext)

font_add(family = "Raleway-Medium", regular = "./new font/Raleway/Raleway-Medium.ttf")
showtext_auto()

loadfonts()

logo <- image_read("https://pngimage.net/wp-content/uploads/2018/06/psg-logo-png-3.png")
gpp <- rasterGrob(logo, interpolate=TRUE)


page <- read_html("https://www.transfermarkt.com/paris-saint-germain/leistungsdaten/verein/583/reldata/%262020/plus/1")

# grab name from photo element instead
result_name <- page %>% 
  html_nodes(".bilderrahmen-fixed") %>% 
  html_attr("title") 

result_name

# grab age
result_age <- page %>% 
  html_nodes(".posrela+ .zentriert") %>% 
  html_text()

# grab minutes played in league
result_mins <- page %>% 
  html_nodes("td.rechts") %>% 
  html_text()

## get length
page <- read_html("https://www.transfermarkt.com/paris-saint-germain/kader/verein/583/saison_id/2020/plus/1")

result_name2 <- page %>% 
  html_nodes(".bilderrahmen-fixed") %>% 
  html_attr("title") 

result_bday <- page %>% 
  html_nodes(".posrela+ .zentriert") %>% 
  html_text()

result_joinedteam <- page %>% 
  html_nodes("td:nth-child(7)") %>% 
  html_text()

result_leaveteam <- page %>% 
  html_nodes("td:nth-child(9)") %>% 
  html_text()

# place each vector into list
resultados <- list(result_name, result_age, result_mins)

col_name <- c("name", "age", "minutes")

results_comb <- resultados %>% 
  reduce(cbind) %>% 
  as_tibble() %>% 
  set_names(col_name)

results_comb

## join + bday
resultados2 <- list(result_name2, result_bday, 
                    result_joinedteam, result_leaveteam)

col_name2 <- c("name", "bday", "join", "leave")

results_comb2 <- resultados2 %>% 
  reduce(cbind) %>% 
  as_tibble() %>% 
  set_names(col_name2)



## combine BOTH
results_comb <- results_comb %>% 
  left_join(results_comb2) 

results_comb

all_team_minutes <- results_comb %>% 
  mutate(age = as.numeric(age),
         minutes = minutes %>% 
           str_replace("\\.", "") %>% 
           str_replace("\\'", ""))

all_team_minutes$minutes <- as.numeric(all_team_minutes$minutes)

all_team_minutes <- all_team_minutes %>%           
  mutate(min_perc = (minutes / 2970) %>% round(digits = 3),
         bday = str_replace_all(bday, "\\(.*\\)", "") %>% mdy(),
         join = join %>%  mdy(),
         join_age = interval(bday, join) / years(1),
         age_now = interval(bday, Sys.Date()) / years(1))

all_team_minutes <- all_team_minutes %>% 
  filter(!is.na(minutes)) %>%
  arrange(desc(min_perc))

head(all_team_minutes)

# rectanglular highlight for players in their prime:
rect_df <- data.frame(
  xmin = 24, xmax = 29,
  ymin = -Inf, ymax = 1)


all_team_minutes %>% 
  ggplot(aes(x = age_now, y = min_perc)) +
  
  geom_rect(data = rect_df, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, 
                ymin = ymin, ymax = ymax),
            alpha = 0.3,
            fill = "#1F8446") +
  
  geom_point(color = "#96D223", size = 2.5) +
  
  geom_segment(aes(x= join_age, xend= age_now, y = min_perc, yend = min_perc), color="#96D223", size = 1) +
  
  geom_text_repel(aes(label = name, family = "Raleway-Medium", fontface = "bold"), 
                  size = 10, colour = "#96D223") + #nudge_x = 0.5, nudge_y = 0.02, seed = 8
  
  scale_y_continuous(expand = c(0.01, 0),
                     limits = c(0, 1.08), 
                     breaks = seq(0, 1, 0.2),
                     labels = percent_format()) +
  
  coord_cartesian(xlim =c(17, 35), clip = 'off') +
  
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  
  annotation_custom(gpp, xmin = 36, xmax = 41, ymin = 0.65, ymax = 0.85) +
  
  annotate("text", x = 26.5, y = 1.05, label = "Peak Age", size= 14, colour ="white",
           family = "Raleway-Medium", fontface = "bold") +
  
  annotate("text", x = 38.3, y = 0.6, label = "Squad size: 32", size= 14, colour ="white",
           family = "Raleway-Medium", fontface = "bold") +
  
  annotate("text", x = 38.3, y = 0.55, label = "Average age: 25.6", size= 14, colour ="white",
           family = "Raleway-Medium", fontface = "bold") +
  
  labs(x = "Age (As of 14/2/2021)", y = "% of Minutes Played", 
       title = "Paris Saint-Germain: Age-Utility Matrix",
       subtitle = "Minutes in all Competitions - 2021",
       caption = "Data from transfermarkt.com - By @atom2r") +
  
  theme(plot.title = element_text(family = "Raleway-Medium", size = 40, colour = "white"),
        plot.subtitle = element_text(family = "Raleway-Medium", size = 28, colour = "white"),
        plot.caption = element_text(family = "Raleway-Medium", size = 26, colour = "white"),
        axis.title = element_text(family = "Raleway-Medium", size = 24, colour = "white"),
        axis.text = element_text(family = "Raleway-Medium", size = 24, colour = "white"),
        
        plot.background = element_rect(fill = "#0E0447", colour = "#0E0447"),
        panel.background = element_rect(fill = "#0E0447", colour = "#0E0447"),
        
        panel.grid = element_line(colour = "#1A0883"),
        panel.border = element_rect(fill = NA, colour = "#1A0883"),
        axis.ticks = element_line(colour = "#1A0883"),
        
        plot.margin = margin(1, 5, 1, 1, "cm"),
        
        panel.grid.minor.y = element_blank())

ggsave("Psg Age.png", width = 10, height = 8)
