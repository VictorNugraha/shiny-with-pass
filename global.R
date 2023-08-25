# Shiny
library(shiny)
library(shinyjs)
library(sever)
library(shinymanager)
library(shinybusy)
library(shinydashboard)
library(shinythemes)
library(dashboardthemes)
library(shinydashboard)
library(shinycssloaders)
library(shiny)
library(shinyWidgets)
library(bslib)
library(slickR)
library(rintrojs)
library(shinyBS)
library(shinyjs)

# Wrangling & Viz
library(dplyr) 
library(ggplot2)
library(scales)
library(plotly)
library(glue)
library(gganimate)
library(caret)

rm_model <- readRDS("model_rm_happy.RDS")

# define some credentials
credentials <- data.frame(
  user = c("happy"), # mandatory
  password = c("happy123") # mandatory
)

set_labels(language = "en",
           "Please authenticate" = "Unlock Your Hapiness")

# Read data
happiness <- readRDS("data_input/Happiness.RDS")

happiness_SS_2022_animate <- happiness %>% 
  group_by(Regional.Indicator) %>% 
  summarise(Social_support = round(mean(Social.Support),3)) %>% 
  arrange(desc(Social_support)) %>% 
  mutate(rank = rank(-Social_support))

plot_anim_1 <- ggplot(data = happiness_SS_2022_animate, mapping = aes(x = Social_support, y = reorder(Regional.Indicator, Social_support))) +
  geom_col(mapping = aes(fill = Social_support)) +
  geom_label(aes(label = Social_support), size = 3, nudge_x = 0.03) +
  labs(title = "Social Support Ranking by Region",
       subtitle = "Year: 2018 - 2022",
       x = "Social Support Score",
       y = "Regional Indicator") +
  theme_minimal() +
  theme(legend.position = "none") +
  transition_states(rank) + shadow_mark()

# animate(plot_anim_1)

## To Select An Entire Group For Picker Input Choices

picker_in <- HTML("
$(function() {
  let observer = new MutationObserver(callback);

  function clickHandler(evt) {
    Shiny.setInputValue('group_select', $(this).children('span').text());
  }

  function callback(mutations) {
    for (let mutation of mutations) {
      if (mutation.type === 'childList') {
        $('.dropdown-header').on('click', clickHandler).css('cursor', 'pointer');
        
      }
    }
  }

  let options = {
    childList: true,
  };

  observer.observe($('.inner')[0], options);
})
")