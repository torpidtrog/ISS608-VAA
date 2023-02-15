pacman::p_load( ggthemes, hrbrthemes, ggrepel, RColorBrewer, gridExtra, gcookbook, tidyverse)

data <- read_csv(file = '/Users/Aishwarya/Documents/AISHWARYA MALOO/SMU ACADEMIC/SEMESTER 5/VISUAL ANALYTICS/Take Home Exercise/Ex02/AgeSexPop.csv')

colnames(data)

# data$Pop_prop <- as.numeric(sub("%", "",data$Pop_Proportion,fixed=TRUE))/100

view(data)

unique(data$`Age Group`)

data$Age_groups <- with(data, dplyr::case_when(`Age Group` %in% c("0 - 4", "5 - 9") ~ '0 - 9',
                                               `Age Group` %in% c("10 - 14", "15 - 19") ~ '10 - 19',
                                               `Age Group` %in% c("20 - 24", "25 - 29") ~ '20 - 29',
                                               `Age Group` %in% c("30 - 34", "35 - 39") ~ '30 - 39',
                                               `Age Group` %in% c("40 - 44", "45 - 49") ~ '40 - 49',
                                               `Age Group` %in% c("50 - 54", "55 - 59") ~ '50 - 59',
                                               `Age Group` %in% c("60 - 64", "65 - 69") ~ '60 - 69',
                                               `Age Group` %in% c("70 - 74", "75 - 79") ~ '70 - 79',
                                               `Age Group` %in% c("80 - 84", "85 & Over") ~ '80+'))

view(data)

data_PlanningArea <- data %>%
  group_by(`Planning Area`) %>%
  summarise(sum_pop = sum(Population), .groups = 'drop') %>%
  arrange(sum_pop,.by_group = TRUE) %>%
  top_n(9)

data_PlanningArea



top9_PA <- data %>% filter(`Planning Area` %in% c("Choa Chu Kang", "Ang Mo Kio", "Yishun", "Sengkang", "Hougang", "Woodlands", "Tampines", "Jurong West","Bedok"))

top9_PA

str(data_top9)


plot_PA <- ggplot(data = top9_PA, aes(x = Age_groups, y = Population, fill = Sex)) +
  
  geom_bar(data = top9_PA %>% filter(Sex == "Male"),
           stat = "identity",
           position = "identity") +
  
  geom_bar(data = top9_PA %>% filter(Sex == "Female"),
           stat = "identity",
           position = "identity",
           mapping = aes(y = -(Population))) +
  
  scale_fill_manual(values=c("coral4", "paleturquoise4")) +
  
  labs ( x = "Age Group", y = "Population in Planning Area in Thousands",  title = "Age-Sex Pyramid of the 9 most populated planning areas in Singapore - June 2022", subtitle = " ", caption = "Source: Department of Statistics, Singapore (SingStat)") +

  scale_y_continuous(breaks = seq(-15000,15000,3000), labels = c("15", "12", "09", "06", "03", "0", "03", "06", "09", "12", "15")) +
  
  coord_flip() +
  theme_classic() +
  facet_wrap(~`Planning Area`, ncol = 3)

plot_PA

#just for fonts, size, placements
plot_PA + 
  theme(text = element_text(family = "Garamond"),
        plot.margin = margin(t=0.1, r=0.1, b=0.1, l=0.1),
        plot.title = element_text(hjust = 0.2, face = "bold", size = 14), 
        plot.caption = element_text(hjust = 0, face = "italic", size = 13),
        plot.caption.position = 'plot',
        
        panel.grid.major.x = element_line(colour = 'grey', linewidth = 0.2, linetype = "dotted"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        
        axis.text = element_text(size = 8, face = "bold"),
        axis.line = element_line(color = 'grey'),
        axis.ticks = element_line(color = 'grey'),
        axis.title = element_text(face = "bold", size = 12),
        
        legend.title = element_text(size = 10),
        legend.key.size = unit(5, "mm"),
        legend.position = "bottom") +
  
  theme(strip.text = element_text(size = 10, face = "bold"))
  
#need to change 

# only with colours of the strip and bars - need to change and adjust palette 
p1 + theme(strip.text = element_text(size = 12, face = "bold"), strip.background = element_rect(fill = "lavendarblush4"))+
  scale_fill_brewer(palette = "Dark2")