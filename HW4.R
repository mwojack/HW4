library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
cars93 <- MASS::Cars93


####################################3
#LM
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = TRUE, method = "lm", color = "#8fe388") +
  labs(title = "LM Method") +
  theme(plot.title = (text = element_text(color='#8fe388', size = 14))) +
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000")
  ) +
  scale_y_continuous(name = "fuel-tank capacity( Gallons)")


# GLM
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = TRUE, method = "glm", color = "#fe8d6d") +
  labs(title = "GLM Method") +
  theme(plot.title = (text = element_text(color='#fe8d6d', size = 14))) +
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000")
  ) +
  scale_y_continuous(name = "fuel-tank capacity(Gallons)")


# GAM
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = TRUE, method = "gam", color = "#7c6bea") +
  labs(title = "GAM Method") +
  theme(plot.title = (text = element_text(color='#7c6bea', size = 14))) +
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000")
  ) +
  scale_y_continuous(name = "fuel-tank capacity\n(US gallons)")
#######################4
library(tidyverse)
library(dplyr)
load("C:/Users/mwoja/Downloads/Rscripts/preprint_growth.rda") 
head(preprint_growth)
#a
preprint_growth %>% filter(archive == "bioRxiv") %>%
  filter(count > 0) -> biorxiv_growth
preprints<-preprint_growth %>% filter(archive %in%
                                        c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")) %>%filter(count > 0) %>%
  mutate(archive = factor(archive, levels = c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")))
#e
preprints_final <- filter(preprints, date == ymd("2017-01-01"))
ggplot(preprints) +
  aes(date, count, color = archive, fill = archive) +
  geom_line(size = 1) +
  scale_y_continuous(
    limits = c(0, 600), expand = c(0, 0),
    name = "preprints / month",
    sec.axis = dup_axis( #this part is for the second y axis
      breaks = preprints_final$count, #and we use the counts to position our labels
      labels = c("arXivq-bio", "PeerJPreprints", "bioRxiv"),
      name = NULL)
  ) +
  scale_x_date(name = "year",
               limits = c(min(biorxiv_growth$date), ymd("2017-01-01"))) +
  scale_color_manual(values = c("#0072b2", "#D55E00", "#009e73"),
                     name = NULL) +
  labs(title='Preprint Counts')
  #theme(legend.position = "none")
#########################  a-f
preprint_growth %>% drop_na()

preprint_full<-preprint_growth %>% filter(archive %in% c("bioRxiv", "F1000Research")) %>% filter(count > 0) %>% filter(date > ymd("2014-02-01"))

ggplot(preprint_full) +
  aes(date, count, color = archive, fill = archive) +
  geom_line(size = 1) + 
  scale_color_manual(values = c("#7c6bea", "#fe8d6d"), name = NULL) +
  scale_x_date(name = "year",
               limits = c(min(preprint_full$date), ymd("2018-01-01"))) +
  theme(legend.position = "right") +
  scale_x_date(name = "year") 

### added title in the code sample code for #4


