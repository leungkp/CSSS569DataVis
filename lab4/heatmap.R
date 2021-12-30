##### Heatmap example 
##### From https://www.royfrancis.com/a-guide-to-elegant-tiled-heatmaps-in-r-2019/
library(tidyverse)

############### Data wrangling ###############
m <- read_csv("lab4/data/measles_lev1.csv", skip = 2)

m2 <- m %>%
  # convert data to long format
  gather(key="state",value="value",-YEAR,-WEEK) %>%
  # rename columns
  setNames(c("year","week","state","value")) %>%
  # convert year to factor
  mutate(year=factor(year)) %>%
  # convert week to factor
  mutate(week=factor(week)) %>%
  # convert value to numeric (also converts '-' to NA, gives a warning)
  mutate(value=as.numeric(value))

fn_tc <- function(x) paste(str_to_title(unlist(strsplit(x,"[.]"))),collapse=" ")
m2$state <- sapply(m2$state,fn_tc)

na_sum <- function(x)
{
  if(all(is.na(x))) val <- sum(x,na.rm=F)
  if(!all(is.na(x))) val <- sum(x,na.rm=T)
  return(val)
}

# sum incidences for all weeks into one year
m3 <- m2 %>%
  group_by(year,state) %>%
  summarise(count=na_sum(value)) %>%
  as.data.frame()

m4 <- m3 %>%
  as_tibble() %>%
  # convert state to factor and reverse order of levels
  mutate(state=factor(state,levels=rev(sort(unique(state))))) %>%
  # create a new variable from count
  mutate(countfactor=cut(count,breaks=c(-1,0,1,10,100,500,1000,max(count,na.rm=T)),
                         labels=c("0","0-1","1-10","10-100","100-500","500-1000",">1000"))) %>%
  # change level order
  mutate(countfactor=factor(as.character(countfactor),levels=rev(levels(countfactor)))) %>%
  rename(countCat = countfactor)

head(m4)

############### Saving the wrangled data ###############
write_csv(m4, "measles.csv")
########################################################

################## Data visualization ##################
# Load data and factorize variables
measles <- read_csv("lab4/data/measles.csv")

levels <- rev(c("0", "0-1", "1-10", "10-100", "100-500", "500-1000", ">1000"))

measles <- measles %>%
  mutate(year = factor(year),
         state = factor(state),
         countCat = factor(countCat, levels = levels))

head(measles)

# Visualization: Basic
ggplot(measles, aes(x = year, y = state, fill = count))+
  geom_tile() 
  
# Scale_fill_gradient: http://colorbrewer2.org/
ggplot(measles, aes(x = year, y = state, fill = count))+
  geom_tile()
  scale_fill_gradient(high = "#d7191c", low = "#2b83ba", na.value = "grey90")

# Visualization: Intermediate
ggplot(measles, aes(x = year, y = state, fill = countCat)) +
  geom_tile()

# scale_fill_brewer
ggplot(measles, aes(x = year, y = state, fill = countCat)) +
  geom_tile() +
  scale_fill_brewer(palette = "YlGnBu", direction = -1, na.value = "grey90")

# Alternatively, using RColorBrewer
blues <- rev(brewer.pal(7, "YlGnBu"))

ggplot(measles, aes(x = year, y = state, fill = countCat)) +
  geom_tile() + 
  scale_fill_manual(values = blues, na.value = "grey90")

# Adavanced
hm <- 
  ggplot(measles, aes(x = year, y = state, fill = countCat))+
  # add border white colour of line thickness 0.25
  geom_tile(colour = "white", size = 0.25)+
  # scale fill with "YlGnBu" palette
  scale_fill_brewer(palette = "YlGnBu", direction = -1, na.value = "grey90") +
  # define new breaks on x-axis
  scale_x_discrete(expand = c(0, 0), breaks = seq(1930, 2000, 10))+
  # remove extra space
  scale_y_discrete(expand = c(0, 0))+
  # set a base size for all fonts
  theme_grey(base_size = 8)+
  # theme options
  theme(
    # bold font for legend text
    legend.text = element_text(face = "bold"),
    # set thickness of axis ticks
    axis.ticks = element_line(size = 0.4),
    # remove plot background
    plot.background = element_blank(),
    # remove plot border
    panel.border = element_blank(),
    # reshape the legend keys
    legend.key.height=grid::unit(0.8,"cm"),
    legend.key.width=grid::unit(0.2,"cm")
  ) +
  #legend title
  guides(fill = guide_legend(title="Cases per\n100,000 people"))+
  #remove x and y axis labels; define title
  labs(x = "",y = "",title = "Incidence of Measles in the US")

ggsave("heatmap.pdf", height = 5.5, width = 8.8, units = "in")
