library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)

load('nyc_property.RData')

## DQR
smr = data.frame(unclass(summary(mydata)), check.names = FALSE, stringsAsFactors = FALSE)

## BBLE
length(unique(mydata$BBLE))

## BLOCK
length(unique(mydata$BLOCK))
sum(is.na(mydata$BLOCK))

ggplot(mydata, aes(x = BLOCK)) +
  geom_histogram()

block_order = mydata %>%
  mutate(BLOCK=as.character(BLOCK)) %>%
  group_by(BLOCK) %>%
  summarise(count = n(),
            percentage = count / nrow(mydata)*100) %>%
  arrange(-count)

ggplot(block_order[1:10,], aes(x = reorder(BLOCK, -count), y = count)) +
  geom_bar(stat = "identity", fill = '#99d8c9') +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  labs(x='BLOCK',y='Count', title='The Top 10 Most Frequently Appeared BLOCK Code')

## LOT
length(unique(mydata$LOT))
sum(is.na(mydata$LOT))

lot_unique = mydata %>%
  mutate(boro_block = str_extract(BBLE, "(^[0-9]{6})")) %>%
  select(boro_block, LOT) %>%
  group_by(boro_block, LOT) %>%
  summarise(count = n())

lot_order = mydata %>%
  mutate(LOT = as.factor(LOT)) %>%
  group_by(LOT) %>%
  summarise(count = n(),
            percentage = count / nrow(mydata)*100) %>%
  arrange(-count)

ggplot(lot_order[1:10,], aes(x = reorder(LOT, -count), y = count)) +
  geom_bar(stat = "identity", fill = '#fc9272') +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  labs(x='LOT',y='Count', title='The Top 10 Most Frequently Appeared LOT Code')

## EASEMENT

data.frame(unclass(summary(mydata$EASEMENT)), check.names = FALSE, stringsAsFactors = FALSE)
sum(is.na(mydata$EASEMENT))

mydata %>%
  group_by(EASEMENT) %>%
  summarise(count = n(), 
            percentage = 100 * count / nrow(mydata)) %>%
  arrange(-count)


mydata %>%
  replace_na(list(EASEMENT = '')) %>%
  group_by(EASEMENT) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = reorder(EASEMENT, -count), y = count)) +
  geom_bar(stat = "identity", fill = '#feb24c') + 
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  labs(x='EASEMENT', y='Log Tranformed Count', title='The Distribution of EASEMENT Code') +
  scale_y_log10()

## OWNER
length(unique(mydata$OWNER))
sum(is.na(mydata$OWNER))

owner_order = mydata %>%
  replace_na(list(OWNER = '')) %>%
  group_by(OWNER) %>%
  summarise(count = n(),
            percentage = count / nrow(mydata) * 100) %>%
  arrange(-count) 

ggplot(owner_order[1:10,], aes(x = reorder(OWNER, - count), y = count)) +
  geom_bar(stat = "identity", fill = '#43a2ca') +
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  labs(x='OWNER', y='Log Tranformed Count', title='The Top 10 Most Frequently Appeared OWNER Name') +
  scale_y_log10()


## BLDGCL
length(unique(mydata$BLDGCL))
summary(mydata$BLDGCL)
sum(is.na(mydata$BLDGCL))

bldgcl_order = mydata %>%
  group_by(BLDGCL) %>%
  summarise(count = n(),
            percentage = count / nrow(mydata)*100) %>%
  arrange(-count) 

ggplot(bldgcl_order[1:10,], aes(x = reorder(BLDGCL, - count), y = count)) +
  geom_bar(stat = "identity", fill = '#c994c7') +
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  labs(x='Building Class', y='Count', title='The Top 10 Most Frequently Appeared Building Class') 



## TAXCLASS
length(unique(mydata$TAXCLASS))
data.frame(unclass(summary(mydata$TAXCLASS)), check.names = FALSE, stringsAsFactors = FALSE)
sum(is.na(mydata$TAXCLASS))

tax_order = mydata %>%
  group_by(TAXCLASS) %>%
  summarise(count = n(),
            percentage = count / nrow(mydata)*100) %>%
  arrange(-count) 

ggplot(tax_order, aes(x = reorder(TAXCLASS, - count), y = count)) +
  geom_bar(stat = "identity", fill = '#91cf60') +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  labs(x='TAXCLASS', y='Log Transformed Count', title='The Distribution of Tax Class') +
  scale_y_log10()

## LTFRONT

length(unique(mydata$LTFRONT))
summary(mydata$LTFRONT)
summary(factor(mydata$LTFRONT))[1:10]
sum(is.na(mydata$LTFRONT))
sum(mydata$LTFRONT == 0)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

mydata$LTFRONT <- as.numeric(as.character(mydata$LTFRONT))

mydata %>%
  filter(LTFRONT != 0) %>%
  summarise(min = min(LTFRONT),
            max = max(LTFRONT),
            median = median(LTFRONT, na.rm = TRUE),
            mean = mean(LTFRONT, na.rm = TRUE),
            mode = Mode(LTFRONT),
            sd = sd(LTFRONT))

summary(filter(mydata, LTFRONT != 0)$LTFRONT)

mydata %>%
  filter(LTFRONT != 0) %>%
  ggplot(aes(x = LTFRONT)) +
  geom_histogram(bins = 20, fill = '#d8b365') +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  labs(x='LTFRONT', y='Log Transformed Count', 
       title='The Distribution of Lot Frontage Excluding 0') +
  scale_y_log10()


## LTDEPTH
length(unique(mydata$LTDEPTH))
summary(mydata$LTDEPTH)
summary(factor(mydata$LTDEPTH))[1:10]
sum(is.na(mydata$LTDEPTH))
sum(mydata$LTDEPTH == 0)

mydata$LTDEPTH <- as.numeric(as.character(mydata$LTDEPTH))

mydata %>%
  filter(LTDEPTH != 0) %>%
  summarise(min = min(LTDEPTH),
            max = max(LTDEPTH),
            median = median(LTDEPTH),
            mean = mean(LTDEPTH),
            mode = Mode(LTDEPTH),
            sd = sd(LTDEPTH))

summary(filter(mydata, LTDEPTH != 0)$LTDEPTH)

mydata %>%
  filter(LTDEPTH != 0) %>%
  ggplot(aes(x = LTDEPTH)) +
  geom_histogram(bins = 20, fill = '#fc8d59') +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  labs(x='LTDEPTH', y='Log Transformed Count', 
       title='The Distribution of Lot Depth Excluding 0') +
  scale_y_log10()

# STORIES

length(unique(mydata$STORIES))
summary(mydata$STORIES)
sum(is.na(mydata$STORIES))

mydata$STORIES <- as.numeric(as.character(mydata$STORIES))

mydata %>%
  filter(!is.na(STORIES)) %>%
  summarise(min = min(STORIES),
            max = max(STORIES),
            median = median(STORIES),
            mean = mean(STORIES),
            mode = Mode(STORIES),
            sd = sd(STORIES))

summary(filter(mydata, STORIES != 0)$STORIES)

ggplot(mydata, aes(x = STORIES)) +
  geom_histogram(bins = 20, fill = '#a6611a') +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  labs(x='STORIES', y='Log Transformed Count', 
       title='The Distribution of Stories Records') +
  scale_y_log10()

# FULLVAL

length(unique(mydata$FULLVAL))
summary(mydata$FULLVAL)
summary(factor(mydata$FULLVAL))[1:10]
sum(is.na(mydata$FULLVAL))

mydata %>%
  filter(FULLVAL!= 0) %>%
  summarise(min = min(FULLVAL),
            max = max(FULLVAL),
            median = median(FULLVAL),
            mean = mean(FULLVAL),
            mode = Mode(FULLVAL),
            sd = sd(FULLVAL))

summary(filter(mydata, FULLVAL != 0)$FULLVAL)

mydata %>%
  filter(FULLVAL!= 0) %>%
  ggplot(aes(x = FULLVAL)) +
  geom_histogram(bins = 100, fill = '#4dac26') +
  coord_cartesian(xlim=c(0,1e+09)) +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  labs(x='FULLVAL', y='Log Transformed Count', 
       title='The Distribution of FULLVAL Records Excluding 0') +
  scale_y_log10()


# AVLAND

length(unique(mydata$AVLAND))
summary(mydata$AVLAND)
summary(factor(mydata$AVLAND))[1:10]
sum(is.na(mydata$AVLAND))

mydata %>%
  filter(AVLAND != 0) %>%
  summarise(min = min(AVLAND),
            max = max(AVLAND),
            median = median(AVLAND),
            mean = mean(AVLAND),
            mode = Mode(AVLAND),
            sd = sd(AVLAND))

summary(filter(mydata, AVLAND != 0)$AVLAND)

mydata %>%
  filter(AVLAND != 0) %>%
  ggplot(aes(x = AVLAND)) +
  geom_histogram(bins = 100, fill = '#fdb863') +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  labs(x='AVLAND', y='Log Transformed Count', 
       title='The Distribution of AVLAND Records Excluding 0') +
  scale_y_log10() +
  coord_cartesian(xlim=c(0,5e+08)) 


# AVTOT

length(unique(mydata$AVTOT))
summary(mydata$AVTOT)
summary(factor(mydata$AVTOT))[1:10]
sum(is.na(mydata$AVTOT))

mydata %>%
  filter(AVTOT != 0) %>%
  summarise(min = min(AVTOT),
            max = max(AVTOT),
            median = median(AVTOT),
            mean = mean(AVTOT),
            mode = Mode(AVTOT),
            sd = sd(AVTOT))

summary(filter(mydata, AVTOT != 0)$AVTOT)

mydata %>%
  filter(AVTOT != 0) %>%
  ggplot(aes(x = AVTOT)) +
  geom_histogram(bins = 100, fill = '#66c2a5') +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  labs(x='AVTOT', y='Log Transformed Count', 
       title='The Distribution of AVTOT Records Excluding 0') +
  scale_y_log10() +
  coord_cartesian(xlim=c(0,1e+09))


# EXLAND

length(unique(mydata$EXLAND))
summary(mydata$EXLAND)
summary(factor(mydata$EXLAND))[1:10]
sum(is.na(mydata$EXLAND))

summary(filter(mydata, EXLAND != 0)$EXLAND)

mydata %>%
  filter(EXLAND != 0) %>%
  summarise(min = min(EXLAND),
            max = max(EXLAND),
            median = median(EXLAND),
            mean = mean(EXLAND),
            mode = Mode(EXLAND),
            sd = sd(EXLAND))

ggplot(mydata, aes(x = EXLAND)) +
  geom_histogram(bins = 100, fill = '#d6604d') +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  labs(x='EXLAND', y='Log Transformed Count', 
       title='The Distribution of EXLAND Records Excluding 0') +
  scale_y_log10() +
  coord_cartesian(xlim=c(0,5e+08)) 

# EXTOT

length(unique(mydata$EXTOT))
summary(mydata$EXTOT)
summary(factor(mydata$EXTOT))[1:10]
sum(is.na(mydata$EXTOT))

mydata %>%
  filter(EXTOT != 0) %>%
  summarise(min = min(EXTOT),
            max = max(EXTOT),
            median = median(EXTOT),
            mean = mean(EXTOT),
            mode = Mode(EXTOT),
            sd = sd(EXTOT))

summary(filter(mydata, EXTOT != 0)$EXTOT)

ggplot(mydata, aes(x = EXTOT)) +
  geom_histogram(bins = 100, fill = '#74add1') +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  labs(x='EXTOT', y='Log Transformed Count', 
       title='The Distribution of EXTOT Records Excluding 0') +
  scale_y_log10() +
  coord_cartesian(xlim=c(0,1e+09)) 

# EXCD1

length(unique(mydata$EXCD1))
summary(mydata$EXCD1)
sum(is.na(mydata$EXCD1))

EX = mydata %>%
  filter(is.na(EXCD1)) %>%
  select(EXLAND, EXTOT, EXCD1)

excd_order = mydata %>%
  group_by(EXCD1) %>%
  filter(!is.na(EXCD1)) %>%
  summarise(count = n(),
            percentage = 100 * count / nrow(mydata)) %>%
  arrange(-count) 

ggplot(excd_order[1:10,], aes(x = reorder(EXCD1, - count), y = count)) +
  geom_bar(stat = "identity", fill = '#9970ab') +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  labs(x='EXCD1', y='Log Transformed Count', 
       title='The Top 10 Most Frequently Appeared EXCD1 Excluding 0') +
  scale_y_log10()


# STADDR

length(unique(mydata$STADDR))
summary(mydata$STADDR)
sum(is.na(mydata$STADDR))

add_order = mydata %>%
  group_by(STADDR) %>%
  filter(!is.na(STADDR)) %>%
  summarise(count = n()) %>%
  arrange(-count) 

ggplot(add_order[1:10,], aes(x = reorder(STADDR, - count), y = count)) +
  geom_bar(stat = "identity", fill = '#fee08b') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
  xlab("STADDR")  +
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  labs(x='STADDR', y='Count', 
       title='The Top 10 Most Frequently Appeared Street Address Excluding Null') 

# ZIP

length(unique(mydata$ZIP))
summary(mydata$ZIP)
sum(is.na(mydata$ZIP))

zip_order = mydata %>%
  group_by(ZIP) %>%
  filter(!is.na(ZIP)) %>%
  summarise(count = n(),
            percentage = 100 * count / nrow(mydata)) %>%
  arrange(-count) 

ggplot(zip_order[1:10,], aes(x = reorder(ZIP, - count), y = count)) +
  geom_bar(stat = "identity", fill = '#d6604d') +
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  labs(x='ZIP', y='Count', 
       title='The Top 10 Most Frequently ZIP Code') 

# EXMPTCL

length(unique(mydata$EXMPTCL))
summary(mydata$EXMPTCL)
sum(is.na(mydata$EXMPTCL))

n = nrow(filter(mydata, EXMPTCL != ""))

EXMPTCL_order = mydata %>%
  group_by(EXMPTCL) %>%
  filter(EXMPTCL!= "") %>%
  summarise(count = n(),
            percentage = 100 * count / n) %>%
  arrange(-count) 

ggplot(EXMPTCL_order, aes(x = reorder(EXMPTCL, - count), y = count)) +
  geom_bar(stat = "identity", fill = '#b2182b') +
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  labs(x='EXMPTCL', y="Log Transformed Count", 
       title='The Distribution of EXMPTCL Excluding NA') +
  scale_y_log10()


# BLDFRONT

length(unique(mydata$BLDFRONT))
summary(mydata$BLDFRONT)
summary(factor(mydata$BLDFRONT))[1:10]
sum(is.na(mydata$BLDFRONT))
sum(mydata$BLDFRONT == 0)

mydata$BLDFRONT <- as.numeric(as.character(mydata$BLDFRONT))

mydata %>%
  filter(BLDFRONT != 0) %>%
  summarise(min = min(BLDFRONT),
            max = max(BLDFRONT),
            median = median(BLDFRONT),
            mean = mean(BLDFRONT),
            mode = Mode(BLDFRONT),
            sd = sd(BLDFRONT))

summary(filter(mydata, BLDFRONT != 0)$BLDFRONT)

mydata %>%
  filter(BLDFRONT != 0) %>%
  ggplot(aes(x = BLDFRONT)) +
  geom_histogram(bins = 20, fill = '#35978f') +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  labs(x='BLDFRONT', y="Log Transformed Count", 
       title='The Distribution of BLDFRONT Excluding 0') +
  scale_y_log10()


# BLDDEPTH

length(unique(mydata$BLDDEPTH))
summary(mydata$BLDDEPTH)
summary(factor(mydata$BLDDEPTH))[1:10]
sum(is.na(mydata$BLDDEPTH))
sum(mydata$BLDDEPTH == 0)

mydata$BLDDEPTH <- as.numeric(as.character(mydata$BLDDEPTH))

mydata %>%
  filter(BLDDEPTH != 0) %>%
  summarise(min = min(BLDDEPTH),
            max = max(BLDDEPTH),
            median = median(BLDDEPTH),
            mean = mean(BLDDEPTH),
            mode = Mode(BLDDEPTH),
            sd = sd(BLDDEPTH))

summary(filter(mydata, BLDDEPTH != 0)$BLDDEPTH)

mydata %>%
  filter(BLDDEPTH != 0) %>%
  ggplot(aes(x = BLDDEPTH)) +
  geom_histogram(bins = 20, fill = '#4393c3') +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  labs(x='BLDDEPTH', y="Log Transformed Count", 
       title='The Distribution of BLDDEPTH Excluding 0') +
  scale_y_log10()

# AVLAND2

length(unique(mydata$AVLAND2))
summary(mydata$AVLAND2)
summary(factor(mydata$AVLAND2))[1:10]
sum(is.na(mydata$AVLAND2))

avland = mydata %>%
  select(AVLAND, AVLAND2) %>%
  filter(!is.na(AVLAND2))
sum(avland$AVLAND < avland$AVLAND2)

mydata %>%
  filter(!is.na(AVLAND2)) %>%
  summarise(min = min(AVLAND2),
            max = max(AVLAND2),
            median = median(AVLAND2),
            mean = mean(AVLAND2),
            mode = Mode(AVLAND2),
            sd = sd(AVLAND2))

ggplot(mydata, aes(x = AVLAND2)) +
  geom_histogram(bins = 100, fill = '#66c2a5') +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  labs(x='AVLAND2', y="Log Transformed Count", 
       title='The Distribution of AVLAND2') +
  scale_y_log10() +
  coord_cartesian(xlim=c(0,5e+08))

# AVTOT2

length(unique(mydata$AVTOT2))
summary(mydata$AVTOT2)
summary(factor(mydata$AVTOT2))[1:10]
sum(is.na(mydata$AVTOT2))

avtot = mydata %>%
  select(AVTOT, AVTOT2) %>%
  filter(!is.na(AVTOT2))
sum(avtot$AVTOT < avtot$AVTOT2)

mydata %>%
  filter(!is.na(AVTOT2)) %>%
  summarise(min = min(AVTOT2),
            max = max(AVTOT2),
            median = median(AVTOT2),
            mean = mean(AVTOT2),
            mode = Mode(AVTOT2),
            sd = sd(AVTOT2))

ggplot(mydata, aes(x = AVTOT2)) +
  geom_histogram(bins = 100, fill = '#9970ab') +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  labs(x='AVTOT2', y="Log Transformed Count", 
       title='The Distribution of AVTOT2') +
  scale_y_log10() +
  coord_cartesian(xlim=c(0,1e+09))


# EXLAND2

length(unique(mydata$EXLAND2))
summary(mydata$EXLAND2)
summary(factor(mydata$EXLAND2))[1:10]
sum(is.na(mydata$EXLAND2))

exland = mydata %>%
  select(EXLAND, EXLAND2) %>%
  filter(!is.na(EXLAND2))
sum(exland$EXLAND < exland$EXLAND2)

mydata %>%
  filter(!is.na(EXLAND2)) %>%
  summarise(min = min(EXLAND2),
            max = max(EXLAND2),
            median = median(EXLAND2),
            mean = mean(EXLAND2),
            mode = Mode(EXLAND2),
            sd = sd(EXLAND2))

ggplot(mydata, aes(x = EXLAND2)) +
  geom_histogram(bins = 100, fill = '#a50026') +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  labs(x='EXLAND2', y="Log Transformed Count", 
       title='The Distribution of EXLAND2') +
  scale_y_log10() +
  coord_cartesian(xlim=c(0,5e+08))

# EXTOT2

length(unique(mydata$EXTOT2))
summary(mydata$EXTOT2)
summary(factor(mydata$EXTOT2))[1:10]
sum(is.na(mydata$EXTOT2))

extot = mydata %>%
  select(EXTOT, EXTOT2) %>%
  filter(!is.na(EXTOT2))
sum(extot$EXTOT < extot$EXTOT2)

mydata %>%
  filter(!is.na(EXTOT2)) %>%
  summarise(min = min(EXTOT2),
            max = max(EXTOT2),
            median = median(EXTOT2),
            mean = mean(EXTOT2),
            mode = Mode(EXTOT2),
            sd = sd(EXTOT2))

ggplot(mydata, aes(x = EXTOT2)) +
  geom_histogram(bins = 100, fill = '#66c2a5') +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  labs(x='EXTOT2', y="Log Transformed Count", 
       title='The Distribution of EXTOT2') +
  scale_y_log10() +
  coord_cartesian(xlim=c(0,1e+09))

# EXCD2

length(unique(mydata$EXCD2))
summary(mydata$EXCD2)
sum(is.na(mydata$EXCD2))

EX2 = mydata %>%
  filter(is.na(EXCD2)) %>%
  select(EXLAND2, EXTOT2, EXCD2)

n = nrow(filter(mydata, !is.na(EXCD2)))

excd_order = mydata%>%
  group_by(EXCD2) %>%
  filter(!is.na(EXCD2)) %>%
  summarise(count = n(),
            percentage = count / n * 100) %>%
  arrange(-count) 

ggplot(excd_order[1:10,], aes(x = reorder(EXCD2, - count), y = count)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  labs(x='EXCD2', y="Log Transformed Count", 
       title='The Distribution of EXCD2') +
  scale_y_log10()
