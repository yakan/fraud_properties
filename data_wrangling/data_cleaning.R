library(stringr)
library(dplyr)
library(tidyr)

#### load raw data
ny_data = readRDS("NY.rds")

summary(ny_data)

#### transform data type (5 numerical variables)
ny_data$LTDEPTH = as.numeric(ny_data$LTDEPTH)
ny_data$LTFRONT = as.numeric(ny_data$LTFRONT)
ny_data$BLDFRONT = as.numeric(ny_data$BLDFRONT)
ny_data$BLDDEPTH = as.numeric(ny_data$BLDDEPTH)
ny_data$STORIES = as.numeric(ny_data$STORIES)

#### transform data type (3 character variables)
ny_data$RECORD = as.character(ny_data$RECORD)
ny_data$BLOCK = as.character(ny_data$BLOCK)
ny_data$LOT = as.character(ny_data$LOT)

#### subset BBLE to get BORO
ny_data$BORO = str_sub(ny_data$BBLE,1,1)

#### create ratio
ny_data$LTR = ny_data$LTDEPTH/ny_data$LTFRONT
ny_data$BLDR = ny_data$BLDDEPTH/ny_data$BLDFRONT
ny_data$LTR = ifelse(ny_data$LTR==0|ny_data$LTR==Inf,NA,ny_data$LTR)
ny_data$BLDR = ifelse(ny_data$BLDR==0|ny_data$BLDR==Inf,NA,ny_data$LTR)

#### choose the most possible relevant var
colnames(ny_data)
ny = ny_data[,c(1:3,6:14,19,21,22,31,32,33)]
colnames(ny)

#### dummy data points
ny1000 = ny[1:1000,]


#### filling missing zip with the most frequent zip in the BORO
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# most_zip = ny_data %>%
#   group_by(BORO,ZIP) %>%
#   summarise(count=n()) %>%
#   slice(which.max(count)) #use slice to select rows by position based on max count
  
ny=ny%>%
  group_by(BORO)%>%
  mutate(ZIP_most=Mode(ZIP))

ny$ZIP =ifelse(is.na(ny$ZIP),ny$ZIP_most,ny$ZIP)

#### add zip3
ny$ZIP3 = str_sub(ny$ZIP,1,3)

#### calculate average stories group by zip, zip3, and boro
ny = ny %>%
  group_by(ZIP) %>%
  mutate(avg_str_ZIP = mean(STORIES, na.rm = T))

ny = ny %>%
  group_by(ZIP3) %>%
  mutate(avg_str_ZIP3 = mean(STORIES, na.rm = T))

ny = ny %>%
  group_by(BORO) %>%
  mutate(avg_str_BORO = mean(STORIES, na.rm = T))

## replace missing stories, order zip->zip3->boro
ny$STORIES = ifelse(is.na(ny$STORIES),
                    ifelse(is.na(ny$avg_str_ZIP),
                           ifelse(is.na(ny$avg_str_ZIP3),
                                  ny$avg_str_BORO,ny$avg_str_ZIP3),ny$avg_str_ZIP),ny$STORIES)


#### calculate average lot ratio (LTR) group by zip, zip3, and boro
ny=ny%>%
  group_by(ZIP)%>%
  mutate(avg_ltr_ZIP=mean(LTR,na.rm = T))

ny=ny%>%
  group_by(ZIP3)%>%
  mutate(avg_ltr_ZIP3=mean(LTR,na.rm = T))

ny=ny%>%
  group_by(BORO)%>%
  mutate(avg_ltr_BORO=mean(LTR,na.rm = T))

## replace missing LTR, , order zip->zip3->boro

ny$LTR = ifelse(is.na(ny$LTR),
                    ifelse(is.na(ny$avg_ltr_ZIP),
                           ifelse(is.na(ny$avg_ltr_ZIP3),
                                  ny$avg_ltr_BORO,ny$avg_ltr_ZIP3),ny$avg_ltr_ZIP),ny$LTR)

#### calculate average building ratio (BLDR) group by zip, zip3, and boro
ny=ny%>%
  group_by(ZIP)%>%
  mutate(avg_bldr_ZIP=mean(BLDR,na.rm = T))

ny=ny%>%
  group_by(ZIP3)%>%
  mutate(avg_bldr_ZIP3=mean(BLDR,na.rm = T))

ny=ny%>%
  group_by(BORO)%>%
  mutate(avg_bldr_BORO=mean(BLDR,na.rm = T))

## replace missing BLDR, , order zip->zip3->boro
ny$BLDR = ifelse(is.na(ny$BLDR),
                ifelse(is.na(ny$avg_bldr_ZIP),
                       ifelse(is.na(ny$avg_bldr_ZIP3),
                              ny$avg_bldr_BORO,ny$avg_bldr_ZIP3),ny$avg_bldr_ZIP),ny$BLDR)

#### calculate average lot front (LTFRONT) group by zip, zip3, and boro
ny=ny%>%group_by(ZIP)%>%
  mutate(avg_ltfront_ZIP=sum(LTFRONT)/sum(NROW(LTFRONT)-NROW(LTFRONT[LTFRONT==0])))

ny=ny%>%group_by(ZIP3)%>%
  mutate(avg_ltfront_ZIP3=sum(LTFRONT)/sum(NROW(LTFRONT)-NROW(LTFRONT[LTFRONT==0])))

ny=ny%>%group_by(BORO)%>%
  mutate(avg_ltfront_BORO=sum(LTFRONT)/sum(NROW(LTFRONT)-NROW(LTFRONT[LTFRONT==0])))

# ny[,c("avg_ltfront_BORO", "avg_ltfront_zip3", "avg_ltfront_zip")] <- list(NULL)

# Note:
# The difference between NROW() and NCOL() and their lowercase variants (ncol() and nrow()) is 
# that the lowercase versions will only work for objects that have dimensions (arrays, matrices, data frames). 
# The uppercase versions will work with vectors, which are treated as if they were a 1 column matrix, 
# and are robust if you end up subsetting your data such that R drops an empty dimension

## replace missing ltfront, order ZIP->ZIP3->BORO
ny$LTFRONT1 = ifelse(ny$LTFRONT==0,
                 ifelse(is.na(ny$avg_ltfront_ZIP)|ny$avg_ltfront_ZIP==0,
                        ifelse(is.na(ny$avg_ltfront_ZIP3)|ny$avg_ltfront_ZIP3==0,
                               ny$avg_ltfront_BORO,ny$avg_ltfront_ZIP3),ny$avg_ltfront_ZIP),ny$LTFRONT)
#ny$LTRatio=ny$LTDEPTH1/ny$LTFRONT1

## calculate average ltdepth, zip, zip3, boro
ny=ny%>%group_by(ZIP)%>%
  mutate(avg_ltdepth_ZIP=sum(LTDEPTH)/sum(NROW(LTDEPTH)-NROW(LTDEPTH[LTDEPTH==0])))
ny=ny%>%group_by(ZIP3)%>%
  mutate(avg_ltdepth_ZIP3=sum(LTDEPTH)/sum(NROW(LTDEPTH)-NROW(LTDEPTH[LTDEPTH==0])))
ny=ny%>%group_by(BORO)%>%
  mutate(avg_ltdepth_BORO=sum(LTDEPTH)/sum(NROW(LTDEPTH)-NROW(LTDEPTH[LTDEPTH==0])))

## replace missing ltfront, order ZIP->ZIP3->BORO
ny$LTDEPTH1 = ifelse(ny$LTDEPTH==0,
                     ifelse(is.na(ny$avg_ltdepth_ZIP)|ny$avg_ltdepth_ZIP==0,
                            ifelse(is.na(ny$avg_ltdepth_ZIP3)|ny$avg_ltdepth_ZIP3==0,
                                   ny$avg_ltdepth_BORO,ny$avg_ltdepth_ZIP3),ny$avg_ltdepth_ZIP),ny$LTDEPTH)

# replace LTFRONT and LTDEPTH
# dealing with either LTFRONT or LTDEPTH is zero
ny$LTFRONT=ifelse(ny$LTFRONT==0&ny$LTDEPTH!=0,ny$LTDEPTH/ny$LTR,ny$LTFRONT)
ny$LTDEPTH=ifelse(ny$LTDEPTH==0&ny$LTFRONT!=0,ny$LTFRONT*ny$LTR,ny$LTDEPTH)
# dealing with both LTFRONT and LTDEPTH are zero
ny$LTFRONT=ifelse(ny$LTFRONT==0,ny$LTFRONT1,ny$LTFRONT)
ny$LTDEPTH=ifelse(ny$LTDEPTH==0,ny$LTDEPTH1,ny$LTDEPTH)

## replace missing BLDFRONT with
ny=ny%>%group_by(ZIP)%>%
  mutate(avg_BLDFRONT_ZIP=sum(BLDFRONT)/sum(NROW(BLDFRONT)-NROW(BLDFRONT[BLDFRONT==0])))
ny=ny%>%group_by(ZIP3)%>%
  mutate(avg_BLDFRONT_ZIP3=sum(BLDFRONT)/sum(NROW(BLDFRONT)-NROW(BLDFRONT[BLDFRONT==0])))
ny=ny%>%group_by(BORO)%>%
  mutate(avg_BLDFRONT_BORO=sum(BLDFRONT)/sum(NROW(BLDFRONT)-NROW(BLDFRONT[BLDFRONT==0])))