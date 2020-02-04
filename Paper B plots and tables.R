library(chron)
library(lubridate)
library(plyr)
library(dplyr) #for piping, summary stats
library(zoo) 
library(reshape2)
library(ggplot2)
library(lattice)
library(grid)
library(stringi) # for week of month
library(stringr)

# Working in GMT to avoid having the labels shifted by 5 hours!
Sys.setenv(TZ='GMT')

setwd("~/Google Drive (jstowe@vt.edu)/VT coursework/Capstone/Analysis") # Dir for prepped data on John's computer
load("Arl_Webdata_Cleaned.Rda")
load("Averagedays.Rda")
load("RushCounts.Rda")
load("Averagedays_long.Rda")
load("Counters_processed.Rda")

##### helper functions #####
boolean_labeller <- labeller(
  Workday = c("FALSE" = "Weekend","TRUE" ="Workday"),
  .default = label_both
)


##### summary statistics for paper #####

summary <- combineddata_Cleaned %>%
  group_by(counter_num,Weekend,Workday) %>%
  summarise(n_obs = n(), days = n_distinct(date) )
summary$daytype <- "other"
summary$daytype[summary$Workday] <- "Workday"
summary$daytype[summary$Weekend] <- "Weekend"
summary$daytype <- factor(summary$daytype,levels=c("Workday","Weekend","other"),ordered = TRUE) # Helps order columns later


##### table 1: overall counter info #####

summary_n <- dcast(summary,counter_num ~ daytype,sum,value.var = "n_obs", margins = TRUE)
summary_days <- dcast(summary,counter_num ~ daytype,sum,value.var = "days", margins = TRUE)
names(summary_n)[2:5] <- paste0("n_",names(summary_n)[2:5]) # rename columns before merging
names(summary_days)[2:5] <- paste0("days_",names(summary_days)[2:5])

table_1 <- merge(summary_days,summary_n)
table_1 <- merge(counters,table_1, all.y = TRUE)

write.csv(table_1, file = "Table 1.csv", row.names = FALSE)

#####  Table 2: 2015 info #####

summary2015 <- combineddata_Cleaned %>%
  filter(Year == 2015 ) %>%
  group_by(counter_num,direction,mode,dir_mode,Weekend,Workday) %>%
  summarise(days = n_distinct(date), total = sum(count),average = sum(count)/n_distinct(date) )
summary2015$daytype <- "other"
summary2015$daytype[summary2015$Workday] <- "Workday"
summary2015$daytype[summary2015$Weekend] <- "Weekend"
summary2015$daytype <- factor(summary2015$daytype,levels=c("Workday","Weekend","other"),ordered = TRUE) # Helps order columns later

# days each counter was active in 2015
summary2015_days <- dcast(summary2015,counter_num ~ daytype,sum,value.var = "days",margins = TRUE, subset = .(dir_mode == "inbound bicycle"))

# total 2015 traffic for each counter
summary2015_total <- dcast(summary2015,counter_num ~ daytype + mode,sum,value.var = "total",margins = TRUE)
lastcol <- tail(names(summary2015_total), 1) #check on the last margin column
summary2015_total$"(all)_B" <- summary2015_total$Workday_B + summary2015_total$Weekend_B + summary2015_total$other_B
summary2015_total$"(all)_P" <- summary2015_total$Workday_P + summary2015_total$Weekend_P + summary2015_total$other_P
summary2015_total <- summary2015_total[,c(setdiff(names(summary2015_total),lastcol),lastcol)]

# mean 2015 traffic for each counter
summary2015_mean <- data.frame(counter_num = summary2015_total$counter_num) # initialize
for (i in names(summary2015_total)[-1]) summary2015_mean[[i]] <- summary2015_total[[i]] / summary2015_days[[unlist(strsplit(i,"_"))[1]]] # calculate the right means

# I/O ratios
factors <- expand.grid(c(levels(summary2015$mode),"(all)"), c(levels(summary2015$daytype),"(all)"))
factors$text <- paste(factors$Var2,factors$Var1,sep = "_")

summary2015_IO <- dcast(summary2015, counter_num ~ daytype + mode + direction, sum, value.var = "total", margins = TRUE)
# some aggregates ("margins") are missing after dcast. Find which ones:
missingcols <- data.frame(colname = setdiff(c(paste0(factors$text,"_I"),paste0(factors$text,"_O")),names(summary2015_IO)))
# create search strings to pull out the columns that should be summed for each missing aggregate
missingcols$search_str <- gsub("(all)","[a-zA-Z]+",missingcols$colname, fixed = TRUE)
for (i in 1:nrow(missingcols)) summary2015_IO[[as.character(missingcols$colname[i])]] <- rowSums(select(summary2015_IO,matches(missingcols$search_str[i])))

summary2015_IOratio <- data.frame(counter_num = summary2015_IO$counter_num)
for (i in factors$text) summary2015_IOratio[[i]] <- summary2015_IO[[paste0(i,"_I")]] / summary2015_IO[[paste0(i,"_O")]]

# assign unique column names
names(summary2015_days)[-1] <- paste0("days_2015_", names(summary2015_days)[-1])
names(summary2015_total)[-1] <- paste0("total_2015_", names(summary2015_total)[-1])
names(summary2015_mean)[-1] <- paste0("mean_2015_", names(summary2015_mean)[-1])
names(summary2015_IOratio)[-1] <- paste0("IO_2015_", names(summary2015_IOratio[-1]))



summary2015_combined <- merge(summary2015_days,summary2015_total)
summary2015_combined <- merge(summary2015_combined,summary2015_mean)
summary2015_combined <- merge(summary2015_combined,summary2015_IOratio)



table_2 <- merge(table_1[,1:2],summary2015_combined, by = "counter_num",all = TRUE)

write.csv(table_2, file = "Table 2.csv", row.names = FALSE)

##### Figure 2: Rosslyn Bikeometer example #####
RushRects <- data.frame(xmin = c(7/24, 16/24),xmax = c(10/24,19/24),ymin = -Inf, ymax = Inf, alpha = 0.2, Daytype = factor(c("Weekday","Weekday"), levels = c("Weekday","Holiday","Weekend","Likely abnormal","OPM action","Other events")))

p <- ggplot(Averagedays_monthyear[!Averagedays_monthyear$Likely.abnormal & 
                          !Averagedays_monthyear$OPM.action & 
                            Averagedays_monthyear$counter_num == "28" &
                            Averagedays_monthyear$Year == "2015" &
                            Averagedays_monthyear$Month == "May",],
            aes(x = time, y = count)) + 
  geom_rect(data=RushRects, mapping = aes(x=NULL, y=NULL, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), alpha=0.2) +
  geom_line(aes(color = dir_mode)) + 
  scale_x_chron(format="%H:%M",n=8, minor_breaks=seq(0,1,1/24))
p + 
  ggtitle("Average traffic May 2015,\nRosslyn Bikeometer") + facet_grid(Daytype ~ .) + scale_colour_brewer(palette = "Set1")

ggsave("Fig 2 Rosslyn Bikeometer.png",width = 3,height = 2, units = "in",scale = 2,path = "~/Google Drive (jstowe@vt.edu)/VT coursework/Capstone/figures/Paper B")

# some numbers relevant to describing Fig. 2
Fig2a <- 
  #low-level summary
  Averagedays_monthyear %>%
    filter(!Averagedays_monthyear$Likely.abnormal & 
      !Averagedays_monthyear$OPM.action & 
      Averagedays_monthyear$counter_num == "28" &
      Averagedays_monthyear$Year == "2015" &
      Averagedays_monthyear$Month == "May") %>%
    group_by(Daytype, mode, direction) %>%
    summarise(max = max(count),
              time_max = time[which(count == max(count))],
              daily = sum(count),
              pct_rush = sum(count[which(cut(time,rushbins,right = FALSE) %in% c("[0.292,0.417)","[0.667,0.792)"))]) / daily,
              n_obs = n()
              )
Fig2b <- merge(
  #combine across directions
  Averagedays_monthyear %>%
    filter(!Averagedays_monthyear$Likely.abnormal & 
             !Averagedays_monthyear$OPM.action & 
             Averagedays_monthyear$counter_num == "28" &
             Averagedays_monthyear$Year == "2015" &
             Averagedays_monthyear$Month == "May"
           ) %>%
    group_by(Daytype, mode, time, direction) %>% #need to aggregate across the directions for each first so the max calculations work right
    summarise(count = sum(count),
              inbound = sum(count[which(direction=="I")]),
              outbound = sum(count[which(direction=="O")])
              ) %>%
    summarise(count = sum(count),
              inbound = sum(inbound),
              outbound = sum(outbound)
              ) %>% 
    summarise(mode_max = max(count),
              time_mode_max = time[which(count == max(count))],
              mode_daily = sum(count),
              mode_pct_rush = sum(count[which(cut(time,rushbins,right = FALSE) %in% c("[0.292,0.417)","[0.667,0.792)"))]) / mode_daily,
              inbound = sum(inbound),
              outbound = sum(outbound),
              mode_IO_ratio = inbound/outbound
              )
  ,
  RushMeans_monthyear  %>%
    filter(!RushMeans_monthyear$Likely.abnormal & 
             !RushMeans_monthyear$OPM.action & 
             RushMeans_monthyear$counter_num == "28" &
             RushMeans_monthyear$Year == "2015" &
             RushMeans_monthyear$Month == "May"
    ) %>%
    group_by(Daytype, mode, cuts, direction) %>%
    summarise(count = sum(count),
              inbound = sum(count[which(direction=="I")]),
              outbound = sum(count[which(direction=="O")])
              ) %>%
    summarise(count = sum(count),
              inbound = sum(inbound),
              outbound = sum(outbound),
              IO_ratio = inbound/outbound) %>% 
    summarise(mode_daily = sum(count),
              inbound = sum(inbound),
              outbound = sum(outbound),
              mode_pct_rush = sum(count[which(cuts %in% c("[0.292,0.417)","[0.667,0.792)"))]) / mode_daily,
              morning_rush_IO_ratio = IO_ratio[which(cuts=="[0.292,0.417)")],
              evening_rush_IO_ratio = IO_ratio[which(cuts=="[0.667,0.792)")],
              IO_ratio = inbound/outbound
              )
)

