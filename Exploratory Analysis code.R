#notes
# - age rounding

library(rio)    #package for tibble
ATUS_dataset <- import("Data and Resources/ATUS Activity Data/atussum_0317.csv", setclass = "tibble")    # import data raw as TIBBLE
ATUS_Respondent_file <- import("Data and Resources/ATUS Respondent File/atusresp_0317.csv", setclass = "tibble")
#Tidy Data
library(dplyr) #mutate
library(zoo) #date
library(lubridate)
#use unique to check values as NA's not used
sum(is.na(ATUS_dataset))
#met cols combine
ATUS_Respondent_file$TUDIARYDATE <- as.Date(as.character(ATUS_Respondent_file$TUDIARYDATE), "%Y%m%d")
ATUS_dataset <- mutate(ATUS_dataset, TESEX = ifelse(TESEX == 1, "M", "F"), TUDIARYDATE = ATUS_Respondent_file$TUDIARYDATE)
ATUS_dataset <- mutate(ATUS_dataset, Met_Status = as.factor(pmax(GEMETSTA, GTMETSTA)))
ATUS_dataset <- select(ATUS_dataset, -one_of(c("GEMETSTA", "GTMETSTA")))

#factors
ATUS_dataset <- mutate(ATUS_dataset, c() = 
                         as.factor(c(PEEDUCA, PEHSPNON, PTDTRACE, TELFS, TEMJOT, TESCHENR, TESCHLVL, TESEX, TESPEMPNOT, TRDPFTPT, TRERNWA, TRHOLIDAY, TRSPFTPT, TRSPPRES, TUDIARYDAY, TEHRUSLT)))

#remove full time vs part time as has -1
#remove full time vs part time spouse as has -1
#remove age of youngest child as has -1
#remove 2 jobs within last 7 days as has -1
#remove enrolled at high school etc as has -1 and -3 and also question about which one
#remove employment status of spouse as has -1
#remove weekly earnings as has -1
#remove total hours usually worked per week due to -1 and -4 for vary

#ATUS_dataset <- select(ATUS_dataset, -one_of(c("TRDPFTPT", "TRSPFTPT", "TRYHHCHILD", "TEMJOT", "TESCHENR", "TESCHLVL", "TESPEMPNOT", "TRERNWA", "TEHRUSLT")))



for (i in 1:9){
  assign(paste0("tu0", i), ATUS_dataset[,grep(paste0("^t0", i), names(ATUS_dataset))])
  assign(paste0("tu0",i), rowSums(get(paste0("tu0",i))))
  ATUS_dataset <- bind_cols(ATUS_dataset, tu0 = get(paste0("tu0", i)))
  names(ATUS_dataset)[ncol(ATUS_dataset)] <- paste0("tu0", i)
  assign(paste0("part_tu0", i), 100 * (get(paste0("tu0",i)) > 0))
  ATUS_dataset <- bind_cols(ATUS_dataset, part_tu0 = get(paste0("part_tu0", i)))
  names(ATUS_dataset)[ncol(ATUS_dataset)] <- paste0("part_tu0", i)
}
for (i in c(10:16, 18)){
  assign(paste0("tu", i), ATUS_dataset[,grep(paste0("^t", i), names(ATUS_dataset))])
  assign(paste0("tu",i), rowSums(get(paste0("tu",i))))
  ATUS_dataset <- bind_cols(ATUS_dataset, tu = get(paste0("tu", i)))
  names(ATUS_dataset)[ncol(ATUS_dataset)] <- paste0("tu", i)
  assign(paste0("part_tu", i), 100 * (get(paste0("tu",i)) > 0))
  ATUS_dataset <- bind_cols(ATUS_dataset, part_tu0 = get(paste0("part_tu", i)))
  names(ATUS_dataset)[ncol(ATUS_dataset)] <- paste0("part_tu", i)
}
ATUS_data_minus_july <- filter(ATUS_dataset, month(TUDIARYDATE) != 7)
library(ggplot2)

library(gridExtra)
proportion_names <- names(ATUS_data_minus_july)[grep("^part_tu", names(ATUS_dataset))]
proportion_plots <- list()
for(j in proportion_names){
  proportion_plots[[j]] <- ggplot(ATUS_data_minus_july, aes(color=TESEX)) + geom_smooth(aes_string(x="TUDIARYDATE", y=j), se=FALSE)
}
do.call(grid.arrange,proportion_plots)

library(utils)
part_Activities_per_date <- select(ATUS_data_minus_july, "TUDIARYDATE", grep("^part_tu", names(ATUS_dataset)))
part_Activities_per_date <- aggregate(part_Activities_per_date, by = list(part_Activities_per_date$TUDIARYDATE), mean)
part_Activities_per_date <- mutate(part_Activities_per_date, date_sum = rowSums(select(part_Activities_per_date, part_tu01:part_tu18)))
part_Activities_per_date <- mutate_at(part_Activities_per_date, .vars = vars(part_tu01:part_tu18) ,.funs = (~.*(100/(part_Activities_per_date$date_sum))))
plot1_data <- data.frame(Date = part_Activities_per_date$TUDIARYDATE, stack(part_Activities_per_date[3:19]))
plot1 <- ggplot(plot1_data, aes(Date, values, fill = ind)) + geom_area(position = "stack")

part_Activities_per_year <- select(ATUS_data_minus_july, "TUYEAR", grep("^part_tu", names(ATUS_dataset)))
part_Activities_per_year <- aggregate(part_Activities_per_year, by = list(part_Activities_per_year$TUYEAR), mean)
part_Activities_per_year <- mutate(part_Activities_per_year, year_sum = rowSums(select(part_Activities_per_year, part_tu01:part_tu18)))
part_Activities_per_year <- mutate_at(part_Activities_per_year, .vars = vars(part_tu01:part_tu18) ,.funs = (~.*(100/(part_Activities_per_year$year_sum))))
plot2_data <- data.frame(Years = part_Activities_per_year$TUYEAR, stack(part_Activities_per_year[3:19]))
plot2 <- ggplot(plot2_data, aes(Years, values, fill = ind)) + geom_area(position = "stack")

