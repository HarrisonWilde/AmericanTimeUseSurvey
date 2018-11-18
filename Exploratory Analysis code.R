#notes
# - age rounding
# - -1 means blank, -2 means don't know, -3 means refused
###### Import #######

library(rio)    #package for tibble
ATUS_dataset <- import("Data and Resources/ATUS Activity Data/atussum_0317.csv", setclass = "tibble")    # import data raw as TIBBLE
ATUS_Respondent_file <- import("Data and Resources/ATUS Respondent File/atusresp_0317.csv", setclass = "tibble")


######## Tidy Data #########

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
factor_columns <- c("PEEDUCA", "PEHSPNON", "PTDTRACE", "TELFS", "TEMJOT", "TESCHENR", "TESCHLVL", "TESEX", "TESPEMPNOT", "TRDPFTPT", "TRERNWA", "TRHOLIDAY", "TRSPFTPT", "TRSPPRES", "TUDIARYDAY", "TEHRUSLT")
ATUS_dataset[,factor_columns] <- lapply(ATUS_dataset[,factor_columns], factor)

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

#remove columns with -1's, -3's, -4's

#remove full time vs part time as has -1 (66499)
Data_minus_columns <- select(ATUS_data_minus_july, -one_of("TRDPFTPT"))
#remove full time vs part time spouse as has -1 (112134)
Data_minus_columns <- select(Data_minus_columns, -one_of("TRSPFTPT"))
#remove age of youngest child as has -1 (95663)
Data_minus_columns <- select(Data_minus_columns, -one_of("TRYHHCHILD"))
#remove 2 jobs within last 7 days as has -1 (66499)
Data_minus_columns <- select(Data_minus_columns, -one_of("TEMJOT"))
#remove enrolled at high school etc as has -1 and -3 and also question about which (76179 and 158595)
Data_minus_columns <- select(Data_minus_columns, -one_of(c("TESCHENR", "TESCHLVL")))
#remove employment status of spouse as has -1 (82506)
Data_minus_columns <- select(Data_minus_columns, -one_of("TESPEMPNOT"))
#remove weekly earnings as has -1 (78897)
Data_minus_columns <- select(Data_minus_columns, -one_of("TRERNWA"))
#remove total hours usually worked per week due to -1 and -4 for vary
Data_minus_columns <- select(Data_minus_columns, -one_of("TEHRUSLT"))

#remove rows with -1 or -3
Data_minus_rows <- ATUS_data_minus_july[apply(ATUS_data_minus_july, 1, function(row) all(row != -1)), ]

###### Initial Summary #########

library(reshape2)
melted_participation_data <- melt(select(Data_minus_columns, "TUYEAR", grep("^part_tu", names(Data_minus_columns))), id.vars="TUYEAR")
#Initial Summary
library(ggplot2)
part_summary_plot <- ggplot(melted_participation_data, aes(x=TUYEAR, y=value, color = variable)) + geom_smooth(se=FALSE)
print(part_summary_plot)

melted_data <- melt(select(Data_minus_columns, "TUYEAR", grep("^tu", names(Data_minus_columns))), id.vars="TUYEAR")
summary_plot <- ggplot(melted_data, aes(x=TUYEAR, y=value, color=variable)) + geom_smooth(se=FALSE)
print(summary_plot)


####### Weighted Means #########

Data_minus_columns_year_grouped <- Data_minus_columns %>% group_by(TUYEAR)

weighted_means <- as_tibble(data.frame(TUYEAR = 2003:2017))
for (i in 1:9){
  weighted_means <- full_join(weighted_means, summarise_at(Data_minus_columns_year_grouped, vars((paste0("tu0", i))), funs(weighted.mean(., TUFNWGTP))))
}
for (i in c(10:16, 18)){
  weighted_means <- full_join(weighted_means, summarise_at(Data_minus_columns_year_grouped, vars((paste0("tu", i))), funs(weighted.mean(., TUFNWGTP))))
}
variance <- sapply(weighted_means, function(col) var(col))
percentage_change <- (weighted_means[15,]/weighted_means[1,] - 1)*100
Activity_year_measures <- bind_rows(variance, percentage_change)
Activity_year_measures <- cbind(as_tibble(data.frame(Measure = c("Variance", "Percentage Change"))), select(Activity_year_measures, -TUYEAR))

part_weighted_means <- as_tibble(data.frame(TUYEAR = 2003:2017))
for (i in 1:9){
  part_weighted_means <- full_join(part_weighted_means, summarise_at(Data_minus_columns_year_grouped, vars(paste0("part_tu0", i)), funs(weighted.mean(., TUFNWGTP))))
}
for (i in c(10:16, 18)){
  part_weighted_means <- full_join(part_weighted_means, summarise_at(Data_minus_columns_year_grouped, vars((paste0("part_tu", i))), funs(weighted.mean(., TUFNWGTP))))
}
part_variance <- sapply(part_weighted_means, function(col) var(col))
part_percentage_change <- (part_weighted_means[15,]/part_weighted_means[1,] - 1)*100
part_Activity_year_measures <- bind_rows(part_variance, part_percentage_change)
part_Activity_year_measures <- cbind(as_tibble(data.frame(Measure = c("Variance", "Percentage Change"))), select(part_Activity_year_measures, -TUYEAR))


######### looking after Non_household members (reference plotly bookmark on phone) (tu04) ##########
n <- nrow(Data_minus_columns)
library(splines)
tu04_summary_model <- glm(tu04 ~ ns(TUYEAR, knots = seq(2004, 2016, 2)), data = Data_minus_columns, 
              family = quasi(link = "log", variance = "mu^2"),
              mustart = rep(5, n)
)
ggplot(Data_minus_columns, aes(x=TUYEAR, y=tu04)) + geom_smooth(method = "glm", formula = y ~ ns(x, knots = seq(2004, 2016, 2)),
                                                                method.args = list(family = quasi(link = "log", variance = "mu^2"), 
                                                                                   mustart = rep(5, n)))
tu04_variables_model <- update(tu04_summary_model, . ~ . + TESEX + PEEDUCA + PEHSPNON + PTDTRACE + TEAGE + TELFS + TRCHILDNUM + TRHOLIDAY + TRSPPRES)

tu04_sex_model <- update(tu04_summary_model, . ~ -1 + TESEX + TESEX:ns(TUYEAR, knots = seq(2004, 2016, 2)))
tu04_sex_model_data <- as_tibble(data.frame(TUYEAR = rep(seq(2003, 2017, length.out = 1000), 2), TESEX = c(rep("M",1000), rep("F",1000))))
tu04_sex_predicted <- predict(tu04_sex_model, newdata = tu04_sex_model_data, type = "link", se = TRUE)
tu04_sex_predicted <-bind_cols(tu04_sex_model_data, 
                               as_tibble(data.frame(fitted = exp(tu04_sex_predicted$fit),
                                                    ymin = exp(tu04_sex_predicted$fit-1.128*tu04_sex_predicted$se.fit), #80%
                                                    ymax = exp(tu04_sex_predicted$fit+1.128*tu04_sex_predicted$se.fit))))
ggplot(Data_minus_columns, aes(x=TUYEAR, y=tu04)) + 
  geom_line(data = tu04_sex_predicted, aes(y=fitted, col=TESEX), size=1) +
  geom_ribbon(data = filter(tu04_sex_predicted, TESEX =="M"), aes(y=fitted, ymin=ymin, ymax=ymax),alpha=0.1) + 
  geom_ribbon(data = filter(tu04_sex_predicted, TESEX =="F"), aes(y=fitted, ymin=ymin, ymax=ymax),alpha=0.1)




part_tu04_summary_model <- glm(part_tu04 ~ ns(TUYEAR, knots = seq(2004, 2016, 2)), data = Data_minus_columns, 
                          family = quasi(link = "log", variance = "mu^2"),
                          mustart = rep(5, n))
part_tu04_sex_model <- update(part_tu04_summary_model, . ~ -1 + TESEX + TESEX:ns(TUYEAR, knots = seq(2004, 2016, 2)))
part_tu04_sex_model_data <- as_tibble(data.frame(TUYEAR = rep(seq(2003, 2017, length.out = 1000), 2), TESEX = c(rep("M",1000), rep("F",1000))))
part_tu04_sex_predicted <- predict(part_tu04_sex_model, newdata = part_tu04_sex_model_data, type = "link", se = TRUE)
part_tu04_sex_predicted <-bind_cols(part_tu04_sex_model_data, 
                               as_tibble(data.frame(fitted = exp(part_tu04_sex_predicted$fit),
                                                    ymin = exp(part_tu04_sex_predicted$fit-1.128*part_tu04_sex_predicted$se.fit), #80%
                                                    ymax = exp(part_tu04_sex_predicted$fit+1.128*part_tu04_sex_predicted$se.fit))))
ggplot(Data_minus_columns, aes(x=TUYEAR, y=part_tu04)) + 
  geom_line(data = part_tu04_sex_predicted, aes(y=fitted, col=TESEX), size=1) +
  geom_ribbon(data = filter(part_tu04_sex_predicted, TESEX =="M"), aes(y=fitted, ymin=ymin, ymax=ymax),alpha=0.1) + 
  geom_ribbon(data = filter(part_tu04_sex_predicted, TESEX =="F"), aes(y=fitted, ymin=ymin, ymax=ymax),alpha=0.1)





######## education (tu06) ##########

#education
ggplot(Data_minus_columns, aes(x=TUYEAR, y=part_tu06)) + geom_smooth(se=FALSE)
ggplot(Data_minus_columns, aes(x=TUYEAR, y=part_tu06, color=TESEX)) + geom_smooth(se=FALSE)
#Race
data <- Data_minus_columns %>% group_by(TUYEAR, PTDTRACE) %>% summarise_at(vars(tu06), funs(weighted.mean(., TUFNWGTP)))
#only 1:10 have values for all years
tu06_PTDTRACE_data <- filter(data, PTDTRACE %in% (1:10))
library(viridis)
ggplot(tu06_PTDTRACE_data, aes(x=TUYEAR, y=tu06, fill=PTDTRACE)) + geom_area(position="stack") + scale_fill_viridis_d()
#Sex
tu06_TESEX_data <- Data_minus_columns %>% group_by(TUYEAR, TESEX) %>% summarise_at(vars(part_tu06), funs(weighted.mean(., TUFNWGTP)))
#only 1:10 have values for all years
ggplot(tu06_TESEX_data, aes(x=TUYEAR, y=part_tu06, fill=TESEX)) + geom_area(position="stack") + scale_fill_viridis_d(alpha = 0.5)















######## PCA #########

dlibrary(ggbiplot)
#PCA_1 / SVD_1
PCA_1 <- prcomp(Data_minus_columns[,grep("^tu", names(Data_minus_columns))], scale. = TRUE, center = TRUE)
plot(x = PCA_1$rotation[, 1], y = PCA_1$rotation[,2])
SVD_1 <- svd(scale(Data_minus_columns[,grep("^tu", names(Data_minus_columns))]))
plot(SVD_1$d^2/sum(SVD_1$d^2))
summary(PCA_1)
PCA_2_biplot <- ggbiplot(PCA_1, groups=interaction(Data_minus_columns$TESEX, Data_minus_columns$Met_Status), size=0.3)
PCA_2_biplot$layers <- c(PCA_2_biplot$layers, PCA_2_biplot$layers[[1]])
print(PCA_2_biplot)

#PCA_2 / SVD_2
PCA_2 <- prcomp(Data_minus_rows[,grep("^tu", names(Data_minus_rows))], scale. = TRUE, center = TRUE)
plot(x = PCA_2$rotation[, 1], y = PCA_2$rotation[,2])
SVD_2 <- svd(scale(Data_minus_columns[,grep("^tu", names(Data_minus_columns))]))
plot(SVD_2$d^2/sum(SVD_2$d^2))
summary(PCA_2)
ggbiplot(PCA_2) + geom_point(aes(colour=Data_minus_rows$TESEX : Data_minus_rows$PEHSPNON))

PCA_3 <- prcomp(Data_minus_columns[,grep("^tu", names(Data_minus_columns))], scale. = TRUE, center = TRUE)
PCA_3_values <- data.frame(Sex = Data_minus_columns$TESEX, Met_Status = Data_minus_columns$Met_Status, PCA_3$x)
PCA_3_loadings <- data.frame(Variables = rownames(PCA_3$rotation), PCA_3$rotation)
PCA_3_loadings$Angle = ((180/pi)*atan(PCA_3_loadings$PC2/PCA_3_loadings$PC1))
PCA_3_loadings$Offset <- ((-4*sign(PCA_3_loadings$PC1))/2)
ggplot(PCA_3_values, aes(x=PC1, y=PC2, color = Sex:Met_Status)) + 
  geom_point(size = 0.5) + 
  geom_segment(data = PCA_3_loadings, aes(x=0, y=0, xend=PC1, yend=PC2), arrow = arrow(length = unit(0.5, "picas")), color="Black") + 
  geom_text(data = PCA_3_loadings, aes(label=Variables, x=PC1, y=PC2), color="black", size=4, angle=PCA_3_loadings$Angle, hjust=PCA_3_loadings$Offset)  +
  theme_classic() + theme(legend.justification = c(1,1), legend.position = c(1,1))

PCA_4 <- prcomp(Data_minus_columns[,grep("^tu", names(Data_minus_columns))], scale. = TRUE, center = TRUE)
PCA_4_values <- data.frame(Sex = Data_minus_columns$TESEX, PEHSPNON = Data_minus_columns$PEHSPNON, PCA_4$x)
PCA_4_loadings <- data.frame(Variables = rownames(PCA_4$rotation), PCA_4$rotation)
PCA_4_loadings$Angle = ((180/pi)*atan(PCA_4_loadings$PC2/PCA_4_loadings$PC1))
PCA_4_loadings$Offset <- ((-4*sign(PCA_4_loadings$PC1))/2)
ggplot(PCA_4_values, aes(x=PC1, y=PC2, color = Sex:PEHSPNON)) + 
  geom_point(size = 0.5) + 
  geom_segment(data = PCA_4_loadings, aes(x=0, y=0, xend=3*PC1, yend=3*PC2), arrow = arrow(length = unit(0.5, "picas")), color="Black") + 
  geom_text(data = PCA_4_loadings, aes(label=Variables, x=PC1, y=PC2), color="black", size=4, angle=PCA_4_loadings$Angle, hjust=PCA_4_loadings$Offset)  +
  theme_classic() + theme(legend.justification = c(1,1), legend.position = c(1,1))

small_data <- select(Data_minus_columns, TESEX, PEHSPNON, tu02, tu05, tu11, tu12, tu18)
small_data <- small_data[sample(1:nrow(small_data), 1000),]
PCA_5 <- prcomp(small_data[,grep("^tu", names(small_data))], scale. = TRUE, center = TRUE)
PCA_5_values <- data.frame(Sex = small_data$TESEX, PEHSPNON = small_data$PEHSPNON, PCA_5$x)
PCA_5_loadings <- data.frame(Variables = rownames(PCA_5$rotation), PCA_5$rotation)
PCA_5_loadings$Angle = ((180/pi)*atan(PCA_5_loadings$PC2/PCA_5_loadings$PC1))
PCA_5_loadings$Offset <- ((-4*sign(PCA_5_loadings$PC1))/2)
ggplot(PCA_5_values, aes(x=PC1, y=PC2, color = Sex:PEHSPNON)) + 
  geom_point(size = 1) + 
  geom_segment(data = PCA_5_loadings, aes(x=0, y=0, xend=3*PC1, yend=3*PC2), arrow = arrow(length = unit(0.5, "picas")), color="Black") + 
  geom_text(data = PCA_5_loadings, aes(label=Variables, x=PC1, y=PC2), color="black", size=4, angle=PCA_5_loadings$Angle, hjust=PCA_5_loadings$Offset)  +
  theme_classic() + theme(legend.justification = c(1,1), legend.position = c(1,1))

#don't show much as little variance explained by each PC. So we look at which variables seem to be clustered, vary quite a lot and not too small


####### Many Plots #######

library(gridExtra)
activity_names <- names(ATUS_data_minus_july)[grep("^tu", names(ATUS_dataset))]
activity_plots <- list()
for(j in activity_names){
  activity_plots[[j]] <- ggplot(ATUS_data_minus_july, aes(color=TESEX:PEHSPNON)) + geom_smooth(aes_string(x="TUDIARYDATE", y=j), se=FALSE)
}
do.call(grid.arrange,activity_plots)

proportion_names <- names(ATUS_data_minus_july)[grep("^part_tu", names(ATUS_dataset))]
proportion_plots <- list()
for(j in proportion_names){
  proportion_plots[[j]] <- ggplot(ATUS_data_minus_july, aes(color=TESEX)) + geom_smooth(aes_string(x="TUDIARYDATE", y=j), se=FALSE)
}
do.call(grid.arrange,proportion_plots)


###### Stacked Area Plots ########

library(utils)
part_Activities_per_date <- select(ATUS_data_minus_july, "TUDIARYDATE", grep("^part_tu", names(ATUS_data_minus_july)))
part_Activities_per_date <- aggregate(part_Activities_per_date, by = list(part_Activities_per_date$TUDIARYDATE), mean)
part_Activities_per_date <- mutate(part_Activities_per_date, date_sum = rowSums(select(part_Activities_per_date, part_tu01:part_tu18)))
part_Activities_per_date <- mutate_at(part_Activities_per_date, .vars = vars(part_tu01:part_tu18) ,.funs = (~.*(100/(part_Activities_per_date$date_sum))))
plot1_data <- data.frame(Date = part_Activities_per_date$TUDIARYDATE, stack(part_Activities_per_date[3:19]))
plot1 <- ggplot(plot1_data, aes(Date, values, fill = ind)) + geom_area(position = "stack")

part_Activities_per_year <- select(ATUS_data_minus_july, "TUYEAR", grep("^part_tu", names(ATUS_data_minus_july)))
part_Activities_per_year <- aggregate(part_Activities_per_year, by = list(part_Activities_per_year$TUYEAR), mean)
part_Activities_per_year <- mutate(part_Activities_per_year, year_sum = rowSums(select(part_Activities_per_year, part_tu01:part_tu18)))
part_Activities_per_year <- mutate_at(part_Activities_per_year, .vars = vars(part_tu01:part_tu18) ,.funs = (~.*(100/(part_Activities_per_year$year_sum))))
plot2_data <- data.frame(Years = part_Activities_per_year$TUYEAR, stack(part_Activities_per_year[3:19]))
plot2 <- ggplot(plot2_data, aes(Years, values, fill = ind)) + geom_area(position = "stack")

