# 1, preprocess time point----
sleeptime <- function(){
  
  filename <- readline(prompt = "Please Enter File Name: ")
  filepath <- paste("health_data/", filename, ".csv", sep = '')
  
  sleep <- read.csv(filepath)
  time <- substr(sleep$BedTime, 1, 19)
  property <- unclass(as.POSIXlt(time))
  day_num <- property$mday
  
  # to avoid duplicates----
  day_num[1912] = -1 #| used to avoid (i + 1)
  day_num2 <- numeric(0) #| used to record whether use/non-use
  for(i in 1:1911){
    a = day_num[i]
    b = day_num[i + 1]
    if(a == b){
      day_num2[i] = 0
    } else{
      day_num2[i] = 1
    }
  }
  
  df <- data.frame(sleep, day_num2) #| put together to find use value
  BedTime = df[day_num2 == 1, 1] #| get useful 706 data
  property2 <- unclass(as.POSIXlt(BedTime)) #| process time with function
  
  df_final = data.frame(year = property2$year + 1900,
                        mon = property2$mon + 1,
                        day = property2$mday,
                        hour = property2$hour,
                        min = property2$min,
                        sec = property2$sec)
  
  df_final$X <- 1:nrow(df_final)
  df_final$hour_min <- round((df_final$hour + df_final$min/60), 2)
  
  for(i in 1:706){
    if(df_final$hour_min[i] > 12){
      df_final$hour_[i] = df_final$hour_min[i] - 24
    } else{
      df_final$hour_[i] = df_final$hour_min[i]
    }
  }
  
  return(df_final)
}
overall <- function(df){
  library(ggplot2)
  ggplot(df, aes(x = X, y = hour_, color = factor(year))) +
    geom_point(size = 0.8) +
    theme_light() +
    geom_smooth(color = 'red', lwd = 0.5) +
    geom_hline(yintercept = 0, color = 'black', linetype = 'dashed') +
    labs(x = "Counted days", y = "Sleep time") +
    ggtitle("Sleep Time in each counted days") +
    theme(plot.title = element_text(hjust = 0.5))
}
specific <- function(df, num){
  df_ <- df[df$year == num, 2:9]
  ggplot(df_, aes(x = factor(mon), y = hour_, color = factor(mon))) +
    geom_violin() +
    geom_boxplot(width = 0.05) +
    stat_summary(fun.y=median, geom="point", size=2, color="black") +
    stat_summary(fun.y=median, geom="line", aes(group=1), color='black') +
    geom_hline(yintercept = 0, color = 'red', linetype = 'dashed') +
    theme_light() +
    labs(x = 'Month', y = 'Sleep time') +
    ggtitle("Sleep Time with in a year") +
    theme(plot.title = element_text(hjust = 0.5))
}

names(table(sleep$year))
#| what num you should type in

# 2, preprocess time period----
preprocess <- function(){
  print("---Start---")
  filename <- readline(prompt = "Please Enter File Name: ")
  filepath <- paste("health_data/", filename, ".csv", sep = '') #concatenate
  df2 = NULL
  
  print(paste('Is the filepath correct? : ', filepath, sep = ''))
  choice <- readline(prompt = "[Y/other inputs]: ")
  
  if(choice == "Y"){
    tryCatch({
      df1 <- read.csv(filepath)
      len <- length(df1[,1])
      df1$id <- 1:len
      # split start ti=me
      property1 <- unclass(as.POSIXlt(df1$startDate))
      start = data.frame(id = 1:len,
                         year1 = property1$year + 1900,
                         mon1 = property1$mon + 1,
                         day1 = property1$mday,
                         hour1 = property1$hour,
                         min1 = property1$min,
                         sec1 = property1$sec)
      # split end time
      property2 <- unclass(as.POSIXlt(df1$endDate))
      end = data.frame(id = 1:len,
                       year2 = property2$year + 1900,
                       mon2 = property2$mon + 1,
                       day2 = property2$mday,
                       hour2 = property2$hour,
                       min2 = property2$min,
                       sec2 = property2$sec)
      # merge dfs
      df2 <- merge(start, end, by = 'id')
      df2$unit <- df1$unit
      df2$value <- df1$value
    }, warning = function(w){
      print('Wrong filename, Process Failed')
    }, error = function(e){
      print('Wrong filename, Process Failed')
    }, finally = {
    })
  }
  
  if(is.null(df2)){}else{
    print('Process Succeed')
  }
  output <- df2
  print('---End---')
  return(output)
}
#| compare with summary()
basicinfo_y <- function(df){
  len <- length(df[,1])
  print(paste('there are', as.character(len), 'data'))
  print(paste('data is range from ', as.character(df$year1[1]),
        ',', as.character(df$mon1[1]), ',', as.character(df$day1[1]),
        ' to ', as.character(df$year1[len]), ',', as.character(df$mon1[len]),
        ',', as.character(df$day1[len]), sep = ''))
  (year_table <- table(df$year1))
  print(paste('we have ', length(names(year_table)), ' years here, they are:', sep = ''))
  print(names(year_table))
  print(paste('each data numbers is:', sep = ''))
  print(as.numeric(year_table))
  print('the basic info are listed here:')
  summary_df <- data.frame()
  for(i in as.numeric(names(year_table))){
    subset <- subset(df, year1==i, select = value)
    mean <- round(mean(subset$value), 2)
    median <- round(median(subset$value), 2)
    range <- paste(as.character(range(subset$value))[1], '~',
                   as.character(range(subset$value))[2])
    var <- round(var(subset$value), 2)
    sd <- round(sd(subset$value), 2)
    quartile <- paste(as.character(fivenum(subset$value))[2], ', ',
                      as.character(fivenum(subset$value))[3], ', ',
                      as.character(fivenum(subset$value))[4], sep = '')
    summary_df <- rbind(summary_df, c(mean, median, range, var, sd, quartile))
  }
  colnames(summary_df) <- c('mean', 'median', 'range', 'var', 'sd', 'quartile')
  print(summary_df)
  
}
basicinfo_m <- function(df, num){
  df_ <- df[df$year1 == num,]
  len <- length(df_[,1])
  print(paste('there are', as.character(len), 'data'))
  print(paste('data is range from ', as.character(df$year1[1]),
              ',', as.character(df$mon1[1]), ',', as.character(df$day1[1]),
              ' to ', as.character(df$year1[len]), ',', as.character(df$mon1[len]),
              ',', as.character(df$day1[len]), sep = ''))
  (month_table <- table(df_$mon1))
  print(paste('we have ', length(names(month_table)), ' months here, they are:', sep = ''))
  print(names(month_table))
  print(paste('each data numbers is:', sep = ''))
  print(as.numeric(month_table))
  print('the basic info are listed here:')
  summary_df <- data.frame()
  for(i in as.numeric(names(month_table))){
    subset <- subset(df_, mon1==i, select = value)
    mean <- round(mean(subset$value), 2)
    median <- round(median(subset$value), 2)
    range <- paste(as.character(range(subset$value))[1], '~',
                   as.character(range(subset$value))[2])
    var <- round(var(subset$value), 2)
    sd <- round(sd(subset$value), 2)
    quartile <- paste(as.character(fivenum(subset$value))[2], ', ',
                      as.character(fivenum(subset$value))[3], ', ',
                      as.character(fivenum(subset$value))[4], sep = '')
    summary_df <- rbind(summary_df, c(mean, median, range, var, sd, quartile))
  }
  colnames(summary_df) <- c('mean', 'median', 'range', 'var', 'sd', 'quartile')
  print(summary_df)
}
getit <- function(num){
  if(substr(num, 1, 1) == '0'){
    output = substr(num, 2, 2)
  }else{
    output = num
  }
  return(output)
}
#| to make '01' to '1' and stay '12' to '12'
newsubset <- function(df){
  print("---Start---")
  start <- readline(prompt = "Please Enter Start Time: ")
  start_ <- strsplit(start, '-')
  end <- readline(prompt = "Please Enter End Time: ")
  end_ <- strsplit(end, '-')
  pre_df <- subset(subset(subset(df,
                                 year1 >= as.numeric(start_[[1]][1])),
                                 mon1 >= as.numeric(getit(start_[[1]][2]))),
                                 day1 >= as.numeric(getit(start_[[1]][3])))
  fin_df <- subset(subset(subset(pre_df,
                                 year1 = as.numeric(end_[[1]][1])),
                                 mon1 = as.numeric(getit(end_[[1]][2]))),
                                 day1 = as.numeric(getit(end_[[1]][3])))
  output <- fin_df
  print("---End---")
  return(output)
}

# 3, statistical graphs----
library(ggplot2)
# exm1 stripchart----
ggplot(period, aes(x = mon1, y = value, color = factor(mon1))) +
  labs(x = "time", y = "walking speed(km/hr)") +
  ggtitle("WalkingSpeed stripchart by month") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_jitter(position = position_jitter(0.2), cex = 1.2)

# exm2 boxplot----
ggplot(period, aes(x = value, y = year1, color = factor(year1))) +
  labs(x = "walking speed(km/hr)", y = "year") +
  geom_boxplot(width = 1) +
  ggtitle("WalkingSpeed boxplot by year") +
  theme(plot.title = element_text(hjust = 0.5))

# exm3 violinplot----
ggplot(period, aes(x = day1, y = value, color = factor(day1))) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  labs(x = 'walking speed(km/hr)', y = 'day') +
  ggtitle("WalkingSpeed violin plot by day") +
  theme(plot.title = element_text(hjust = 0.5))

# exm4 histogram----
ggplot(period) +
  geom_histogram(aes(period$value, color = factor(year1))) +
  labs(x ='walking speed(km/hr)', y = 'count') +
  ggtitle("WalkingSpeed histogram by year") +
  theme(plot.title = element_text(hjust = 0.5))

# exm5 scatter plot----
ggplot(period) +
  geom_point(aes(y = value, x = id, color = factor(mon1)), size = 1.5) +
  geom_smooth(aes(y = value, x = id), lwd = 0.5, se = FALSE, color = 'red') +
  labs(x ='time in origin dataframe', y = 'walking speed(km/hr)') +
  ggtitle("WalkingSpeed scatter plot by time") +
  theme(plot.title = element_text(hjust = 0.5))


# exm6 density curve----
ggplot(period, aes(x = value, color = factor(year1), fill = factor(year1))) +
  geom_density(alpha = 0.5) +
  labs(x ='count', y = 'walking speed(km/hr)') +
  ggtitle("WalkingSpeed density curve by year") +
  theme(plot.title = element_text(hjust = 0.5))


# 4, hypothesis tests----
x_bar <- mean(period$value)
n <- as.numeric(length(period$value))
s <- sd(ws$value)
se <- s / sqrt(n)
z_critical <- qnorm(0.95)
z_score <- (x_bar - 4.7)/se
p_value <- pnorm(z_score, lower.tail = FALSE)

# 5, regression and correlation----
regress <- lm(value ~ id, data = period)
regress$coefficients

cor(period$id, period$value)
