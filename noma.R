library(dplyr)
library(readr)
library(ggplot2)

# My ask is to create a timeline of symptoms. You will see that for every symptom (Pain, Fever, Anorexia, Apathy) there is an "SYMPTOM_since_ref", this is the number of days before/after the reference diagnostic which is day 0, sometimes its a negative value e.g. -40, some times its positive e.g. 40. I envision the timeline as the example attached, mean and range for each symptom and the size of the dot to represent the number of datapoints used.


# read in the data
noma <- read_csv('data/Noma database Paola.csv')

# lower case all column names
colnames(noma) <- tolower(colnames(noma))

# get ref columns
noma <- noma[,grepl('_since_ref', names(noma))]

# clean
for(j in 1:ncol(noma)){
  input <- as.character(unlist(noma[,j]))
  out <- trimws(input, which = 'both')
  out <- gsub("[^0-9.-]", "", out)
  out <- gsub("\\(|\\)", "", out)
  out <- as.numeric(out)
  out <- ifelse(out < -1000, NA, 
                ifelse(out > 1000, NA, out))
  noma[,j] <- out
}

# outputs
if(!dir.exists('outputs')){
  dir.create('outputs')
}

# Get average for each column
average <- function(x){mean(x, na.rm = TRUE)}
output_averages <- noma %>%
  summarise_all(average)
output_averages
write_csv(output_averages, 'outputs/averages.csv')

# Get median for each column
get_median <- function(x){median(x, na.rm = TRUE)}
output_medians <- noma %>%
  summarise_all(get_median)
output_medians
write_csv(output_medians, 'outputs/medians.csv')

# Visualization with variance
long_data <- 
  tidyr::gather(noma, key, value, pain_since_ref:death_since_ref) %>%
  filter(!is.na(value)) %>%
  mutate(key = gsub('_since_ref', '', key)) %>%
  mutate(key = gsub('_', ' ', key))
write_csv(long_data, 'outputs/long_data.csv')

ggplot(long_data,
       aes(x = value,
           y = 1)) +
  geom_jitter(aes(color = key))

# Visualization with only means
long_data_averages <- output_averages %>%
  tidyr::gather(key, value, pain_since_ref:death_since_ref) %>%
  mutate(key = gsub('_since_ref', '', key)) %>%
  mutate(key = gsub('_', ' ', key))
  
ggplot(long_data,
       aes(x = value,
           y = 1)) +
  geom_point(aes(color = key)) +
  theme_bw() +
  scale_y_continuous(breaks = NULL) +
  labs(x = 'Day', y = '') +
  theme(legend.position = 'bottom') +
  scale_colour_discrete(name = '')

# Range plot
pd <- long_data %>%
  group_by(key) %>%
  summarise(avg = mean(value),
            minimum = min(value),
            maximum = max(value))

ggplot(data = pd,
       aes(x = avg,
           y = key)) +
  geom_point() +
  geom_errorbar(aes(
    xmin = minimum,
    xmax = maximum)) +
  theme_bw() +
  labs(x = 'Days',
       y = 'Symptom')


# More requests, May 2024
new_data_labels <- read_csv('data/2024-05-24_labeled_NomaFeaturesSystemat_DATA_LABELS__1552.csv')
new_data <- read_csv('data/2024-05-24__raw_NomaFeaturesSystemat_DATA__1552.csv')
# Read in clinical features groups
cf <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vRkqLDGDGdIxBQEYq3QY65U20bz_RwyHJteCI2ESY8fNuInycyfv-DG1-kwQ-4l2ftf1osnWt_v7X-J/pub?output=csv')
names(cf)[1] <- 'cf'
cf$cf[!cf$cf %in% names(new_data)]


new_data <- new_data %>%
  mutate(day0 = as.numeric(necrosis_day)) %>%
  filter(!is.na(day0))

# Clean up 

# lower case all column names
colnames(new_data) <- tolower(colnames(new_data))

# get ref columns
new_data <- new_data[,names(new_data) == 'day0' | names(new_data) %in% cf$cf]

# clean
for(j in 1:ncol(new_data)){
  input <- as.character(unlist(new_data[,j]))
  out <- trimws(input, which = 'both')
  out <- gsub("[^0-9.-]", "", out)
  out <- gsub("\\(|\\)", "", out)
  out <- as.numeric(out)
  out <- ifelse(out < -180, NA, 
                ifelse(out > 180, NA, out))
  new_data[,j] <- out
}

# Change everything to reference day adjusted
for(j in 1:29){
  new_data[,j] <- as.numeric(unlist(new_data[,j])) - new_data$day0 
}

# Get each clinical features median and IQR
long_data <- 
  tidyr::gather(new_data, key, value, pain_day:nasal_regurg_day) %>%
  filter(!is.na(value)) 
# Bring in the groups / stages
long_data <- left_join(long_data,
                       cf, by = c('key' = 'cf'))
# Clean up
long_data <- long_data %>%
  mutate(key = gsub('_day', '', key)) 

# Get median and IQR
plot_data <- long_data %>%
  group_by(key, stage) %>%
  summarise(the_median = median(value, na.rm = TRUE),
            the_25 = quantile(value, 0.25, na.rm = TRUE),
            the_75 = quantile(value, 0.75, na.rm = TRUE),
            sample_size = length(which(!is.na(value)))) %>%
  ungroup %>%
  mutate(stage = paste0('Stage ', stage)) %>%
  mutate(key = gsub('_', ' ', key)) %>%
  mutate(key = Hmisc::capitalize(key)) %>%
  mutate(orderer = paste0(stage, '-', the_median)) %>%
  arrange(desc(orderer)) %>%
  mutate(key = factor(key, levels = key))


cols <- RColorBrewer::brewer.pal(n = length(unique(plot_data$stage)),
                                 'Spectral')
cols[3] <- 'purple'
ggplot(data = plot_data,
       aes(x = the_median,
           y = key,
           color = stage)) +
  geom_point() +
  geom_errorbar(aes(
    xmin = the_25,
    xmax = the_75)) +
  theme_bw() +
  labs(x = 'Days',
       y = 'Symptom') +
  scale_color_manual(name = '', values = cols) +
  geom_text(aes(#x = the_25,
                y = key,
                label = paste0('(n = ', sample_size, ')')),
            # nudge_x = -2,
            x = 29,
            size = 3) +
  xlim(-30, 30) 
