
library(brms)
library(ggplot2)
library(tibble)
library(marginaleffects)
library(performance)

# Load data
recordings <- read.csv("recordings.csv")
sensorinfo <- read.csv("sensorinfo.csv")


# Data exploration
unique(recordings$watertemp)
hist(recordings$songlength)

# We need to change watertemp from character to numeric data
recordings$watertemp <- as.numeric(recordings$watertemp)

# Merge the 2 data frames using sensorid
merged_df <- merge(recordings, sensorinfo, by = "sensorid", all.x = TRUE)


# Model 1: Song length ~ Boat noise ####

noise_lengthmod <- brm(songlength ~ scale(boatnoise) +
                                    scale(boatactivity)+
                                    scale(watertemp) +
                                    (1|sensorid), 
                                    data=merged_df, 
                                    family="gamma")
# Look at the outputs

# Model fit
plot(noise_lengthmod)
mae(noise_lengthmod)
bayes_R2(noise_lengthmod)
summary(noise_lengthmod)

# Hypothesis testing
mcmc_plot(noise_lengthmod, type="areas") + theme_bw()

# Effect size
plot_predictions(noise_lengthmod, condition = c("boatnoise")) + theme_bw()


# Model 2: Song count ~ Boat noise ####

noise_totmod <- brm(totsongs ~ scale(boatnoise) +
                               scale(boatactivity) +
                               scale(watertemp) + 
                               (1|sensorid), 
                               data=merged_df, 
                               family = "negbinomial")

# Look at the outputs

# Model fit
plot(noise_totmod)
mae(noise_totmod)
bayes_R2(noise_totmod)
summary(noise_totmod)

# Hypothesis testing
mcmc_plot(noise_totmod, type="areas") + theme_bw()

# Effect size
plot_predictions(noise_totmod, condition = c("boatnoise")) + theme_bw()


# Model 3: Song length ~ Boat activity ####

activity_lengthmod <- brm(songlength ~ scale(boatactivity) +
                                       scale(boatnoise) +
                                       scale(watertemp) +
                                       scale(waterdepth) +
                                       scale(distshore) +
                                       (1|sensorid), 
                                       data=merged_df,
                                       family="gamma")

# Look at the outputs

# Model fit
plot(activity_lengthmod)
mae(activity_lengthmod)
bayes_R2(activity_lengthmod)
summary(activity_lengthmod)

# Hypothesis testing
mcmc_plot(activity_lengthmod, type="areas") + theme_bw()

# Effect size
plot_predictions(activity_lengthmod, condition = c("boatactivity")) + theme_bw()


# Model 4: Song count ~ Boat activity ####

activity_totmod <- brm(totsongs ~ scale(boatactivity) +
                                  scale(boatnoise) +
                                  scale(watertemp) +
                                  scale(waterdepth) +
                                  scale(distshore) +
                                  (1|sensorid), 
                                  data=merged_df,
                                  family = "negbinomial")

# Look at the outputs

# Model fit
plot(activity_totmod)
mae(activity_totmod)
bayes_R2(activity_totmod)
summary(activity_totmod)

# Hypothesis testing
mcmc_plot(activity_totmod, type="areas") + theme_bw()

# Effect size
plot_predictions(activity_totmod, condition = c("boatactivity")) + theme_bw()

