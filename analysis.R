library(brms)
library(tidyr)
library(dplyr)
library(ggplot2)

# colorPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                  # "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## load data ----
load("data/nigmatullina.Rda")

NigmatullinaData <-  NigmatullinaData %>%
    select(participant,imagery, direction, cue, direction_num, cue_num,
           response, correct, rt, nystagmus, imagery_score) %>%
    # drop_na() %>%
    mutate(direction = as.factor(direction),
           cue = as.factor(cue),
           imagery = as.factor(imagery)) %>%
    filter(rt < 40)

NigmatullinaData$imagery <- factor(NigmatullinaData$imagery,
                                   levels = c("congruent", "neutral", "incongruent"))



## -----

p_rt <- NigmatullinaData %>% ggplot(aes(x = rt, fill = imagery)) +
    # geom_histogram(aes(y = ..density..), binwidth = 1, alpha = 0.8) +
    geom_density(alpha = 0.8) +
    facet_wrap(~participant) +
    scale_fill_viridis_d() +
    theme_bw()
p_rt

p_nystagmus <- NigmatullinaData %>% ggplot(aes(x = nystagmus, fill = imagery)) +
    # geom_histogram(aes(y = ..density..), binwidth = 1, alpha = 0.8) +
    geom_density(alpha = 0.8) +
    facet_wrap(~participant) +
    scale_fill_viridis_d() +
    theme_bw()
p_nystagmus
## -----

std_error <- function(x) {
    sd(x)/sqrt(length(x))
}

RTSummary <- NigmatullinaData %>%
    select(participant, imagery, rt) %>%
    drop_na() %>%
    group_by(participant, imagery) %>%
    summarise_all(funs(mean, median, sd, std_error))

VORSummary <- NigmatullinaData %>%
    select(participant, imagery, nystagmus) %>%
    drop_na() %>%
    group_by(participant, imagery) %>%
    summarise_all(funs(mean, median, sd, std_error))

ImageryScoreSummary <- NigmatullinaData %>%
    select(participant, imagery, imagery_score) %>%
    group_by(participant, imagery) %>%
    summarise_all(funs(mean, median, sd, std_error)) %>%
    drop_na()


ProportionCorrect <- NigmatullinaData %>%
    select(participant, imagery, correct) %>%
    drop_na() %>%
    group_by(participant, imagery) %>%
    summarise_all(funs(mean, sd, std_error))

## ----

RTSummary %>% ggplot(aes(x = imagery, y = mean,
                         color = participant)) +
    geom_errorbar(aes(ymin = mean-std_error,
                      ymax = mean+std_error),
                  width = 0.1,
                  position = "identity") +
    geom_line(aes(group = participant))

# RTSummary %>%
#     ggplot(aes(x = mean, fill = imagery)) +
#     geom_histogram(position = "identity", alpha = 0.6)


VORSummary %>% ggplot(aes(x = imagery,
                          y = mean,
                          color = participant)) +
    geom_point() +
    geom_line(aes(group = participant))


ImageryScoreSummary %>% ggplot(aes(x = imagery, y = mean,
                                   color = participant)) +
    geom_point() +
    geom_line(aes(group = participant))



ProportionCorrect %>%
    # filter(participant != 75) %>%
    ggplot(aes(x = imagery, y = mean,
                                   color = participant)) +
    geom_point() +
    geom_line(aes(group = participant))

# we should probably remove participant 75


## Nigmatullina figures ----

Summary <- RTSummary %>%
    group_by(imagery) %>%
    summarise(mu = mean(mean), se = std_error(mean))

Summary %>% ggplot(aes(x = imagery, y = mu,
                       fill = imagery)) +
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin = mu-se,
                      ymax = mu+se),
                  width = 0.1,
                  position = "dodge") +
    # geom_line(aes(group = 1)) +
    # scale_color_viridis_d() +
    scale_fill_viridis_d() +
    theme_bw()


## brms models ----

fit <- brm(rt ~ imagery)
NigmatullinaData
