library(brms)
library(tidyr)
library(dplyr)
library(ggplot2)

# colorPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                  # "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## load data ----
load("data/nigmatullina.Rda")

NigmatullinaData <-  NigmatullinaData %>%
    select(id,imagery, direction, cue, direction_num, cue_num,
           response, correct, rt, nystagmus, threshold, imagery_score,
           vel_post, vel_pre) %>%
    # drop_na() %>%
    mutate(direction = as.factor(direction),
           cue = as.factor(cue),
           imagery = as.factor(imagery)) %>%
    filter(id != 75)

NigmatullinaData$imagery <- factor(NigmatullinaData$imagery,
                                   levels = c("congruent", "neutral", "incongruent"))



## -----

p_rt_1 <- NigmatullinaData %>% ggplot(aes(x = rt, fill = imagery)) +
    # geom_histogram(aes(y = ..density..), binwidth = 1, alpha = 0.8) +
    geom_density(alpha = 0.8) +
    facet_wrap(~id) +
    scale_fill_viridis_d() +
    theme_bw()
p_rt_1

p_rt_2 <- NigmatullinaData %>% ggplot(aes(x = rt, fill = cue)) +
    # geom_histogram(aes(y = ..density..), binwidth = 1, alpha = 0.8) +
    geom_density(alpha = 0.8) +
facet_grid(direction ~ id) +
    scale_fill_viridis_d() +
    theme_bw()
p_rt_2

p_threshold <- NigmatullinaData %>% ggplot(aes(x = threshold, fill = imagery)) +
    # geom_histogram(aes(y = ..density..), binwidth = 1, alpha = 0.8) +
    geom_density(alpha = 0.8) +
    facet_wrap(~id) +
    scale_fill_viridis_d() +
    theme_bw()
p_threshold


p_nystagmus <- NigmatullinaData %>%
    ggplot(aes(x = nystagmus, fill = cue)) +
    # geom_histogram(aes(y = ..density..), binwidth = 1, alpha = 0.8) +
    geom_density(alpha = 0.8) +
    # facet_wrap(~id) +
    facet_grid(direction~id) +

    scale_fill_viridis_d() +
    theme_bw()
p_nystagmus

p_vel_pre <- NigmatullinaData %>%
    ggplot(aes(x = vel_pre, fill = cue)) +
    # geom_histogram(aes(y = ..density..), binwidth = 1, alpha = 0.8) +
    geom_density(alpha = 0.8) +
    # facet_wrap(~id) +
    facet_grid(direction~id) +

    scale_fill_viridis_d() +
    theme_bw()
p_vel_pre

## -----

std_error <- function(x) {
    sd(x)/sqrt(length(x))
}

RTSummary <- NigmatullinaData %>%
    select(id, imagery, rt) %>%
    drop_na() %>%
    group_by(id, imagery) %>%
    summarise_all(funs(mean, median, sd, std_error))

ThresholdSummary <- NigmatullinaData %>%
    select(id, imagery, threshold) %>%
    drop_na() %>%
    group_by(id, imagery) %>%
    summarise_all(funs(mean, median, sd, std_error))

VORSummary <- NigmatullinaData %>%
    select(id, imagery, nystagmus) %>%
    drop_na() %>%
    group_by(id, imagery) %>%
    summarise_all(funs(mean, median, sd, std_error))

ImageryScoreSummary <- NigmatullinaData %>%
    select(id, imagery, imagery_score) %>%
    group_by(id, imagery) %>%
    summarise_all(funs(mean, median, sd, std_error)) %>%
    drop_na()


ProportionCorrect <- NigmatullinaData %>%
    select(id, imagery, correct) %>%
    drop_na() %>%
    group_by(id, imagery) %>%
    summarise_all(funs(mean, sd, std_error))

## ----

RTSummary %>% ggplot(aes(x = imagery, y = mean,
                         color = id)) +
    geom_errorbar(aes(ymin = mean-std_error,
                      ymax = mean+std_error),
                  width = 0.1,
                  position = "identity") +
    geom_line(aes(group = id))

# RTSummary %>%
#     ggplot(aes(x = mean, fill = imagery)) +
#     geom_histogram(position = "identity", alpha = 0.6)

# ThresholdSummary %>% ggplot(aes(x = imagery, y = mean,
#                          color = id)) +
#     # geom_errorbar(aes(ymin = mean-std_error,
#     #                   ymax = mean+std_error),
#     #               width = 0.1,
#     #               position = "identity") +
#     geom_point() +
#     geom_line(aes(group = id))

VORSummary %>% ggplot(aes(x = imagery,
                          y = mean,
                          color = id)) +
    geom_point() +
    geom_line(aes(group = id))


ImageryScoreSummary %>% ggplot(aes(x = imagery, y = mean,
                                   color = id)) +
    geom_point() +
    geom_line(aes(group = id))



ProportionCorrect %>%
    # filter(id != 75) %>%
    ggplot(aes(x = imagery, y = mean,
                                   color = id)) +
    geom_point() +
    geom_line(aes(group = id))

# we should probably remove id 75


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

fit <- brm(response ~ direction * cue,
           family = bernoulli(),
           data = NigmatullinaData)
plot(marginal_effects(fit))

# doesn't work
fit2 <- brm(bf(rt ~ imagery, ndt ~ imagery),
            family = shifted_lognormal(),
            data = NigmatullinaData)
plot(marginal_effects(fit2))


# Weibull regression is also a possibility
# https://en.wikipedia.org/wiki/Weibull_distribution

fit3 <- brm(bf(rt ~ imagery,
               shape ~ imagery),
            family = weibull(),
            data = NigmatullinaData)

plot(marginal_effects(fit3))
plot(fit3)


fit4 <- brm(bf(rt | dec(correct) ~ imagery),
            family = wiener(),
            data = NigmatullinaData)
plot(marginal_effects(fit4))
plot(fit4)




## DDM model ----



formula <- bf(rt | dec(response) ~ 0 + direction:cue + (0 + direction:cue|p|id),
              bs ~ 0 + cue + (0 + cue|p|id),
              ndt ~ 0 + cue + (0 + cue|p|id),
              bias ~ 0 + cue + (0 + cue|p|id))




# get_prior(formula)
prior <- c(
    prior("cauchy(0, 5)", class = "b"),
    set_prior("normal(1.5, 1)", class = "b", dpar = "bs"),
    set_prior("normal(0.2, 0.1)", class = "b", dpar = "ndt"),
    set_prior("normal(0.5, 0.2)", class = "b", dpar = "bias")
)


tmp_dat <- make_standata(formula,
                         family = wiener(link_bs = "identity",
                                         link_ndt = "identity",
                                         link_bias = "identity"),
                         data = NigmatullinaData,
                         prior = prior)

str(tmp_dat, 1, give.attr = FALSE)

initfun <- function() {
    list(
        b = rnorm(tmp_dat$K),
        b_bs = runif(tmp_dat$K_bs, 1, 2),
        b_ndt = runif(tmp_dat$K_ndt, 0.1, 0.15),
        b_bias = rnorm(tmp_dat$K_bias, 0.5, 0.1),
        sd_1 = runif(tmp_dat$M_1, 0.5, 1),
        z_1 = matrix(rnorm(tmp_dat$M_1*tmp_dat$N_1, 0, 0.01),
                     tmp_dat$M_1, tmp_dat$N_1),
        L_1 = diag(tmp_dat$M_1)
    )
}

fit5 <- brm(formula,
            family = wiener(link_bs = "identity",
                            link_ndt = "identity",
                            link_bias = "identity"),
            data = NigmatullinaData,
            prior = prior, inits = initfun,
            iter = 1000, warmup = 500,
            chains = 4, cores = 4,
            control = list(max_treedepth = 15))

NPRED <- 500

pred5 <- predict(fit5,
                       summary = FALSE,
                       negative_rt = TRUE,
                       nsamples = NPRED)


save(fit5, file = "brms_wiener_fit5.rda",
     compress = "xz")
save(pred5, file = "brms_wiener_predictions5.rda",
     compress = "xz")

fit5
plot(marginal_effects(fit5))
plot(fit5)
