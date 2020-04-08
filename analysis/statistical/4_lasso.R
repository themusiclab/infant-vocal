## lasso analyses for IDS

# set your directory to the cloned repo
setwd("~/git/ids")

library(data.table)
library(foreach)
library(doMC)
registerDoMC(cores = detectCores())
library(ggplot2)
library(glmnet)

`%.%` <- paste0

###############
## read data ##
###############

# todo: should loop this whole script over the winsorized vs imputed versions of the datasets

d <- fread('./data/IDS_Winsor.csv')

## reformat identifiers
d$id_recording <- d$id; d$id <- NULL
d$id_person <- d$id_site %.% d$id_person

## label of interest
d$type <-
  ifelse(d$infantdir == 1,
         'infant',
         'adult'
         ) %.%
  ifelse(d$song == 1,
         '_song',
         '_speech'
         )
## drop redundant
d$ids <- NULL

## drop people that didn't speak in all four modes
people.with.all.modes <- names(which(table(d$id_person) == 4))
d <- d[id_person %in% people.with.all.modes,]

## subset columns
id.vars <- c('id_person',
             'id_recording',
             'id_site',
             'type',
             'song',
             'infantdir'
             )
feature.vars <- c('mir_attack_iqr',
                  'mir_attack_med',
                  'mir_inharmonicity',
                  'mir_pulseclarity',
                  'mir_rolloff85',
                  'mir_roughness_iqr',
                  'mir_roughness_med',
                  'mir_tempo',
                  'npvi_phrase',
                  'praat_f0_IQR',
                  'praat_f0_median',
                  'praat_f0travel_IQR',
                  'praat_f0travel_median',
                  'praat_f0travel_rate_IQR',
                  'praat_f0travel_rate_median',
                  'praat_f1_IQR',
                  'praat_f1_median',
                  'praat_f2_IQR',
                  'praat_f2_median',
                  'praat_intensity_IQR',
                  'praat_intensity_median',
                  'praat_intensitytravel_IQR',
                  'praat_intensitytravel_median',
                  'praat_intensitytravel_rate_IQR',
                  'praat_intensitytravel_rate_median',
                  'praat_voweltravel_IQR',
                  'praat_voweltravel_median',
                  'praat_voweltravel_rate_IQR',
                  'praat_voweltravel_rate_median',
                  'tm_peak_hz',
                  'tm_std_hz'
                  )

## drop NA features
d <- na.omit(d[, c(id.vars, feature.vars), with = FALSE])

####################################
## log-transform skewed variables ##
####################################

for (j in colnames(d)[grep('tm_peak_.*', colnames(d))]){
  d[[j %.% '_log']]  <-  log(d[[j]] + 1)
  d[[j]] <- NULL
}

for (j in colnames(d)[grep('mir_roughness_.*', colnames(d))]){
  d[[j %.% '_log']]  <-  log(d[[j]] + 1)
  d[[j]] <- NULL
}

feature.vars <- colnames(d)[!colnames(d) %in% id.vars]

###################################################
## alternate feature set: demean within speakers ##
###################################################

d.demean <- copy(d)
for (j in feature.vars){
  d.demean[[j]] <- as.numeric(d.demean[[j]])
  d.demean[
   ,
   (j) := get(j) - mean(get(j)),
   by = 'id_person'
  ]
}

######################################
## 4-way categorical classification ##
######################################

## subset to features only and standardize
features.raw.mat <- as.matrix(d[, feature.vars, with = FALSE])
features.raw.mat <- scale(features.raw.mat)
features.demean.mat <- as.matrix(d.demean[, feature.vars, with = FALSE])
features.demean.mat <- scale(features.demean.mat)

## classify on within-speaker variation, cross-validate on site
set.seed(02139)
mod.inspeaker <- cv.glmnet(
  x = features.demean.mat,
  y = factor(d$type),
  alpha = 1,
  family = 'multinomial',
  foldid = as.integer(factor(d$id_site)),
  grouped = FALSE,
  standardize = TRUE,
  keep = TRUE,
  parallel = TRUE
)
## find cv-selected value of lambda
lambda.ind.inspeaker <- match(mod.inspeaker$lambda.min,
                                   mod.inspeaker$glmnet.fit$lambda
                                   )
## get label predictions at this lambda
probs.inspeaker <-
  mod.inspeaker$fit.preval[,,lambda.ind.inspeaker]
colnames(probs.inspeaker) <- 'pred_' %.% levels(factor(d$type))
predict.inspeaker <- levels(factor(d$type))[
  apply(
    probs.inspeaker,
    1,
    which.max
  )
]
## compare against actual label
correct.inspeaker <- predict.inspeaker == d$type
confusion.inspeaker <- table(actual = d$type,
                                  predicted = predict.inspeaker
                                  )
acc.inspeaker <-
  sum(diag(confusion.inspeaker)) / sum(confusion.inspeaker)
## output
results.inspeaker <- data.table(
  id_site = d$id_site,
  id_person = d$id_person,
  actual = d$type,
  pred = predict.inspeaker,
  probs.inspeaker,
  correct = predict.inspeaker == d$type
)
fwrite(results.inspeaker,
       './results/lasso_cat_inspeaker_cvsite.csv'
       )
fwrite(results.inspeaker[, .N, by = c('actual', 'pred')],
       './results/lasso_cat_inspeaker_confusion.csv'
       )

## cross-speaker classification, cross-validate on site
set.seed(02139)
mod.crossspeaker <- cv.glmnet(
  x = features.raw.mat,
  y = factor(d$type),
  alpha = 1,
  family = 'multinomial',
  foldid = as.integer(factor(d$id_site)),
  grouped = FALSE,
  standardize = TRUE,
  keep = TRUE,
  parallel = TRUE
)
## find cv-selected value of lambda
lambda.ind.crossspeaker <- match(mod.crossspeaker$lambda.min,
                                 mod.crossspeaker$glmnet.fit$lambda
                                 )
## get label predictions at this lambda
probs.crossspeaker <-
  mod.crossspeaker$fit.preval[,,lambda.ind.crossspeaker]
colnames(probs.crossspeaker) <- 'pred_' %.% levels(factor(d$type))
predict.crossspeaker <- levels(factor(d$type))[
  apply(
    probs.crossspeaker,
    1,
    which.max
  )
]
## compare against actual label
correct.crossspeaker <- predict.crossspeaker == d$type
confusion.crossspeaker <- table(actual = d$type,
                                predicted = predict.crossspeaker
                                )
acc.crossspeaker <-
  sum(diag(confusion.crossspeaker)) / sum(confusion.crossspeaker)
## output
results.crossspeaker <- data.table(
  id_site = d$id_site,
  id_person = d$id_person,
  actual = d$type,
  pred = predict.crossspeaker,
  probs.crossspeaker,
  correct = predict.crossspeaker == d$type
)
fwrite(results.crossspeaker,
       './results/lasso_cat_crossspeaker_cvsite.csv'
       )
fwrite(results.crossspeaker[, .N, by = c('actual', 'pred')],
       './results/lasso_cat_crossspeaker_confusion.csv'
       )

###########################
## binary classification ##
###########################

set.seed(02139)
mod.adult.all.infant.all <- cv.glmnet(
  x = features.demean.mat,
  y = d$infantdir,
  alpha = 1,
  family = 'binomial',
  foldid = as.integer(factor(d$id_site)),
  grouped = FALSE,
  standardize = TRUE,
  keep = TRUE,
  parallel = TRUE
)
## find cv-selected value of lambda
lambda.ind.adult.all.infant.all <- match(
  mod.adult.all.infant.all$lambda.min,
  mod.adult.all.infant.all$glmnet.fit$lambda
)
## get coefs at this lambda
coef.adult.all.infant.all <- mod.adult.all.infant.all$glmnet.fit$beta[
  , lambda.ind.adult.all.infant.all
]
## get label predictions at this lambda
probs.adult.all.infant.all <-
  mod.adult.all.infant.all$fit.preval[,lambda.ind.adult.all.infant.all]
predict.adult.all.infant.all <- probs.adult.all.infant.all >= .5
## compare against actual label
correct.adult.all.infant.all <- predict.adult.all.infant.all == d$infantdir
confusion.adult.all.infant.all <- table(actual = d$type,
                                        predicted = predict.adult.all.infant.all
                                        )
## output
results.adult.all.infant.all <- data.table(
  id_site = d$id_site,
  id_person = d$id_person,
  actual = d$type,
  actual_infantdir = d$infantdir,
  pred_infantdir = predict.adult.all.infant.all,
  prob_infantdir = probs.adult.all.infant.all,
  correct = predict.adult.all.infant.all == d$infantdir
)
fwrite(results.adult.all.infant.all,
       './results/lasso_bin_adultall_infantall_cvsite.csv'
       )
write.csv(as.matrix(coef.adult.all.infant.all),
          './results/lasso_bin_adultall_infantall_coefs.csv'
          )
results.adult.all.infant.all[, pred_infantdir := as.numeric(pred_infantdir)]
fwrite(results.adult.all.infant.all[,
                                    .N,
                                    by = c('actual_infantdir', 'pred_infantdir')
                                    ],
       './results/lasso_bin_adultall_infantall_confusion.csv'
       )

## (2) infant song vs adult song
set.seed(02139)
mod.adult.song.infant.song <- cv.glmnet(
  x = features.demean.mat[d$song == 1,],
  y = d$infantdir[d$song == 1],
  alpha = 1,
  family = 'binomial',
  foldid = as.integer(factor(d$id_site[d$song == 1])),
  grouped = FALSE,
  standardize = TRUE,
  keep = TRUE,
  parallel = TRUE
)
## find cv-selected value of lambda
lambda.ind.adult.song.infant.song <- match(
  mod.adult.song.infant.song$lambda.min,
  mod.adult.song.infant.song$glmnet.fit$lambda
)
## get coefs at this lambda
coef.adult.song.infant.song <- mod.adult.song.infant.song$glmnet.fit$beta[
  , lambda.ind.adult.song.infant.song
]
## get label predictions at this lambda
probs.adult.song.infant.song <-
  mod.adult.song.infant.song$fit.preval[,lambda.ind.adult.song.infant.song]
predict.adult.song.infant.song <- probs.adult.song.infant.song >= .5
## compare against actual label
correct.adult.song.infant.song <- predict.adult.song.infant.song == d$infantdir[d$song == 1]
confusion.adult.song.infant.song <- table(actual = d$type[d$song == 1],
                                          predicted = predict.adult.song.infant.song
                                          )
## output
results.adult.song.infant.song <- data.table(
  id_site = d$id_site[d$song == 1],
  id_person = d$id_person[d$song == 1],
  actual = d$type[d$song == 1],
  actual_infantdir = d$infantdir[d$song == 1],
  pred_infantdir = predict.adult.song.infant.song,
  prob_infantdir = probs.adult.song.infant.song,
  correct = predict.adult.song.infant.song == d$infantdir[d$song == 1]
)
fwrite(results.adult.song.infant.song,
       './results/lasso_bin_adultsong_infantsong_cvsite.csv'
       )
write.csv(as.matrix(coef.adult.song.infant.song),
          './results/lasso_bin_adultsong_infantsong_coefs.csv'
          )
results.adult.song.infant.song[, pred_infantdir := as.numeric(pred_infantdir)]
fwrite(results.adult.song.infant.song[,
                                      .N,
                                      by = c('actual_infantdir', 'pred_infantdir')
                                      ],
       './results/lasso_bin_adultsong_infantsong_confusion.csv'
       )

## (3) infant speech vs adult speech
set.seed(02139)
mod.adult.speech.infant.speech <- cv.glmnet(
  x = features.demean.mat[d$song == 0,],
  y = d$infantdir[d$song == 0],
  alpha = 1,
  family = 'binomial',
  foldid = as.integer(factor(d$id_site[d$song == 0])),
  grouped = FALSE,
  standardize = TRUE,
  keep = TRUE,
  parallel = TRUE
)
## find cv-selected value of lambda
lambda.ind.adult.speech.infant.speech <- match(
  mod.adult.speech.infant.speech$lambda.min,
  mod.adult.speech.infant.speech$glmnet.fit$lambda
)
## get coefs at this lambda
coef.adult.speech.infant.speech <- mod.adult.speech.infant.speech$glmnet.fit$beta[
  , lambda.ind.adult.speech.infant.speech
]
## get label predictions at this lambda
probs.adult.speech.infant.speech <-
  mod.adult.speech.infant.speech$fit.preval[,lambda.ind.adult.speech.infant.speech]
predict.adult.speech.infant.speech <- probs.adult.speech.infant.speech >= .5
## compare against actual label
correct.adult.speech.infant.speech <-
  predict.adult.speech.infant.speech == d$infantdir[d$song == 0]
confusion.adult.speech.infant.speech <- table(actual = d$type[d$song == 0],
                                          predicted = predict.adult.speech.infant.speech
                                          )
## output
results.adult.speech.infant.speech <- data.table(
  id_site = d$id_site[d$song == 0],
  id_person = d$id_person[d$song == 0],
  actual = d$type[d$song == 0],
  actual_infantdir = d$infantdir[d$song == 0],
  pred_infantdir = predict.adult.speech.infant.speech,
  prob_infantdir = probs.adult.speech.infant.speech,
  correct = predict.adult.speech.infant.speech == d$infantdir[d$song == 0]
)
fwrite(results.adult.speech.infant.speech,
       './results/lasso_bin_adultspeech_infantspeech_cvsite.csv'
       )
write.csv(as.matrix(coef.adult.speech.infant.speech),
          './results/lasso_bin_adultspeech_infantspeech_coefs.csv'
          )
results.adult.speech.infant.speech[, pred_infantdir := as.numeric(pred_infantdir)]
fwrite(results.adult.speech.infant.speech[,
                                    .N,
                                    by = c('actual_infantdir', 'pred_infantdir')
                                    ],
       './results/lasso_bin_adultspeech_infantspeech_confusion.csv'
       )

## (4) infant song vs infant speech
set.seed(02139)
mod.infant.speech.infant.song <- cv.glmnet(
  x = features.demean.mat[d$infantdir == 1,],
  y = d$song[d$infantdir == 1],
  alpha = 1,
  family = 'binomial',
  foldid = as.integer(factor(d$id_site[d$infantdir == 1])),
  grouped = FALSE,
  standardize = TRUE,
  keep = TRUE,
  parallel = TRUE
)
## find cv-selected value of lambda
lambda.ind.infant.speech.infant.song <- match(
  mod.infant.speech.infant.song$lambda.min,
  mod.infant.speech.infant.song$glmnet.fit$lambda
)
## get coefs at this lambda
coef.infant.speech.infant.song <- mod.infant.speech.infant.song$glmnet.fit$beta[
, lambda.ind.infant.speech.infant.song
]
## get label predictions at this lambda
probs.infant.speech.infant.song <-
  mod.infant.speech.infant.song$fit.preval[,lambda.ind.infant.speech.infant.song]
predict.infant.speech.infant.song <- probs.infant.speech.infant.song >= .5
## compare against actual label
correct.infant.speech.infant.song <-
  predict.infant.speech.infant.song == d$song[d$infantdir == 1]
confusion.infant.speech.infant.song <- table(actual = d$type[d$infantdir == 1],
                                             predicted = predict.infant.speech.infant.song
                                             )
## output
results.infant.speech.infant.song <- data.table(
  id_site = d$id_site[d$infantdir == 1],
  id_person = d$id_person[d$infantdir == 1],
  actual = d$type[d$infantdir == 1],
  actual_song = d$song[d$infantdir == 1],
  pred_song = predict.infant.speech.infant.song,
  prob_song = probs.infant.speech.infant.song,
  correct = predict.infant.speech.infant.song == d$song[d$infantdir == 1]
)
fwrite(results.infant.speech.infant.song,
       './results/lasso_bin_infantspeech_infantsong_cvsite.csv'
       )
write.csv(as.matrix(coef.infant.speech.infant.song),
          './results/lasso_bin_infantspeech_infantsong_coefs.csv'
          )
results.infant.speech.infant.song[, pred_song := as.numeric(pred_song)]
fwrite(results.infant.speech.infant.song[,
                                         .N,
                                         by = c('actual_song', 'pred_song')
                                         ],
       './results/lasso_bin_infantspeech_infantsong_confusion.csv'
       )

############################
## preliminary inspection ##
############################

## sink('./results/acc_all.txt')
{
  cat('cross speaker (4 categories) accuracy:',
      mean(correct.crossspeaker),
      '\n'
      )
  cat('demeaning within speaker (4 categories) accuracy:',
      mean(correct.inspeaker),
      '\n'
      )
  cat('adult (all) vs infant (all) accuracy:',
      mean(correct.adult.all.infant.all),
      '\n'
      )
  cat('adult (song) vs infant (song) accuracy:',
      mean(correct.adult.song.infant.song),
      '\n'
      )
  cat('adult (speech) vs infant (speech) accuracy:',
      mean(correct.adult.speech.infant.speech),
      '\n'
      )
  cat('infant (speech) vs infant (song) accuracy:',
      mean(correct.infant.speech.infant.song),
      '\n'
      )
}
## sink()

sort(coef.adult.all.infant.all[coef.adult.all.infant.all != 0])
sort(coef.adult.song.infant.song[coef.adult.song.infant.song != 0])
sort(coef.adult.speech.infant.speech[coef.adult.speech.infant.speech != 0])
sort(coef.infant.speech.infant.song[coef.infant.speech.infant.song != 0])

##############################################################
## inference for binary classification generalization error ##
##############################################################

## nadeau & bengio 1999: corrected resampled t-test
##   (a rough approximation to variance of generalization error)

## corrective factor for comparisons (note that halving n within each fold, as
##   when analyzing infant-directed recordings only or songs only, leaves the
##   corrective factor unchanged)
nb1.j <- uniqueN(d$id_site)  # number of folds
nb1.n <- nrow(d)             # total data size
nb1.n2 <- table(d$id_site)   # test set sizes
nb1.rho0s <- nb1.n2 / nb1.n  # "naive surrogate" of inter-fold correlation
nb1.rho0 <- mean(nb1.rho0s)  # take average rho approximation across folds
nb1.factor <- sqrt(1 / nb1.j + nb1.rho0 / (1 - nb1.rho0))

## variability of test accuracy across folds
sd.crossspeaker <-
  sd(tapply(correct.crossspeaker, d$id_site, mean))
sd.inspeaker <-
  sd(tapply(correct.inspeaker, d$id_site, mean))
sd.adult.all.infant.all <-
  sd(tapply(correct.adult.all.infant.all, d$id_site, mean))
sd.adult.song.infant.song <-
  sd(tapply(correct.adult.song.infant.song, d[song == 1, id_site], mean))
sd.adult.speech.infant.speech <-
  sd(tapply(correct.adult.speech.infant.speech, d[song == 0, id_site], mean))
sd.infant.speech.infant.song <-
  sd(tapply(correct.infant.speech.infant.song, d[infantdir == 1, id_site], mean))

## average accuracy
results <- data.table(
  comparison = c(
    'cross speaker, 4-way comparison',
    'within speaker, 4-way comparison',
    'adult all vs infant all',
    'adult song vs infant song',
    'adult speech vs infant speech',
    'infant speech vs infant song'
  ),
  subset = c(
    'all recordings',
    'all recordings',
    'all recordings',
    'songs only',
    'speech only',
    'infant-directed only'
  ),
  acc = c(
    mean(correct.crossspeaker),
    mean(correct.inspeaker),
    mean(correct.adult.all.infant.all),
    mean(correct.adult.song.infant.song),
    mean(correct.adult.speech.infant.speech),
    mean(correct.infant.speech.infant.song)
  ),
  se = c(
    sd.crossspeaker * nb1.factor,
    sd.inspeaker * nb1.factor,
    sd.adult.all.infant.all * nb1.factor,
    sd.adult.song.infant.song * nb1.factor,
    sd.adult.speech.infant.speech * nb1.factor,
    sd.infant.speech.infant.song * nb1.factor
  )
)

results[, cilo := acc + qnorm(.025) * se]
results[, cihi := acc + qnorm(.975) * se]
results

fwrite(results,
       './results/performance_cvsite.csv'
       )

ggplot(results,
       aes(x = comparison,
           y = acc,
           ymin = cilo,
           ymax = cihi
           )
       ) +
  geom_point() +
  geom_errorbar() +
  theme_minimal() +
  geom_hline(yintercept = c(.25, .5), linetype = 'dashed') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
