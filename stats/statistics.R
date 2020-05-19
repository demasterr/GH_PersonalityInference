# Pre-requisites
list.packages <- c("RMariaDB", "dplyr", "plyr", "ggpubr", "glue", "BSDA", "moments", "MASS", "effsize", "xtable", "reshape2", "kableExtra", "forcats", "coin", "lsr")
filtered.packages <- list.packages[!(list.packages %in% installed.packages()[,"Package"])]
if(length(filtered.packages)) install.packages(filtered.packages, dependencies = TRUE)
for (lib in list.packages){
  library(lib, character.only=TRUE)
}
library(DBI)
library(dplyr)
library(ggpubr)
library(car)

disableKable = FALSE

# Few default transformations:
log10_transform <- function(x) log10(x + 0.001)
sqrt_transform <- function(x) sqrt(x)

# Few default colors.
color <- list(
  c1 = rgb(173,216,230, max = 255, names = "lt.blue"),
  c2 = rgb(255,192,203, max = 255, names = "lt.pink"),
  c3 = rgb(165, 255, 140, max = 255, names = "lt.green"),
  c4 = rgb(194, 140, 255, max = 255, names = "lt.purple"),
  c5 = rgb(252, 186, 3, max = 255, names = "lt.orange"),
  c6 = rgb(252, 28, 28, max = 255, names = "lt.red"),
  
  c_sc1 = rgb(0, 34, 82, max = 255, names = "sc.1"),
  c_sc2 = rgb(0, 104, 120, max = 255, names = "sc.2"),
  c_sc3 = rgb(0, 154, 108, max = 255, names = "sc.3"),
  c_sc4 = rgb(216, 238, 204, max = 255, names = "sc.4")
)

alphcolor <- function(c, alpha=1.0) {
  adjustcolor(color[[c]], alpha)
}

colors <- function(idxs) {
  out <- c()
  for (i in idxs) {
    out <- c(out, color[[i]])
  }
  out
}

# Trait names
traits = c('Openness', 'Conscientiousness', 'Extraversion', 'Agreeableness', 'Neuroticism')

# Database configuration
config_ <- function() {
  path <- "../config.json"
  if(!file.exists(path)) {
    stop("Can't find the config file at '", path, "'")
  }
  jsonlite::read_json(path)
}
config <- function(cfg) {
  unlist(config_()[cfg])
}
if (Sys.getenv('PASSWORD') == '') {
  password <- readline(prompt='Password missing in environment, please enter password: ')
  Sys.setenv('PASSWORD'=password)
}

# Get the LIWC scores for the 2007 version.
get_liwc_scores <- function(data, filename) {
  liwc_scores <- read.table(paste0('../out/LIWC/output/LIWC2007 Results - ', filename,'.csv'), header=TRUE, sep=',')
  liwc_scores <- liwc_scores %>% mutate_all(~gsub('.txt', '', .))
  liwc_scores <- liwc_scores[(liwc_scores$Filename %in% data$id),]
  liwc_scores <- liwc_scores[,c('Filename', 'Exclam')]
  liwc_scores <- transform(liwc_scores, Filename=as.numeric(Filename), Exclam=as.numeric(Exclam))
  liwc_scores[order(liwc_scores[,1]),]
}

# Remove all outliers after the 75th quantile.
remove_outliers <- function(df, liwc_scores) {
  q_threshold = quantile(liwc_scores$Exclam)[['75%']]
  df[!(df$golbeck_neuroticism > q_threshold),]
}

# Remove all effect caused by Exclam scores.
remove_exclam_liwc <- function(df, liwc_scores) {
  df$golbeck_conscientiousness <- df$golbeck_conscientiousness - 0.26 * liwc_scores$Exclam
  df$golbeck_neuroticism <- df$golbeck_neuroticism - 0.317 * liwc_scores$Exclam
  df$golbeck_openness <- df$golbeck_openness + 0.295 * liwc_scores$Exclam
  df
}

# Get the ground-truth scores.
get_ground_truth <- function() {
  gt <- read.table("../data/bfi_results.csv", header=TRUE, sep=",")
  gt <- transform(gt, id=as.numeric(id), Openness=as.numeric(Openness), Conscientiousness=as.numeric(Conscientiousness), Extraversion=as.numeric(Extraversion), Agreeableness=as.numeric(Agreeableness), Neuroticism=as.numeric(Neuroticism), word_count=as.numeric(word_count))
  return(gt)
}

# Get a MySQL database connection.
get_connection <- function() {
  dbConnect(RMariaDB::MariaDB(), user=config('user'), password=Sys.getenv('PASSWORD'), dbname=config('database'))
}

# Read two databases and apply necessary steps to prepare the datasets.s
readdbs <- function(dbname1, dbname2, normalize = TRUE, remove_exclam = FALSE, reduce_outliers = FALSE, outlier_quantile = NULL, transformations=list(), raw=c(TRUE, TRUE), sort=TRUE, where='pi_filled=1') {
  mydb = get_connection()
  
  res1 <- readdb(dbname1, mydb, raw=raw[[1]], where=where)
  res2 <- readdb(dbname2, mydb, raw=raw[[2]], where=where)
  
  res1 <- res1[(res1$id %in% res2$id),]
  res2 <- res2[(res2$id %in% res1$id),]
  
  # Remove exclamation scores for Golbeck.
  if (remove_exclam) {
    filename <- substring(unlist(strsplit(dbname2, 'selected_dev_parsed'))[2], 2,)
    liwc_scores <- get_liwc_scores(res1, filename=filename)
    res1 <- remove_exclam_liwc(df=res1, liwc_scores=liwc_scores)
    res2 <- remove_exclam_liwc(df=res2, liwc_scores=liwc_scores)
  } else if(reduce_outliers) {
    filename <- substring(unlist(strsplit(dbname2, 'selected_dev_parsed'))[2], 2,)
    liwc_scores <- get_liwc_scores(res1, filename=filename)
    res1 <- remove_outliers(df=res1, liwc_scores=liwc_scores)
    res2 <- remove_outliers(df=res2, liwc_scores=liwc_scores)
    res1 <- res1[(res1$id %in% res2$id),]
    res2 <- res2[(res2$id %in% res1$id),]
  }
  # If normalization is enabled.
  if (normalize) {
    if (raw[[1]]) {  # If not raw, it is already normalized.
      res1 <- normalize_data(res1, outlier_quantile)
    }
    if (raw[[2]]) {
      res2 <- normalize_data(res2, outlier_quantile)
    }
  }
  # If there are transformations, you always normalize.
  if (length(transformations) > 0) {
    if (normalize == FALSE) {
      stop('Normalize should be TRUE if transformation is applied!')
    }
    if (raw[[1]]) {  # If not raw, it is already normalized.
      res1 <- normalize_data(transform_columns(res1, transformations), outlier_quantile)
    }
    if (raw[[2]]) {  # If not raw, it is already normalized.
      res2 <- normalize_data(transform_columns(res2, transformations), outlier_quantile)
    }
  }
  
  # Round all personality columns to two decimal values.
  res1[,-1] <- round(res1[,-1], 2)
  res2[,-1] <- round(res2[,-1], 2)
  
  if (sort) {
    res1 <- res1[order(res1$id),] # Sort on the id column
    res2 <- res2[order(match(res2$id, res1$id)),]
    row.names(res1) <- NULL
    row.names(res2) <- NULL
  }
  
  RMariaDB::dbDisconnect(mydb)

  list('db1' = res1, 'db2' = res2)
}

read_single_db <- function(dbname, normalize=TRUE, transformations=list(), raw=TRUE, where="pi_filled=1") {
  mydb = get_connection()
  
  res1 <- readdb(dbname=dbname, mydb=mydb, raw=raw, where=where)
  
  RMariaDB::dbDisconnect(mydb)
  
  # If normalization is enabled.
  if (normalize) {
    if (raw) {  # If not raw, it is already normalized.
      res1 <- normalize_data(res1)      
    }
  }
  # If there are transformations, you always normalize.
  if (length(transformations) > 0) {
    if (normalize == FALSE) {
      stop('Normalize should be TRUE if transformation is applied!')
    }
    if (raw) {  # If not raw, it is already normalized.
      res1 <- normalize_data(transform_columns(res1, transformations))
    }
  }
  
  # Round all personality columns to two decimal values.
  res1[,-1] <- round(res1[,-1], 2)
  
  return(res1)
}

# Read the database with default query.
readdb <- function(dbname, mydb, raw, where) {
  if (raw) {
    query <- glue("
                   SELECT id, pi_openness_raw as pi_openness, pi_conscientiousness_raw as pi_conscientiousness, pi_extraversion_raw as pi_extraversion, pi_agreeableness_raw as pi_agreeableness, pi_neuroticism_raw as pi_neuroticism,
                   liwc_openness_yarkoni_raw as yarkoni_openness, liwc_conscientiousness_yarkoni_raw as yarkoni_conscientiousness, liwc_extraversion_yarkoni_raw as yarkoni_extraversion, liwc_agreeableness_yarkoni_raw as yarkoni_agreeableness, liwc_neuroticism_yarkoni_raw as yarkoni_neuroticism,
                   liwc_openness_golbeck_raw as golbeck_openness, liwc_conscientiousness_golbeck_raw as golbeck_conscientiousness, liwc_extraversion_golbeck_raw as golbeck_extraversion, liwc_agreeableness_golbeck_raw as golbeck_agreeableness, liwc_neuroticism_golbeck_raw as golbeck_neuroticism
                   FROM {dbname} WHERE ", where)
  } else {
    query <- glue("
                   SELECT id, pi_openness, pi_conscientiousness, pi_extraversion, pi_agreeableness, pi_neuroticism,
                   liwc_openness_yarkoni_raw as yarkoni_openness, liwc_conscientiousness_yarkoni_raw as yarkoni_conscientiousness, liwc_extraversion_yarkoni_raw as yarkoni_extraversion, liwc_agreeableness_yarkoni_raw as yarkoni_agreeableness, liwc_neuroticism_yarkoni_raw as yarkoni_neuroticism,
                   liwc_openness_golbeck_raw as golbeck_openness, liwc_conscientiousness_golbeck_raw as golbeck_conscientiousness, liwc_extraversion_golbeck_raw as golbeck_extraversion, liwc_agreeableness_golbeck_raw as golbeck_agreeableness, liwc_neuroticism_golbeck_raw as golbeck_neuroticism
                   FROM {dbname} WHERE ", where)
  }
  res <- dbSendQuery(mydb, query)
  data <- dbFetch(res)
  dbHasCompleted(res)
  data
}

# Read a CSV file into a data frame.
readcsv <- function(file_name) {
  read.csv(file_name, header=TRUE, colClasses = c(
    "NULL", "integer", rep("character",2), rep("Date",2), rep("character", 5), "logical", rep("integer", 3), "character",
    rep("factor", 2), rep("character", 4), "integer"
  ))
}

# Normalize the data using min-max normalization.
normalize_data <- function(data, outlier_quantile = NULL) {
  if (!is.null(outlier_quantile)) {
    for (i in 2:range(length(data))) {
      q_lower <- quantile(data[,i], 1-outlier_quantile)
      q_upper <- quantile(data[,i], outlier_quantile)
      data[,i] <- ifelse(data[,i] > q_upper, q_upper, data[,i])
      data[,i] <- ifelse(data[,i] < q_lower, q_lower, data[,i])
    }
  }
  data %>% mutate_at(-1, funs((.-min(.))/max(.-min(.))))
}

# Transform column using the transformations specified.
transform_columns <- function(df, transformations) {
  for (transformation in transformations) {
    idx <- transformation[[1]]
    df[,idx] <- sapply(df[,idx], transformation[[2]]) # Apply the actual transformation
  }
  df
}

# Textually describe the dataframes.
summarize_df <- function(dfs, dfnames=c(NULL,NULL), fix_naming=FALSE, scale_100=FALSE) {
  
  skip_names_col <- is.null(dfnames[[1]])
  
  summ_df <- data.frame(Df=character(),
                        Trait=character(), 
                        Min=double(), 
                        'Q1'=double(), 
                        Median=double(), 
                        Mean=double(), 
                        'Q3'=double(), 
                        Max=double(), 
                        stringsAsFactors = FALSE)
  if (skip_names_col) {
    summ_df <- subset(summ_df, select=-1)
    dfnames = rep(NULL, length(dfs))
  }

  scaler <- ifelse(scale_100, 100, 1)
  
  summarize_col <- function(in_df, i, res_df, dfname) {
    res <- summary(in_df[,i])
    fill_values <- c(dfname,
                     fix_column_name(colnames(in_df)[i], fix_naming),
                     round(res['Min.'],2)*scaler,
                     round(res['1st Qu.'],2)*scaler, 
                     round(res['Median'],2)*scaler,
                     round(res['Mean'],2)*scaler,
                     round(res['3rd Qu.'],2)*scaler,
                     round(res['Max.'],2)*scaler)
    res_df[nrow(res_df)+1, ] <- fill_values
    res_df
  }
  for (col_i in 2:length(colnames(dfs[[1]]))) {
    for (df_i in 1:length(dfs)) {
      summ_df <- summarize_col(dfs[[df_i]], col_i, summ_df, dfnames[df_i])
    }
  }
  summ_df
}

fix_column_name <- function(colname, fix_naming=TRUE) {
  if (!fix_naming) {
    return(colname)
  }
  spl <- strsplit(colname,split='_')[[1]]
  Caps <- function(x) {
    stringr::str_to_title(x)
  }
  spl <- lapply(spl, Caps)
  if (spl[[1]] == 'Pi') {
    spl[[1]] <- 'PI'
  }
  out <- paste(spl, sep="", collapse=" ")
  return(sanitize(out, type="latex"))
}

shorten_column_name <- function(colname) {
  # If the column name is not fixed, fix this first.
  if (grepl('_', colname, fixed=TRUE)) {
    colname <- fix_column_name(colname)
  }
  # Split on the space.
  spl <- strsplit(colname,split=' ')[[1]]
  # Paste everything together with the last word shortened to the first leterr with a dot.
  paste(spl[1:(length(spl)-1)],
        paste0(substring(spl[[length(spl)]],1,1),'.'))
}

# Shapiro-Wilk test.
shap <- function(df, sample_size=50, p=0.05) {
  
  sample_df <- dplyr::sample_n(df, sample_size)
  shapiro_df <- data.frame(Trait=character(), W=double(), pvalue=double(), signf=logical(), stringsAsFactors = FALSE)
  
  for (i in 2:length(colnames(df))) {
    res <- shapiro.test(sample_df[,i])
    shapiro_df[nrow(shapiro_df)+1, ] <- c(colnames(sample_df)[i], round(res[['statistic']][['W']],2), round(res[['p.value']],2), res[['p.value']] < p)
  }
  shapiro_df
}

# Compare different dataframes with each other. This is especially handy if you have more than two dataframes to compare.
compare <- function(func, dfs, dfnames, columns=NULL, formatting="html", has_return=FALSE, ...) {
  if (has_return) {
    out_list <- list()
  }
  for (i in 1:(length(dfs) - 1)) {
    for (j in (i+1):length(dfs)) {
      if (is.null(columns)) {
        output <- func(dfs[[i]], dfs[[j]], ...)
      } else {
        output <- func(dfs[[i]][,columns], dfs[[j]][,columns], ...)
      }
      if (disableKable) {
        print(output)
      } else {
        if ('signf' %in% colnames(output)) {
          output <- output %>% mutate(signf = cell_spec(signf, format=formatting, color=ifelse(signf == TRUE, "green", "red")))
        }
        caption <- paste(dfnames[[i]], 'vs', dfnames[[j]])
        if (has_return) {
          out_list[[caption]] <- output
        } else {
          print(output
                %>% kable(caption=caption, format=formatting, escape=FALSE)
                %>% kable_styling(bootstrap_options = c("striped", "condensed")))
        }
      }
    }
  }
  if (has_return) {
    out_list
  }
}

# Check how often a test is wrong (different signf found)
repeated <- function(func, n=1000, ...) {
  lngth <- length(list(...)[[1]]) - 1
  true_signf <- rep(0, lngth)
  false_signf <- rep(0, lngth)
  for (i in 1:n) {
    res <- func(...)
    signf <- res[['signf']]
    for (c in 1:length(signf)) {
      if (equals("TRUE",signf[c])) {
        true_signf[c] <- true_signf[c] + 1
      } else {
        false_signf[c] <- false_signf[c] + 1
      }
    }
  }
  comp_signf <- function(tr, fa) {
    if (tr > fa) {
      return("TRUE")
    }
    return("FALSE")
  } 
  
  print(paste('Summarized repeated test with', n, 'repeats.'))
  for (c in 1:length(true_signf)) {
    print(paste(c+1, comp_signf(true_signf[c], false_signf[c]), ':', max(true_signf[c], false_signf[c])))
  }
}

# Fisher test
# > Tests for the difference in variance in two sets. Note that the test is highly sensitive to outliers!
# F = variance of the groups (mean square between) / mean of the within group variances (mean squared error)
fisherf <- function(df1, df2, p=0.05) {
  
  fisherf_df <- data.frame(Trait=character(), 'F'=double(), pvalue=double(), signf=logical(), 
                           var_df1=double(), var_df2=double(), diff=double(), stringsAsFactors = FALSE)
  
  for (i in 2:length(colnames(df1))) {
    res <- var.test(df1[,i], df2[,i])
    fisherf_df[nrow(fisherf_df)+1, ] <- c(colnames(df1)[i], round(res[['statistic']],2), round(res[['p.value']],2),
                                          res[['p.value']] < p, round(var(df1[,i]),2), round(var(df2[,i]),2),
                                          round(var(df1[,i])-var(df2[,i]),2))
  }
  fisherf_df
}

# Wilcoxon signed-rank test.
# > Tests for the difference between two related variables; takes into account the magnitude and direction of difference
wilc <- function(df1, df2, paired, p=0.05, fix_column=TRUE, method="wilcoxon") {
  
  wilcox_df <- data.frame(Trait=character(), V=double(), p.value=double(), signf=logical(), r=double(), effect=character(), stringsAsFactors = FALSE)
  
  for (i in 2:length(colnames(df1))) {
    if (length(df2) < length(df1)) {
      i_df2 <- ((i-2) %% 5) + 2 # If df2 is gt.
    } else {
      i_df2 <- i
    }
    if (method == "wilcoxon") {
      res <- wilcox.test(df1[,i], df2[,i_df2], paired = paired)
      r <- rFromWilcox(res, nrow(df1))
      wilcox_df[nrow(wilcox_df)+1, ] <- c(
        fix_column_name(colnames(df1)[i],fix_column),
        round(res[['statistic']],2),
        round(res[['p.value']],2),
        res[['p.value']] < p,
        round(r,2),
        effsize_translation_r(r)
      )
    } else if (method == "wilcoxon-pratt") {
      res <- wilcoxsign_test(df1[,i] ~ df2[,i_df2], distribution="exact")
      r <- rFromWilcox(res, nrow(df1))
      wilcox_df[nrow(wilcox_df)+1, ] <- c(
        fix_column_name(colnames(df1)[i],fix_column),
        round(statistic(res),2),
        round(pvalue(res),2),
        pvalue(res) < p,
        round(r,2),
        effsize_translation_r(r)
      )
    } else {
      warning(paste('Unknown method specified:', method))
    }

  }
  colnames(wilcox_df)[[4]] <- paste0('p<',p)
  wilcox_df
}

rFromWilcox <- function(wilcoxModel, N){
  if (typeof(wilcoxModel) == 'S4') {
    p <- pvalue(wilcoxModel)
  } else {
    p <- wilcoxModel$p.value
  }
  z <- qnorm(p/2)
  r <- z / sqrt(N)
  r
}

effsize_translation_r <- function(r) {
  if (abs(r) < 0.1) return('negligible')
  if (abs(r) < 0.3) return('small')
  if (abs(r) < 0.5) return('medium')
  return('large')
}

effsize_translation_d <- function(d) {
  if (abs(d) < 0.2) return('negligible')
  if (abs(d) < 0.5) return('small')
  if (abs(d) < 0.8) return('medium')
  return('large')
}

# Student t-test (for normal distribution)
ttest <- function(df1, df2, p=0.05, fix_column=TRUE, paired=TRUE) {
  
  ttest_df <- data.frame(Trait=character(), V=double(), p.value=double(), signf=logical(), stringsAsFactors = FALSE)
  
  if (length(df2) < length(df1)) {
    i_df2 <- ((i-2) %% 5) + 2 # If df2 is gt.
  } else {
    i_df2 <- i
  }
  
  for (i in 2:length(colnames(df1))) {
    res <- t.test(df1[,i], df2[,i_df2], paired = paired, alternative='two.sided')
    ttest_df[nrow(ttest_df)+1, ] <- c(fix_column_name(colnames(df1)[i],fix_column),
                                      round(res[['statistic']],2),
                                      round(res[['p.value']],2),
                                      res[['p.value']] < p)
  }
  colnames(ttest_df)[[4]] <- paste0('p<',p)
  ttest_df
}

ttest_eff <- function(df1, df2, p=0.05, paired=TRUE) {
  ttest_df <- ttest(df1, df2, p=p, fix_column=TRUE, paired=paired)
  cohensd <- cohensd(df1, df2, paired=paired)[c(1,2,5)]
  join(ttest_df, cohensd, by="Trait")
}

ks_test <- function(df1, df2, p=0.05) {
  ks_df <- data.frame(Trait=character(), D=double(), p.value=double(), signf=logical(), stringsAsFactors = FALSE)
  
  if (length(df2) < length(df1)) {
    i_df2 <- ((i-2) %% 5) + 2 # If df2 is gt.
  } else {
    i_df2 <- i
  }
  
  for (i in 2:length(colnames(df1))) {
    res <- ks.test(df1[,i], df2[,i_df2])
    ks_df[nrow(ks_df)+1, ] <- c(fix_column_name(colnames(df1)[[i]]),
                                      round(res[['statistic']],2),
                                      round(res[['p.value']],2),
                                      res[['p.value']] < p)
  }
  colnames(ks_df)[[4]] <- paste0('p<',p)
  ks_df 
}

# Mann Whitney test (for non-normal distribution)
# > Tests if two related variables are different; ignores the magnitude of change-only takes into account direction

# TODO


# SIGN test
# > Tests if two related variables are different; ignores the magnitude of change-only takes into account direction
sign_test <- function(df1, df2, p=0.05) {
  
  sign_df <- data.frame(Trait=character(), S=double(), pvalue=double(), signf=logical(), stringsAsFactors = FALSE)
  
  for (i in 2:length(colnames(df1))) {
    res <- SIGN.test(df1[,i], df2[,i], paired = TRUE, alternative='two.sided')
    sign_df[nrow(sign_df)+1, ] <- c(colnames(df1)[i], round(res[['statistic']],2), round(res[['p.value']],2), res[['p.value']] < p)
  }
  sign_df
}

# Cohen's d
cohensd <- function(df1, df2, paired=TRUE, fix_column=TRUE) {
  cohensd <- data.frame(Trait=character(), d=double(), lower=double(), upper=double(), magnitude=character(), stringsAsFactors = FALSE)
  
  if (length(df2) < length(df1)) {
    i_df2 <- ((i-2) %% 5) + 2 # If df2 is gt.
  } else {
    i_df2 <- i
  }
  
  for (i in 2:length(colnames(df1))) {
    res <- cohen.d(df1[,i], df2[,i_df2], paired = paired)
    cohensd[nrow(cohensd)+1, ] <- c(fix_column_name(colnames(df1)[i],fix_column),
                                    round(res[['estimate']],2),
                                    round(res[["conf.int"]][["lower"]],2),
                                    round(res[["conf.int"]][["upper"]],2),
                                    effsize_translation_d(res[['estimate']]))
  }
  cohensd
}

# Chi-squared test with Yates' continuity correction.
# > Test if two are correlated (either positively or negatively). Null hypothesis: variable df1 is independent of df2.
chi_sq_test <- function(df1, df2, simulatep, p=0.05) {
  
  chi_sq_df <- data.frame(Trait=character(), X=double(), p.value=double(), signf=logical(), stringsAsFactors = FALSE)
  
  for (i in 2:length(colnames(df1))) {
    res <- chisq.test(df1[,i], df2[,i], simulate.p.value = simulatep)
    chi_sq_df[nrow(chi_sq_df)+1, ] <- c(colnames(df1)[i], round(res[['statistic']],2), round(res[['p.value']],2), res[['p.value']] < p)
  }
  chi_sq_df
}

levene <- function(df1, df2, p=0.05) {
  
  levene_df <- data.frame(Trait=character(), 'F(df1,df2)=value'=character(), pvalue=double(), signf=logical(), stringsAsFactors = FALSE)
  
  for (i in 2:length(colnames(df1))) {
    res <- leveneTest(df1[,i], df2[,i])
    print(colnames(df1)[i])
    # levene_df[nrow(levene_df)+1, ] <- c(colnames(df1)[i],
    #                                     paste0('F(',test[['Df']][[1]],',',test[['Df']][[2]],')=',round(test[['F value']][[1]],2)),
    #                                     round(test[['Pr(>F)']][[1]],2),
    #                                     test[['Pr(>F)']][[1]] < p)
  }
  levene_df
}

# Draw histograms
hist_all <- function(df1, df2) {
  for (i in 2:length(colnames(df1))) {
    col_name <- fix_column_name(colnames(df1)[[i]])
    his1 <- hist(df1[,i], plot=FALSE)
    his2 <- hist(df2[,i], plot=FALSE)

    plot(his1, col = adjustcolor(color$c1, alpha.f=0.5), main=col_name, ylim=c(0,ceiling(max(his1[['counts']],his2[['counts']]))))
    plot(his2, col = adjustcolor(color$c2, alpha.f=0.5), add=TRUE)

    legend(x="topright", y=0.92, legend=c('OG', 'CMP'), col=c(color$c1,color$c2), pch=c('-','-'), lwd=10)
  }
}

# Draw density plots
density_all <- function(df1, df2, name1='original', name2='CMP') {
  for (i in 2:length(colnames(df1))) {
    col_name <- fix_column_name(colnames(df1)[[i]])
    density1 <- density(df1[,i])
    density2 <- density(df2[,i])

    plot(density1, col = color$c1, xlab=col_name, main=paste('Density graph',col_name), ylim=c(0,ceiling(max(density1[['y']],density2[['y']]))))
    lines(density2, col = color$c2)
    legend(x="topright", y=0.92, legend=c(name1, name2), col=c(color$c1,color$c2), pch=c('-','-'), lwd=10)
  }
}

# Draw QQ-plots
draw_qqplots <- function(df1, df2) {
  for (i in 2:length(colnames(df1))) {
    op <- par(mfcol=c(1,2))
    col_name <- colnames(df1)[i]
    qqnorm(df1[,i], pch = 1, frame = FALSE, main=paste(col_name, 'OG'))
    qqline(df1[,i], col = "steelblue", lwd = 2)
    qqnorm(df2[,i], pch = 1, frame = FALSE, main=paste(col_name, 'CMP'))
    qqline(df2[,i], col = "steelblue", lwd = 2)
    par(op)
  }
}

# Plot line graph ordered on df1 per column.
plot_line_ordered <- function(df1, df2) {
  for (i in 2:length(colnames(df1))) {
    column <- i
    colname <- colnames(df1)[i]
    ordered_df1 <- df1[order(df1[,column]),]
    plot(df2[order(match(df2$id, ordered_df1$id)),][,column], main=paste(colname, 'CMP', sep=' '), ylab=colname)
    lines(ordered_df1[,column], main=paste(colname, 'OG', sep=' '), ylab=colname, col='green')
  }
}

# Plot line graph ordered on df1 per column.
ggplot_line_ordered <- function(df1, df2) {
  for (i in 2:length(colnames(df1))) {
    colname <- colnames(df1)[i]
    joined <- inner_join(df1[,c(1,i)],df2[,c(1,i)],by="id",suffix=c('.x','.y'))
    joined <- joined[order(joined[,2]),]
    row.names(joined) <- NULL
    joined$idx <- 1:nrow(joined)

    p <- ggplot() +
      geom_line(data=joined, aes(x=idx, y=joined[,3]), color="red") +
      geom_line(data=joined, aes(x=idx, y=joined[,2]), color="blue") +
      ylim(0.0, 1.0) +
      ylab(colname) +
      xlab("") +
      ggtitle(paste("Ordered scores for", colname)) +
      theme_bw() +
      theme(axis.text.x = element_blank())
    print(p)
  }
}

trait_boxplots <- function(df1, df2) {
  op <- par(no.readonly = TRUE) # Save old settings
  par(mar = c(8, 4.1, 4.1, 2.1))
  boxplot(df1[,-1], boxfill=NA, border=NA, las=2, cex.axis=0.7)
  title('Personality trait boxplots')
  boxplot(df1[,-1], add=TRUE, xaxt='n', yaxt='n', boxfill="red", boxwex=0.25, at=1:ncol(df1[,-1]) - 0.15)
  boxplot(df2[,-1], add=TRUE, xaxt='n', yaxt='n', boxfill="red", boxwex=0.25, at=1:ncol(df2[,-1]) + 0.15)
  par(op)  # Reset settings.
}

# Create a shiny latex table from a data.frame.
# Example usage 1: latex_table(summarize_df, caption='Summarization of the original dataset (df1) and the reference dataset (df2).', label='tab:data_sanitization/at/summarization', data_og, data_cmp)
# Example usage 2: latex_table(latex_table(wilc_df, caption='Wilcoxon signed-rank test of the original dataset and reference dataset.', label='tab:data_sanitization/at/wilc')
latex_table <- function(func, caption, label, include_index=FALSE, fix_column = NULL, transpose=FALSE, shorten_names=NULL, ...) {
  bold <- function(x) {paste('\\textbf{',x,'}', sep='')}
  if (is.null(func)) {
    warning("Given func type was NULL. No Latex table could be reported.")
    return()
  }
  if (typeof(func) == "closure") {
    df <- func(...)
  } else if (inherits(func, "data.frame")) {
    df <- func
  } else {
    stop(paste("Unknown func type given: ", typeof(func)))
  }
  if (transpose) {
    new_col_names <- df[,1]
    df <- as.data.frame(t(df[,-1]))
    colnames(df) <- new_col_names
  }
  
  # Fix column names if this is specified
  if (!is.null(fix_column)) {
    if (fix_column == 0) {
      rownames(df) <- lapply(rownames(df), fix_column_name)
    } else if (fix_column > 0 && fix_column <= length(df)) {
      df[,fix_column] <- lapply(df[,fix_column], fix_column_name)
    } else {
      warning('Unknown fix_column specified. Is the value a valid column in the data frame?')
    }
  }
  # Shorten column names if this is specified
  if (!is.null(shorten_names)) {
    if (shorten_names == 0) {
      rownames(df) <- lapply(rownames(df), shorten_column_name)
    } else if (shorten_names > 0 && shorten_names <= length(df)) {
      df[,shorten_names] <- unlist(lapply(df[,shorten_names], shorten_column_name))
    } else {
      warning('Unknown shorten_column specified. Is the value a valid column in the data frame?')
    }
  }
  # Sanitize the column names (i.e., convert to Latex)
  colnames(df) <- lapply(colnames(df), sanitize)
  
  # Add odd row colors.
  if (nrow(df) > 1) {
    rws <- seq(1, (nrow(df) - 1), by=2)
    col <- rep('\\rowcol ', length(rws))
  } else {
    rws <- c()
    col <- c()
  }
  # If there is no 'tab:' prefix, add this prefix.
  if (!startsWith(label, 'tab:')) {
    label <- paste0('tab:',label)
  }
  
  print(xtable(df, caption=caption, label=label), 
        booktabs=FALSE,
        add.to.row = list(pos = as.list(c(rws, -1, 0,nrow(df))),command = as.vector(c(col,'\\topline\n\\headcol ','\\midline\n','\\bottomrule\n'))),
        include.rownames=include_index, # remove indices before each row
        caption.placement = 'top', # caption above table
        sanitize.colnames = bold, # bold headers
        hline.after=NULL,       # remove default hlines
        latex.environments=NULL, # remove centering
        comment = FALSE # Remove the preceding comment.
      )
}

### Ground-truth comparison methods

# Get the trait id that is linked to the `trait_name`.
# E.g., `pi_conscientiousness` is linked to `Conscientiousness`, 
# so `2` or `3` is returned depending if the index should be excluded or included with the count, respectively.
get_trait_id <- function(trait_name, index) {
  lower_trait_name <- tolower(trait_name)
  for (t in 1:5) {
    trait <- tolower(traits[[t]])
    if (grepl(trait, lower_trait_name)) {
      if (index) {
        t <- t + 1
      }
      return(t)
    }
  }
  warning(paste("Unknown trait name found:",traitname))
}

# Get the absolute error
AE <- function(df, gt) {
  df <- df[df$id %in% gt$id,]
  df <- df[order(df$id),]
  gt <- gt[gt$id %in% df$id,]
  gt <- gt[order(gt$id),]
  gt <- dplyr::select_if(gt, is.numeric)
  row.names(df) <- NULL
  row.names(gt) <- NULL
  out <- data.frame(matrix(ncol=length(df),nrow=length(df[,1])), stringsAsFactors = FALSE)
  colnames(out) <- colnames(df)
  out[,1] <- df[,1]
  for (i in 2:length(df)) {
    if (length(gt) < length(df)) {
      out[,i] <- abs(df[,i] - gt[,get_trait_id(colnames(df)[[i]],index=TRUE)])
    } else {
      out[,i] <- abs(df[,i] - gt[,i])
    }
  }
  return(out)
}

# Get the Mean-Absolute-Error.
MAE <- function(df, gt, rounding=2) {
  ae <- AE(df, gt)
  out <- data.frame(matrix(ncol=length(df)-1,nrow=1))
  colnames(out) <- colnames(df)[2:length(df)]
  for (i in 2:length(ae)) {
    out_i <- i-1
    out[,out_i] <- mean(ae[,i])
    if (rounding >= 0) {
      out[,out_i] <- round(out[,out_i],rounding)
    }
  }
  return(out)
}

max_AE <- function(df, gt, rounding=2) {
  ae <- AE(df, gt)
  out <- data.frame(matrix(ncol=length(df)-1,nrow=1))
  colnames(out) <- colnames(df)[2:length(df)]
  for (i in 2:length(ae)) {
    out_i <- i-1
    out[,out_i] <- max(ae[,i])
    if (rounding >= 0) {
      out[,out_i] <- round(out[,out_i],rounding)
    }
  }
  return(out)
}

# Get the squared-error.
SE <- function(df, gt) {
  out <- AE(df, gt)
  for (i in 2:length(df)) {
    out[,i] <- out[,i]^2
  }
  return(out)
}

# Get the Mean-quared-error.
MSE <- function(df, gt) {
  se <- SE(df, gt)
  out <- data.frame(matrix(ncol=length(df)-1,nrow=1))
  colnames(out) <- colnames(df)[2:length(df)]
  for (i in 2:length(se)) {
    out[,i-1] <- mean(se[,i])
  }
  return(out)
}

# Get the Root-mean-squared-error, which is NRMSE (normalized) at the same time due to 0-1 scaling!
RMSE <- function(df, gt, rounding=2) {
  out <- MSE(df, gt)
  for (i in 1:length(out)) {
    out[,i] <- sqrt(out[,i])
    if (rounding >= 0) {
      out[,i] <- round(out[,i],rounding)
    }
  }
  return(out)
}

# Remove scaling. Rescale to zero mean.
zero_scale <- function(df) {
  if (is.data.frame(df)) {
    for (i in starting_index(df):length(df)) {
      df[,i] <- df[,i] - mean(df[,i])
    }
  } else {
    df <- df - mean(df)
  }
  df
}

# Get the correlation between the dataset and the ground-truth.
Correlation <- function(df, gt, method="pearson", rounding=2, signf=0.05) {
  df <- df[df$id %in% gt$id,]
  gt <- gt[gt$id %in% df$id,]
  gt <- gt[,1:6]
  gt <- gt[order(gt[,1]),]
  df <- df[order(match(df$id, gt$id)),]
  cnames <- c("trait","t", "cor", "confint", "p-value", "signf")
  out <- data.frame(matrix(ncol=length(cnames),nrow=length(df)-1))
  colnames(out) <- cnames
  out[,1] <- colnames(df)[-1]
  for (i in 1:(length(cols) - 1)) {
    trait_idx <- get_trait_id(colnames(df)[[i]],index=FALSE)
    trait <- traits[[trait_idx]]
    cor_out <- cor.test(x=df[,i], y=gt[,trait_idx + 1], method=method)
    out[i,2] <- round(cor_out[["statistic"]][["t"]],rounding)
    out[i,3] <- round(cor_out[["estimate"]][["cor"]],rounding)
    out[i,4] <- paste0('[',round(cor_out[["conf.int"]][[1]],rounding),',',round(cor_out[["conf.int"]][[2]],rounding),']')
    out[i,5] <- round(cor_out[["p.value"]],rounding)
    out[i,6] <- cor_out[["p.value"]] < signf
  }
  return(out)
}

# Plot metrics bars with traits and methods on the x-axis and the scores of the metric on the y-axis.
plot_metric_bars <- function(dM, cn, ylab, stds=NULL, feature='size', main="", show_legend=TRUE, legend_title=NULL) {
  dM <- melt(dM, measure.vars=cn, id.vars='size')
  do_std <- !is.null(stds) && nrow(stds) > 0
  if (do_std) {
    print(stds)
    stdsM <- melt(stds, measure.vars=cn, id.vars='size')
    colnames(stdsM) <- c('size', 'variable', 'std')
    dM <- merge(dM, stdsM, by=c('size', 'variable'))
  }
  methods <- vapply(strsplit(as.character(dM$variable)," "), `[`, 1, FUN.VALUE=character(1))
  methods <- methods[!duplicated(methods)]
  methods <- methods[order(factor(methods, levels=c('PI', 'Yarkoni', 'Golbeck')))]
  legend_title <- ifelse(is.null(legend_title), feature, legend_title)
  p <- ggplot(dM, aes(x=variable, fill=size)) +
    geom_bar(aes(y=value), stat='identity', position=position_dodge(), show.legend = show_legend) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size=8)) +
    ylab(ylab) + xlab('Trait') +
    geom_vline(xintercept = c(5.5, 10.5), linetype='dotted') +
    ggtitle(main) + guides(fill=guide_legend(title=legend_title))
  if (do_std) {
    p <- p + geom_errorbar(aes(ymin=value-std, ymax=value+std), width=.05, position=position_dodge(.9), alpha=0.4, size=1.1)
  }
  x <- 3
  for (method in methods) {
    p <- p + annotate("text",x=x,y=max(dM$value),label=method,alpha=.3)
    x <- x + 5
  }
  p
}

# Draw comparison plots with a comparison of all dfs in {dfs} with the names {dfnames}.
# For each df in {dfs} check against the ground-truth {gt} with measurement function {func}.
measure_plot <- function(dfs, dfnames, gt, func, std=0, feature='size', main="", truncate_columns=FALSE, size_in_legend=TRUE, ...) {
  func_dfs <- list()
  std_dfs <- data.frame(matrix(nrow=0,ncol=15), stringsAsFactors = FALSE)
  cn <- NULL
  i <- 1
  is_gt_list <- ifelse(inherits(gt, "list"), TRUE, FALSE)
  for (df_i in 1:length(dfs)) {
    df <- dfs[[df_i]]
    if (is_gt_list) {
      gt_ <- gt[[df_i]]
    } else {
      gt_ <- gt
    }
    func_df <- data.frame(func(df, gt_))
    cn <- colnames(func_df)
    if (size_in_legend) {
      size_var <- as.factor(paste0(dfnames[[i]],' (',nrow(gt_[gt_$id %in% df$id,]),')'))
    } else {
      size_var <- as.factor(dfnames[[i]])
    }
    func_df$size <- size_var
    func_dfs <- append(func_dfs, list(func_df))
    
    if (std > 0 && std <= 1) {
      ae <- AE(df, gt_)
      std_df <- data.frame(matrix(nrow=1,ncol=15), stringsAsFactors = FALSE)
      for (col in 1:length(std_df)) {
        std_col <- sd(ae[,col+1])
        std_df[[1,col]] <- round(std * std_col,2)
      }
      std_df$size <- size_var
      std_dfs <- rbind(std_dfs, std_df)
    }
    i <- i + 1
  }
  dM <- do.call(rbind, func_dfs)
  cn <- lapply(cn, fix_column_name)
  if (truncate_columns) {
    cn <- lapply(cn, shorten_column_name)
  }
  colnames(dM) <- c(cn, 'size')
  if (std > 0 && std <= 1) {
    colnames(std_dfs) <- c(cn, 'size')
    row.names(std_dfs) <- dfnames
    print(std_dfs)
    latex_table(std_dfs, transpose=TRUE, label='RQ3-std-AE-size',
                caption=paste0(std*100,'\\% Confidence interval of the absolute error for each method and trait for the different number of words fed to the analyses. The columns indicate the number of words given to the analyes (i.e, 100 words, 600 words, 1200 words, and 3000 or more words).'),
                include_index = TRUE)
  }
  plot_metric_bars(dM, cn, stds=std_dfs, ylab=deparse(substitute(func)), feature=feature, main=main, ...)
}

# Draw comparison plots with a comparison of one df {df} with the name {dfname}.
# For each ground-truth in {gts} with measurement function {func}.
measure_plot_gts <- function(df, dfnames, gts, func, feature='size', main="") {
  func_dfs <- list()
  cn <- NULL
  i <- 1
  for (gt in gts) {
    func_df <- data.frame(func(df, gt))
    cn <- colnames(func_df)
    func_df$size <- as.factor(paste0(dfnames[[i]],' (',nrow(gt[gt$id %in% df$id,]),')'))
    func_dfs <- append(func_dfs, list(func_df))
    i <- i + 1
  }
  dM <- do.call(rbind, func_dfs)
  cn <- lapply(cn, fix_column_name)
  colnames(dM) <- c(cn, 'size')
  plot_metric_bars(dM, cn, ylab=deparse(substitute(func)), feature=feature, main=main)
}

# Inspect the dataframes compared to the ground-truth based on their RMSE and MAE values for all rows.
inspect_gt <- function(df1, df2, dfnames, gt, columns=NULL) {
  ae <- AE(df=df1, gt=df2)

  df1 <- df1[df1$id %in% ae$id,]
  df2 <- df2[df2$id %in% ae$id,]
  
  print(nrow(df1[df1$id %in% gt$id,]))

  out <- cbind(c(paste(dfnames[[1]],'RMSE'),paste(dfnames[[2]],'RMSE'),
                 paste(dfnames[[1]],'MAE'),paste(dfnames[[2]],'MAE')),
               rbind(RMSE(df1,gt),RMSE(df2,gt),
                     MAE(df1,gt),MAE(df2,gt)))
  colnames(out)[1] <- 'Df'
  if (!is.null(columns)) {
    out <- out[,columns]
  }
  out
}

# Add a column indicating the number of affected people with a ground-truth.
add_people_column <- function(ae, gt, out) {
  people_df <- c('people')
  for (i in 2:length(ae)) {
    people_df <- c(people_df, nrow(ae[(ae[,i] > 0) & (ae$id %in% gt$id),]))
  }
  out[,1] <- as.character(out[,1])
  out[nrow(out) + 1,] <- people_df
  out[,1] <- as.factor(out[,1])
  row.names(out) <- NULL
  out
}

# Inspect the dataframes compared to the ground-truth based on their RMSE and MAE values where the rows are non-null.
inspect_gt_non_zero <- function(df1, df2, dfnames, gt, people=TRUE, columns=NULL) {
  ae <- AE(df=df1, gt=df2)
  row_sub = apply(ae, 1, function(row) any(row[2:length(ae)] != 0))
  non_zero_rows <- ae[row_sub,]
  
  df1 <- df1[df1$id %in% non_zero_rows$id,]
  df2 <- df2[df2$id %in% non_zero_rows$id,]

  out <- cbind(c(paste(dfnames[[1]],'RMSE'),paste(dfnames[[2]],'RMSE'),
                 paste(dfnames[[1]],'MAE'),paste(dfnames[[2]],'MAE')),
               rbind(RMSE(df1,gt),RMSE(df2,gt),
                     MAE(df1,gt),MAE(df2,gt)))
  
  cat(paste("People with gt in df:", nrow(df1[df1$id %in% gt$id,]),'\n'))
  if (people) {
    out <- add_people_column(ae, gt, out)
  }
  
  colnames(out)[1] <- 'Df'
  if (!is.null(columns)) {
    out <- out[,columns]
  }
  out
}

# Inspect the dataframes compared to the ground-truth based on their RMSE and MAE values for each column individually.
compare_metrics <- function(df1, df2, dfnames, gt, people=TRUE) {
  ae <- AE(df=df1, gt=df2)
  total_out <- NULL
  for (i in 2:length(ae)) {
    row_sub = apply(ae, 1, function(row) any(row[i] != 0))
    non_zero_rows <- ae[row_sub,]
    row.names(non_zero_rows) <- NULL
    df1_sub <- df1[df1$id %in% non_zero_rows$id,c(1,i)]
    df2_sub <- df2[df2$id %in% non_zero_rows$id,c(1,i)]
    
    out <- cbind(c(paste(dfnames[[1]],'RMSE'),paste(dfnames[[2]],'RMSE'),
                   paste(dfnames[[1]],'MAE'),paste(dfnames[[2]],'MAE')),
                 rbind(RMSE(df1_sub,gt),RMSE(df2_sub,gt),
                       MAE(df1_sub,gt),MAE(df2_sub,gt)))
    colnames(out)[1] <- 'Df'
    if (is.null(total_out)) {
      total_out <- out
    } else {
      total_out <- inner_join(total_out, out, by="Df")
    }
  }
  if (people) {
    total_out <- add_people_column(ae, gt, total_out)
  }
  total_out
}

# Inspect the absolute differences of both dataframes.
inspect_AE_differences <- function(df1, df2, error_margin=0) {
  if (error_margin > 0.5) {
    warning('Are you sure you want to accept an error above 0.5?')
  }
  
  ae <- AE(df=df1, gt=df2)
  row_sub = apply(ae, 1, function(row) any(row[2:length(ae)] > error_margin))
  non_zero_rows <- ae[row_sub,]
  non_zero_rows[,1:length(non_zero_rows)] <- sapply(non_zero_rows[,1:length(non_zero_rows)], as.numeric)
  # Get the summed differences of each trait.
  non_zero_rows_sum <- round(non_zero_rows %>% summarize_if(is.numeric, sum, na.rm=TRUE),2)
  non_zero_rows_sum[1,1] <- 'Summed diff.'
  # Get the maximum differences for each trait.
  non_zero_rows_max <- round(non_zero_rows %>% summarize_if(is.numeric, max, na.rm=TRUE),2)
  non_zero_rows_max[1,1] <- 'Max diff.'
  # Get the mean differences for each trait.
  non_zero_rows_mean <- round(non_zero_rows %>% summarize_if(is.numeric, mean, na.rm=TRUE),2)
  non_zero_rows_mean[1,1] <- 'Mean diff.'
  # Get the number of people with differences for each trait.
  people_involved <- function(x) {
    sum(x > 0, na.rm = TRUE)
  }
  non_zero_rows_count <- sapply(X=ae, FUN=people_involved)
  non_zero_rows_count[1] <- 'People'
  
  
  merged <- rbind(non_zero_rows_sum, non_zero_rows_max, non_zero_rows_mean, non_zero_rows_count)
  colnames(merged)[1] <- 'Metric'
  transposed <- as.data.frame(t(merged[,-1]))
  colnames(transposed) <- merged$Metric
  transposed
}

# Check which dataframe has mores times less errors.
whos_better <- function(df1, df2, gt, dfnames, allowed_error=0) {
  ae1 <- AE(df=df1, gt=gt)
  ae2 <- AE(df=df2, gt=gt)
  
  merged <- inner_join(ae1, ae2, by='id', suffix=c('.x','.y'))
  better1 <- c()
  better2 <- c()
  draws <- c()
  next_col_i <- (length(merged) - 1)/2
  for (i in 2:length(ae1)) {
    right1 <- nrow(merged[merged[,i] + allowed_error < merged[,i + next_col_i],])
    right2 <- nrow(merged[merged[,i] > merged[,i + next_col_i] + allowed_error,])
    equal <- nrow(ae1) - right1 - right2
    better1 <- c(better1, right1)
    better2 <- c(better2, right2)
    draws <- c(draws, equal)
    best_df <- ifelse(right1 > right2, dfnames[[1]], dfnames[[2]])
    best_df <- ifelse((right1 == right2) && (right1 == 0), '-', best_df)
    print(paste(right1,right2,equal,'Best dataframe:', best_df))
  }
  df <- data.frame(matrix(better1, ncol=length(better1), byrow=TRUE))
  df <- rbind(df, better2, draws)
  df <- cbind(c(dfnames[[1]], dfnames[[2]], 'draws'), df)
  colnames(df) <- c('Result',lapply(colnames(ae1)[-1],FUN = fix_column_name))
  df$Result <- factor(df$Result, levels=c(dfnames[[1]], 'draws', dfnames[[2]]))
  df.m <- melt(df, id.vars='Result')
  ggplot(df.m, aes(x=variable, y=value, fill=Result)) + 
    geom_bar(stat='identity') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.title.x=element_blank()) +
    ylab('No. times right') + xlab('') + ggtitle('Bar plots showing which data frame was right how many times.')
}

# Get the starting index considering there might be an 'indexing' column. If the first column is an indexing column,
# The starting column is 2. Otherwise the starting column is 1.
starting_index <- function(df) {
  first_col <- tolower(colnames(df)[[1]])
  if (first_col %in% c('id', 'index', 'idx')) {
    return(2)
  }
  1
}

# Get the index of the feature or return the feature if already is an index.
get_feature_idx <- function(df, feature) {
  if (is.numeric(feature)) {
    feature_idx <- feature
  } else {
    feature_idx <- which(c(colnames(df)) == feature)
  }
}

# Get the columns for which there is difference in the rows specified.
get_diff_cols <- function(df, rows=list(c(1,2), c(3,4))) {
  if (typeof(rows) != 'list') {
    warning('Please specify rows as a list of vectors of size 2.')
  }
  differences <- c(1)
  for (vec in rows) {
    for (i in 2:length(df)) {
      if (df[vec[[1]],i] != df[vec[[2]],i]) {
        differences <- c(differences, i)
      }
    }
  }
  return(sort(unique(differences)))
}

# Subset the columns of a dataframe and return the dataframe with the subsetted columns.
subset_cols <- function(df, cols) {
  df <- df[,cols]
  row.names(df) <- NULL
  df
}

# Reorder a list to get a column and row ordering for ggarrange.
column_order <- function(l, rows=5, cols=3) {
  if (rows*cols > length(l)) {
    stop("Number of rows and cols is not feasible with provided list.")
  }
  out <- list()
  for (row in 1:rows) {
    for (col in 1:cols) {
      idx <- (col-1)*rows + row
      out[[length(out)+1]] <- l[[idx]]
    }
  }
  out
}
