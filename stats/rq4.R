library(countrycode)

boxplots_rq4 <- function() {
  for (i in 3:length(colnames(EPI))) {
    colname <- colnames(EPI)[[i]]
    EPI.data <- melt(EPI, id.vars = 'EPI', measure.vars = i:i)
    q <- quantile(EPI.data$value, c(0.1, 0.9))
    p <- ggplot(EPI.data, aes(EPI, value))
    p <- p + geom_boxplot(outlier.shape=NA, aes(fill=EPI)) + ggtitle(colname) + scale_y_continuous(limit=q) + coord_cartesian(ylim = q)
    print(p)
  }
}

density_rq4 <- function() {
  for (i in 3:length(colnames(EPI))) {
    colname <- colnames(EPI)[[i]]
    EPI.data <- melt(EPI, id.vars = 'EPI', measure.vars = i:i)
    p <- ggplot(EPI.data, aes(x=value, fill=EPI))
    p <- p + geom_density(alpha=0.25) + ggtitle(colname)
    print(p)
  }
}

draw_qqplots_rq4 <- function(dfs, dfnames) {
  for (i in 1:length(dfs)) {
    df <- dfs[[i]]
    for (column in 2:length(df)) {
      qqnorm(df[,column], pch=1, frame=FALSE, main = paste0(dfnames[[i]],": ",colnames(df)[[column]]))
      qqline(df[,column], col="steelblue", lwd=2)
    }
  }
}

repeated_shap_rq4_epi <- function() {
  for (epi in epi_order) {
    print(paste0(epi, ':'))
    selection <- EPI[EPI[, 'EPI'] == epi,2:length(colnames(EPI))]
    repeated(shap, n=1000, df=selection, sample_size=50, p=0.05)
  }
}

repeated_shap_rq4_epi <- function() {
  for (epi in epi_order) {
    print(paste0(epi, ':'))
    selection <- EPI[EPI[, 'EPI'] == epi,2:length(colnames(EPI))]
    repeated(shap, n=1000, df=selection, sample_size=50, p=0.05)
  }
}

repeated_shap_rq4 <- function(df, gt) {
  df <- df[df$id %in% gt$id,]
  row.names(df) <- NULL
  repeated(shap, n=1000, df=df, sample_size=50, p=0.05)
}

variances_rq4 <- function() {
  l <- list()
  for (column in 3:length(EPI)) {
    subl <- c()
    for (epi in epi_order) {
      selection <- EPI[EPI[, 'EPI'] == epi,column]
      subl <- c(subl, var(selection))
    }
    l[[column - 2]] <- subl
  }
  plot(0,type='n', xlim=c(1,length(epi_order)), ylim=c(min(unlist(l)), max(unlist(l))), xaxt='n', ylab='Variance', xlab='EPI')
  colors <- c(color$c1, color$c2, color$c3)
  for(i in 1:length(l)) {
    lines(l[[i]], col=colors[[ceiling(i / 5)]])
  }
  axis(1, at=1:length(epi_order), labels=epi_order)
  legend(4.3, 0.022, legend=c('PI','Yarkoni','Golbeck'), col=colors, pch='-', lwd = 3)
}

get_seperate_lists <- function(df, column_id, levels) {
  l <- list()
  for (level in 1:length(levels)) {
    l[[level]] <- data.frame(EPI) %>% filter(EPI == levels[[level]]) %>% select(-c(EPI))
  }
  return(l)
}

