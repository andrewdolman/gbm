# Understanding boosting
library(gbm)
library(ggplot2)
library(dplyr)

# Data

make_data <- function(n){
  fun <- function(x){
    sin(x) + rnorm(length(x), 0, 0.5)
  }
  
  x = seq(0, 10, length.out = n)
  dat <- data.frame(x = x, 
                    y = fun(x))
  plot(y ~ x, data = dat)
  lines(x, sin(x), col = "Red")
  
  
  # Define test and training sets
  
  dat$Set <- FALSE
  dat[sample(1:nrow(dat), nrow(dat) / 5, replace = FALSE), "Set"] <- "Test"
  dat$Set <- factor(ifelse(dat$Set == "Test", "Test", "Train"), ordered = TRUE,
                    levels = c("Train", "Test"))  
  dat <- dat[(order(dat$Set)),]
  return(dat)
}

dat <- make_data(n = 1000) %>% 
  tbl_df()

dat$x2 <- factor("A")

#dat <- sample_n(dat, nrow(dat))
gbm1 <- gbm(y ~ x + x2, shrinkage = 0.05, n.trees = 10000, cv.folds = 10, data = dat)

best_iter <- gbm.perf(gbm1)

newdat <- expand.grid(x = dat$x, Set = levels(dat$Set))
newdat$x2 <- factor("A")
newdat$True <- sin(newdat$x)
newdat$pred <- predict(gbm1, newdata = newdat, n.trees = best_iter)
newdat$pred2 <- predict(gbm1, newdata = newdat, n.trees = 10000)

p <- dat  %>% 
  ggplot(aes(y = y, x = x)) %>%
  + geom_point(alpha = 0.25) %>%
  + geom_line(data = newdat, aes(y = True), colour = "Blue") %>%
  + geom_line(data = newdat, aes(y = pred), colour = "Red") %>%
  + geom_line(data = newdat, aes(y = pred2), colour = "Green") %>%
  + facet_wrap(~Set) %>% 
  + theme_bw()
p

hist((sapply(gbm1$cv.models, function(x) gbm.perf(x, plot.it = F))))
newdat$pred3 <- rowMeans(sapply(gbm1$cv.models, function(x) predict(x, newdata = newdat[c(1,3)], n.trees = best_iter)))

p <- dat  %>% 
  ggplot(aes(y = y, x = x)) %>%
  + geom_point(alpha = 0.25) %>%
  + geom_line(data = newdat, aes(y = True), colour = "Blue") %>%
  + geom_line(data = newdat, aes(y = pred), colour = "Red") %>%
  + geom_line(data = newdat, aes(y = pred3), colour = "Orange") %>%
  + facet_wrap(~Set) %>% 
  + theme_bw()
p



lst <- data.frame(t(plyr::ldply(gbm1$cv.models, function(x) (predict(x, newdata = newdat[c(1,3)], n.trees = best_iter))))) %>% 
  cbind(newdat, .) %>% 
  tidyr::gather(cv_fold, value, -x, -x2, -Set, -True, -pred, -pred2, -pred3)

p <- dat  %>% 
  ggplot(aes(y = y, x = x)) %>%
  + geom_point(alpha = 0.25) %>%
  + geom_line(data = lst, aes(y = value, colour = cv_fold)) %>%
  + geom_line(data = newdat, aes(y = pred3), colour = "Orange", lwd = 1) %>%
  + facet_wrap(~Set) %>% 
  + theme_bw()
p

 
