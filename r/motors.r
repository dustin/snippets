s50303s <- read.csv("/tmp/x/ss2204_5030_3s_2.log", header=F)

names(s50303s) <- c('cycle', 'pwm', 'rpm', 'grams', 'voltage', 'current')

s50303s$test <- 'ss2204 5030 3s'

combined <- rbind(s50303s, s50453s, s60303s, s60304s)

combined$test <- factor(combined$test)

combined <- combined[order(combined$test, combined$cycle),]

ggplot(combined, aes(grams, current, color=current)) +
    geom_line() + stat_smooth() + facet_wrap(~test, scales='free_x') +
    scale_color_gradient(low='black', high='red')

