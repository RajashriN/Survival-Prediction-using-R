new <- read.csv(file.choose())
library(ggplot2)
ggplot(new,aes(meal.cal,wt.loss))+geom_point()
ggplot(new,aes(meal.cal,wt.loss))+geom_abline(aes(intercept = 0,slope = 1))
##meal.cal and weight loss has linear relationship

ggplot(new,aes(sex,ph.ecog))+geom_count()
##ph.ecog has 0-3 levels ,sex has 2 levels 1 & 2 ,plot shows more counts for ph.ecog level 1 for both 1&2 levels
## for sex - 1 level 3 is lower & sex -2 no count under level 3
ggplot(new,aes(sex,ph.karno))+geom_jitter()

ggplot(new,aes(ph.karno))+geom_density(kernel = "gaussian")
##density plot shows that the data having ph.karno is high for value 90

ggplot(new) + geom_density(aes(x = pat.karno, color = time)) + facet_wrap(~inst)
##shows null value ,using imputed data

ggplot(classif) + geom_density(aes(x = pat.karno, color = time)) + facet_wrap(~inst)
##visuals show high pat.karno for inst 33

ggplot(new) + geom_point(aes(x = pat.karno, y = time)) + facet_wrap(~inst)
##visuals show inst 1 has more data points with pat.karno 100 and inst 33 has fewer data points

ggplot(new) + geom_point(aes(x = pat.karno, y = time)) + facet_wrap(~sex)

ggplot(classif)+geom_jitter(aes(x = sex , y = meal.cal))+facet_grid(~sex)
##gender 1 shows few data points having high calorie meal,gender 2 has inly 1 point having high calorific meal
ggplot(classif)+geom_smooth(aes(x = ph.karno , y = pat.karno),method = "lm")+facet_grid(~sex)

ggplot(classif) + geom_density(aes(x = pat.karno, color = time)) + facet_wrap(~ph.karno)

hist(classif$time)
##more data for poin < 400 in time

hist(classif$status)
