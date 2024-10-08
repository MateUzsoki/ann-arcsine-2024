
source("DALC.R")
source("A_Sz.R")
library(BSDA)
Z_yearly = NULL

ANNmethod = "ANN-AS-S2500T126K-SIGM"

Z_yearly = get_detailed_simulation_results_for_method("arcsine",Z_yearly,FALSE)

Z_yearly = get_detailed_simulation_results_for_method(ANNmethod,Z_yearly,FALSE)

#Z_yearly = Z_yearly[Z_yearly$year > 1998,]
Z_yearly = Z_yearly[Z_yearly$year > 2003,]
Z_yearly["ZAE"] = abs(Z_yearly$Z)

arcsine = Z_yearly %>%     filter(str_detect(method,"arcsine"))
ANN26TF = Z_yearly %>%     filter(str_detect(method,ANNmethod))

#t.test(Z ~ method, data = Z_yearly, paired = FALSE)
#t.test(Z ~ method, data = Z_yearly, paired = FALSE, alternative = 'greater')
#t.test(Z ~ method, data = Z_yearly, paired = TRUE)


#t.test(ZAE ~ method, data = Z_yearly, paired = FALSE )
#t.test(ZAE ~ method, data = Z_yearly, paired = FALSE , alternative = 'less')

# EZ KELL és az eredmény legyen 0.05 alatt (p érték)
t.test(ZAE ~ method, data = Z_yearly, paired = TRUE)

#z.test(ANN26TF$Z, arcsine$Z, alternative= "greater", sigma.x = sd(ANN26TF$Z), sigma.y = sd(arcsine$Z),  conf.level=0.95)

#z.test(ANN26TF$ZAE, arcsine$ZAE, alternative= "less", sigma.x = sd(ANN26TF$ZAE), sigma.y = sd(arcsine$ZAE),  conf.level=0.95)

#Z_yearly2 = Z_yearly[Z_yearly$ZAE< 0.0005,]
#Z_yearly2$method =
#ggplot(Z_yearly2, aes(method, Z)) +   geom_boxplot()


#x = rnorm(500)
# y = rnorm(500,2)
# t.test(x,y)

