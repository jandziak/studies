library (ROCR)

ggplot(gbmFit3)
ggplot(rfFit)
ggplot(knnFit_best)


trellis.par.set(caretTheme())
plot(gbmFit3, plotType = "level",
     scales = list(x = list(rot = 90)))

resamps <- resamples(list(GBM = gbmFit3,
                          RF = rfFit,
                          KNN = knnFit,
                          GLM = glmFit_best))
resamps
theme1 <- trellis.par.get()
summary(resamps)


trellis.par.set(theme1)
bwplot(resamps, layout = c(3, 1))



splom(resamps)

difValues <- diff(resamps)
difValues 

trellis.par.set(theme1)
bwplot(difValues, layout = c(3, 1))

summary(difValues)


