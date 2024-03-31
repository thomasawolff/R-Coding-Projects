

skew(housing$beds)

ExploratoryAnalysis(housing,housing$beds)

scatterplot(housing,housing$sqft,housing$price)

histogram(housing,housing$sqft,housing$price,55)

lmResidualsPlot(model,residuals(model),predict(model))

plotPointsResiduals(housing,housing$year_built,housing$price_per_sqft,residuals(model),predict(model))
plotPointsResiduals(housing,housing$year_built,housing$sqft,residuals(model),predict(model))

modelInteractions(model)

lmPerformance(model_multiplicative,proposed_model,housing_cleaned$price_per_sqft)