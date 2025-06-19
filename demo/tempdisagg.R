require(tempdisagg)

# Suppose we have an annual series and want to create quarterly values that sum
# up to the annual values. Let us explore the annual sales of the pharmaceutical
# and chemical industry in Switzerland, from which we want to create a quarterly
# series.

data(swisspharma)
plot(sales.a)

# The most simple method is \code{denton-cholette} without an indicator. It
# performs a simple interpolation that meets the temporal additivity constraint.
# In R, this can be done the following way:

m1 <- td(sales.a ~ 1, to = "quarterly", method = "denton-cholette")

# td() produces an object of class "td". The formula, 'sales.a ~ 1', indicates
# that our low frequency variable will be disaggregated with a constant. The
# resulting quarterly values of sales can be extracted with the 'predict'
# function:

predict(m1)

# As there is no additional information on quarterly movements, the resulting
# series is very smooth:

plot(predict(m1))

# While this purely mathematical approach is easy to perform and does not need
# any other data series, the economic value of the resulting series may be
# limited. There might be a related quarterly series that follows a similar
# movement than sales. For example, we may use quarterly exports of
# pharmaceutical and chemical products:

plot(exports.q)
m2 <- td(sales.a ~ 0 + exports.q, method = "denton-cholette")

# Because we cannot use more than one indicator with the 'denton-cholette' or
# 'denton' method, the intercept must be specified as missing in the formula
# (0). Contrary to the first example, the 'to' argument is redundant, because
# the destination frequency can be interfered from the time series properties of
# 'exports.q'.

# The resulting model leads to a much more interesting series:

plot(predict(m2))

# As the indicator series is longer than the annual series, there is an
# extrapolation period, in which the quarterly sales are forecasted.

# With an indicator, the 'denton-cholette' method simply transfers the movement
# of the indicator to the resulting series. Even if in fact there were no
# correlation between the two series, there would be a strong similarity between
# the indicator and the resulting series.

# In contrast, regression based methods transfer the movement only if the
# indicator series and the resulting series are actually correlated on the
# annual level. For example, a Chow-Lin regression of the same problem as above
# can be performed the following way:

m3 <- td(sales.a ~ exports.q)

# As 'chow-lin-maxlog' is the default method, it does not need to be specified.
# Like with the corresponding 'lm' method, summary() produces an overview of the
# regression:

summary(m3)

# There is indeed a strong correlation between exports and sales, as it has been
# assumed in the example above. The coefficient of 'exports.q' is highly
# significant, and the very high adjusted R-squared points to a strong
# relationship between the two variables. The coefficients are the result of a
# GLS regression between the annual series.

# The estimation of the AR1 parameter, Rho, was estimated to be negative; in
# order to avoid the undesirable side-effects of a negative Rho, it has been
# truncated to 0. This feature can be turned off:

td(sales.a ~ exports.q, truncated.rho = -1)

# Again, we can extract the resulting quarterly series of sales:
plot(predict(m3))

# Like all regression based methods, 'chow-lin-maxlog' can also be used with
# more than one indicator series ('imports.q' is only significant on a 10% level
# in the following example, it probably will not help to produce a more accurate
# temporal disaggregation):

m4 <- td(formula = sales.a ~ exports.q + imports.q)
summary(m4)

# In our example, we actually know the true data on quarterly sales, so we can
# compare the artificial values to the true values:

plot(sales.q)
lines(predict(m2), col = "blue") # Denton-Cholette
lines(predict(m3), col = "red") # Chow-Lin

# With an indicator series, both the Denton method and Chow-Lin produce a series
# that is close to the true series. This is, of course, due to fact that in this
# example, exports are a good indicator for sales. If the indicator is less
# close to the series of interest, the resulting series will be less close to
# the true series.
