# Code to reproduce https://github.com/KWB-R/fhpredict/issues/51
#
# Fehler: Constant variable(s) found: log_e.coli
# Zusätzlich: Warnmeldungen:
# 1: attempting model selection on an essentially perfect fit is nonsense
# 2: In summary.lm(model) :

name <- "spot-data_user-9_spot-41_2020-02-04.RData"
file <- system.file("extdata/testdata", name, package = "fhpredict")
spot_data <- kwb.utils::loadObject(file, "spot_data")
set.seed(1)
result <- fhpredict:::build_and_validate_model(spot_data)

spot_data$hygiene_spot41$e.coli <- spot_data$hygiene_spot41$e.coli + rnorm(nrow(spot_data$hygiene_spot41), 0, 5)

# Code to reproduce https://github.com/KWB-R/fhpredict/issues/52
#
# Fehler in lmtest::bptest(model) :
# the auxiliary variance regression requires at least an intercept and a regressor
# Zusätzlich: Warnmeldungen:
# 1: attempting model selection on an essentially perfect fit is nonsense

name <- "spot-data_user-9_spot-26_2020-02-04.RData"
file <- system.file("extdata/testdata", name, package = "fhpredict")
spot_data <- kwb.utils::loadObject(file, "spot_data")
set.seed(1)
result <- fhpredict:::build_and_validate_model(spot_data)

# Code to reproduce https://github.com/KWB-R/fhpredict/issues/53
#
# Could not create a valid model!

name <- "spot-data_user-9_spot-31_2020-02-04.RData"
file <- system.file("extdata/testdata", name, package = "fhpredict")
spot_data <- kwb.utils::loadObject(file, "spot_data")
set.seed(1)
result <- fhpredict:::build_and_validate_model(spot_data)
