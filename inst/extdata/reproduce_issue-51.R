# Code to reproduce the following error
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
