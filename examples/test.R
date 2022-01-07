setwd("examples")

library("readxl")
df = read_excel("AD.xlsx", sheet = "CVDII")

library(caret)
k = ncol(df)
hpc = preProcess(df[,-k], method = c("knnImpute", "center", "scale"))
transformed = predict(hpc, newdata = df[,-k])
transformed$resp = df$resp

outcome = "resp"
features_raw = colnames(transformed)
features = features_raw[!features_raw %in% outcome]

library("muSignAl")
set.seed(200)
sign = muSignAl(transformed, features, outcome, 100, 0.9)

library("writexl")
write_xlsx(sign, "sign_cvd2.xlsx")
