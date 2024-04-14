install.packages("rjson")

library(rjson)
json_str <- toJSON(accuracy_measures)
write(json_str, file = "accuracy_measures.json")