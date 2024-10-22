  
  BASE_DIR = "./StableForecasting/"
  DATA_DIR = file.path(BASE_DIR, "datasets")

  input_file_names = list.files(DATA_DIR)

coef_var = list()
all_sdevs = list()

#  input_file_name = "favorita_dataset.txt"
for(input_file_name in input_file_names)
{
  dataset <- readLines(file.path(DATA_DIR, input_file_name))
  dataset <- strsplit(dataset, ",")

sdevs = rep(NA, length(dataset))
means = rep(NA, length(dataset))

#i = 1
for(i in 1:length(dataset)){
    time_series <- as.numeric(dataset[[i]])
    sdevs[i] = sd(time_series)
    means[i] = mean(time_series)
    #hist(time_series)
}

coef_var[[input_file_name]] = sdevs/means
all_sdevs[[input_file_name]] = sdevs
}

#coef_var

boxplot(coef_var, col="lightblue")
#summary(sdevs)

summary_sdevs = do.call(rbind, lapply(all_sdevs, summary))


library(xtable)

xtable(summary_sdevs)
