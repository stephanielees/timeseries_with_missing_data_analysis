#The data is from the Bogotá Air Quality Monitoring Network 
#http://201.245.192.252:81/home/map 

#To download a dataset, this is the link 
#http://201.245.192.252:81/Report/stationreport

#the link to video demo: https://youtu.be/U9vWDmP82WE


library(openxlsx)
library(dplyr)
library(plotly)
library(dlm)


usm <- read.xlsx("StationsReport_2023814115512.xlsx", startRow = 4,
                 detectDates = T, rows = c(4, 6:(6+8759)))
DateTime <- usm[,1]
DateTime <- as.POSIXct(DateTime, format = '%d-%m-%Y %H:%M')

usm <- as.list.data.frame(usm[,-1])
usm <- lapply(usm, as.numeric)
usm <- as.data.frame(usm)
usm <- cbind(DateTime, usm)
usm <- usm[, -14]


fig <- plot_ly(usm, x = ~DateTime) %>%
    add_lines(y = usm$PM2.5, name = 'PM 2.5',
              line = list(color = '#008B00')) %>%
    layout(yaxis = list(title = TeX("\\mu g/m^{3}")))
fig


X = replace(usm$PM10, is.na(usm$PM10), 0)
model = dlmModPoly(1) + dlmModTrig(12) + dlmModReg(X, addInt = F)
model$FF
model_filter = dlmFilter(usm$PM2.5, model)

#MAE (excluding missing values)
mean(abs(model_filter$y - model_filter$f), na.rm = T)


fig <- plot_ly(usm, x = ~DateTime) %>%
    add_markers(y = ~PM2.5, name = 'data', 
                marker = list(color = 'gray')) %>%
    add_lines(y = abs(model_filter$f), 
              name = 'mean of forecast distribution', 
              line = list(color = '#FFB90F'))
fig

#combine the raw and imputed data
make_data <- function(raw, imp){
    combined <- c()
    for (i in 1:length(raw)){
        raw_dat = raw[i]
        imp_dat = imp[i]
        
        if (is.na(raw_dat)) {
            combined <- append(combined, imp_dat)
        }
        else {
            combined <- append(combined, raw_dat)
        }
    }
    combined
}

final_data <- make_data(usm$PM2.5, abs(model_filter$f))

