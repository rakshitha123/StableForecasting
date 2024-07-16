
calculateWeights = function(totalSols) { # nolint
    js = vector()

    for(k in 1:totalSols) {
        js[k] = 1/(sum(1/(1:k)))
    }

    wj = js/sum(js)

    wj
}

compositeRanks = function(solData, objData){
    
    if(nrow(solData)==1) return(1)

    solRanks = apply(solData, 2, rank)
    objRanks = as.numeric(objData)

    solWeights = calculateWeights(nrow(solData))
    objWeights = calculateWeights(ncol(solData))

    #TODO: This is currently not working for average ranks, 
    #need to do the same as what I do below for the objectives then
    rankedSolWeights = apply(solRanks, 2, function(x) {solWeights[x]})
    #rankedObjWeights = objWeights[objRanks]

    rankedCompWeights <- matrix(NA, nrow = nrow(rankedSolWeights), ncol = ncol(rankedSolWeights))

    rankedObjWeights = vector()

    for(i in 1:length(objRanks)) {
        
        f = floor(objRanks[i])
        c = ceiling(objRanks[i])
        if(f==c) 
            rankedObjWeights[i] = objWeights[f]
        else 
            rankedObjWeights[i] = (objWeights[f]+objWeights[c])/2

        rankedCompWeights[,i] = rankedSolWeights[,i]*rankedObjWeights[i]  
    }

    compositeWeights = rowSums(rankedCompWeights)

    rank(-compositeWeights)
}


# #---------------------------------------------------
# # Example
# #---------------------------------------------------

# allData = read.csv("./pareto_ranking_example.csv")

# allData["1",2]

# #O1 is the higher the better, so need to switch around
# allData[1:(nrow(allData)-1),"O1"] = - allData[1:(nrow(allData)-1),"O1"]

# solData = allData[-nrow(allData),-1]
# objData = allData[nrow(allData),-1]

# compositeRanks(solData, objData)

# #---------------------------------------------------
# #NBEATS
# #---------------------------------------------------

# allData = read.csv("./vert_stab_NBEATS.csv")

# solData = allData[-nrow(allData),c(4,5)]
# objData = allData[nrow(allData),c(4,5)]

# compRanksM3=c(compositeRanks(solData, objData),NA)

# compRanksM4=c(compositeRanks(allData[-nrow(allData),c(2,3)], 
#                             allData[nrow(allData),c(2,3)]),NA)

# cbind(allData, compRanksM3, compRanksM4)

#---------------------------------------------------
#PR
#---------------------------------------------------


is_dominated <- function(point, point_cloud) {
  # Check if a point is dominated by any other point in the cloud
  for (i in 1:nrow(point_cloud)) {
    other_point = point_cloud[i,]
    if (all(point[1] >= other_point[1]) && any(point[2] > other_point[2])) {
      return(TRUE)
    }
  }
  return(FALSE)
}

pareto_front <- function(point_cloud) {
  # Find the Pareto front from a given point cloud
  pareto_points <- matrix(NA, ncol = ncol(point_cloud), nrow = 0)
  for (i in 1:nrow(point_cloud)) {
    if (!is_dominated(point_cloud[i,], point_cloud)) {
      pareto_points <- rbind(pareto_points, point_cloud[i,])
    }
  }
  return(pareto_points)
}

# Define a function to calculate curvature
calculate_curvature <- function(x, y) {
  if(length(x) == 1) return(NA)

  dx <- diff(x)
  dy <- diff(y)
  ddx <- diff(dx)
  ddy <- diff(dy)
  curvature <- abs(ddx * dy[2:length(dy)] - dx[2:length(dx)] * ddy) / ((dx[2:length(dx)]^2 + dy[2:length(dy)]^2)^(3/2))
  return(c(NA,curvature,NA))
}

# Define a function to calculate the second derivative
calculate_second_derivative <- function(x, y) {
    if(length(x) == 1) return(NA)
#   n <- length(x)
#   h <- diff(x) # Step size
#   dy <- diff(y) # First differences of y
  
#   # Compute second differences of y
#   d2y <- diff(dy) / h[-length(h)]
#   return(c(NA,d2y,NA))

#   # Extend d2y to the length of x
#   d2y_extended <- rep(NA, n)
#   d2y_extended[2:(n-1)] <- d2y

# first version, can sometimes not be good
#https://stackoverflow.com/questions/75013143/how-can-i-calculate-the-second-order-derivative-of-a-vector-using-finite-differe  
    # dydx = diff(y) / diff(x)
    # d2ydx2 = c(NA, NA, diff(dydx) / diff(x[-1]))
    # d2ydx2  


    #calculate the first deriative and the new mean x value
    xprime <- x[-1] - diff(x)/2
    dydx <- diff(y)/diff(x)

    #calculate the 2nd deriative and the new mean x value
    xpprime <- xprime[-1] - diff(xprime)/2
    d2ydx2 <- diff(dydx)/diff(xprime)
    c(NA, NA, d2ydx2)
}


library(cobs)

datasetNames = c("M4", "M3", "Favorita", "M5")


library("readxl")
myPath = "./"

inFile = "revision_results2.xlsx"
sheets = c("v-mase", "v-rmsse", "h-mase", "h-rmsse")

#inFile = "revision_results_horizontal_edtCB.xlsx"
#sheets = c("h-mase", "h-rmsse")

xlData = list()
allDataList = list()
allDataListPareto = list()
allOptList = list()

for(sheet in sheets) {
    xlData[[sheet]] <- as.data.frame(read_excel(file.path(myPath, inFile), sheet = sheet))

    colnames(xlData[[sheet]]) = c("Model", "Weight", "M4_Err", "M3_Err", "Favorita_Err", "M5_Err", 
                                "M4_Stab", "M3_Stab", "Favorita_Stab", "M5_Stab", 
                                "M4_Stab_I", "M3_Stab_I", "Favorita_Stab_I", "M5_Stab_I")
    #names(xlData)
    #xlData[,"Model"]

    allData = split(xlData[[sheet]], xlData[[sheet]][,"Model"])

    allDataList[[sheet]] = lapply(allData, function(x) {
        rownames(x) = x[,"Weight"]

        dataList = list()

        for (dsName in datasetNames) {
            #browser()
            tempData = x[, c(paste0(dsName, "_Err"), paste0(dsName, "_Stab"))]
            if(!all(is.na(tempData))) dataList[[dsName]] = tempData
        }
        dataList
    })

    allDataListPareto[[sheet]] = list()
    allOptList[[sheet]] = list()

    sortMethodNames = if(length(names(allDataList[[sheet]]))==5) 
                        names(allDataList[[sheet]])[c(4,5,3,2,1)]
                      else 
                        names(allDataList[[sheet]])[c(4,3,2,1)]



    for (currMethod in sortMethodNames) {

        dataList = allDataList[[sheet]][[currMethod]]
        

        dataListPareto = lapply(dataList, pareto_front)

        #order points on the pareto front
        dataListPareto = lapply(dataListPareto, function(x){
            x[order(x[,1]),]
        })

        #smoothing
        # Convex Cobs Spline


        dataListPareto = lapply(dataListPareto, function(x){
            if(nrow(x)>3) {
                spCobs = cobs(x[,1] , x[,2], constraint = c("decrease", "convex"), nknots = 8)
                smoothed_y = predict(spCobs, x[,1])[,2] 
                smoothed_y_2nd_der = predict(spCobs, x[,1], deriv = 2)[,2] 
            } else {
                smoothed_y=rep(NA, nrow(x))
                smoothed_y_2nd_der=rep(NA, nrow(x))
            }
            cbind(x, smoothed_y=smoothed_y)#, smoothed_y_2nd_der=smoothed_y_2nd_der)
        })

        #if(sheet == "h-mase") browser()

        dataListPareto = lapply(dataListPareto, function(x) {
            res = cbind(x, comp_ranks=compositeRanks(x[,c(1,2)], c(1,2)),
                        curv=calculate_curvature(x[,1], x[,2]),
                        sm_curv=calculate_curvature(x[,1], x[,3]),
                        scnd_der=calculate_second_derivative(x[,1], x[,2]),
                        sm_scnd_der=calculate_second_derivative(x[,1], x[,3])
                        )
            cbind(res, rank_curv=rank(-res[,"curv"]),
                    rank_sm_curv=rank(-res[,"sm_curv"]),
                    rank_scnd_der=rank(-res[,"scnd_der"]),
                    rank_sm_scnd_der=rank(-res[,"sm_scnd_der"]))                
            })

        #browser()

        allDataListPareto[[sheet]][[currMethod]] = dataListPareto


        #-----------------------------------------------------------------
        optList = list()
        allOptList[[sheet]][[currMethod]] = list()

        for(i in names(dataList)) {
            

            x = dataListPareto[[i]][,1]
            y = dataListPareto[[i]][,2]
            smoothed_y = dataListPareto[[i]][,3]

            # #plot with same size for units in x and y axis direction
            # s = c(max(x)-min(x), max(y)-min(y))
            # span = max(s)/2
            # arg_span = which(s==max(s))

            # plot(x, y, xlim=if(arg_span==1) c(min(x), max(x)) else c(min(x)-span, max(x)+span),
            #             ylim=if(arg_span==2) c(min(y), max(y)) else c(min(y)-span, max(y)+span))                      

            #plot(x, y, asp=1)

            labels <- rownames(dataList[[i]])
            colors <- 1:length(labels)

            x_all = dataList[[i]][,1]
            y_all = dataList[[i]][,2]

            #browser()
            #y_total = 

            asp_ratio = (max(x_all)-min(x_all))/(max(y_all)-min(y_all))

            #pdf(file.path(myPath, paste0(i, "_prop%d.pdf")), width=5, height=10, onefile=FALSE)
            pdf(file.path(file.path(myPath, "plots"), paste0(sheet, "_", i, "_", currMethod, ".pdf")), width=8*asp_ratio+2, height=8, onefile=FALSE)
            
            isRMSSE = FALSE
            if(substr(sheet,3,7)=="rmsse") isRMSSE = TRUE

            #dev.new(height=10,width=10*asp_ratio+2)
            plot(x_all, y_all, pch = 19, col = colors, 
                asp=1, 
                xlab=if(isRMSSE) "RMSSE" else "MASE", ylab=if(isRMSSE) "RMSSC" else "MASC") #, xaxp=c(12,14,4)
            text(x_all, y_all, labels, pos = 4, cex = 0.8, offset = 0.5)

            rng <- par("usr")
            #rng
            
            lines(x,smoothed_y, col = "blue")
            #points(x, smoothed_y, pch = 2, col = "blue")

            #horizontal segments
            #segments(x0 = x[1:(length(x)-1)], y0 = y[1:(length(x)-1)], x1 = x[2:length(x)], y1 = y[1:(length(x)-1)], col = "black")  
            segments(x0 = x, y0 = y, x1 = c(x[2:length(x)],rng[2]), y1 = y, col = "black")  

            #vertical segments    
            #segments(x0 = x[2:length(x)], y0 = y[1:(length(x)-1)], x1 = x[2:length(x)], y1 = y[2:length(x)], col = "black")  
            segments(x0 = x, y0 = c(rng[4], y[1:(length(x)-1)]), x1 = x, y1 = y, col = "black")  

            dataListPareto[[i]][dataListPareto[[i]][,"rank_curv"]==1,c(1,2)]

            sol_ranks = dataListPareto[[i]][,c(4,9:ncol(dataListPareto[[i]]))]
            best_sols = do.call(rbind, apply(sol_ranks,2,function(x) {
                    ind=which(x==1)
                    c(dataListPareto[[i]][ind,c(1,2)],
                        rownames(dataListPareto[[i]][ind,]))
                }))

            best_sols = as.data.frame(best_sols)
            best_sols[,1] = as.numeric(best_sols[,1])
            best_sols[,2] = as.numeric(best_sols[,2])
            

            #points(best_sols[,1], best_sols[,2], pch = 19, col = colors)
            #text(best_sols[,1], best_sols[,2], rownames(best_sols), pos = 4, cex = 0.8, offset = 0.5)

            pd_acc = 100*(best_sols[,1]-min(x))/min(x)
            dec_acc = best_sols[,1]-min(x)

            pi_stab = -100*(best_sols[,2]-max(y))/max(y)
            inc_stab = -(best_sols[,2]-max(y))

            allOptList[[sheet]][[currMethod]][[i]] = cbind(best_sols, pd_acc, pi_stab, dec_acc, inc_stab)

            dev.off()
        }
        #-----------------------------------------------------------------



        }

}




# resTab = lapply(allOptList[[1]][[1]], function(x) {
#         res = x["rank_sm_scnd_der",]
#         colnames(res)[1:3] = c("Err", "Stab", "Weight")
#         res
#     })

# resTab = do.call(rbind, resTab)

# resTab

resPerDataset = list()
for(sheet in sheets) {

    resPerDataset[[sheet]] = list()

    for(currMethod in names(allOptList[[sheet]])) {
        
        for(currDS in names(allOptList[[sheet]][[currMethod]])) {

                if(is.null(resPerDataset[[sheet]][[currDS]]))
                    resPerDataset[[sheet]][[currDS]]=list()

                allOptList[[sheet]][[currMethod]][[currDS]]
                res = allOptList[[sheet]][[currMethod]][[currDS]]["rank_sm_scnd_der",]
                colnames(res)[1:3] = c("Err", "Stab", "Weight")
                resPerDataset[[sheet]][[currDS]][[currMethod]] = res
        }
        # resTab = lapply(allOptList[[sheet]][[currMethod]], function(x) {
        #         res = x["rank_sm_scnd_der",]
        #         colnames(res)[1:3] = c("Err", "Stab", "Weight")
        #         res
        #     })

        # resTab = do.call(rbind, resTab)

        # resTab
    }
    
    resPerDataset[[sheet]] = lapply(resPerDataset[[sheet]], function(x) {
        do.call(rbind, x)
    })
}

resPerDataset
resPerDataset[[1]]
#optList

# "v" is vertical stab., "h" is horizontal stab.
useCase = "v"

prettyMethodNames <- t(data.frame(
                            c("rank_sm_scnd_der", "Smoothed 2nd Der.")
                            ))

rownames(prettyMethodNames) = NULL

prettyColNames = c(" ", "Method", "MASE/RMSSE", "MASC/RMSSC", "$dec_{acc}$ (\\%)", "$inc_{stab}$ (\\%)", "$dec_{acc}$", "$inc_{stab}$")

resPrettyList <- list()
resPrettyList[[1]] = prettyColNames

i = 2


#category = names(resPerDataset[[1]])[1]
for(category in names(resPerDataset[[1]])) {
    resPrettyList[[i]] = c("", "", "", category, "", "", "", "")
    i = i + 1
    currUseCase = paste0(useCase,"-", if(category %in% c("M3","M4")) "mase" else "rmsse")
    currRes = resPerDataset[[currUseCase]][[category]][,c(1,2,4,5,6,7)]
    methods = resPerDataset[[currUseCase]][[category]][,3]

    # find for each column which value is best, to mark it in boldface
    #whichBest = apply(abs(sweep(currRes[,-ncol(currRes)], 2, c(0,0,0,0), "-")), 2, which.min)

    #currResPretty = format(currRes, digits=2)
    currResPretty = format(round(currRes, 3), nsmall = 3)

    #currResPretty[which(endsWith(currResPretty, "NA"))] = "-"
    #for(j in 1:(ncol(currResPretty)-1)) {
    #    currResPretty[whichBest[j],j] = paste0("\\textbf{", currResPretty[whichBest[j],j],"}")
    #}

    # use lookup table to pretty-print method names
    #indices <- match(rownames(currResPretty), prettyMethodNames[,1])

    

    resPrettyList[[i]] = cbind(rownames(currResPretty), gsub("_", "\\\\_", unlist(methods)), 
                            currResPretty)
    
    colnames(resPrettyList[[i]]) = prettyColNames
    #rownames(resPrettyList[[i]]) = NULL

    #browser()
    i = i + 1
}

resPrettyTab = do.call(rbind, resPrettyList)
#xtable(resPrettyTab, include.rownames=FALSE)

#resPrettyTab = resPrettyTab[,1:6]

colnames(resPrettyTab) = NULL
rownames(resPrettyTab) = NULL

resPrettyTab


library(xtable)

latex_table <- xtable(resPrettyTab, align="llrrrrrrr")
identity_function <- function(x) { x }

#sink("./pareto_table.tex")
print(latex_table, type="latex", include.rownames=FALSE, include.colnames=FALSE,
                sanitize.text.function = identity_function, 
                floating.environment = "table*", size="\\footnotesize")
#sink()




