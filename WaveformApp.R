#library(rconnect)
library(shiny)
library(RJSONIO)
library(xlsx)
library(rJava)
library(shinyWidgets)
library(dplyr)
library(kableExtra)
library(lqmm)
library(reshape2)
library(ggplot2)
library(MASS)
library(curl)

ui <- fluidPage(
    titlePanel("Waveform Analysis"),
    sidebarLayout(
        sidebarPanel(
            p("Please upload a file that has the subject in the first column and the various time points as separate columns following it, with one line per trial for each subject."),
            fileInput("file", "Choose File:",
                       multiple = FALSE),
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            # Input: File upload type ----
            radioButtons("uploadType", "Upload Type:", 
                         choices = c("CSV" = ".csv",
                                     "XLS/XLSX" = ".xls"),
                         inline = TRUE),
            hr(),
            numericRangeInput(
                inputId = "slider1", label = "Range for Margin of Error (Trial Calculation tabs):",
                value = c(.01, 2)
            ),
            numericInput("by", label = "Increment for Margin of Error (Trial Calculation tabs):", value = 0.01),
            hr(),
            numericRangeInput(
                inputId = "slider2", label = "Range for Number of Trials (MOE Calculation tabs):",
                value = c(2, 20)
            ),
            numericInput("by2", label = "Increment for Number of Trials (MOE Calculation tabs):", value = 2),
            hr(),
            numericRangeInput(
                inputId = "subgraph", label = "Range of subjects to be included in the plot of means by subject over time points (2nd graph in Graphics tab):",
                value = c(1, 5)
            ),
            numericInput("subjselect", label = "Subject selection for plot of values over time points (3rd graph in Graphics tab and Graphics with CB - one subject tab):", value = 1),
            hr(),
            numericRangeInput(
                inputId = "subbands", label = "Range of subjects to be included in the plot of means by subject over time points (Graphics with Confidence Bands tab):",
                value = c(1, 2)
            ),
            hr(),
            numericInput("sampsize", label = "Sample size selection for plot of values over time points (Graphics with Confidence Bands tab):", value = 10),
            hr(),
            numericInput("sampsize1", label = "Sample size selection for plot of values over time points (Graphics with CB tab):", value = 10),
            numericInput("sampsize2", label = "Sample size selection for plot of values over time points (Graphics with CB tab):", value = 20),
            hr(),
            numericRangeInput(
                inputId = "numtime", label = "Range of time points to take into account for sample size calculations (optional, leaving this blank will include all time points):",
                value = c(NULL, NULL)
        ),
        ),
        mainPanel(
            h5(a("Link to User Guide", href =  "https://drive.google.com/file/d/11UdMrx2ieeRnlxdgcbjwoiKy3V6BrAdt/view?usp=sharing")),
            tabsetPanel(
              tabPanel("Raw Data (First 6 Entries)", tableOutput("preview1")),
              tabPanel("Graphics", plotOutput("graphics"), plotOutput("graphics2"), plotOutput("graphics3")),
              tabPanel("Trial Calculations - T-Based", tableOutput("results")),
              tabPanel("Trial Calculations - Band-Based", tableOutput("results2")),
              tabPanel("MOE Calculations - T-Based", tableOutput("results3")),
              tabPanel("MOE Calculations - Band-Based", tableOutput("results4")),
              tabPanel("Graphics with Confidence Bands", plotOutput("graphics4"), plotOutput("graphics5")),
              tabPanel("Graphics with CB - One Subject", plotOutput("graphics6"), plotOutput("graphics7"))
            )
        )
    )
)

server <- function(input, output, session) {
    options(shiny.maxRequestSize=90*1024^2) 
    # Upload ---------------------------------------------------------------
    raw <- reactive({
        req(input$file)
        if(input$uploadType==".csv"){
            read.csv(input$file$datapath, header=input$header)
            }
        else if(input$uploadType==".xls") {
            if(input$header==FALSE){
            readxl::read_excel(input$file$datapath, skip=1, na = "NA")
            }
            else if(input$header==TRUE){
            readxl::read_excel(input$file$datapath, na = "NA")
            }
        }
        })
    
    #Preview Section
    output$preview1 <-renderTable({
        dataset <- as.data.frame(raw())
        if(is.data.frame(dataset) && nrow(dataset)==0){
        print("Please Upload a Dataset")
        }
        else{
            df <- head(dataset)
            df
        }
    })
    
    #Graphics Section
    output$graphics <- renderPlot({
        if(input$uploadType==".csv" | input$uploadType==".xls"){
        dataset <- as.data.frame(raw())
        #Renaming columns from V1 - VX (where X = total number of columns in dataset)
        col.tot <- ncol(dataset)
        colnames(dataset) <- paste("V", 1:col.tot, sep = "")
        meanbysub <- aggregate(.~V1, data=dataset, na.rm=TRUE, na.action=NULL, mean)
        means.subj <- meanbysub[,-1]
        matplot(0:as.numeric(ncol(means.subj)-1),t(means.subj), type="l", xlab = "Time Points", ylab = "Values")
        title(main = "Plots of Mean Values by Subject over Time Points")
        }
    })
    
    output$graphics2 <- renderPlot({
        if(input$uploadType==".csv" | input$uploadType==".xls"){
        dataset <- as.data.frame(raw())
        #Renaming columns from V1 - VX (where X = total number of columns in dataset)
        col.tot <- ncol(dataset)
        colnames(dataset) <- paste("V", 1:col.tot, sep = "")
        meanbysub <- aggregate(.~V1, data=dataset, na.rm=TRUE, na.action=NULL,  mean)
        means.subj <- meanbysub[,-1]
        matplot(0:as.numeric(ncol(means.subj)-1),t(means.subj[input$subgraph[1]:input$subgraph[2],]), type="l", xlab = "Time Points", ylab = "Values")
        title(main = "Plots of Mean Values by Subject over Time Points (Selected Subjects)")
        }
    })
    
    output$graphics3 <- renderPlot({
        if(input$uploadType==".csv" | input$uploadType==".xls"){
            dataset <- as.data.frame(raw())
            #Renaming columns from V1 - VX (where X = total number of columns in dataset)
            col.tot <- ncol(dataset)
            colnames(dataset) <- paste("V", 1:col.tot, sep = "")
            x <- dataset[dataset$V1==input$subjselect,-1]
            
            matplot(0:as.numeric(ncol(x)-1),t(x), type="l", xlab = "Time Points", ylab = "Values")
            subnum <- toString(input$subjselect)
            title(main = paste("Plots of Values over Time Points for Subject",subnum))
        }
    })
    
    output$graphics4 <- renderPlot({
        if(input$uploadType==".csv" | input$uploadType==".xls"){
            dataset <- as.data.frame(raw())
            #Renaming columns from V1 - VX (where X = total number of columns in dataset)
            col.tot <- ncol(dataset)
            colnames(dataset) <- paste("V", 1:col.tot, sep = "")
            meanbysub <- aggregate(.~V1, data=dataset, na.rm=TRUE, na.action=NULL, mean)
            #Renaming columns
            colnames(meanbysub) <- c("sub",0:(ncol(meanbysub)-2))
            df_long <- melt(data = meanbysub, 
                            id.vars = "sub",
                            variable.name = "TimePoint",
                            value.name = "Value")
            
            num.of.trials <- dataset %>% count(V1)
            num.of.trials <- as.vector(num.of.trials[,2])
            #Getting numerator for the pooled within-subject covariance matrix
            Subs<-unique(dataset$V1)
            listofsub <- vector("list", length(Subs))
            for (i in 1:length(Subs)){ 
                listofsub[[i]] <- as.data.frame(dataset[which(dataset$V1==i),-1]) 
            }
            Cov <- vector("list", length(listofsub))
            for (i in 1:length(listofsub)){
                Cov[[i]] <- (num.of.trials[i])*cov(as.data.frame(listofsub[[i]]),use="pairwise.complete.obs")
            }
            cov.numer <- 0
            for (i in 1:length(listofsub)){
                if(as.numeric(length(is.na.data.frame(Cov[[i]])[is.na.data.frame(Cov[[i]]) == FALSE]))){
                    cov.numer <- cov.numer+Cov[[i]]
                }
            }
            #Getting denominator
            denom <- 0
            for (i in 1:length(listofsub))
            {
                #Check to see if the dataframe is fully NA or not
                if(as.numeric(length(is.na.data.frame(Cov[[i]])[is.na.data.frame(Cov[[i]]) == FALSE]))){
                    denom <- denom + num.of.trials[i]
                }
            }
            Sigma.Within <- cov.numer/denom
            d.Sigma.Within <- diag(Sigma.Within)
            
            mofe.t <- function(nn, ddss){
                qt(.975, nn-1)*sqrt(max(ddss, na.rm=T)/nn)
            }
            
            df_long$uppert <- df_long$Value + mofe.t(nn=input$sampsize,ddss=d.Sigma.Within)
            df_long$lowert <- df_long$Value - mofe.t(nn=input$sampsize,ddss=d.Sigma.Within)
            
            #Band-based graph
            
            #Getting the within-subject correlation matrix
            #Getting the numerator
            Cor <- vector("list", length(listofsub))
            for (i in 1:length(listofsub)){
                Cor[[i]] <- (num.of.trials[i])*cor(as.data.frame(listofsub[[i]]),use="pairwise.complete.obs")
            }
            cor.numer <- 0
            for (i in 1:length(listofsub)){
                if(as.numeric(length(is.na.data.frame(Cor[[i]])[is.na.data.frame(Cor[[i]]) == FALSE]))){
                    cor.numer <- cor.numer+Cor[[i]]
                }
            }
            Rho.Within <-cor.numer/denom
            
            em.cutoff<- function(rho,t){
                tmp<-mvrnorm(100000,matrix(0,nrow=t, ncol=1,),make.positive.definite(rho))
                ptile<-quantile(apply(tmp, 2, max), .975)
                return(ptile)
            }
            
            mofe.band <- function(nn, ddss, pptt){
                pptt*sqrt(max(ddss, na.rm=T)/nn)
            }
            Rho.Within <- as.data.frame(Rho.Within)
            # Removing NA rows and NA columns to get square matrix
            narows <- Rho.Within[rowSums(is.na(Rho.Within)) != ncol(Rho.Within), ]
            Rho.Within.noNA <- narows[,colSums(is.na(narows)) != nrow(narows)]
            ptile <- em.cutoff(Rho.Within.noNA, ncol(Rho.Within.noNA))
            
            df_long$upperb <- df_long$Value + mofe.band(nn=input$sampsize,ddss=d.Sigma.Within,pptt=ptile)
            df_long$lowerb <- df_long$Value - mofe.band(nn=input$sampsize,ddss=d.Sigma.Within,pptt=ptile)
            
            #Select subjects for graph
            CIbands <- subset(df_long, sub %in% c(input$subbands[1]:input$subbands[2]))
            
            #Output graphs
            ggplot(CIbands, aes(x=as.numeric(TimePoint),y=Value,group=sub)) +  geom_line(aes(colour=factor(sub))) + geom_ribbon(aes(ymin=lowert, ymax=uppert), linetype=2, alpha=0.1) + ggtitle("Plots of Subject Means over Time Points with Confidence Bands - T-Based") +
                xlab("Time Points") + ylab("Values") + labs(color='Subject')
            
        }
    })
    
    output$graphics5 <- renderPlot({
        if(input$uploadType==".csv" | input$uploadType==".xls"){
            dataset <- as.data.frame(raw())
            #Renaming columns from V1 - VX (where X = total number of columns in dataset)
            col.tot <- ncol(dataset)
            colnames(dataset) <- paste("V", 1:col.tot, sep = "")
            meanbysub <- aggregate(.~V1, data=dataset, na.rm=TRUE, na.action=NULL, mean)
            #Renaming columns
            colnames(meanbysub) <- c("sub",0:(ncol(meanbysub)-2))
            df_long <- melt(data = meanbysub, 
                            id.vars = "sub",
                            variable.name = "TimePoint",
                            value.name = "Value")
            
            num.of.trials <- dataset %>% count(V1)
            num.of.trials <- as.vector(num.of.trials[,2])
            #Getting numerator for the pooled within-subject covariance matrix
            Subs<-unique(dataset$V1)
            listofsub <- vector("list", length(Subs))
            for (i in 1:length(Subs)){ 
                listofsub[[i]] <- as.data.frame(dataset[which(dataset$V1==i),-1]) 
            }
            Cov <- vector("list", length(listofsub))
            for (i in 1:length(listofsub)){
                Cov[[i]] <- (num.of.trials[i])*cov(as.data.frame(listofsub[[i]]),use="pairwise.complete.obs")
            }
            cov.numer <- 0
            for (i in 1:length(listofsub)){
                if(as.numeric(length(is.na.data.frame(Cov[[i]])[is.na.data.frame(Cov[[i]]) == FALSE]))){
                    cov.numer <- cov.numer+Cov[[i]]
                }
            }
            #Getting denominator
            denom <- 0
            for (i in 1:length(listofsub))
            {
                #Check to see if the dataframe is fully NA or not
                if(as.numeric(length(is.na.data.frame(Cov[[i]])[is.na.data.frame(Cov[[i]]) == FALSE]))){
                    denom <- denom + num.of.trials[i]
                }
            }
            Sigma.Within <- cov.numer/denom
            d.Sigma.Within <- diag(Sigma.Within)
            
            mofe.t <- function(nn, ddss){
                qt(.975, nn-1)*sqrt(max(ddss, na.rm=T)/nn)
            }
            
            df_long$uppert <- df_long$Value + mofe.t(nn=input$sampsize,ddss=d.Sigma.Within)
            df_long$lowert <- df_long$Value - mofe.t(nn=input$sampsize,ddss=d.Sigma.Within)
            
            #Band-based graph
            
            #Getting the within-subject correlation matrix
            #Getting the numerator
            Cor <- vector("list", length(listofsub))
            for (i in 1:length(listofsub)){
                Cor[[i]] <- (num.of.trials[i])*cor(as.data.frame(listofsub[[i]]),use="pairwise.complete.obs")
            }
            cor.numer <- 0
            for (i in 1:length(listofsub)){
                if(as.numeric(length(is.na.data.frame(Cor[[i]])[is.na.data.frame(Cor[[i]]) == FALSE]))){
                    cor.numer <- cor.numer+Cor[[i]]
                }
            }
            Rho.Within <-cor.numer/denom
            
            em.cutoff<- function(rho,t){
                tmp<-mvrnorm(100000,matrix(0,nrow=t, ncol=1,),make.positive.definite(rho))
                ptile<-quantile(apply(tmp, 2, max), .975)
                return(ptile)
            }
            
            mofe.band <- function(nn, ddss, pptt){
                pptt*sqrt(max(ddss, na.rm=T)/nn)
            }
            Rho.Within <- as.data.frame(Rho.Within)
            # Removing NA rows and NA columns to get square matrix
            narows <- Rho.Within[rowSums(is.na(Rho.Within)) != ncol(Rho.Within), ]
            Rho.Within.noNA <- narows[,colSums(is.na(narows)) != nrow(narows)]
            ptile <- em.cutoff(Rho.Within.noNA, ncol(Rho.Within.noNA))
            
            df_long$upperb <- df_long$Value + mofe.band(nn=input$sampsize,ddss=d.Sigma.Within,pptt=ptile)
            df_long$lowerb <- df_long$Value - mofe.band(nn=input$sampsize,ddss=d.Sigma.Within,pptt=ptile)
            
            #Select subjects for graph
            CIbands <- subset(df_long, sub %in% c(input$subbands[1]:input$subbands[2]))
            
            #Output graph
            
            ggplot(CIbands, aes(x=as.numeric(TimePoint),y=Value,group=sub)) +  geom_line(aes(colour=factor(sub))) + geom_ribbon(aes(ymin=lowerb, ymax=upperb), linetype=2, alpha=0.1) + ggtitle("Plots of Subject Means over Time Points with Confidence Bands - Band-Based") +
                xlab("Time Points") + ylab("Values") + labs(color='Subject') 
            
        }
    })
    
    output$graphics6 <- renderPlot({
        if(input$uploadType==".csv" | input$uploadType==".xls"){
            dataset <- as.data.frame(raw())
            #Renaming columns from V1 - VX (where X = total number of columns in dataset)
            col.tot <- ncol(dataset)
            colnames(dataset) <- paste("V", 1:col.tot, sep = "")
            meanbysub <- aggregate(.~V1, data=dataset, na.rm=TRUE, na.action=NULL, mean)
            #Renaming columns
            colnames(meanbysub) <- c("sub",0:(ncol(meanbysub)-2))
            df_long <- melt(data = meanbysub, 
                            id.vars = "sub",
                            variable.name = "TimePoint",
                            value.name = "Value")
            
            num.of.trials <- dataset %>% count(V1)
            num.of.trials <- as.vector(num.of.trials[,2])
            #Getting numerator for the pooled within-subject covariance matrix
            Subs<-unique(dataset$V1)
            listofsub <- vector("list", length(Subs))
            for (i in 1:length(Subs)){ 
                listofsub[[i]] <- as.data.frame(dataset[which(dataset$V1==i),-1]) 
            }
            Cov <- vector("list", length(listofsub))
            for (i in 1:length(listofsub)){
                Cov[[i]] <- (num.of.trials[i])*cov(as.data.frame(listofsub[[i]]),use="pairwise.complete.obs")
            }
            cov.numer <- 0
            for (i in 1:length(listofsub)){
                if(as.numeric(length(is.na.data.frame(Cov[[i]])[is.na.data.frame(Cov[[i]]) == FALSE]))){
                    cov.numer <- cov.numer+Cov[[i]]
                }
            }
            #Getting denominator
            denom <- 0
            for (i in 1:length(listofsub))
            {
                #Check to see if the dataframe is fully NA or not
                if(as.numeric(length(is.na.data.frame(Cov[[i]])[is.na.data.frame(Cov[[i]]) == FALSE]))){
                    denom <- denom + num.of.trials[i]
                }
            }
            Sigma.Within <- cov.numer/denom
            d.Sigma.Within <- diag(Sigma.Within)
            
            mofe.t <- function(nn, ddss){
                qt(.975, nn-1)*sqrt(max(ddss, na.rm=T)/nn)
            }
            
            df_long$uppert1 <- df_long$Value + mofe.t(nn=input$sampsize1,ddss=d.Sigma.Within)
            df_long$lowert1 <- df_long$Value - mofe.t(nn=input$sampsize1,ddss=d.Sigma.Within)
            df_long$uppert2 <- df_long$Value + mofe.t(nn=input$sampsize2,ddss=d.Sigma.Within)
            df_long$lowert2 <- df_long$Value - mofe.t(nn=input$sampsize2,ddss=d.Sigma.Within)
            
            #Band-based graph
            
            #Getting the within-subject correlation matrix
            #Getting the numerator
            Cor <- vector("list", length(listofsub))
            for (i in 1:length(listofsub)){
                Cor[[i]] <- (num.of.trials[i])*cor(as.data.frame(listofsub[[i]]),use="pairwise.complete.obs")
            }
            cor.numer <- 0
            for (i in 1:length(listofsub)){
                if(as.numeric(length(is.na.data.frame(Cor[[i]])[is.na.data.frame(Cor[[i]]) == FALSE]))){
                    cor.numer <- cor.numer+Cor[[i]]
                }
            }
            Rho.Within <-cor.numer/denom
            
            em.cutoff<- function(rho,t){
                tmp<-mvrnorm(100000,matrix(0,nrow=t, ncol=1,),make.positive.definite(rho))
                ptile<-quantile(apply(tmp, 2, max), .975)
                return(ptile)
            }
            
            mofe.band <- function(nn, ddss, pptt){
                pptt*sqrt(max(ddss, na.rm=T)/nn)
            }
            Rho.Within <- as.data.frame(Rho.Within)
            # Removing NA rows and NA columns to get square matrix
            narows <- Rho.Within[rowSums(is.na(Rho.Within)) != ncol(Rho.Within), ]
            Rho.Within.noNA <- narows[,colSums(is.na(narows)) != nrow(narows)]
            ptile <- em.cutoff(Rho.Within.noNA, ncol(Rho.Within.noNA))
            
            df_long$upperb <- df_long$Value + mofe.band(nn=input$sampsize,ddss=d.Sigma.Within,pptt=ptile)
            df_long$lowerb <- df_long$Value - mofe.band(nn=input$sampsize,ddss=d.Sigma.Within,pptt=ptile)
            
            #Select subjects for graph
            CIbands <- subset(df_long, sub %in% c(input$subjselect))
            
            #Output graphs
            ggplot(CIbands, aes(x=as.numeric(TimePoint),y=Value,group=sub)) +  geom_line(aes(colour=factor(sub))) + geom_ribbon(aes(ymin=lowert1, ymax=uppert1), linetype=2, alpha=0.1) + geom_ribbon(aes(ymin=lowert2, ymax=uppert2), linetype=2, alpha=0.1)  + ggtitle("Plots of Subject Means over Time Points with Confidence Bands - T-Based") +
                xlab("Time Points") + ylab("Values") + labs(color='Subject')
            
        }
    })
    
    output$graphics7 <- renderPlot({
        if(input$uploadType==".csv" | input$uploadType==".xls"){
            dataset <- as.data.frame(raw())
            #Renaming columns from V1 - VX (where X = total number of columns in dataset)
            col.tot <- ncol(dataset)
            colnames(dataset) <- paste("V", 1:col.tot, sep = "")
            meanbysub <- aggregate(.~V1, data=dataset, na.rm=TRUE, na.action=NULL, mean)
            #Renaming columns
            colnames(meanbysub) <- c("sub",0:(ncol(meanbysub)-2))
            df_long <- melt(data = meanbysub, 
                            id.vars = "sub",
                            variable.name = "TimePoint",
                            value.name = "Value")
            
            num.of.trials <- dataset %>% count(V1)
            num.of.trials <- as.vector(num.of.trials[,2])
            #Getting numerator for the pooled within-subject covariance matrix
            Subs<-unique(dataset$V1)
            listofsub <- vector("list", length(Subs))
            for (i in 1:length(Subs)){ 
                listofsub[[i]] <- as.data.frame(dataset[which(dataset$V1==i),-1]) 
            }
            Cov <- vector("list", length(listofsub))
            for (i in 1:length(listofsub)){
                Cov[[i]] <- (num.of.trials[i])*cov(as.data.frame(listofsub[[i]]),use="pairwise.complete.obs")
            }
            cov.numer <- 0
            for (i in 1:length(listofsub)){
                if(as.numeric(length(is.na.data.frame(Cov[[i]])[is.na.data.frame(Cov[[i]]) == FALSE]))){
                    cov.numer <- cov.numer+Cov[[i]]
                }
            }
            #Getting denominator
            denom <- 0
            for (i in 1:length(listofsub))
            {
                #Check to see if the dataframe is fully NA or not
                if(as.numeric(length(is.na.data.frame(Cov[[i]])[is.na.data.frame(Cov[[i]]) == FALSE]))){
                    denom <- denom + num.of.trials[i]
                }
            }
            Sigma.Within <- cov.numer/denom
            d.Sigma.Within <- diag(Sigma.Within)
            
            mofe.t <- function(nn, ddss){
                qt(.975, nn-1)*sqrt(max(ddss, na.rm=T)/nn)
            }
            
            df_long$uppert <- df_long$Value + mofe.t(nn=input$sampsize,ddss=d.Sigma.Within)
            df_long$lowert <- df_long$Value - mofe.t(nn=input$sampsize,ddss=d.Sigma.Within)
            
            #Band-based graph
            
            #Getting the within-subject correlation matrix
            #Getting the numerator
            Cor <- vector("list", length(listofsub))
            for (i in 1:length(listofsub)){
                Cor[[i]] <- (num.of.trials[i])*cor(as.data.frame(listofsub[[i]]),use="pairwise.complete.obs")
            }
            cor.numer <- 0
            for (i in 1:length(listofsub)){
                if(as.numeric(length(is.na.data.frame(Cor[[i]])[is.na.data.frame(Cor[[i]]) == FALSE]))){
                    cor.numer <- cor.numer+Cor[[i]]
                }
            }
            Rho.Within <-cor.numer/denom
            
            em.cutoff<- function(rho,t){
                tmp<-mvrnorm(100000,matrix(0,nrow=t, ncol=1,),make.positive.definite(rho))
                ptile<-quantile(apply(tmp, 2, max), .975)
                return(ptile)
            }
            
            mofe.band <- function(nn, ddss, pptt){
                pptt*sqrt(max(ddss, na.rm=T)/nn)
            }
            Rho.Within <- as.data.frame(Rho.Within)
            # Removing NA rows and NA columns to get square matrix
            narows <- Rho.Within[rowSums(is.na(Rho.Within)) != ncol(Rho.Within), ]
            Rho.Within.noNA <- narows[,colSums(is.na(narows)) != nrow(narows)]
            ptile <- em.cutoff(Rho.Within.noNA, ncol(Rho.Within.noNA))
            
            df_long$upperb1 <- df_long$Value + mofe.band(nn=input$sampsize1,ddss=d.Sigma.Within,pptt=ptile)
            df_long$lowerb1 <- df_long$Value - mofe.band(nn=input$sampsize1,ddss=d.Sigma.Within,pptt=ptile)
            df_long$upperb2 <- df_long$Value + mofe.band(nn=input$sampsize2,ddss=d.Sigma.Within,pptt=ptile)
            df_long$lowerb2 <- df_long$Value - mofe.band(nn=input$sampsize2,ddss=d.Sigma.Within,pptt=ptile)
        
            
            #Select subjects for graph
            CIbands <- subset(df_long, sub %in% c(input$subjselect))
            
            #Output graph
            
            ggplot(CIbands, aes(x=as.numeric(TimePoint),y=Value,group=sub)) +  geom_line(aes(colour=factor(sub))) + geom_ribbon(aes(ymin=lowerb1, ymax=upperb1), linetype=2, alpha=0.1) + geom_ribbon(aes(ymin=lowerb2, ymax=upperb2), linetype=2, alpha=0.1) + ggtitle("Plots of Subject Means over Time Points with Confidence Bands - Band-Based") +
                xlab("Time Points") + ylab("Values") + labs(color='Subject') 
            
        }
    })
   
    #Sample Size Calculations Section
    output$results <- function(){
        if(input$uploadType==".csv" | input$uploadType==".xls"){
            dataset <- as.data.frame(raw())
            #Renaming columns from V1 - VX (where X = total number of columns in dataset)
            col.tot <- ncol(dataset)
            colnames(dataset) <- paste("V", 1:col.tot, sep = "")
            num.of.trials <- dataset %>% count(V1)
            num.of.trials <- as.vector(num.of.trials[,2])
            #Getting numerator for the pooled within-subject covariance matrix
            Subs<-unique(dataset$V1)
            listofsub <- vector("list", length(Subs))
            for (i in 1:length(Subs)){ 
                listofsub[[i]] <- as.data.frame(dataset[which(dataset$V1==i),-1]) 
            }
            Cov <- vector("list", length(listofsub))
            for (i in 1:length(listofsub)){
                Cov[[i]] <- (num.of.trials[i])*cov(as.data.frame(listofsub[[i]]),use="pairwise.complete.obs")
            }
            cov.numer <- 0
            for (i in 1:length(listofsub)){
                if(as.numeric(length(is.na.data.frame(Cov[[i]])[is.na.data.frame(Cov[[i]]) == FALSE]))){
                    cov.numer <- cov.numer+Cov[[i]]
                }
            }
            #Getting denominator
            denom <- 0
            for (i in 1:length(listofsub))
            {
                #Check to see if the dataframe is fully NA or not
                if(as.numeric(length(is.na.data.frame(Cov[[i]])[is.na.data.frame(Cov[[i]]) == FALSE]))){
                    denom <- denom + num.of.trials[i]
                }
            }
            Sigma.Within <- cov.numer/denom
            d.Sigma.Within <- diag(Sigma.Within)
            
            #Getting the within-subject correlation matrix
            #Getting the numerator
            Cor <- vector("list", length(listofsub))
            for (i in 1:length(listofsub)){
                Cor[[i]] <- (num.of.trials[i])*cor(as.data.frame(listofsub[[i]]),use="pairwise.complete.obs")
            }
            cor.numer <- 0
            for (i in 1:length(listofsub)){
                if(as.numeric(length(is.na.data.frame(Cor[[i]])[is.na.data.frame(Cor[[i]]) == FALSE]))){
                    cor.numer <- cor.numer+Cor[[i]]
                }
            }
            Rho.Within <-cor.numer/denom
            
            ####functions####
            ## function to compute the t-based margin of error
            mofe.t <- function(nn, ddss){
                qt(.975, nn-1)*sqrt(max(ddss, na.rm=T)/nn)
            }
            
            #
            ## function to find the needed n
            n.need.t <- function(ddss,tol){
                n.curr = 1
                mofe.t.curr = qt(.975, 1)*sqrt(max(ddss, na.rm=T))
                while(mofe.t.curr > tol){
                    n.curr=n.curr+1
                    mofe.t.curr = mofe.t(n.curr,ddss)
                }
                list(n=n.curr, mofe=mofe.t.curr)
            }
            
            ## function to get the empirical cut-off
            em.cutoff<- function(rho,t){
                tmp<-mvrnorm(100000,matrix(0,nrow=t, ncol=1,),make.positive.definite(rho))
                ptile<-quantile(apply(tmp, 2, max), .975)
                return(ptile)
            }
            
            tol.vals <- seq(input$slider1[1],input$slider1[2],by=input$by)
            need.t <- data.frame(t(rbind(tol.vals, sapply(tol.vals, n.need.t, ddss=d.Sigma.Within))))
            need_t <- data.frame(cbind(need.t[1],need.t[2]))
            colnames(need_t) <- c("Margin of Error","Number of Trials Needed")
            kable(need_t) %>%
                kable_styling(full_width = F)
        }
    }
    
    output$results2 <- function(){
        if (((is.null(input$numtime[1]) == TRUE) & (is.null(input$numtime[2]) == TRUE))) {
        if((input$uploadType==".csv" | input$uploadType==".xls")){
            dataset <- as.data.frame(raw())
            #Renaming columns from V1 - VX (where X = total number of columns in dataset)
            col.tot <- ncol(dataset)
            colnames(dataset) <- paste("V", 1:col.tot, sep = "")
            num.of.trials <- dataset %>% count(V1)
            num.of.trials <- as.vector(num.of.trials[,2])
            #Getting numerator for the pooled within-subject covariance matrix
            Subs<-unique(dataset$V1)
            listofsub <- vector("list", length(Subs))
            for (i in 1:length(Subs)){ 
                listofsub[[i]] <- as.data.frame(dataset[which(dataset$V1==i),-1]) 
            }
            Cov <- vector("list", length(listofsub))
            for (i in 1:length(listofsub)){
                Cov[[i]] <- (num.of.trials[i])*cov(as.data.frame(listofsub[[i]]),use="pairwise.complete.obs")
            }
            cov.numer <- 0
            for (i in 1:length(listofsub)){
                if(as.numeric(length(is.na.data.frame(Cov[[i]])[is.na.data.frame(Cov[[i]]) == FALSE]))){
                    cov.numer <- cov.numer+Cov[[i]]
                }
            }
            #Getting denominator
            denom <- 0
            for (i in 1:length(listofsub))
            {
                if(as.numeric(length(is.na.data.frame(Cov[[i]])[is.na.data.frame(Cov[[i]]) == FALSE]))){
                    denom <- denom + num.of.trials[i]
                }
            }
            Sigma.Within <- cov.numer/denom
            d.Sigma.Within <- diag(Sigma.Within)
            
            #Getting the within-subject correlation matrix
            #Getting the numerator
            Cor <- vector("list", length(listofsub))
            for (i in 1:length(listofsub)){
                Cor[[i]] <- (num.of.trials[i])*cor(as.data.frame(listofsub[[i]]),use="pairwise.complete.obs")
            }
            cor.numer <- 0
            for (i in 1:length(listofsub)){
                if(as.numeric(length(is.na.data.frame(Cor[[i]])[is.na.data.frame(Cor[[i]]) == FALSE]))){
                    cor.numer <- cor.numer+Cor[[i]]
                }
            }
            Rho.Within <-cor.numer/denom
            
            ## function to get the empirical cut-off
            em.cutoff<- function(rho,t){
                tmp<-mvrnorm(100000,matrix(0,nrow=t, ncol=1,), make.positive.definite(rho))
                ptile<-quantile(apply(tmp, 2, max), .975)
                return(ptile)
            }
            ## function to compute the band-based margin of error
            mofe.band <- function(nn, ddss, pptt){
                pptt*sqrt(max(ddss, na.rm=T)/nn)
            }
            #
            ## function to find the needed n
            n.need.band <- function(ddss, pptt,tol){
                n.curr = 1
                mofe.curr = pptt*sqrt(max(ddss, na.rm=T))
                while(mofe.curr > tol){
                    n.curr=n.curr+1
                    mofe.curr = mofe.band(n.curr,ddss,pptt)
                }
                list(n=n.curr, mofe=mofe.curr)
            }
            
            Rho.Within <- as.data.frame(Rho.Within)
            # Removing NA rows and NA columns to get square matrix
            narows <- Rho.Within[rowSums(is.na(Rho.Within)) != ncol(Rho.Within), ]
            Rho.Within.noNA <- narows[,colSums(is.na(narows)) != nrow(narows)]
            ptile <- em.cutoff(Rho.Within.noNA, ncol(Rho.Within.noNA))
            
            tol.vals <- seq(input$slider1[1],input$slider1[2],by=input$by)
            need.band <- data.frame(t(rbind(tol.vals, sapply(tol.vals, n.need.band, ddss=d.Sigma.Within, pptt = ptile))))
            need_band <- data.frame(cbind(need.band[1],need.band[2]))
            colnames(need_band) <- c("Margin of Error","Number of Trials Needed")
            kable(need_band) %>%
                kable_styling(full_width = F)
        }
        }
        else {
            if((input$uploadType==".csv" | input$uploadType==".xls")){
                dataset <- as.data.frame(raw())
                #Renaming columns from V1 - VX (where X = total number of columns in dataset)
                col.tot <- ncol(dataset)
                colnames(dataset) <- paste("V", 1:col.tot, sep = "")
                num.of.trials <- dataset %>% count(V1)
                num.of.trials <- as.vector(num.of.trials[,2])
                
                Subs<-unique(dataset$V1)
                listofsub <- vector("list", length(Subs))
                for (i in 1:length(Subs)){ 
                    listofsub[[i]] <- as.data.frame(dataset[which(dataset$V1==i),-1]) 
                }
                Cov <- vector("list", length(listofsub))
                for (i in 1:length(listofsub)){
                    Cov[[i]] <- (num.of.trials[i])*cov(as.data.frame(listofsub[[i]] %>% dplyr::select(input$numtime[1]:input$numtime[2])),use="pairwise.complete.obs")
                }
                cov.numer <- 0
                for (i in 1:length(listofsub)){
                    if(as.numeric(length(is.na.data.frame(Cov[[i]])[is.na.data.frame(Cov[[i]]) == FALSE]))){
                        cov.numer <- cov.numer+Cov[[i]]
                    }
                }
                #Getting denominator
                denom <- 0
                for (i in 1:length(listofsub))
                {
                    if(as.numeric(length(is.na.data.frame(Cov[[i]])[is.na.data.frame(Cov[[i]]) == FALSE]))){
                        denom <- denom + num.of.trials[i]
                    }
                }
                Sigma.Within <- cov.numer/denom
                d.Sigma.Within <- diag(Sigma.Within)
                
                #Getting the within-subject correlation matrix
                #Getting the numerator
                Cor <- vector("list", length(listofsub))
                for (i in 1:length(listofsub)){
                    Cor[[i]] <- (num.of.trials[i])*cor(as.data.frame(listofsub[[i]]) %>% dplyr::select(input$numtime[1]:input$numtime[2]),use="pairwise.complete.obs")
                }
                cor.numer <- 0
                for (i in 1:length(listofsub)){
                    if(as.numeric(length(is.na.data.frame(Cor[[i]])[is.na.data.frame(Cor[[i]]) == FALSE]))){
                        cor.numer <- cor.numer+Cor[[i]]
                    }
                }
                Rho.Within <-cor.numer/denom
                
                ## function to get the empirical cut-off
                em.cutoff<- function(rho,t){
                    tmp<-mvrnorm(100000,matrix(0,nrow=t, ncol=1,), make.positive.definite(rho))
                    ptile<-quantile(apply(tmp, 2, max), .975)
                    return(ptile)
                }
                ## function to compute the band-based margin of error
                mofe.band <- function(nn, ddss, pptt){
                    pptt*sqrt(max(ddss, na.rm=T)/nn)
                }
                #
                ## function to find the needed n
                n.need.band <- function(ddss, pptt,tol){
                    n.curr = 1
                    mofe.curr = pptt*sqrt(max(ddss, na.rm=T))
                    while(mofe.curr > tol){
                        n.curr=n.curr+1
                        mofe.curr = mofe.band(n.curr,ddss,pptt)
                    }
                    list(n=n.curr, mofe=mofe.curr)
                }
                
                Rho.Within <- as.data.frame(Rho.Within)
                # Removing NA rows and NA columns to get square matrix
                narows <- Rho.Within[rowSums(is.na(Rho.Within)) != ncol(Rho.Within), ]
                Rho.Within.noNA <- narows[,colSums(is.na(narows)) != nrow(narows)]
                ptile <- em.cutoff(Rho.Within.noNA, ncol(Rho.Within.noNA))
                
                tol.vals <- seq(input$slider1[1],input$slider1[2],by=input$by)
                need.band <- data.frame(t(rbind(tol.vals, sapply(tol.vals, n.need.band, ddss=d.Sigma.Within, pptt = ptile))))
                need_band <- data.frame(cbind(need.band[1],need.band[2]))
                colnames(need_band) <- c("Margin of Error","Number of Trials Needed")
                kable(need_band) %>%
                    kable_styling(full_width = F)
            }
        }
    }
    
    output$results3 <- function(){
        if(input$uploadType==".csv" | input$uploadType==".xls"){
            dataset <- as.data.frame(raw())
            #Renaming columns from V1 - VX (where X = total number of columns in dataset)
            col.tot <- ncol(dataset)
            colnames(dataset) <- paste("V", 1:col.tot, sep = "")
            num.of.trials <- dataset %>% count(V1)
            num.of.trials <- as.vector(num.of.trials[,2])
            #Getting numerator for the pooled within-subject covariance matrix
            Subs<-unique(dataset$V1)
            listofsub <- vector("list", length(Subs))
            for (i in 1:length(Subs)){ 
                listofsub[[i]] <- as.data.frame(dataset[which(dataset$V1==i),-1]) 
            }
            Cov <- vector("list", length(listofsub))
            for (i in 1:length(listofsub)){
                Cov[[i]] <- (num.of.trials[i])*cov(as.data.frame(listofsub[[i]]),use="pairwise.complete.obs")
            }
            cov.numer <- 0
            for (i in 1:length(listofsub)){
                if(as.numeric(length(is.na.data.frame(Cov[[i]])[is.na.data.frame(Cov[[i]]) == FALSE]))){
                    cov.numer <- cov.numer+Cov[[i]]
                }
            }
            #Getting denominator
            denom <- 0
            for (i in 1:length(listofsub))
            {
                #Check to see if the dataframe is fully NA or not
                if(as.numeric(length(is.na.data.frame(Cov[[i]])[is.na.data.frame(Cov[[i]]) == FALSE]))){
                    denom <- denom + num.of.trials[i]
                }
            }
            Sigma.Within <- cov.numer/denom
            d.Sigma.Within <- diag(Sigma.Within)
            
            #Getting the within-subject correlation matrix
            #Getting the numerator
            Cor <- vector("list", length(listofsub))
            for (i in 1:length(listofsub)){
                Cor[[i]] <- (num.of.trials[i])*cor(as.data.frame(listofsub[[i]]),use="pairwise.complete.obs")
            }
            cor.numer <- 0
            for (i in 1:length(listofsub)){
                if(as.numeric(length(is.na.data.frame(Cor[[i]])[is.na.data.frame(Cor[[i]]) == FALSE]))){
                    cor.numer <- cor.numer+Cor[[i]]
                }
            }
            Rho.Within <-cor.numer/denom
            
            ####functions####
            ## function to compute the t-based margin of error
            mofe.t <- function(nn, ddss){
                qt(.975, nn-1)*sqrt(max(ddss, na.rm=T)/nn)
            }
            
            #
            ## function to find the needed n
            n.need.t <- function(ddss,tol){
                n.curr = 1
                mofe.t.curr = qt(.975, 1)*sqrt(max(ddss, na.rm=T))
                while(mofe.t.curr > tol){
                    n.curr=n.curr+1
                    mofe.t.curr = mofe.t(n.curr,ddss)
                }
                list(n=n.curr, mofe=mofe.t.curr)
            }
            
            ## function to get the empirical cut-off
            em.cutoff<- function(rho,t){
                tmp<-mvrnorm(100000,matrix(0,nrow=t, ncol=1,),make.positive.definite(rho))
                ptile<-quantile(apply(tmp, 2, max), .975)
                return(ptile)
            }
            
            n.vals <- seq(input$slider2[1],input$slider2[2], by=input$by2)
            mofe.t<-data.frame(t(rbind(n.vals, sapply(n.vals, mofe.t, ddss=d.Sigma.Within))))
            mofe_t <- data.frame(cbind(mofe.t[1],mofe.t[2]))
            colnames(mofe_t) <- c("Number of Trials","Margin of Error")
            rownames(mofe_t) <- NULL
            kable(mofe_t) %>%
                kable_styling(full_width = F)
        }
    }
    
    output$results4 <- function(){
        if (((is.null(input$numtime[1]) == TRUE) & (is.null(input$numtime[2]) == TRUE))) {
            if((input$uploadType==".csv" | input$uploadType==".xls")){
                dataset <- as.data.frame(raw())
                #Renaming columns from V1 - VX (where X = total number of columns in dataset)
                col.tot <- ncol(dataset)
                colnames(dataset) <- paste("V", 1:col.tot, sep = "")
                num.of.trials <- dataset %>% count(V1)
                num.of.trials <- as.vector(num.of.trials[,2])
                #Getting numerator for the pooled within-subject covariance matrix
                Subs<-unique(dataset$V1)
                listofsub <- vector("list", length(Subs))
                for (i in 1:length(Subs)){ 
                    listofsub[[i]] <- as.data.frame(dataset[which(dataset$V1==i),-1]) 
                }
                Cov <- vector("list", length(listofsub))
                for (i in 1:length(listofsub)){
                    Cov[[i]] <- (num.of.trials[i])*cov(as.data.frame(listofsub[[i]]),use="pairwise.complete.obs")
                }
                cov.numer <- 0
                for (i in 1:length(listofsub)){
                    if(as.numeric(length(is.na.data.frame(Cov[[i]])[is.na.data.frame(Cov[[i]]) == FALSE]))){
                        cov.numer <- cov.numer+Cov[[i]]
                    }
                }
                #Getting denominator
                denom <- 0
                for (i in 1:length(listofsub))
                {
                    if(as.numeric(length(is.na.data.frame(Cov[[i]])[is.na.data.frame(Cov[[i]]) == FALSE]))){
                        denom <- denom + num.of.trials[i]
                    }
                }
                Sigma.Within <- cov.numer/denom
                d.Sigma.Within <- diag(Sigma.Within)
                
                #Getting the within-subject correlation matrix
                #Getting the numerator
                Cor <- vector("list", length(listofsub))
                for (i in 1:length(listofsub)){
                    Cor[[i]] <- (num.of.trials[i])*cor(as.data.frame(listofsub[[i]]),use="pairwise.complete.obs")
                }
                cor.numer <- 0
                for (i in 1:length(listofsub)){
                    if(as.numeric(length(is.na.data.frame(Cor[[i]])[is.na.data.frame(Cor[[i]]) == FALSE]))){
                        cor.numer <- cor.numer+Cor[[i]]
                    }
                }
                Rho.Within <-cor.numer/denom
                
                ## function to get the empirical cut-off
                em.cutoff<- function(rho,t){
                    tmp<-mvrnorm(100000,matrix(0,nrow=t, ncol=1), make.positive.definite(rho))
                    ptile<-quantile(apply(tmp, 2, max), .975)
                    return(ptile)
                }
                ## function to compute the band-based margin of error
                mofe.band <- function(nn, ddss, pptt){
                    pptt*sqrt(max(ddss, na.rm=T)/nn)
                }
                #
                ## function to find the needed n
                n.need.band <- function(ddss, pptt,tol){
                    n.curr = 1
                    mofe.curr = pptt*sqrt(max(ddss, na.rm=T))
                    while(mofe.curr > tol){
                        n.curr=n.curr+1
                        mofe.curr = mofe.band(n.curr,ddss,pptt)
                    }
                    list(n=n.curr, mofe=mofe.curr)
                }
                
                Rho.Within <- as.data.frame(Rho.Within)
                # Removing NA rows and NA columns to get square matrix
                narows <- Rho.Within[rowSums(is.na(Rho.Within)) != ncol(Rho.Within), ]
                Rho.Within.noNA <- narows[,colSums(is.na(narows)) != nrow(narows)]
                ptile <- em.cutoff(Rho.Within.noNA, ncol(Rho.Within.noNA))
                
                n.vals <- seq(input$slider2[1],input$slider2[2], by=input$by2)
                mofe.band <- data.frame(t(rbind(n.vals, sapply(n.vals, mofe.band, ddss=d.Sigma.Within,pptt=ptile))))
                mofe_band <- data.frame(cbind(mofe.band[1],mofe.band[2]))
                colnames(mofe_band) <- c("Number of Trials","Margin of Error")
                rownames(mofe_band) <- NULL
                kable(mofe_band) %>%
                    kable_styling(full_width = F)
            }
        }
        else {
            if((input$uploadType==".csv" | input$uploadType==".xls")){
                dataset <- as.data.frame(raw())
                #Renaming columns from V1 - VX (where X = total number of columns in dataset)
                col.tot <- ncol(dataset)
                colnames(dataset) <- paste("V", 1:col.tot, sep = "")
                num.of.trials <- dataset %>% count(V1)
                num.of.trials <- as.vector(num.of.trials[,2])
                
                Subs<-unique(dataset$V1)
                listofsub <- vector("list", length(Subs))
                for (i in 1:length(Subs)){ 
                    listofsub[[i]] <- as.data.frame(dataset[which(dataset$V1==i),-1]) 
                }
                Cov <- vector("list", length(listofsub))
                for (i in 1:length(listofsub)){
                    Cov[[i]] <- (num.of.trials[i])*cov(as.data.frame(listofsub[[i]] %>% dplyr::select(input$numtime[1]:input$numtime[2])),use="pairwise.complete.obs")
                }
                cov.numer <- 0
                for (i in 1:length(listofsub)){
                    if(as.numeric(length(is.na.data.frame(Cov[[i]])[is.na.data.frame(Cov[[i]]) == FALSE]))){
                        cov.numer <- cov.numer+Cov[[i]]
                    }
                }
                #Getting denominator
                denom <- 0
                for (i in 1:length(listofsub))
                {
                    if(as.numeric(length(is.na.data.frame(Cov[[i]])[is.na.data.frame(Cov[[i]]) == FALSE]))){
                        denom <- denom + num.of.trials[i]
                    }
                }
                Sigma.Within <- cov.numer/denom
                d.Sigma.Within <- diag(Sigma.Within)
                
                #Getting the within-subject correlation matrix
                #Getting the numerator
                Cor <- vector("list", length(listofsub))
                for (i in 1:length(listofsub)){
                    Cor[[i]] <- (num.of.trials[i])*cor(as.data.frame(listofsub[[i]]) %>% dplyr::select(input$numtime[1]:input$numtime[2]),use="pairwise.complete.obs")
                }
                cor.numer <- 0
                for (i in 1:length(listofsub)){
                    if(as.numeric(length(is.na.data.frame(Cor[[i]])[is.na.data.frame(Cor[[i]]) == FALSE]))){
                        cor.numer <- cor.numer+Cor[[i]]
                    }
                }
                Rho.Within <-cor.numer/denom
                
                ## function to get the empirical cut-off
                em.cutoff<- function(rho,t){
                    tmp<-mvrnorm(100000,matrix(0,nrow=t, ncol=1,), make.positive.definite(rho))
                    ptile<-quantile(apply(tmp, 2, max), .975)
                    return(ptile)
                }
                ## function to compute the band-based margin of error
                mofe.band <- function(nn, ddss, pptt){
                    pptt*sqrt(max(ddss, na.rm=T)/nn)
                }
                #
                ## function to find the needed n
                n.need.band <- function(ddss, pptt,tol){
                    n.curr = 1
                    mofe.curr = pptt*sqrt(max(ddss, na.rm=T))
                    while(mofe.curr > tol){
                        n.curr=n.curr+1
                        mofe.curr = mofe.band(n.curr,ddss,pptt)
                    }
                    list(n=n.curr, mofe=mofe.curr)
                }
                
                Rho.Within <- as.data.frame(Rho.Within)
                # Removing NA rows and NA columns to get square matrix
                narows <- Rho.Within[rowSums(is.na(Rho.Within)) != ncol(Rho.Within), ]
                Rho.Within.noNA <- narows[,colSums(is.na(narows)) != nrow(narows)]
                ptile <- em.cutoff(Rho.Within.noNA, ncol(Rho.Within.noNA))
                
                n.vals <- input$slider2[1]:input$slider2[2]
                mofe.band <- data.frame(t(rbind(n.vals, sapply(n.vals, mofe.band, ddss=d.Sigma.Within,pptt=ptile))))
                mofe_band <- data.frame(cbind(mofe.band[1],mofe.band[2]))
                colnames(mofe_band) <- c("Number of Trials","Margin of Error")
                rownames(mofe_band) <- NULL
                kable(mofe_band) %>%
                    kable_styling(full_width = F)
            }
        }
    }
}
shinyApp(ui = ui, server = server)