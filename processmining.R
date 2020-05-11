library(bupaR)
library(openxlsx)
library(dplyr)
library(processanimateR)
library(heuristicsmineR)
library(petrinetR)
library(lubridate)

filename<-"besteladviezen.xlsx"

read_file<-TRUE

if (read_file) {
eventlog<-read.xlsx(filename,sheet=1,detectDates=TRUE,colNames=TRUE)

eventlog$TimeStamp<-as.POSIXct(eventlog$TimeStamp, tz = "", format="%d/%m/%y %H:%M:%OS" ,
           tryFormats = c("%Y-%m-%d %H:%M:%OS",
                          "%Y/%m/%d %H:%M:%OS",
                          "%d/%m/%Y %H:%M:%OS",
                          "%Y-%m-%d %H:%M",
                          "%Y/%m/%d %H:%M",
                          "%Y-%m-%d",
                          "%Y/%m/%d"),
           optional = FALSE)
}

evLog<-eventlog %>%
mutate(activity_instance = 1:nrow(.)) %>%
  eventlog(
    case_id = "CaseID",
    activity_id = "Activity",
    lifecycle_id = "status",
    activity_instance_id = "activity_instance",
    timestamp = "TimeStamp",
    resource_id = "Approver"
  )


# filter on year
evLog<-evLog[evLog$Datum==2014,]

# filter on begin and end activities
evLog <- evLog %>%
  filter_endpoints(start_activities = c("BA maken","BA-regel maken"), end_activities = c("IO maken","IO-regel maken"))

# filter on timestamp for the starting activity
# evLog <- evLog %>%
# filter_time_period(interval = ymd(c(20140115, 20140120)), filter_method = "start")

evLog %>%  
  dotted_chart


# dependency matrix with threshold
dependency_matrix(evLog, threshold = .5) %>% render_dependency_matrix()

# causal net with threshold
causal_net(evLog, threshold = .7) %>% render_causal_net()

# Efficient precedence matrix
m <- precedence_matrix_absolute(evLog)
as.matrix(m)



evLog %>%
  process_map(type = frequency("relative"))

evLog %>%
  process_map(type = frequency("absolute"))

evLog %>%
  process_map(type = frequency("relative_case", color_scale = "Purples"))

evLog %>%
  process_map(performance(median, "days"))

evLog %>%
  process_map(performance(mean, "hours"))

evLog %>%
  process_map(type_nodes = frequency("relative_case"),
              type_edges = performance(mean))



evLog %>%
  trace_explorer()

evLog<-evLog %>%
  # filter_activity(c("LacticAcid", "CRP", "Leucocytes", "Return ER", "IV Liquid", "IV Antibiotics"), reverse = T) %>%
  filter_trace_frequency(percentage = 0.60)

# animate log on process map
# animate_process(evLog %>% filter_trace_frequency(percentage = 0.50),
#                 mode = "relative",
#                 legend = "CaseId", 
#                 mapping = token_aes(color = token_scale("amount", 
#                                                         scale = "linear", 
#                                                         range = c("yellow","red"))))

citation("processmapR")


