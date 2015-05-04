prod <- read.csv("~/prod.data.csv")%>%
  dplyr::select(Time.Stamp:D_UV2FLOW_ACC)%>%
  rename(Time = Time.Stamp)%>%
  mutate_each(., funs(.-lag(.)), -Time)%>%
  mutate_each(., funs(ifelse(. < 0, 0,.)), -Time)%>%
  mutate_each(., funs(ifelse(. == max(., na.rm = TRUE), 0,.)), -Time)%>%
  slice(-1)%>%
  gather(meter, flow, -Time)%>%
  mutate(Time = mdy_hm(Time),
         time = floor_date(Time, "day"))%>%
  dplyr::select(-Time)%>%
  group_by(time)%>%
  summarise(flow = sum(flow, na.rm = TRUE))


devtools::use_data(prod, overwrite = TRUE)

model_d_h(production, energy)

#===========Data Cleaning ==========
energy <- read_csv("~/Documents/data sets/energy.data.full.csv")%>%
  mutate(Time = mdy_hm(`Time Stamp`))%>%
  dplyr::select(-`Time Stamp`)%>%
  mutate_each(., funs(.-lag(.)), -Time)%>%#hour cutter
  mutate_each(., funs(ifelse(. == max(., na.rm = TRUE), 0,.)), -Time)%>%
  mutate(bin = cut(Time, "hour"))%>%
  dplyr::select(-Time)%>%
  group_by(bin)%>%
  summarise_each(., funs(sum(., na.rm = TRUE)))%>%
  mutate(Time = ymd_hms(bin))%>%
  dplyr::select(-bin)

#==== Data as a percent
energy.nm <- energy%>%
  mutate_each(., funs(./max(.)), -Time)#Each column as a percent of max reading

#====Plotting===
energy.nm%>%
  gather(meter, load_percent, BM_KWH1_RAW:WTP_KWH2)%>%
  mutate(Day = ymd(paste(year(Time), month(Time), day(Time))),
         Hour = hour(Time),
         load_percent = ifelse(load_percent<0, 0, load_percent))%>%
  ggplot(aes(x=Day, y = Hour))+
  geom_tile(aes(fill = load_percent))+
  facet_grid(meter ~.)+
  theme_bw()+
  scale_fill_gradient2(low = "#ffffd9",
                       high = "#081d58")

#---Lazy Eval


lazy_eval(Time, energy)

subset2_ <- function(df, condition) {
  r <- lazy_eval(condition, df)
  r <- r & !is.na(r)
  df[r, , drop = FALSE]
}

m <- function(df, var.name){
  lazy(var.name)

  #df%>%
  #  dplyr::select(paste(var.name))

}
