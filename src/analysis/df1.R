df1 <- read.csv("data_frame_1.csv")
affected1 <- df1$affected>0


ggplot(df1, aes(x=th_initiated, color=affected1, fill=affected1))+
  geom_histogram(position = "stack")+
  geom_histogram(position="identity")+
  facet_grid(severity~birth_place)



