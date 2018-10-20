#
 # library(dplyr)
 # library(ggplot2)
 # library(ggformula)
 # library(readr)
 # library(ggpmisc)

Pitching_2018 <- read_csv("~/Desktop/Coding/StatCast_Data/PITCHING/2018/FanGraph_Pitching_2018.csv")


# Adding "Pitcher_Type" to Pitching_2018 df
FlyBall <- Pitching_2018 %>%
            filter(FlyBall_per > .45)%>%
            mutate(Pitcher_Type = "FlyBall")
GroundBall <- Pitching_2018 %>%
            filter(GB_per > .45)%>%
            mutate(Pitcher_Type = "GroundBall")
Neither  <- Pitching_2018 %>%
            filter(GB_per < .45 & FlyBall_per < .45)%>%
            mutate(Pitcher_Type = "Mixed")
FBvGB <- full_join(FlyBall, GroundBall, by = c())
FBvGB <- full_join(FBvGB, Neither, by=c())



###################

# Graph of Swinging Strike Percentage v. xFIP, grouped by Pitcher type .

FBvGB_SSP <- FBvGB %>%
        filter(IP > 80)


xFIPvSSP <-gf_point(xFIP ~ SwStrike_per,
               data = FBvGB_SSP,
               color = ~ Pitcher_Type) %>%
  gf_lm(xFIP ~ SwStrike_per,
        data = FBvGB_SSP,
        color = ~Pitcher_Type ) +
  stat_poly_eq(data = FBvGB_SSP,
               formula = y~x,
               eq.with.lhs = "italic(y) ~`=`~",
               eq.x.rhs = "~italic(x)",
               aes(label= paste(..eq.label.., ..rr.label.., sep = "~~~")),
               label.x.npc = "right",
               label.y.npc = "top",
               parse = TRUE) +
  labs(x = expression(italic("Swinging Strike Percentage ")),
       y = expression(italic("xFIP")),
       title = "xFIP ~ Swinging Strike Percentage")
xFIPvSSP

model <- lm(xFIP ~ SwStrike_per, data = FBvGB_SSP)
anova(model)
