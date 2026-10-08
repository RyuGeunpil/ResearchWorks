getwd()
setwd("D:/Data_Analytics/RProject/Coaching")

library(tidyverse)
library(readxl)
coach_data <- read_excel("coaching.xlsx")
str(coach_data)
colnames(coach_data)[1] <- "id"
str(coach_data)

#Variable Renames
coach_data <- coach_data %>%
  rename(
    job_duration  = `job duration_Q70`,
    manager       = `job honor_Q71`
  )


str(coach_data)

coaching_data2 <- coach_data %>%
  select(
    coaching_Q1:coaching_Q6,
    coaching_Q7:coaching_Q11,
    coaching_Q12:coaching_Q14,
    coaching_Q17:coaching_Q19,
    cogtrust_Q20:cogtrust_Q24,
    emotrust_Q26:emotrust_Q28,
    starts_with("unpredict"),
    starts_with("workload"),
    starts_with("timepress"),
    male_Q66,
    age_Q67,
    education_Q68,
    job_duration,
    manager
  )

#variable recoding
coaching_data2 <- coaching_data2 %>%
  mutate(
    manager_dummy = case_when(
      manager == 1 ~ 1,   # 관리자 '1'
      manager == 2 ~ 0,   # 비관리자 '0'
      TRUE ~ NA_real_
    )
  )


#cronbach's alpah
library(psych)
library(tidyverse)
coaching_res <- coaching_data2 %>%
  select(coaching_Q1:coaching_Q6)
coaching_cp <- coaching_data2 %>%
  select(coaching_Q12:coaching_Q14)
coaching_bf <- coaching_data2 %>%
  select(coaching_Q17:coaching_Q19)
cog_trst <- coaching_data2 %>%
  select(starts_with("cog"))
emo_trst <- coaching_data2 %>%
  select(starts_with("emo"))
con_unpre <- coaching_data2 %>%
  select(starts_with("unpredict"))
con_wl <- coaching_data2 %>%
  select(starts_with("workload"))

psych::alpha(coaching_res)
psych::alpha(coaching_cp)
psych::alpha(coaching_bf)
psych::alpha(cog_trst)
psych::alpha(emo_trst)
psych::alpha(con_unpre)
psych::alpha(con_wl)

#explanatory factor analysis
coaching2 <- coaching_data2 %>%
  select(
    coaching_Q1:coaching_Q6, 
    coaching_Q12:coaching_Q14,
    coaching_Q17:coaching_Q19,
    starts_with("cog"),
    starts_with("emo"))

exfac1 <-data.frame(coaching2)
exfa <- fa(exfac1, rotate = "varimax") #checking out number of 
exfa$e.values
#checking eigenvaluesa
exfa1 <- fa(exfac1, nfactors = 5, rotate = "varimax") #checking out initial number of factors
summary(exfa1)
exfa1$e.values
exfa1$loadings[]
write.csv(loadings(exfa1), 'factorloadings3.csv')
variance_table <- as.data.frame(t(exfa1$Vaccounted))
variance_table

#EFA 분석결과 아래와 같이 최종 변수 도출#
# 코칭 존중: 1~6
# 코칭 관점변화" 12~14
# 코칭 믿음: 17~19
#인지적 신뢰: Q20~Q24
#감정적 신뢰: Q26~28

#confirmatory factor analysis
library(lavaan)
library(tidyverse)
library(semTools)
library(semPlot)
library(stargazer)
library(stargazer)
library(tibble)

#null model
cfa.model0 <- '
FA1 =~ coaching_Q1 + coaching_Q2 + coaching_Q3 + coaching_Q4 + coaching_Q5 + coaching_Q6 
      + coaching_Q12 + coaching_Q13 + coaching_Q14 
      + coaching_Q17 + coaching_Q18 + coaching_Q19 
      + cogtrust_Q20 + cogtrust_Q21 + cogtrust_Q22 + cogtrust_Q23 + cogtrust_Q24   
      + emotrust_Q26 + emotrust_Q27 + emotrust_Q28
'
null0 <- cfa(cfa.model0, data=coaching_data2)
summary(null0, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE)
fitmeasures(null0, c("npar", "chisq", "df", "cfi", "rmsea", "srmr"))
semPaths(null0, whatLabels = "std", style = "lisrel", nCharNodes = 0)

#expected model
cfa.model3 <- '
RSPT =~ coaching_Q1 + coaching_Q2 + coaching_Q3 + coaching_Q4 + coaching_Q5 + coaching_Q6 
CP =~ coaching_Q12 + coaching_Q13 + coaching_Q14
BF =~  coaching_Q17 + coaching_Q18 + coaching_Q19
GT =~  cogtrust_Q20 + cogtrust_Q21 + cogtrust_Q22 + cogtrust_Q23 + cogtrust_Q24   
ET =~  emotrust_Q26 + emotrust_Q27 + emotrust_Q28 
'
mod3 <- cfa(cfa.model3, data=coaching_data2)
summary(mod3, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE)
fitmeasures(mod3, c("npar", "chisq", "df", "cfi", "rmsea", "srmr"))
parameterEstimates(mod3, standardized = T)
modificationIndices(mod3, sort=TRUE)
lavInspect(mod3, what = "rsquare")
dev.new(width = 12, height = 10)
semPaths(mod3, what = "std", layout = "tree2", edge.label.cex = 0.7, edge.color = "royalblue",
         color = list(lat="lightcoral", man="lavenderblush"), fade=F,
         style = "lisrel", curvature = 2)


#Dimension Reduction using Means

coaching_data2 <- coaching_data2 %>%
  mutate(
    # 코칭리더십
    m_coach_resp = rowMeans(across(coaching_Q1:coaching_Q6), na.rm = TRUE),
    m_coach_feedback = rowMeans(across(c(coaching_Q8, coaching_Q11)), na.rm = TRUE),
    m_coach_CP = rowMeans(across(coaching_Q12:coaching_Q14), na.rm = TRUE),
    m_coach_growth = rowMeans(across(coaching_Q17:coaching_Q19), na.rm = TRUE),
    
    # 신뢰
    m_cogtrust = rowMeans(across(cogtrust_Q20:cogtrust_Q24), na.rm = TRUE),
    m_emotrust = rowMeans(across(emotrust_Q26:emotrust_Q28), na.rm = TRUE),
    
    # 상황요인
    m_sit_unpredict = rowMeans(across(starts_with("unpredict")), na.rm = TRUE),
    m_sit_workload = rowMeans(across(starts_with("work")), na.rm = TRUE),
    m_sit_timepressure = rowMeans(across(starts_with("time")), na.rm = TRUE)
  )

coaching_data2 %>%
  summarise(
    across(
      c(
        male_Q66,
        age_Q67,
        education_Q68,
        job_duration,
        manager_dummy,
        m_coach_resp,
        m_coach_feedback,
        m_coach_CP,
        m_coach_growth,
        m_sit_unpredict,
        m_sit_workload,
        m_sit_timepressure,
        m_cogtrust,
        m_emotrust
      ),
      list(
        Mean = ~mean(.x, na.rm = TRUE),
        SD = ~sd(.x, na.rm = TRUE)
      )
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", ".value"),
    names_pattern = "(.+)_(Mean|SD)$"
  )

#correlation analysis
library(haven)
library(Hmisc)
library(psych)

#using basic function
cordata <- coaching_data2 %>%
  select(male_Q66,
         age_Q67,
         education_Q68,
         job_duration,
         manager,
         m_coach_resp,
         m_coach_feedback,
         m_coach_CP,
         m_coach_growth,
         m_sit_unpredict,
         m_sit_workload,
         m_sit_timepressure,
         m_cogtrust,
         m_emotrust)

cor_result <- corr.test(
  cordata,
  method = "pearson",
  use = "pairwise"
)

# 상관계수
cor_result$r

# p-value
cor_result$p

# 표본수
cor_result$n

write.csv(cor_result$r, 'correlation4.csv') #r value saving


library(psych)

cor_result <- corr.test(
  cordata,
  method = "pearson",
  use = "pairwise"
)

r <- cor_result$r
p <- cor_result$p

# p-value에 따른 별표 추가
r_p <- matrix("", nrow = nrow(r), ncol = ncol(r))
rownames(r_p) <- rownames(r)
colnames(r_p) <- colnames(r)

for(i in 1:nrow(r)) {
  for(j in 1:ncol(r)) {
    
    stars <- ifelse(p[i,j] < .001, "***",
                    ifelse(p[i,j] < .01, "**",
                           ifelse(p[i,j] < .05, "*", "")))
    
    r_p[i,j] <- paste0(
      sprintf("%.3f", r[i,j]),
      stars
    )
  }
}

write.csv(r_p, "correlation4.csv")


#Mean_Centering Method
centering <- c(
  "m_coach_resp",
  "m_coach_feedback",
  "m_coach_CP",
  "m_coach_growth",
  "m_sit_unpredict",
  "m_sit_workload",
  "m_sit_timepressure",
  "m_cogtrust",
  "m_emotrust" 
)
coaching_data2 <- coaching_data2 %>%
  mutate(
    across(
      all_of(centering),
      ~ .x - mean(.x, na.rm = TRUE),
      .names = "{.col}_c"
    )
  )

str(coaching_data2)

#regression model result for CogTrust
#only control variables
model1 <- lm(m_cogtrust_c ~ male_Q66 + age_Q67 + education_Q68 + job_duration + manager_dummy, data = coaching_data2)
summary(model1)

#including independent variables for CogTrst
model2 <- lm(m_cogtrust_c ~ male_Q66 + age_Q67 + education_Q68 + job_duration + manager_dummy +
             m_coach_resp_c + m_coach_CP_c + m_coach_growth_c, data = coaching_data2)
summary(model2)

#including moderator variables for CogTrst
model3 <- lm(m_cogtrust_c ~ male_Q66 + age_Q67 + education_Q68 + job_duration + manager_dummy +
               m_coach_resp_c + m_coach_CP_c + m_coach_growth_c +
               m_sit_unpredict_c + m_sit_workload_c, data = coaching_data2)
summary(model3)

#including interaction terms for CogTrst
model4 <- lm(m_cogtrust_c ~ male_Q66 + age_Q67 + education_Q68 + job_duration + manager_dummy +
             m_coach_resp_c + m_coach_CP_c + m_coach_growth_c +
             m_sit_unpredict_c + m_sit_workload_c +
             m_coach_resp_c:m_sit_unpredict_c  + m_coach_CP_c:m_sit_unpredict_c + m_coach_growth_c:m_sit_unpredict_c +
             m_coach_resp_c:m_sit_workload_c + m_coach_CP_c:m_sit_workload_c + m_coach_growth_c:m_sit_workload_c, data = coaching_data2) 
summary(model4)


#regression model result for EmoTrust
model5 <- lm(m_emotrust_c ~ male_Q66 + age_Q67 + education_Q68 + job_duration + manager_dummy, data = coaching_data2)
summary(model5)

#including independent variables for EmoTrust
model6 <- lm(m_emotrust_c ~ male_Q66 + age_Q67 + education_Q68 + job_duration + manager_dummy +
               m_coach_resp_c + m_coach_CP_c + m_coach_growth_c, data = coaching_data2)
summary(model6)

#including moderator variables for EmoTrust
model7 <- lm(m_emotrust_c ~ male_Q66 + age_Q67 + education_Q68 + job_duration + manager_dummy +
               m_coach_resp_c + m_coach_CP_c + m_coach_growth_c +
               m_sit_unpredict_c + m_sit_workload_c, data = coaching_data2)
summary(model7)

#including interaction terms for EmoTrust
model8 <- lm(m_emotrust_c ~ male_Q66 + age_Q67 + education_Q68 + job_duration + manager_dummy +
               m_coach_resp_c + m_coach_CP_c + m_coach_growth_c +
               m_sit_unpredict_c + m_sit_workload_c +
               m_coach_resp_c:m_sit_unpredict_c  + m_coach_CP_c:m_sit_unpredict_c + m_coach_growth_c:m_sit_unpredict_c +
               m_coach_resp_c:m_sit_workload_c + m_coach_CP_c:m_sit_workload_c + m_coach_growth_c:m_sit_workload_c, data = coaching_data2) 
summary(model8)

##Regression Diagnosis##
library(car)
library(lmtest)
library(sandwich)
library(lmtest)

vif(model4) # for Multicollinearity
vif(model8) # for Multicollinearity

bptest(model4) #studentized Breusch-Pagan test
bptest(model8) #studentized Breusch-Pagan test

robust_se <- list(
  sqrt(diag(vcovHC(model1, type = "HC3"))),
  sqrt(diag(vcovHC(model2, type = "HC3"))),
  sqrt(diag(vcovHC(model3, type = "HC3"))),
  sqrt(diag(vcovHC(model4, type = "HC3"))),
  sqrt(diag(vcovHC(model5, type = "HC3"))),
  sqrt(diag(vcovHC(model6, type = "HC3"))),
  sqrt(diag(vcovHC(model7, type = "HC3"))),
  sqrt(diag(vcovHC(model8, type = "HC3")))
)

##Making Tables
library(dplyr)
library(stargazer)
library(tibble)
stargazer(
  model1, model2, model3, model4,
  se = robust_se,
  type = "text",
  title = "Regression Results for Cognitive Trust with Robust SE",
  dep.var.labels = "Cognitive Trust",
  column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
  
  # 절편을 맨 위에 배치하여 covariate.labels의 첫번째("Intercept")와 순서를 맞춤
  intercept.bottom = FALSE,
  
  # 전체 변수 개수(절편 포함 17개)와 정확히 일치해야 함
  covariate.labels = c(
    "Constant",
    "Male",
    "Age",
    "Education",
    "Job Duration",
    "Manager",
    "Coaching Resp.",
    "Coaching Questioning",
    "Coaching Growth",
    "Unpredictability",
    "Workload",
    "Coaching Resp. x Unpredictability",
    "Coaching Questioning x Unpredictability",
    "Coaching Growth x Unpredictability",
    "Coaching Resp. x Workload",
    "Coaching Questioning x Workload",
    "Coaching Growth x Workload"
  ),
  omit.stat = c("f", "ser"),
  digits = 3,
  star.cutoffs = c(0.1, 0.05, 0.01),
  notes = "Robust standard errors are in parentheses. * p < .10, ** p < .05, *** p < .01",
  notes.append = FALSE,                     # 기존 기호 설명 중복 방지
  out = "CogTrst_regression_results.html"
)

#making a table
stargazer(
  model5, model6, model7, model8,
  se = robust_se,
  type = "text",
  title = "Regression Results for Emotional Trust with Robust SE",
  dep.var.labels = "Emotional Trust",
  column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
  
  # 절편을 맨 위에 배치하여 covariate.labels의 첫번째("Intercept")와 순서를 맞춤
  intercept.bottom = FALSE,
  
  # 전체 변수 개수(절편 포함 17개)와 정확히 일치해야 함
  covariate.labels = c(
    "Constant",
    "Male",
    "Age",
    "Education",
    "Job Duration",
    "Manager",
    "Coaching Resp.",
    "Coaching Questioning",
    "Coaching Growth",
    "Unpredictability",
    "Workload",
    "Coaching Resp. x Unpredictability",
    "Coaching Questioning x Unpredictability",
    "Coaching Growth x Unpredictability",
    "Coaching Resp. x Workload",
    "Coaching Questioning x Workload",
    "Coaching Growth x Workload"
  ),
  omit.stat = c("f", "ser"),
  digits = 3,
  star.cutoffs = c(0.1, 0.05, 0.01),
  notes = "Robust standard errors are in parentheses. * p < .10, ** p < .05, *** p < .01",
  notes.append = FALSE,                     # 기존 기호 설명 중복 방지
  out = "Emotional_regression_results.html"
)

##Interaction Effect Visualization
library(ggplot2)
library(grid)

# ============================================================
# H4-3a. 예측불가능성이 조절하는 코칭리더십 반응성 → 정서적 신뢰
# ============================================================

# 1. model8에서 회귀계수 확인
summary(model8)

# ------------------------------------------------------------
# 2. 조절변수(예측불가능성)의 SD 계산
# ------------------------------------------------------------

unpred_mean <- mean(coaching_data2$m_sit_unpredict_c, na.rm = TRUE)
unpred_sd   <- sd(coaching_data2$m_sit_unpredict_c, na.rm = TRUE)

# -1SD / 평균 / +1SD
unpred_values <- c(
  unpred_mean - unpred_sd,
  unpred_mean,
  unpred_mean + unpred_sd
)

# ------------------------------------------------------------
# 3. 예측값 계산을 위한 데이터 생성
# ------------------------------------------------------------

# X축: 코칭리더십 반응성
# -1SD ~ +1SD 범위
resp_mean <- mean(coaching_data2$m_coach_resp_c, na.rm = TRUE)
resp_sd   <- sd(coaching_data2$m_coach_resp_c, na.rm = TRUE)

resp_range <- seq(
  from = resp_mean - resp_sd,
  to   = resp_mean + resp_sd,
  length.out = 100
)

# 각 조절수준별 데이터 생성
newdata <- expand.grid(
  m_coach_resp_c = resp_range,
  m_sit_unpredict_c = unpred_values
)

# ------------------------------------------------------------
# 4. 다른 변수들은 평균값으로 고정
# ------------------------------------------------------------
newdata <- expand.grid(
  m_coach_resp_c = resp_range,
  m_sit_unpredict_c = unpred_values
)

# 통제변수: 평균값으로 고정
newdata$male_Q66 <- mean(
  coaching_data2$male_Q66, na.rm = TRUE
)

newdata$age_Q67 <- mean(
  coaching_data2$age_Q67, na.rm = TRUE
)

newdata$education_Q68 <- mean(
  coaching_data2$education_Q68, na.rm = TRUE
)

newdata$job_duration <- mean(
  coaching_data2$job_duration, na.rm = TRUE
)

newdata$manager_dummy <- mean(
  coaching_data2$manager_dummy, na.rm = TRUE
)

# 나머지 독립변수: 평균값으로 고정
newdata$m_coach_CP_c <- mean(
  coaching_data2$m_coach_CP_c, na.rm = TRUE
)

newdata$m_coach_growth_c <- mean(
  coaching_data2$m_coach_growth_c, na.rm = TRUE
)

# 다른 조절변수: 평균값으로 고정
newdata$m_sit_workload_c <- mean(
  coaching_data2$m_sit_workload_c, na.rm = TRUE
)

# ------------------------------------------------------------
# 5. 예측값 계산
# ------------------------------------------------------------

newdata$predicted <- predict(
  model8,
  newdata = newdata
)

# ------------------------------------------------------------
# 6. 예측불가능성 수준을 명칭으로 변환
# ------------------------------------------------------------

newdata$unpredict_level <- factor(
  newdata$m_sit_unpredict_c,
  levels = unpred_values,
  labels = c(
    "저 예측불가능성 (-1SD)",
    "평균 예측불가능성 (M)",
    "고 예측불가능성 (+1SD)"
  )
)

# ------------------------------------------------------------
# 7. 그래프
# ------------------------------------------------------------

p <- ggplot(
  newdata,
  aes(
    x = m_coach_resp_c,
    y = predicted,
    group = unpredict_level
  )
) +
  
  # 선
  geom_line(
    aes(linetype = unpredict_level),
    color = "black",
    linewidth = 1
  ) +
  
  # X축
  scale_x_continuous(
    breaks = c(
      resp_mean - resp_sd,
      resp_mean,
      resp_mean + resp_sd
    ),
    labels = c(
      "저\n(-1SD)",
      "평균\n(M)",
      "고\n(+1SD)"
    )
  ) +
  
  # Y축
  scale_y_continuous(
    breaks = NULL
  ) +
  
  # 선 종류
  scale_linetype_manual(
    values = c(
      "저 예측불가능성 (-1SD)" = "solid",
      "평균 예측불가능성 (M)" = "dotted",
      "고 예측불가능성 (+1SD)" = "dotdash"
    )
  ) +
  
  labs(
    title = ,
    x = "코칭리더십(신뢰)",
    y = "정서적 신뢰",
    linetype = NULL
  ) +
  
  theme_classic(base_size = 18) +
  
  theme(
    # 전체 배경
    plot.background = element_rect(
      fill = "gray95",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    # 제목
    plot.title = element_text(
      face = "bold",
      hjust = 0,
      size = 20,
      margin = margin(b = 6)
    ),
    
    plot.subtitle = element_text(
      hjust = 0,
      size = 16,
      margin = margin(b = 18)
    ),
    
    # 축 제목
    axis.title.x = element_text(
      size = 18,
      margin = margin(t = 18)
    ),
    
    axis.title.y = element_text(
      size = 18,
      margin = margin(r = 20)
    ),
    
    # 축 글자
    axis.text.x = element_text(
      size = 16,
      lineheight = 1.1
    ),
    
    axis.text.y = element_blank(),
    
    # 축 선
    axis.line = element_line(
      color = "black",
      linewidth = 1
    ),
    
    axis.ticks = element_blank(),
    
    # 범례
    legend.position = c(0.62, 0.18),
    
    legend.justification = c(0, 0),
    
    legend.background = element_rect(
      fill = "white",
      color = "gray70",
      linewidth = 0.8
    ),
    
    legend.key = element_rect(
      fill = "white",
      color = NA
    ),
    
    legend.text = element_text(
      size = 16
    ),
    
    # 주석
    plot.caption = element_text(
      hjust = 0,
      size = 13,
      margin = margin(t = 24)
    ),
    
    # 여백
    plot.margin = margin(
      30, 80, 40, 50
    )
  ) +
  
  guides(
    linetype = guide_legend(
      override.aes = list(
        color = "black",
        linewidth = 1.2
      )
    )
  )

# ------------------------------------------------------------
# 8. 그래프 출력
# ------------------------------------------------------------

print(p)

# ------------------------------------------------------------
# 9. 파일 저장 
# ------------------------------------------------------------
ggsave(
  filename = "D:/Data_Analytics/RProject/Coaching/interaction.png",
  plot = p,
  width = 13,
  height = 8,
  dpi = 300,
  bg = "gray95"
)
