#versao 04/09/2025
# Bibliotecas -------------------------------------------------------------

library(tidyverse)
library(readr)
library(glmmTMB)
library(performance)
library(DHARMa)
library(car)
library(rstatix)
library(sjPlot)
library(ggeffects)
library(dplyr)
library(readxl)
library(patchwork)
library(emmeans)
library(forageR)
library(MuMIn)

# Importação dos dados ----------------------------------------------------

df <- read_csv("Resultados_oficial_observacao_comportamento_animalta - Results_by_ind.csv")
str(df)

#Correção dos fatores
df$Individual <- factor(df$Individual)
df$Group <- factor(df$Group, levels = c('CTR','DEL','IMI','COM'))
df$Sequence <- factor(df$Sequence)
df$REP <- factor(df$REP)

#Df General
df_general <- df %>% 
  filter (Sequence == 'General')

df_general.fil <- df %>% 
  filter (Sequence == 'General') %>% 
  filter(Meander < 999999)


medias <- df_general %>%
  group_by (Group) %>% 
  summarise (media_traveled = mean(Traveled_Dist),
             media_speed = mean(Average_Speed_Moving),
             media_propmovi = mean(Prop_time_moving))

medias.2 <- df_Limpeza %>% 
  group_by (Tratamento) %>% 
  summarise (media_limpeza = mean(Tempo_segundos))

# Exploração boxplots -----------------------------------------------------
#Velocidade média
ggplot(df_general, aes(x = Group, y = Average_Speed))+
  geom_boxplot()+
  geom_point()

#Velocidade média se movendo
ggplot(df_general, aes(x = Group, y = Average_Speed_Moving))+
  geom_boxplot()+
  geom_point()

#Proporção de tempo se movendo
ggplot(df_general, aes(x = Group, y = Prop_time_moving))+
  geom_boxplot()+
  geom_point()

#Distancia caminhada
ggplot(df_general, aes(x = Group, y = Traveled_Dist))+
  geom_boxplot()+
  geom_point()

#Meandro
ggplot(df_general.fil, aes(x = Group, y = Meander))+
  geom_boxplot()+
  geom_point()

#Meandro moving
ggplot(df_general.fil, aes(x = Group, y = Meander_moving))+
  geom_boxplot()+
  geom_point()

ggplot(df_general, aes(x = Group, y = Exploration_absolute_value))+
  geom_boxplot()+
  geom_point()


# Velocidade Média --------------------------------------------------------
outliers <- boxplot.stats(df_general$Average_Speed_Moving)$out
outliers

mod_velocidade.1 <- glmmTMB(Average_Speed_Moving ~ Group,
                          family = nbinom1 (),
                          data = df_general)

mod_velocidade.2 <- glmmTMB(Average_Speed_Moving ~ Group,
                            family = nbinom2 (),
                            data = df_general)

mod_velocidade.3 <- glmmTMB(Average_Speed_Moving ~ Group,
                            family = gaussian (),
                            data = df_general)

mod_velocidade.4 <- glmmTMB(Average_Speed_Moving ~ Group + (1| Video), 
                            family = gaussian (),
                            data = df_general) # aqui considero o bloco de treinamento, pensando que o tempo até os ultimos testes pode ter alterado algo, porém não é o melhor modelo


model.sel(mod_velocidade.1, mod_velocidade.2, mod_velocidade.3,mod_velocidade.4)

plot(simulateResiduals(mod_velocidade.3))

#Extração dos resultados
summary(mod_velocidade.3)

emmeans_vel <- emmeans(mod_velocidade.3, pairwise ~ Group, adjust = "Tukey")
emmeans_vel$contrasts

#Tamanhos de efeito
tab_model(mod_velocidade.3)

predic_velocidade <- ggpredict(mod_velocidade.3, type ="fixed" , terms = c("Group"), ci_level = 0.95)







#PLOT VELOCIDADE

ggplot(df_general, aes(y = Average_Speed_Moving, x = Group, color = Group))+
  geom_violin(width = 0.75,
              fill = NA,
              linewidth = 0.6,
              position = position_dodge(width = 0.75),
              color = 'black',
              show.legend = F)+
  geom_boxplot(width = 0.15,
               aes(fill = Group) ,
               linewidth = 0.7,
               position = position_dodge(width = 0.45),
               color = 'black',
               show.legend = F,
               whiskers = F)+
  geom_jitter(size = 3,
              position = position_jitterdodge(jitter.width = 0.15, 
                                              dodge.width = 1),
              alpha = 0.7,
              show.legend = T)+
  geom_segment(data = predic_velocidade, aes(x = as.numeric(x),
                                         xend = as.numeric(x) + 0.25,
                                         y = predicted, yend = predicted),
               size = 1, color = "black",
               linetype = 'dotted',
               inherit.aes = F,
               show.legend = F) +
  geom_label(data = predic_velocidade, aes(x = x, 
                                       y = predicted,
                                       label = paste("µ =", round(predicted, 2))),
             fill = 'white', 
             color = "black",
             size = 4, 
             fontface = "plain",
             label.padding = unit(0.3, "lines"),
             label.size = 0.5,
             position = position_nudge(x = 0.3, y = -0.85),
             show.legend = F) +
  geom_errorbar(data = predic_velocidade, aes(x = x,
                                          ymax = conf.high,
                                          ymin = conf.low),
                inherit.aes = F,
                size = 1,
                width = 0.1,
                position = position_nudge(x = 0.25),
                show.legend =F,
                color = '#700')+
  geom_point (data = predic_velocidade, 
              aes(y = predicted, x = x),
              size = 4.5,
              color = '#700',
              inherit.aes = F,
              position = position_nudge(x = 0.25),
              # position = position_nudge (x = 1.2),
              show.legend = F)+
  geom_text(data = predic_velocidade, aes(x = x,
                                      y = predicted + 2.3,
                                      # fill = group,
                                      label = c('','','*','')),
            color = "black",
            size = 12,
            fontface = "plain",
            # label.padding = unit(0.3, "lines"), label.size = 0.5,
            position = position_dodge(width = 1.2),
            inherit.aes = F,
            show.legend = F) +

  theme_bw()+
  scale_color_manual(values = c('#000','#000','#000','#000','#928039','#928010'))+
  scale_fill_manual(values = c('#8CA252','#BD9E39','#AD494A','#A55194','#928039','#928010'))+
  # scale_fill_manual(values = c('#ccc','#ccc','#ccc','#ccc','#ccc','#ccc'))+
  labs (x = '')+
  scale_y_continuous(limits = c(0,5))+
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(face = "bold", color = "black"),
    axis.title.y = element_text(face = "bold", color = "black",size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14, color = 'black', face = 'plain'),
    axis.text.y = element_text(size = 12, color = 'black'),
    legend.position = 'none',
    legend.title = element_text(size = 12, face = "bold"),
    panel.border = element_blank(),  
    panel.grid.major.y = element_line(color = "#fff", linetype = "dashed"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = NA),
    plot.background = element_rect(fill = NA),
    axis.line.y.left = element_line(color = 'black'),  
    axis.line.x = element_line(color = 'black'),  
    axis.line.y = element_line(color = 'red'),  
    axis.line.x.top = element_line(color = "white"))+
  scale_x_discrete(labels = c("CTR" = "Control \n n = (15)", "DEL" = "Deltamethrin \n n = (15)", "IMI" = "Imidacloprid \n n = (15)", 'COM' = 'Combination \n n = (15)'))+
  labs (y = 'Average walking speed (cm/s)')

# Distancia caminhada --------------------------------------------------------
outliers <- boxplot.stats(df_general$Traveled_Dist)$out
outliers



mod_distancia.1 <- glmmTMB( Traveled_Dist ~ Group,
                            family = nbinom1 (),
                            data = df_general)

mod_distancia.2 <- glmmTMB(Traveled_Dist ~ Group,
                            family = nbinom2 (),
                            data = df_general)

mod_distancia.3 <- glmmTMB(Traveled_Dist ~ Group,
                            family = gaussian (),
                            data = df_general)

mod_distancia.4 <- glmmTMB(Traveled_Dist ~ Group + (1| Video),
                            family = gaussian (),
                            data = df_general)

AIC(mod_distancia.1, mod_distancia.2, mod_distancia.3, mod_distancia.4)

plot(simulateResiduals(mod_distancia.3))

#Extração dos resultados
summary(mod_distancia.3)

emmeans_dist <- emmeans(mod_distancia.3, pairwise ~ Group, adjust = "none")
emmeans_dist$contrasts

#Tamanhos de efeito
tab_model(mod_distancia.3)

predic_distancia <- ggpredict(mod_distancia.3, type ="fixed" , terms = c("Group"), ci_level = 0.95)





#PLOT distancia

ggplot(df_general, aes(y = Traveled_Dist, x = Group, color = Group))+
  geom_violin(width = 0.75,
              fill = NA,
              linewidth = 0.6,
              position = position_dodge(width = 0.75),
              color = 'black',
              show.legend = F)+
  geom_boxplot(width = 0.15,
               aes(fill = Group),
               linewidth = 0.7,
               position = position_dodge(width = 0.45),
               color = 'black',
               show.legend = F,
               whiskers = F)+
  geom_jitter(size = 3,
              position = position_jitterdodge(jitter.width = 0.15, 
                                              dodge.width = 1),
              alpha = 0.7,
              show.legend = T)+
  geom_segment(data = predic_distancia, aes(x = as.numeric(x),
                                             xend = as.numeric(x) + 0.25,
                                             y = predicted, yend = predicted),
               size = 1, color = "black",
               linetype = 'dotted',
               inherit.aes = F,
               show.legend = F) +
  geom_label(data = predic_distancia, aes(x = x, 
                                           y = predicted,
                                           label = paste("µ =", round(predicted, 2))),
             fill = 'white', 
             color = "black",
             size = 4, 
             fontface = "plain",
             label.padding = unit(0.3, "lines"),
             label.size = 0.5,
             position = position_nudge(x = 0.3, y = -280),
             show.legend = F) +
  geom_errorbar(data = predic_distancia, aes(x = x,
                                              ymax = conf.high,
                                              ymin = conf.low),
                inherit.aes = F,
                size = 1,
                width = 0.1,
                position = position_nudge(x = 0.25),
                show.legend =F,
                color = '#700')+
  geom_point (data = predic_distancia, 
              aes(y = predicted, x = x),
              size = 4.5,
              color = '#700',
              inherit.aes = F,
              position = position_nudge(x = 0.25),
              # position = position_nudge (x = 1.2),
              show.legend = F)+
  geom_text(data = predic_distancia, aes(x = x,
                                          y = predicted + 750,
                                          # fill = group,
                                          label = c('','','*','')),
            color = "black",
            size = 12,
            fontface = "plain",
            # label.padding = unit(0.3, "lines"), label.size = 0.5,
            position = position_dodge(width = 1.2),
            inherit.aes = F,
            show.legend = F) +
  
  theme_bw()+
  scale_color_manual(values = c('#000','#000','#000','#000','#928039','#928010'))+
  scale_fill_manual(values = c('#8CA252','#BD9E39','#AD494A','#A55194','#928039','#928010'))+
  labs (x = '')+
  scale_y_continuous(limits = c(0, 1550))+
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(face = "bold", color = "black"),
    axis.title.y = element_text(face = "bold", color = "black",size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14, color = 'black', face = 'plain'),
    axis.text.y = element_text(size = 12, color = 'black'),
    legend.position = 'none',
    legend.title = element_text(size = 12, face = "bold"),
    panel.border = element_blank(),  
    panel.grid.major.y = element_line(color = "#fff", linetype = "dashed"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = NA),
    plot.background = element_rect(fill = NA),
    axis.line.y.left = element_line(color = 'black'),  
    axis.line.x = element_line(color = 'black'),  
    axis.line.y = element_line(color = 'red'),  
    axis.line.x.top = element_line(color = "white"))+
  scale_x_discrete(labels = c("CTR" = "Control \n n = (15)", "DEL" = "Deltamethrin \n n = (15)", "IMI" = "Imidacloprid \n n = (15)", 'COM' = 'Combination \n n = (15)'))+
  labs (y = 'Walking distance (cm)')


# Prporção de tempo se movendo L--------------------------------------------------------
#Considerando remover outliers
outliers <- boxplot.stats(df_general$Prop_time_moving)$out
outliers


mod_proporcao.1 <- glmmTMB(Prop_time_moving ~ Group,
                            family = beta_family (),
                            data = df_general)

mod_proporcao.2 <- glmmTMB(Prop_time_moving ~ Group + (1|Video),
                            family = beta_family (),
                            data = df_general)

AIC(mod_proporcao.1, mod_proporcao.2)

plot(simulateResiduals(mod_proporcao.1))

#Extração dos resultados
summary(mod_proporcao.1)
summary(mod_proporcao.1)$family


#Tamanhos de efeito
tab_model(mod_proporcao.1)

predic_proporcao <- ggpredict(mod_proporcao.1, type ="fixed" , terms = c("Group"), ci_level = 0.95, back_transform = F)



#PLOT proporcao
ggplot(df_general, aes(y = Prop_time_moving, x = Group, color = Group))+
  geom_violin(width = 1,
              fill = NA,
              linewidth = 0.5,
              position = position_dodge(width = 0.75),
              color = 'black',
              show.legend = F)+
  geom_boxplot(width = 0.15,
               # fill = ,
               linewidth = 0.5,
               position = position_dodge(width = 0.45),
               color = 'black',
               show.legend = F,
               whiskers = F)+
  geom_jitter(size = 3,
              position = position_jitterdodge(jitter.width = 0.3, 
                                              dodge.width = 0.45),
              alpha = 0.7,
              show.legend = T)+
  geom_segment(data = predic_proporcao, aes(x = as.numeric(x),
                                             xend = as.numeric(x) + 0.25,
                                             y = predicted, yend = predicted),
               size = 1, color = "black",
               linetype = 'dotted',
               inherit.aes = F,
               show.legend = F) +
  geom_label(data = predic_proporcao, aes(x = x, 
                                           y = predicted,
                                           label = paste("µ̂ = ", round(predicted, 2))),
             fill = 'white', 
             color = "black",
             size = 4, 
             fontface = "bold",
             label.padding = unit(0.3, "lines"),
             label.size = 0.5,
             position = position_nudge(x = 0.35),
             show.legend = F) +
  geom_errorbar(data = predic_proporcao, aes(x = x,
                                              ymax = conf.high,
                                              ymin = conf.low),
                inherit.aes = F,
                width = 0.07,
                position = position_dodge(width = 1.2),
                show.legend =F,
                color = '#700')+
  geom_point (data = predic_proporcao, 
              aes(y = predicted, x = x),
              size = 4,
              color = '#700',
              inherit.aes = F,
              position = position_dodge (width = 1.2),
              # position = position_nudge (x = 1.2),
              show.legend = F)+
  theme_bw()+
  scale_color_manual(values = c('#000','#000','#000','#000','#928039','#928010'))+
  scale_fill_manual(values = c('#8CA252','#BD9E39','#AD494A','#A55194','#928039','#928010'))+
  labs (x = '')+
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(face = "bold", color = "black"),
    axis.title.y = element_text(face = "bold", color = "black",size = 12),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12, color = 'black', face = 'plain'),
    axis.text.y = element_text(size = 12, color = 'black'),
    legend.position = 'none',
    legend.title = element_text(size = 12, face = "bold"),
    panel.border = element_blank(),  
    panel.grid.major.y = element_line(color = "grey", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.line.y.left = element_line(color = 'black'),  
    axis.line.x = element_line(color = 'black'),  
    axis.line.y = element_line(color = 'red'),  
    axis.line.x.top = element_line(color = "white"))+
  scale_x_discrete(labels = c("CTR" = "Control \n n = (15)", "DEL" = "Deltamethrin \n n = (15)", "IMI" = "Imidacloprid \n n = (15)", 'COM' = 'Combination \n n = (15)'))+
  labs (y = 'Seconds (s)')


# moving time -------------------------------------------------------------
df_general$moving_time <- df_general$Prop_time_moving*300

glm_moving.1 <- glmmTMB(moving_time ~ Group,
                       data = df_general,
                       family = nbinom1 ())

glm_moving.2 <- glmmTMB(moving_time ~ Group,
                       data = df_general,
                       family = gaussian ())



model.sel(glm_moving.2,glm_moving.1) #2 é o melhor
plot(simulateResiduals(glm_moving.2))
summary(glm_moving.2)
tab_model(glm_moving.2)


predic_moving <- ggpredict(glm_moving.2, type ="fixed" , terms = c("Group"), ci_level = 0.95)


# Observação - MANUAL -----------------------------------------------------

# Importacao --------------------------------------------------------------
df_comp <- read_csv("Observação_comportamental_ mortalidade e consumo.xlsx - Comportamento.csv")

df_comp$Tratamento <- factor (df_comp$Tratamento, levels = c('CTR','DEL','IMI','COM'))
df_comp$REP <- factor (df_comp$REP)
df_comp$Comportamento <- factor (df_comp$Comportamento)
df_comp$Observador <- factor (df_comp$Observador)
df_comp$Tempo_segundos <-  as.numeric (df_comp$Tempo_segundos)

str(df_comp)


# Grooming ----------------------------------------------------------------
# Criacao de subsets 
df_Limpeza <- df_comp %>% filter (Comportamento == 'Limpeza')

#identificação de outliers
outliers <- boxplot.stats(df_Limpeza$Tempo_segundos)$out
df_outliers <- df_Limpeza[df_Limpeza$Tempo_segundos %in% outliers, c("Tratamento","REP" ,"Tempo_segundos")]
outliers

df_Limpeza_clean <- df_Limpeza %>%
  filter(Tempo_segundos >= quantile(Tempo_segundos, 0.25) - 1.5*IQR(Tempo_segundos) &
           Tempo_segundos <= quantile(Tempo_segundos, 0.75) + 1.5*IQR(Tempo_segundos)) 




ggplot( data = df_Limpeza_clean, aes(y = Tempo_segundos, x= Comportamento, fill = Tratamento, color = Tratamento))+
  stat_boxplot(geom ='errorbar') +
  geom_boxplot(notch = T)+
  geom_point()+
  theme_minimal()

df_Limpeza_clean$Tratamento <- factor (df_Limpeza_clean$Tratamento, levels = c('CTR','DEL','IMI','COM'))

# df_Limpeza_clean$Tratamento <- factor (df_Limpeza_clean$Tratamento, levels = c('COM','CTR','DEL','IMI'))

glm_Geral.3 <- glmmTMB(Tempo_segundos ~ Tratamento,
                       data = df_Limpeza_clean,
                       # ziformula =   ~ Tratamento,
                       family = gaussian ())

glm_Geral.4 <- glmmTMB(Tempo_segundos ~ Tratamento,
                       data = df_Limpeza_clean,
                       # ziformula =   ~ Tratamento,
                       family = nbinom2 ())

glm_Geral.5 <- glmmTMB(Tempo_segundos ~ Tratamento,
                       data = df_Limpeza_clean,
                       ziformula =   ~ Tratamento,
                       family = nbinom2 ())

glm_Geral.5.1 <- glmmTMB(Tempo_segundos ~ Tratamento + (1|Observador) + (1|REP),
                       data = df_Limpeza_clean,
                       ziformula =   ~ Tratamento,
                       family = nbinom2 ())

glm_Geral.5.2 <- glmmTMB(Tempo_segundos ~ Tratamento + (1|REP),
                       data = df_Limpeza_clean,
                       ziformula =   ~ Tratamento,
                       family = nbinom2 ())

glm_Geral.5.3 <- glmmTMB(Tempo_segundos ~ Tratamento + (1|Observador),
                       data = df_Limpeza_clean,
                       ziformula =   ~ Tratamento,
                       family = nbinom2 ())

glm_Geral.5.3 <- glmmTMB(Tempo_segundos ~ Tratamento + (1|Observador),
                       data = df_Limpeza_clean,
                       ziformula =   ~ Tratamento,
                       family = gamma ())

AIC(glm_Geral.3,glm_Geral.4,glm_Geral.5,glm_Geral.5.1,glm_Geral.5.2, glm_Geral.5.3) # 5 é o melhor
plot(simulateResiduals(glm_Geral.5))
summary(glm_Geral.5)
tab_model(glm_Geral.5)

emmeans_gro2 <- emmeans(glm_Geral.5, pairwise ~ Tratamento, adjust = "none")
emmeans_gro2$contrasts 
summary(emmeans_gro2$contrasts, type = "response")


predic_limpeza_clean <- ggpredict(glm_Geral.5, type ="fixed" , terms = c("Tratamento"), ci_level = 0.95)

#PLOT Limpeza

ggplot(df_Limpeza_clean, aes(y = Tempo_segundos, x = Tratamento, color = Tratamento))+
  # geom_violin(width = 1,
  #             fill = NA,
  #             linewidth = 0.6,
  #             position = position_dodge(width = 0.75),
  #             color = 'black',
  #             show.legend = F)+
  geom_boxplot(width = 0.25,
               aes(fill = Tratamento) ,
               linewidth = 0.7,
               position = position_dodge(width = 0.45),
               color = 'black',
               show.legend = F,
               whiskers = F)+
  geom_jitter(size = 3,
              position = position_jitterdodge(jitter.width = 0.15, 
                                              dodge.width = 1),
              alpha = 0.7,
              show.legend = T)+
  geom_segment(data = predic_limpeza_clean, aes(x = as.numeric(x),
                                            xend = as.numeric(x) + 0.2,
                                            y = predicted, yend = predicted),
               size = 1, color = "black",
               linetype = 'dotted',
               inherit.aes = F,
               show.legend = F) +
  geom_label(
    data = predic_limpeza_clean,
    aes(
      x = x,
      y = predicted,  # ou ajuste o valor como desejar
      label = paste("µ =", round(predicted, 2))
    ),
    fill = NA, 
    color = 'black',
    size = 4.5, 
    fontface = "plain",
    label.padding = unit(0.3, "lines"),
    label.size = 0,
    position = position_nudge(x = 0.43),  # só o X é deslocado aqui
    show.legend = F
  ) +
  
  geom_errorbar(data = predic_limpeza_clean, aes(x = x,
                                             ymax = conf.high,
                                             ymin = conf.low),
                inherit.aes = F,
                size = 1,
                width = 0.1,
                position = position_nudge(x = 0.20),
                show.legend =F,
                color = '#700')+
  geom_point (data = predic_limpeza_clean, 
              aes(y = predicted, x = x),
              size = 4.5,
              color = '#700',
              inherit.aes = F,
              position = position_nudge(x = 0.20),
              # position = position_nudge (x = 1.2),
              show.legend = F)+
  geom_text(data = predic_limpeza_clean, aes(x = x,
                                         y = conf.high + 7,
                                         # fill = group,
                                         label = c('c','b','a','b')),
            color = "black",
            size = 6,
            fontface = 'plain',
            # label.padding = unit(0.3, "lines"), label.size = 0.5,
            position = position_nudge(x = 0.2),
            inherit.aes = F,
            show.legend = F) +
  
  theme_bw()+
  scale_color_manual(values = c('#000','#000','#000','#000','#928039','#928010'))+
  scale_fill_manual(values = c('#8CA252','#BD9E39','#AD494A','#A55194','#928039','#928010'))+
  labs (x = '')+
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(face = "bold", color = "black"),
    axis.title.y = element_text(face = "bold", color = "black",size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14, color = 'black', face = 'plain'),
    axis.text.y = element_text(size = 12, color = 'black'),
    legend.position = 'none',
    legend.title = element_text(size = 12, face = "bold"),
    panel.border = element_blank(),  
    panel.grid.major.y = element_line(color = "#fff", linetype = "dashed"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = NA),
    plot.background = element_rect(fill = NA),
    axis.line.y.left = element_line(color = 'black'),  
    axis.line.x = element_line(color = 'black'),  
    axis.line.y = element_line(color = 'red'),  
    axis.line.x.top = element_line(color = "white"))+
  scale_x_discrete(labels = c("CTR" = "Control \n n = (15)", "DEL" = "Deltamethrin \n n = (15)", "IMI" = "Imidacloprid \n n = (15)", 'COM' = 'Combination \n n = (15)'))+
  labs (y = 'Grooming time (s)')




# Consumo de xarope -------------------------------------------------------

df_consumo <- read_excel("Observação_comportamental_ mortalidade e consumo.xlsx", sheet = 'Mortalidade')


df_consumo$Trat <- factor(df_consumo$Trat, levels = c('CTR','DEL','IMI','COM'))
# df_consumo$Trat <- factor(df_consumo$Trat, levels = c('IMI','COM','DEL','CTR'))
df_consumo$Rep <- factor(df_consumo$Rep)
df_consumo$vivas <- 10-df_consumo$Mortalidade
df_consumo$conabelha <- (df_consumo$Consumo/df_consumo$vivas)*1000
df_consumo$con.abel.total <- df_consumo$Consumo/10
df_consumo$pote <- interaction(df_consumo$Trat,df_consumo$Rep)

str(df_consumo)

#Exploração

ggplot(data = df_consumo, aes(x = Trat, y = Consumo))+
  geom_boxplot()+
  geom_point()

df_consumo %>% 
  group_by(Trat) %>% 
  summarise (media.conabelha = mean(conabelha),
             media.consumo = mean (Consumo),
             media.con.abelha = mean (con.abel.total))

# Modelos 
mod_consumo <- glmmTMB(conabelha ~ Trat,
                          family = gaussian (),
                       # dispformula = ~Trat,
                          data = df_consumo)

mod_consumo.rep <- glmmTMB(conabelha ~ Trat + (1| Rep),
                          family = gaussian (),
                       # dispformula = ~Trat,
                          data = df_consumo)

mod_consumo.1 <- glmmTMB(Consumo ~ Trat*vivas,
                          family = gaussian (),
                          data = df_consumo)


mod_consumo.2 <- glmmTMB(con.abel.total ~ Trat + (1|Rep),
                          family = gaussian (),
                          data = df_consumo) # A mortalidade atua sobre o consumo, + mortalidade - consumo


mod_consumo.g <- glmmTMB(conabelha ~ Trat,
                       family = gaussian (),
                       # dispformula = ~Trat,
                       data = df_consumo)

mod_consumo.g.1 <- glmmTMB(conabelha ~ Trat + (1| Rep),
                         family = gaussian (),
                         # dispformula = ~Trat,
                         data = df_consumo)



AIC(mod_consumo.g, mod_consumo.g.1)
AIC(mod_consumo,mod_consumo.1,mod_consumo.2, mod_consumo.rep, mod_consumo.t, mod_consumo.t.1, mod_consumo.t.2)

plot(simulateResiduals(mod_consumo.g))

summary(mod_consumo.g)
formula(mod_consumo)

ggplot(df_consumo, aes(y = Consumo, x = vivas, color = Trat))+
  # geom_boxplot()+
  geom_point()+
  theme_bw()

tab_model(mod_consumo.t.1)

predic_consumo <- ggpredict(mod_consumo.g, type ="fixed" , terms = c("Trat"), ci_level = 0.95)

predic_consumo <- ggpredict(
  mod_consumo.t.1,
  type = "fixed",
  terms = c("Trat"),
  ci_level = 0.95,
  condition = list(vivas = c(6.55))   # coloque o valor desejado
)


predic_consumo$group <- as.numeric (predic_consumo$group)


ggplot(predic_consumo, aes(x = group, y = predicted, fill = x, color = x))+
  geom_point()+
  geom_line()+
  theme_bw()

ggplot(df_consumo, aes(y = Consumo, x = Trat, color = Trat))+
  geom_violin(width = 0.8,
              fill = NA,
              linewidth = 0.6,
              position = position_dodge(width = 0.75),
              color = 'black',
              show.legend = F)+
  geom_boxplot(width = 0.10,
               aes(fill = Trat),
               linewidth = 0.7,
               position = position_dodge(width = 0.45),
               color = 'black',
               show.legend = F,
               whiskers = F)+
  geom_jitter(size = 3,
              position = position_jitterdodge(jitter.width = 0.15, 
                                              dodge.width = 1),
              alpha = 0.7,
              show.legend = T)+
  geom_segment(data = predic_consumo, aes(x = as.numeric(x),
                                            xend = as.numeric(x) + 0.25,
                                            y = predicted, yend = predicted),
               size = 1, color = "black",
               linetype = 'dotted',
               inherit.aes = F,
               show.legend = F) +
  geom_label(data = predic_consumo, aes(x = x, 
                                          y = conf.high,
                                          label = paste("µ =", round(predicted, 2))),
             fill = 'white', 
             color = "black",
             size = 4, 
             fontface = "plain",
             label.padding = unit(0.3, "lines"),
             label.size = 0.5,
             position = position_nudge(x = 0.3, y = 0.05),
             show.legend = F) +
  geom_errorbar(data = predic_consumo, aes(x = x,
                                             ymax = conf.high,
                                             ymin = conf.low),
                inherit.aes = F,
                size = 1,
                width = 0.1,
                position = position_nudge(x = 0.25),
                show.legend =F,
                color = '#700')+
  geom_point (data = predic_consumo, 
              aes(y = predicted, x = x),
              size = 4.5,
              color = '#700',
              inherit.aes = F,
              position = position_nudge(x = 0.25),
              # position = position_nudge (x = 1.2),
              show.legend = F)+
  geom_text(data = predic_consumo, aes(x = x,
                                         y = predicted + 0.25,
                                         # fill = group,
                                         label = c('','','*','')),
            color = "black",
            size = 12,
            fontface = "plain",
            # label.padding = unit(0.3, "lines"), label.size = 0.5,
            position = position_dodge(width = 1.2),
            inherit.aes = F,
            show.legend = F) +
  
  theme_bw()+
  scale_color_manual(values = c('#000','#000','#000','#000','#928039','#928010'))+
  scale_fill_manual(values = c('#8CA252','#BD9E39','#AD494A','#A55194','#928039','#928010'))+
  labs (x = '')+
  scale_y_continuous(limits = c(0, 0.8),breaks = seq(0, 0.8, by = 0.1))+
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(face = "bold", color = "black"),
    axis.title.y = element_text(face = "bold", color = "black",size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14, color = 'black', face = 'plain'),
    axis.text.y = element_text(size = 12, color = 'black'),
    legend.position = 'none',
    legend.title = element_text(size = 12, face = "bold"),
    panel.border = element_blank(),  
    panel.grid.major.y = element_line(color = "#fff", linetype = "dashed"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = NA),
    plot.background = element_rect(fill = NA),
    axis.line.y.left = element_line(color = 'black'),  
    axis.line.x = element_line(color = 'black'),  
    axis.line.y = element_line(color = 'red'),  
    axis.line.x.top = element_line(color = "white"))+
  scale_x_discrete(labels = c("CTR" = "Control \n n = (5)", "DEL" = "Deltamethrin \n n = (5)", "IMI" = "Imidacloprid \n n = (5)", 'COM' = 'Combination \n n = (5)'))+
  labs (y = 'Sucrose consumption (g)')


#Mortaliade
mod_mortalidade <- glmmTMB(Mortalidade ~ Trat,
                       family = gaussian (),
                       dispformula = ~Trat,
                       data = df_consumo)

mod_mortalidade.2 <- glmmTMB(Mortalidade ~ Trat,
                       family = nbinom2 (),
                       data = df_consumo)

mod_mortalidade.1 <- glmmTMB(Mortalidade ~ Trat,
                       family = gaussian (),
                       data = df_consumo)


AIC(mod_mortalidade, mod_mortalidade.1, mod_mortalidade.2) # Não tem mais que dois pontos em AIC, então usaremos o modelo mais simples
plot(simulateResiduals(mod_mortalidade.1))
summary(mod_mortalidade.1)

predic_mortalidade <- ggpredict(mod_mortalidade.1, type ="fixed" , terms = c("Trat"), ci_level = 0.95)

#PLOT mortalidade


plot.mortalidade <- ggplot(df_consumo, aes(y = Mortalidade, x = Trat, color = Trat))+
  stat_summary(fun = mean, geom = "bar", fill = "white", color = "black", width = 0.5)+
  # geom_jitter(size = 3,
  #             position = position_jitterdodge(jitter.width = 0.35, 
  #                                             dodge.width = 1),
  #             alpha = 0.8,
  #             show.legend = T)+
  geom_segment(data = predic_mortalidade, aes(x = as.numeric(x),
                                          xend = as.numeric(x) + 0.25,
                                          y = predicted, yend = predicted),
               size = 1, color = "black",
               linetype = 'dotted',
               inherit.aes = F,
               show.legend = F) +
  geom_label(data = predic_mortalidade, aes(x = x, 
                                        y = predicted,
                                        label = paste("µ̂ = ", round(predicted, 2))),
             fill = 'white', 
             color = "black",
             size = 4, 
             fontface = "bold",
             label.padding = unit(0.3, "lines"),
             label.size = 0.5,
             position = position_nudge(x = 0.3, y = 0.35),
             show.legend = F) +
  geom_errorbar(data = predic_mortalidade, aes(x = x,
                                           ymax = conf.high,
                                           ymin = conf.low),
                inherit.aes = F,
                size = 1,
                width = 0.1,
                position = position_nudge(x = 0),
                show.legend =F,
                color = '#700')+
  geom_point (data = predic_mortalidade, 
              aes(y = predicted, x = x),
              size = 4.5,
              color = '#700',
              inherit.aes = F,
              position = position_nudge(x = 0),
              # position = position_nudge (x = 1.2),
              show.legend = F)+
  geom_text(data = predic_mortalidade, aes(x = x,
                                       y = (0.1),
                                       # fill = group,
                                       label = c('','','.','')),
            color = "black",
            size = 8,
            fontface = "plain",
            # label.padding = unit(0.3, "lines"), label.size = 0.5,
            position = position_dodge(width = 1.2),
            inherit.aes = F,
            show.legend = F) +
  
  theme_bw()+
  scale_color_manual(values = c('#000','#000','#000','#000','#928039','#928010'))+
  scale_fill_manual(values = c('white','white','#b65DdF','#892829','#928039','#928010'))+
  labs (x = '')+
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(face = "bold", color = "black"),
    axis.title.y = element_text(face = "bold", color = "black",size = 12),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14, color = 'black', face = 'plain'),
    axis.text.y = element_blank(),
    legend.position = 'none',
    legend.title = element_text(size = 12, face = "bold"),
    panel.border = element_blank(),  
    panel.grid.major.y = element_line(color = "#fff", linetype = "dashed"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.line.y.left = element_line(color = 'black'),  
    axis.line.x = element_line(color = 'black'),  
    axis.line.y = element_line(color = 'red'),  
    axis.line.x.top = element_line(color = "white"))+
  scale_x_discrete(labels = c("CTR" = "Control \n n = (5)", "DEL" = "Deltamethrin \n n = (5)", "IMI" = "Imidacloprid \n n = (5)", 'COM' = 'Combination \n n = (5)'))+
  labs (y = 'Mortality (count)')+
  coord_flip()

#Graficos unidos

plot.consumo + plot.mortalidade +  plot_layout(widths = c(7, 3)) + plot_annotation(tag_levels = 'a')

predic_consumo$std.error
# Gráfico de barros -------------------------------------------------------
consumo.barras <- ggplot(df_consumo, aes(y = Consumo, x = Trat, color = Trat))+
  stat_summary(fun = mean,
               geom = "bar", 
               aes(fill = Trat),
               color = "black",
               width = 0.65)+
  # geom_segment(data = predic_consumo, aes(x = as.numeric(x),
  #                                         xend = as.numeric(x) + 0.25,
  #                                         y = predicted, yend = predicted),
  #              size = 1, color = "black",
  #              linetype = 'dotted',
  #              inherit.aes = F,
  #              show.legend = F) +
  # geom_label(data = predic_consumo, aes(x = x, 
  #                                       y = predicted,
  #                                       label = paste("µ̂ = ", round(predicted, 2))),
  #            fill = 'white', 
  #            color = "black",
  #            size = 4, 
  #            fontface = "bold",
  #            label.padding = unit(0.3, "lines"),
  #            label.size = 0.5,
  #            position = position_nudge(x = 0.5, y = -0),
  #            show.legend = F) +
  geom_errorbar(data = predic_consumo, aes(x = x,
                                           ymax = conf.high,
                                           ymin = conf.low),
                inherit.aes = F,
                size = 0.9,
                width = 0.1,
                # position = position_nudge(x = 0.3),
                show.legend =F,
                color = '#700')+
  geom_point (data = predic_consumo, 
              aes(y = predicted, x = x),
              size = 2,
              color = '#700',
              inherit.aes = F,
              # position = position_nudge(x = 0.3),
              # position = position_nudge (x = 1.2),
              show.legend = F)+
  geom_text(data = predic_consumo, aes(x = x,
                                       y = conf.high + 0.05,
                                       # fill = group,
                                       label = c('','','*','')),
            color = "black",
            size = 12,
            fontface = "plain",
            # label.padding = unit(0.3, "lines"), label.size = 0.5,
            position = position_dodge(width = 1.2),
            inherit.aes = F,
            show.legend = F) +
  
  theme_bw()+
  scale_color_manual(values = c('#000','#000','#000','#000','#928039','#928010'))+
  scale_fill_manual(values = c('#8CA252','#BD9E39','#AD494A','#A55194','#928039','#928010'))+
  labs (x = '')+
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(face = "bold", color = "black"),
    axis.title.y = element_text(face = "bold", color = "black",size = 12),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14, color = 'black', face = 'plain'),
    axis.text.y = element_text(size = 12, color = 'black'),
    legend.position = 'none',
    legend.title = element_text(size = 12, face = "bold"),
    panel.border = element_blank(),  
    panel.grid.major.y = element_line(color = "#fff", linetype = "dashed"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = NA),
    plot.background = element_rect(fill = NA),
    axis.line.y.left = element_line(color = 'black'),  
    axis.line.x = element_line(color = 'black'),  
    axis.line.y = element_line(color = 'red'),  
    axis.line.x.top = element_line(color = "white"))+
  scale_x_discrete(labels = c("CTR" = "Control \n n = (5)", "DEL" = "Deltamethrin \n n = (5)", "IMI" = "Imidacloprid \n n = (5)", 'COM' = 'Combination \n n = (5)'))+
  labs (y = 'Syrup consumption (g)')



# Grafico barras mortalidade ----------------------------------------------

# Gráfico de barros -------------------------------------------------------
mortalidade.barras <- ggplot(df_consumo, aes(y = Mortalidade, x = Trat, color = Trat))+
  stat_summary(fun = mean,
               geom = "bar", 
               aes(fill = Trat),
               color = "black",
               width = 0.65)+
  # geom_segment(data = predic_consumo, aes(x = as.numeric(x),
  #                                         xend = as.numeric(x) + 0.25,
  #                                         y = predicted, yend = predicted),
  #              size = 1, color = "black",
  #              linetype = 'dotted',
  #              inherit.aes = F,
  #              show.legend = F) +
  # geom_label(data = predic_consumo, aes(x = x, 
  #                                       y = predicted,
  #                                       label = paste("µ̂ = ", round(predicted, 2))),
  #            fill = 'white', 
  #            color = "black",
  #            size = 4, 
  #            fontface = "bold",
  #            label.padding = unit(0.3, "lines"),
  #            label.size = 0.5,
  #            position = position_nudge(x = 0.5, y = -0),
  #            show.legend = F) +
  geom_errorbar(data = predic_mortalidade, aes(x = x,
                                           ymax = conf.high,
                                           ymin = conf.low),
                inherit.aes = F,
                size = 0.9,
                width = 0.1,
                # position = position_nudge(x = 0.3),
                show.legend =F,
                color = '#700')+
  geom_point (data = predic_mortalidade, 
              aes(y = predicted, x = x),
              size = 2,
              color = '#700',
              inherit.aes = F,
              # position = position_nudge(x = 0.3),
              # position = position_nudge (x = 1.2),
              show.legend = F)+
  geom_text(data = predic_mortalidade, aes(x = x,
                                       y = (-0.03),
                                       # fill = group,
                                       label = c('','','','')),
            color = "black",
            size = 10,
            fontface = "plain",
            # label.padding = unit(0.3, "lines"), label.size = 0.5,
            position = position_dodge(width = 1.2),
            inherit.aes = F,
            show.legend = F) +
  
  theme_bw()+
  scale_color_manual(values = c('#000','#000','#000','#000','#928039','#928010'))+
  scale_fill_manual(values = c('#8CA252','#BD9E39','#AD494A','#A55194','#928039','#928010'))+
  labs (x = '')+
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(face = "bold", color = "black"),
    axis.title.y = element_text(face = "bold", color = "black",size = 12),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14, color = 'black', face = 'plain'),
    axis.text.y = element_text(size = 12, color = 'black'),
    legend.position = 'none',
    legend.title = element_text(size = 12, face = "bold"),
    panel.border = element_blank(),  
    panel.grid.major.y = element_line(color = "#fff", linetype = "dashed"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = NA),
    plot.background = element_rect(fill = NA),
    axis.line.y.left = element_line(color = 'black'),  
    axis.line.x = element_line(color = 'black'),  
    axis.line.y = element_line(color = 'red'),  
    axis.line.x.top = element_line(color = "white"))+
  scale_x_discrete(labels = c("CTR" = "CTR \n n = (5)", "DEL" = "DEL \n n = (5)", "IMI" = "IMI \n n = (5)", 'COM' = 'COM \n n = (5)'))+
  labs (y = 'Dead bees (count)')+
  scale_y_continuous( breaks = c(1,2,3,4,5))
 
consumo.barras + mortalidade.barras +  plot_layout(widths = c(6, 4)) + plot_annotation(tag_levels = 'a')+
  plot_annotation(theme = theme(plot.background = element_rect(fill = NA, color = NA)))


# Wing faning -------------------------------------------------------------
df_voo <- df_comp %>% filter (Comportamento == 'Tentativa_voo')

ggplot( data = df_voo, aes(y = Tempo_segundos, x= Comportamento, fill = Tratamento))+
  stat_boxplot(geom ='errorbar') +
  geom_boxplot(notch = F)+
  geom_point()+
  theme_minimal()

outliers <- boxplot.stats(df_voo$Tempo_segundos)$out
df_outliers <- df_Limpeza[df_voo$Tempo_segundos %in% outliers, c("Tratamento","REP", "Tempo_segundos")]
outliers

df_voo_clean <- df_voo %>%
  filter(Tempo_segundos >= quantile(Tempo_segundos, 0.25) - 1.5*IQR(Tempo_segundos) &
           Tempo_segundos <= quantile(Tempo_segundos, 0.75) + 1.5*IQR(Tempo_segundos))

ggplot(data = df_voo_clean, aes(y = Tempo_segundos, x= Comportamento, fill = Tratamento))+
  stat_boxplot(geom ='errorbar') +
  geom_boxplot(notch = F)+
  geom_point()+
  theme_minimal()

glm_voo <- glmmTMB(Tempo_segundos ~ Tratamento,
                       data = df_voo_clean,
                       # ziformula =   ~ Tratamento,
                       family = nbinom2 ())

glm_voo.1 <- glmmTMB(Tempo_segundos ~ Tratamento,
                       data = df_voo_clean,
                       # ziformula =   ~ Tratamento,
                       family = gaussian ())

glm_voo.2 <- glmmTMB(Tempo_segundos ~ Tratamento,
                   data = df_voo_clean,
                   ziformula =   ~ Tratamento,
                   family = nbinom2 ())

AIC(glm_voo, glm_voo.1, glm_voo.2) # Melhor é glm_voo
summary(glm_voo.2)

predic_voo <- ggpredict(glm_voo.2, type ="fixed" , terms = c("Tratamento"), ci_level = 0.95)

plot(simulateResiduals(glm_voo.2))

# Repouso -----------------------------------------------------------------
df_rep <- df_comp %>% filter (Comportamento == 'Repouso')

ggplot( data = df_rep, aes(y = Tempo_segundos, x= Comportamento, fill = Tratamento))+
  stat_boxplot(geom ='errorbar') +
  geom_boxplot(notch = F)+
  geom_point()+
  theme_minimal()

outliers <- boxplot.stats(df_rep$Tempo_segundos)$out
df_outliers <- df_Limpeza[df_rep$Tempo_segundos %in% outliers, c("Tratamento","REP", "Tempo_segundos")]
outliers

df_rep_clean <- df_rep %>%
  filter(Tempo_segundos >= quantile(Tempo_segundos, 0.25) - 1.5*IQR(Tempo_segundos) &
           Tempo_segundos <= quantile(Tempo_segundos, 0.75) + 1.5*IQR(Tempo_segundos))

ggplot( data = df_rep_clean, aes(y = Tempo_segundos, x= Comportamento, fill = Tratamento))+
  stat_boxplot(geom ='errorbar') +
  geom_boxplot(notch = F)+
  geom_point()+
  theme_minimal()

df_rep_clean$Tratamento <- factor(df_rep_clean$Tratamento, levels = c('CTR','DEL','IMI','COM'))

glm_rep <- glmmTMB(Tempo_segundos ~ Tratamento,
                   data = df_rep_clean,
                   # ziformula =   ~ Tratamento,
                   family = nbinom2 ())

glm_rep.1 <- glmmTMB(Tempo_segundos ~ Tratamento,
                     data = df_rep_clean,
                     # ziformula =   ~ Tratamento,
                     family = gaussian ())

glm_rep.2 <- glmmTMB(Tempo_segundos ~ Tratamento,
                     data = df_rep_clean,
                     ziformula =   ~ Tratamento,
                     family = nbinom2 ())

glm_rep.3 <- glmmTMB(Tempo_segundos ~ Tratamento,
                     data = df_rep_clean,
                     dispformula = ~Tratamento,
                     # ziformula =   ~ Tratamento,
                     family = nbinom1 ())

AIC(glm_rep, glm_rep.1, glm_rep.2, glm_rep.3) # Melhor é glm_voo
summary(glm_rep.2)
tab_model(glm_rep.2)

predic_rep <- ggpredict(glm_rep.2, type ="fixed" , terms = c("Tratamento"), ci_level = 0.95)


# Novos graficos de barras ------------------------------------------------
meu_tema <- theme(
  plot.title = element_text(hjust = 0.5, face = "plain", size = 14),
  plot.subtitle = element_text(hjust = 0.5, size = 14),
  axis.title.x = element_blank(),
  axis.title.y = element_text(face = "bold", color = "black",size = 14),
  axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14, color = 'black', face = 'plain'),
  axis.text.y = element_text(size = 14, color = 'black'),
  legend.position = 'none',
  legend.title = element_text(size = 12, face = "bold"),
  panel.border = element_blank(),  
  panel.grid.major.y = element_line(color = NA, linetype = "dashed"),
  panel.grid.major.x = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = NA),
  plot.background = element_rect(fill = NA),
  axis.line.y.left = element_line(color = 'black'),  
  axis.line.x = element_line(color = 'black'),  
  axis.line.y = element_line(color = 'red'),  
  axis.line.x.top = element_line(color = NA))

# Average speed 
plot_speed <- ggplot(predic_velocidade, aes(x = x, y = predicted, fill = x)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, position = position_dodge(0.9),
                size = 1) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.4),
           # position = "dodge",
           color = "black",
           show.legend = F,
           width = 0.75,
           size = 0.7) +
  # geom_jitter(data = df_general, aes(y = Average_Speed_Moving, x = Group, color = Group),
  #             size = 3,
  #             position = position_jitterdodge(jitter.width = 0.1,
  #                                             dodge.width = 1.5),
  #             alpha = 0.6,
  #             # fill = NA,
  #             # # color = 'black',
  #             # shape = 21,
  #             show.legend = F,
  #             inherit.aes = F)+
  # geom_segment(data = predic_velocidade, aes(x = as.numeric(x),
  #                                            xend = as.numeric(x) + 0.25,
  #                                            y = predicted, yend = predicted),
  #              size = 1.1, color = "black",
  #              linetype = 'dotted',
  #              inherit.aes = F,
  #              show.legend = F) +
  geom_label(data = predic_velocidade, aes(x = x, 
                                           y = predicted,
                                           label = paste("", round(predicted, 2))),
             fill = NA, 
             # color = "black",
             size = 4, 
             fontface = "plain",
             label.padding = unit(0.15, "lines"),
             label.size = 0,
             color = ifelse(predic_velocidade$x == "COM", "white", "black"),
             position = position_nudge(x = 0, y = -0.15),
             show.legend = F)+
  geom_text(data = predic_velocidade, aes(x = x,
                                          y = conf.high + .25,
                                          # fill = group,
                                          label = c('a','ab','b','ab')),
            color = "black",
            size = 7,
            fontface = "plain",
            # label.padding = unit(0.3, "lines"), label.size = 0.5,
            position = position_dodge(width = 1.2),
            inherit.aes = F,
            show.legend = F)+
  theme_bw()+
  meu_tema+
  scale_color_manual(values = c('#000','#000','#000','#000','#928039','#928010'))+
  # scale_fill_manual(values = c('#8CA252','#BD9E39','#AD494A','#A55194','#928039','#928010'))+
  scale_fill_manual(values = c('#fff','#888','#888','#000','#ccc','#ccc'))+
  labs (x = '')+
  scale_y_continuous(limits = c(0,5))+
  scale_x_discrete(labels = c("CTR" = "CTR \n(15)", "DEL" = "DEL \n(15)", "IMI" = "IMI \n(15)", 'COM' = 'COM \n(15)'))+
  labs (y = 'Average walking speed (cm/s)') 


# Distancia caminhada
plot_dist <-  ggplot(predic_distancia, aes(x = x, y = predicted, fill = x)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, position = position_dodge(0.9),
                size = 1) +
  geom_bar(stat = "identity", 
           position = "dodge",
           color = "black",
           show.legend = F,
           width = 0.75,
           size = 0.7) +
  # geom_jitter(data = df_general, aes(y = Traveled_Dist, x = Group, color = Group),
  #             size = 3,
  #             position = position_jitterdodge(jitter.width = 0.15,
  #                                             dodge.width = 1),
  #             alpha = 0.3,
  #             show.legend = F,
  #             inherit.aes = F)+
  # geom_segment(data = predic_distancia, aes(x = as.numeric(x),
  #                                            xend = as.numeric(x) + 0.25,
  #                                            y = predicted, yend = predicted),
  #              size = 1.1, color = "black",
  #              linetype = 'dotted',
  #              inherit.aes = F,
  #              show.legend = F) +
  geom_label(data = predic_distancia, aes(x = x, 
                                           y = predicted,
                                           label = paste("", round(predicted, 2))),
             fill = NA, 
             # color = "black",
             size = 4, 
             fontface = "plain",
             label.padding = unit(0.15, "lines"),
             label.size = 0,
             position = position_nudge(x = 0, y = -33),
             color = ifelse(predic_distancia$x == "COM", "white", "black"),
             show.legend = F)+
  geom_text(data = predic_distancia, aes(x = x,
                                          y = conf.high + 70,
                                          # fill = group,
                                          label = c('a','ab','b','ab')),
            color = "black",
            size = 7,
            fontface = "plain",
            # label.padding = unit(0.3, "lines"), label.size = 0.5,
            position = position_dodge(width = 1.2),
            inherit.aes = F,
            show.legend = F)+
  theme_bw()+
  meu_tema+
  scale_color_manual(values = c('#000','#000','#000','#000','#928039','#928010'))+
  # scale_fill_manual(values = c('#8CA252','#BD9E39','#AD494A','#A55194','#928039','#928010'))+
  scale_fill_manual(values = c('#fff','#888','#888','#000','#ccc','#ccc'))+
  labs (x = '')+
  # scale_y_continuous(limits = c(0,5))+
  scale_x_discrete(labels = c("CTR" = "CTR \n(15)", "DEL" = "DEL \n(15)", "IMI" = "IMI \n(15)", 'COM' = 'COM \n(15)'))+
  labs (y = 'Distance traveled (cm)') 

#Proporção de tempo movimentando

plot_prop <- ggplot(predic_proporcao, aes(x = x, y = predicted, fill = x)) +
  geom_bar(aes(y = 1),stat = "identity", 
           position = "dodge",
           color = "black",
           fill = 'white',
           show.legend = F,
           width = 0.75,
           size = 0.7) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, position = position_dodge(0.9),
                size = 1) +
  geom_bar(stat = "identity", 
           position = "dodge",
           color = "black",
           show.legend = F,
           width = 0.75,
           size = 0.7) +
  # geom_jitter(data = df_general, aes(y = Traveled_Dist, x = Group, color = Group),
  #             size = 3,
  #             position = position_jitterdodge(jitter.width = 0.15,
  #                                             dodge.width = 1),
  #             alpha = 0.3,
  #             show.legend = F,
  #             inherit.aes = F)+
  # geom_segment(data = predic_distancia, aes(x = as.numeric(x),
  #                                            xend = as.numeric(x) + 0.25,
  #                                            y = predicted, yend = predicted),
  #              size = 1.1, color = "black",
  #              linetype = 'dotted',
  #              inherit.aes = F,
  #              show.legend = F) +
  geom_label(data = predic_proporcao, aes(x = x, 
                                          y = predicted,
                                          label = paste("", round(predicted, 2))),
             fill = NA, 
             # color = "black",
             size = 4, 
             fontface = "plain",
             label.padding = unit(0.15, "lines"),
             label.size = 0,
             position = position_nudge(x = 0, y = -.04),
             color = ifelse(predic_distancia$x == "COM", "white", "black"),
             show.legend = F)+
  geom_text(data = predic_proporcao, aes(x = x,
                                         y = conf.high +0.04 ,
                                         # fill = group,
                                         label = c('a','a','a','a')),
            color = "black",
            size = 7,
            fontface = "plain",
            # label.padding = unit(0.3, "lines"), label.size = 0.5,
            position = position_dodge(width = 1.2),
            inherit.aes = F,
            show.legend = F)+
  theme_bw()+
  meu_tema+
  scale_color_manual(values = c('#000','#000','#000','#000','#928039','#928010'))+
  # scale_fill_manual(values = c('#8CA252','#BD9E39','#AD494A','#A55194','#928039','#928010'))+
  scale_fill_manual(values = c('#fff','#888','#888','#000','#ccc','#ccc'))+
  labs (x = '')+
  # scale_y_continuous(limits = c(0,5))+
  scale_x_discrete(labels = c("CTR" = "CTR \n(15)", "DEL" = "DEL \n(15)", "IMI" = "IMI \n(15)", 'COM' = 'COM \n(15)'))+
  labs (y = 'Proportion of time moving') 


## Moving time
plot_moving <- ggplot(predic_moving, aes(x = x, y = predicted, fill = x)) +
  geom_bar(aes(y = 1),stat = "identity", 
           position = "dodge",
           color = "black",
           fill = 'white',
           show.legend = F,
           width = 0.75,
           size = 0.7) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, position = position_dodge(0.9),
                size = 1) +
  geom_bar(stat = "identity", 
           position = "dodge",
           color = "black",
           show.legend = F,
           width = 0.75,
           size = 0.7) +
  # geom_jitter(data = df_general, aes(y = Traveled_Dist, x = Group, color = Group),
  #             size = 3,
  #             position = position_jitterdodge(jitter.width = 0.15,
  #                                             dodge.width = 1),
  #             alpha = 0.3,
  #             show.legend = F,
  #             inherit.aes = F)+
  # geom_segment(data = predic_distancia, aes(x = as.numeric(x),
  #                                            xend = as.numeric(x) + 0.25,
  #                                            y = predicted, yend = predicted),
  #              size = 1.1, color = "black",
  #              linetype = 'dotted',
  #              inherit.aes = F,
  #              show.legend = F) +
  geom_label(data = predic_moving, aes(x = x, 
                                          y = predicted,
                                          label = paste("", round(predicted, 2))),
             fill = NA, 
             # color = "black",
             size = 4, 
             fontface = "plain",
             label.padding = unit(0.15, "lines"),
             label.size = 0,
             position = position_nudge(x = 0, y = - 10),
             color = ifelse(predic_distancia$x == "COM", "white", "black"),
             show.legend = F)+
  geom_text(data = predic_moving, aes(x = x,
                                         y = conf.high +15 ,
                                         # fill = group,
                                         label = c('a','a','a','a')),
            color = "black",
            size = 7,
            fontface = "plain",
            # label.padding = unit(0.3, "lines"), label.size = 0.5,
            position = position_dodge(width = 1.2),
            inherit.aes = F,
            show.legend = F)+
  theme_bw()+
  meu_tema+
  scale_color_manual(values = c('#000','#000','#000','#000','#928039','#928010'))+
  # scale_fill_manual(values = c('#8CA252','#BD9E39','#AD494A','#A55194','#928039','#928010'))+
  scale_fill_manual(values = c('#fff','#888','#888','#000','#ccc','#ccc'))+
  labs (x = '')+
  # scale_y_continuous(limits = c(0,5))+
  scale_x_discrete(labels = c("CTR" = "CTR \n(15)", "DEL" = "DEL \n(15)", "IMI" = "IMI \n(15)", 'COM' = 'COM \n(15)'))+
  labs (y = 'Moving time (s)')



#Grooming
plot_grooming <- ggplot(predic_limpeza_clean, aes(x = x, y = predicted, fill = x)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, position = position_dodge(0.9), 
                size = 1) +
  geom_bar(stat = "identity", 
           position = "dodge",
           color = "black",
           show.legend = F,
           width = 0.75,
           size = 0.7) +
  # geom_jitter(data = df_Limpeza_clean, aes(y = Tempo_segundos, x = Tratamento, color = Tratamento),
  #             size = 3,
  #             position = position_jitterdodge(jitter.width = 0.15,
  #                                             dodge.width = 1),
  #             alpha = 0.3,
  #             show.legend = F,
  #             inherit.aes = F)+
  # geom_segment(data = predic_limpeza_clean, aes(x = as.numeric(x),
  #                                           xend = as.numeric(x) + 0.25,
  #                                           y = predicted, yend = predicted),
  #              size = 1.1, color = "black",
  #              linetype = 'dotted',
  #              inherit.aes = F,
  #              show.legend = F) +
  geom_label(data = predic_limpeza_clean, aes(x = x, 
                                          y = predicted + ifelse(predic_limpeza_clean$x != "CTR", y = -0.3,0),
                                          label = paste("", round(predicted, 2))),
             fill = NA, 
             # color = "black",
             size = 4, 
             fontface = "plain",
             label.padding = unit(0.15, "lines"),
             label.size = 0,
             position = position_nudge(x = 0, y = -2.1),
             color = ifelse(predic_limpeza_clean$x == "COM", "white", "black"),
             show.legend = F)+
  geom_text(data = predic_limpeza_clean, aes(x = x,
                                         y = conf.high + 5,
                                         # fill = group,
                                         label = c('c','b','a','b')),
            color = "black",
            size = 7,
            fontface = "plain",
            # label.padding = unit(0.3, "lines"), label.size = 0.5,
            position = position_dodge(width = 1.2),
            inherit.aes = F,
            show.legend = F)+
  theme_bw()+
  meu_tema+
  scale_color_manual(values = c('#000','#000','#000','#000','#928039','#928010'))+
  # scale_fill_manual(values = c('#8CA252','#BD9E39','#AD494A','#A55194','#928039','#928010'))+
  scale_fill_manual(values = c('#fff','#888','#888','#000','#ccc','#ccc'))+
  labs (x = '')+
  # scale_y_continuous(limits = c(0,5))+
  scale_x_discrete(labels = c("CTR" = "CTR \n(14)", "DEL" = "DEL \n(14)", "IMI" = "IMI \n(13)", 'COM' = 'COM \n(13)'))+
  labs (y = 'Grooming time (s)') 

# Feedingd
predic_consumo_summary <- predic_consumo %>%
  group_by(x) %>%
  summarise(predicted_mean = mean(predicted)*1000,
            conf.low = mean(conf.low)*1000,
            conf.high = mean(conf.high)*1000)

# Gráfico de barras com intervalos de confiança
plot_cons_barras <- ggplot(predic_consumo_summary, aes(x = x, y = predicted_mean, fill = x)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, position = position_dodge(0.9),
                size = 1) +
  geom_bar(stat = "identity", 
           position = "dodge",
           color = "black",
           show.legend = F,
           width = 0.75,
           size = 0.7) +
  # geom_jitter(data = df_consumo, aes(y = Consumo, x = Trat, color = Trat),
  #             size = 3,
  #             position = position_jitterdodge(jitter.width = 0.15,
  #                                             dodge.width = 1),
  #             alpha = 0.3,
  #             show.legend = F,
  #             inherit.aes = F)+
  # geom_segment(data = predic_consumo_summary, aes(x = as.numeric(x),
  #                                               xend = as.numeric(x) + 0.25,
  #                                               y = predicted_mean, yend = predicted_mean),
  #              size = 1.1, color = "black",
  #              linetype = 'dotted',
  #              inherit.aes = F,
  #              show.legend = F) +
  geom_label(data = predic_consumo_summary, aes(x = x, 
                                              y = predicted_mean,
                                              label = paste("", round(predicted_mean, 2))),
             fill = NA, 
             # color = "black",
             size = 4, 
             fontface = "plain",
             label.padding = unit(0.15, "lines"),
             label.size = 0,
             position = position_nudge(x = 0, y = -2),
             color = ifelse(predic_consumo_summary$x == "COM", "white", "black"),
             show.legend = F)+
  geom_text(data = predic_consumo_summary, aes(x = x,
                                             y = conf.high + 3,
                                             # fill = group,
                                             label = c('a','a','a','a')),
            color = "black",
            size = 7,
            fontface = "plain",
            # label.padding = unit(0.3, "lines"), label.size = 0.5,
            position = position_dodge(width = 1.2),
            inherit.aes = F,
            show.legend = F)+
  theme_bw()+
  meu_tema+
  scale_color_manual(values = c('#000','#000','#000','#000','#928039','#928010'))+
  # scale_fill_manual(values = c('#8CA252','#BD9E39','#AD494A','#A55194','#928039','#928010'))+
  scale_fill_manual(values = c('#fff','#888','#888','#000','#ccc','#ccc'))+
  labs (x = '')+
  # scale_y_continuous(limits = c(0,5))+
  scale_x_discrete(labels = c("CTR" = "CTR \n(5)", "DEL" = "DEL \n(5)", "IMI" = "IMI \n(5)", 'COM' = 'COM \n(5)'))+
  labs(y = expression(bold(Sucrose~consumption~italic(per)~bee~("mg/ 24h"))))


#Grafico de Consumo X mortalidade
plot_cons_linhas <- ggplot(predic_consumo, aes(x = factor(group), y = predicted, fill = x, shape = x, group = x)) +
  geom_errorbar(aes(ymin = predicted - std.error, ymax = predicted + std.error), 
                width = 0.3, 
                position = position_dodge(width = 0.3),
                alpha = 0.5) +
  geom_line(size = 0.7, position = position_dodge(width = 0.3)) +
  geom_point(size = 4, position = position_dodge(width = 0.3)) +
  theme_bw() +
  scale_color_manual(values = c('#000','#000','#000','#000','#928039','#928010')) +
  scale_fill_manual(values = c('#fff','#888','#888','#000','#ccc','#ccc')) +
  labs(x = '', y = expression(Sucrose~consumption~italic(per)~cage~(g)))+
  scale_shape_manual(values = c(21, 22, 23, 24))+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","9","10"))+
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold", color = "black",size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14, color = 'black', face = 'plain'),
    axis.text.y = element_text(size = 14, color = 'black'),
    legend.position = c(0.9, .8),
    legend.title = element_blank(),
    legend.background = element_blank(),
    panel.border = element_blank(),  
    panel.grid.major.y = element_line(color = "#fff", linetype = "dashed"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = NA),
    plot.background = element_rect(fill = NA),
    axis.line.y.left = element_line(color = 'black'),  
    axis.line.x = element_line(color = 'black'),  
    axis.line.y = element_line(color = 'red'),  
    axis.line.x.top = element_line(color = "white"))


# Wing fanning
plot_voo <- ggplot(predic_voo, aes(x = x, y = predicted, fill = x)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, position = position_dodge(0.9),
                size = 1) +
  geom_bar(stat = "identity", 
           position = "dodge",
           color = "black",
           show.legend = F,
           width = 0.75,
           size = 0.7) +
  # geom_jitter(data = df_Tentativa_voo, aes(y = Tempo_segundos, x = Tratamento, color = Tratamento),
  #             size = 3,
  #             position = position_jitterdodge(jitter.width = 0.15,
  #                                             dodge.width = 1),
  #             alpha = 0.3,
  #             show.legend = F,
  #             inherit.aes = F)+
  # geom_segment(data = predic_voo, aes(x = as.numeric(x),
  #                                               xend = as.numeric(x) + 0.25,
  #                                               y = predicted, yend = predicted),
  #              size = 1.1, color = "black",
  #              linetype = 'dotted',
  #              inherit.aes = F,
  #              show.legend = F) +
  geom_label(data = predic_voo, aes(x = x, 
                                              y = predicted,
                                              label = paste("", round(predicted, 2))),
             fill = NA, 
             # color = "black",
             size = 4, 
             fontface = "plain",
             label.padding = unit(0.15, "lines"),
             label.size = 0,
             # position = ifelse(predic_voo$x =! 'IMI',position_nudge(x = 0, y = -3), position_nudge(x = 0, y = +4)),
             position = position_nudge(x = 0, y = -3.5),
             color = ifelse(predic_voo$x == "COM", "white", "black"),
             show.legend = F)+
  geom_text(data = predic_voo, aes(x = x,
                                             y = conf.high + 7,
                                             # fill = group,
                                             label = c('a','a','a','a')),
            color = "black",
            size = 7,
            fontface = "plain",
            # label.padding = unit(0.3, "lines"), label.size = 0.5,
            position = position_dodge(width = 1.2),
            inherit.aes = F,
            show.legend = F)+
  theme_bw()+
  meu_tema+
  scale_color_manual(values = c('#000','#000','#000','#000','#928039','#928010'))+
  # scale_fill_manual(values = c('#8CA252','#BD9E39','#AD494A','#A55194','#928039','#928010'))+
  scale_fill_manual(values = c('#fff','#888','#888','#000','#ccc','#ccc'))+
  labs (x = '')+
  # scale_y_continuous(limits = c(0,5))+
  scale_x_discrete(labels = c("CTR" = "CTR \n(13)", "DEL" = "DEL \n(13)", "IMI" = "IMI \n(14)", 'COM' = 'COM \n(12)'))+
  labs (y = 'Wing fanning (s)') 

# Repouso
plot_resting <- ggplot(predic_rep, aes(x = x, y = predicted, fill = x)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, position = position_dodge(0.9),
                size = 1) +
  geom_bar(stat = "identity", 
           position = "dodge",
           color = "black",
           show.legend = F,
           width = 0.75,
           size = 0.7) +
  # geom_jitter(data = df_rep_clean, aes(y = Tempo_segundos, x = Tratamento, color = Tratamento),
  #             size = 3,
  #             position = position_jitterdodge(jitter.width = 0.15,
  #                                             dodge.width = 1),
  #             alpha = 0.3,
  #             show.legend = F,
  #             inherit.aes = F)+
  # geom_segment(data = predic_rep, aes(x = as.numeric(x),
  #                                     xend = as.numeric(x) + 0.25,
  #                                     y = predicted, yend = predicted),
  #              size = 1.1, color = "black",
  #              linetype = 'dotted',
  #              inherit.aes = F,
  #              show.legend = F) +
  geom_label(data = predic_rep, aes(x = x, 
                                    y = predicted + ifelse(x == "DEL", +5.2,0),
                                    label = paste("", round(predicted, 2))),
             fill = NA, 
             # color = "black",
             size = 4, 
             fontface = "plain",
             label.padding = unit(0.15, "lines"),
             label.size = 0,
             position = position_nudge(x = 0, y = -2.4),
             color = ifelse(predic_voo$x == "COM", "white", "black"),
             show.legend = F)+
  geom_text(data = predic_rep, aes(x = x,
                                   y = conf.high + 5,
                                   # fill = group,
                                   label = c('a','b','a','a')),
            color = "black",
            size = 7,
            fontface = "plain",
            # label.padding = unit(0.3, "lines"), label.size = 0.5,
            position = position_dodge(width = 1.2),
            inherit.aes = F,
            show.legend = F)+
  theme_bw()+
  meu_tema+
  scale_color_manual(values = c('#000','#000','#000','#000','#928039','#928010'))+
  # scale_fill_manual(values = c('#8CA252','#BD9E39','#AD494A','#A55194','#928039','#928010'))+
  scale_fill_manual(values = c('#fff','#888','#888','#000','#ccc','#ccc'))+
  labs (x = '')+
  # scale_y_continuous(limits = c(0,5))+
  scale_x_discrete(labels = c("CTR" = "CTR \n(13)", "DEL" = "DEL \n(13)", "IMI" = "IMI \n(12)", 'COM' = 'COM \n(13)'))+
  labs (y = 'Resting (s)') 

#versao 16/11
# Bibliotecas -------------------------------------------------------------

library(tidyverse)
library(readr)
library(glmmTMB)
library(performance)
library(DHARMa)
library(car)
library(rstatix)
library(sjPlot)
library(ggeffects)
library(dplyr)
library(readxl)
library(patchwork)
library(emmeans)
library(MuMIn)


# Importação --------------------------------------------------------------

df <- read_csv("Observação_comportamental_ mortalidade e consumo - Comportamento.csv")

df$REP <- factor(df$REP)
df$Comportamento <- factor (df$Comportamento)
df$Tempo_segundos_vagner <- as.numeric (df$Tempo_segundos_vagner)
df$Tempo_segundos <- as.numeric(df$Tempo_segundos)
df$Tratamento <- factor (df$Tratamento, levels = c('COM','IMI','CTR','DEL'))


# Grooming ----------------------------------------------------------------
df_limpeza <- df %>% 
  filter (Comportamento == 'Limpeza')

# exploraçção -------------------------------------------------------------
plot.vagner <- ggplot(df_limpeza, (aes (y = Tempo_segundos_vagner, x = Tratamento)))+
  geom_boxplot()+
  geom_point()+
  labs (title = 'Vagner')

plot.stefani <-ggplot(df_limpeza, (aes (y = Tempo_segundos, x = Tratamento)))+
  geom_boxplot()+
  geom_point()+
  labs (title = 'Stefani')

plot.stefani + plot.vagner
# Seleção de modelos  para LIMPEZA------------------------------------------------------
mod_limpeza.1 <- glmmTMB(Tempo_segundos_vagner ~ Tratamento,
                         family = tweedie (),
                         data = df_limpeza)

mod_limpeza.2 <- glmmTMB(Tempo_segundos_vagner ~ Tratamento + (1| REP),
                         family = tweedie (),
                         data = df_limpeza)

model.sel(mod_limpeza.1, mod_limpeza.2)

plot(simulateResiduals(mod_limpeza.2))

summary(mod_limpeza.2)

# Comparações multiplas
emmeans_vel <- emmeans(mod_limpeza.2, pairwise ~ Tratamento, adjust = "Tukey", type = 'response')
emmeans_vel$contrasts

#Tamanhos de efeito
tab_model(mod_limpeza.2)

predic_limpeza <- ggpredict(mod_limpeza.2, type ="fixed" , terms = c("Tratamento"), ci_level = 0.95)

plot_limpeza <- ggplot(predic_limpeza, aes(x = x, y = predicted, fill = x)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, position = position_dodge(0.9), 
                size = 1) +
  geom_bar(stat = "identity", 
           position = "dodge",
           color = "black",
           show.legend = F,
           width = 0.75,
           size = 0.7) +
  # geom_jitter(data = df_Limpeza_clean, aes(y = Tempo_segundos, x = Tratamento, color = Tratamento),
  #             size = 3,
  #             position = position_jitterdodge(jitter.width = 0.15,
  #                                             dodge.width = 1),
  #             alpha = 0.3,
  #             show.legend = F,
  #             inherit.aes = F)+
  # geom_segment(data = predic_limpeza_clean, aes(x = as.numeric(x),
  #                                           xend = as.numeric(x) + 0.25,
  #                                           y = predicted, yend = predicted),
  #              size = 1.1, color = "black",
  #              linetype = 'dotted',
  #              inherit.aes = F,
  #              show.legend = F) +
  geom_label(data = predic_limpeza, aes(x = x, 
                                        y = predicted + ifelse(predic_limpeza$x != "CTR", y = -0.3,0),
                                        label = paste("", round(predicted, 2))),
             fill = NA, 
             # color = "black",
             size = 4, 
             fontface = "plain",
             label.padding = unit(0.15, "lines"),
             label.size = 0,
             position = position_nudge(x = 0, y = -4.16),
             color = ifelse(predic_limpeza$x == "COM", "white", "black"),
             show.legend = F)+
  geom_text(data = predic_limpeza, aes(x = x,
                                       y = conf.high + 6.5,
                                       # fill = group,
                                       label = c('c','b','a','a')),
            color = "black",
            size = 7,
            fontface = "plain",
            # label.padding = unit(0.3, "lines"), label.size = 0.5,
            position = position_dodge(width = 1.2),
            inherit.aes = F,
            show.legend = F)+
  theme_bw()+
  meu_tema+
  scale_color_manual(values = c('#000','#000','#000','#000','#928039','#928010'))+
  # scale_fill_manual(values = c('#8CA252','#BD9E39','#AD494A','#A55194','#928039','#928010'))+
  scale_fill_manual(values = c('#fff','#888','#888','#000','#ccc','#ccc'))+
  labs (x = '')+
  # scale_y_continuous(limits = c(0,5))+
  scale_x_discrete(labels = c("CTR" = "CTR \n(15)", "DEL" = "DEL \n(15)", "IMI" = "IMI \n(15)", 'COM' = 'COM \n(15)'))+
  labs (y = 'Grooming time (s)') 


# Wing fanning ------------------------------------------------------------
df_wing <- df %>% 
  filter (Comportamento == 'Tentativa_voo')

# exploraçção -------------------------------------------------------------
ggplot(df_wing, (aes (y = Tempo_segundos_vagner, x = Tratamento)))+
  geom_boxplot()+
  geom_point()+
  labs (title = 'Vagner')

ggplot(df_wing, (aes (y = Tempo_segundos, x = Tratamento)))+
  geom_boxplot()+
  geom_point()+
  labs (title = 'Stefani')


# Seleção de modelos  para  WING------------------------------------------------------

mod_wing.1 <- glmmTMB(Tempo_segundos_vagner ~ Tratamento,
                      family = tweedie (),
                      data = df_wing)

mod_wing.2 <- glmmTMB(Tempo_segundos_vagner ~ Tratamento + (1| REP),
                      family = tweedie (),
                      data = df_wing)

model.sel(mod_wing.1, mod_wing.2)

plot(simulateResiduals(mod_wing.1))

summary(mod_wing.1)

#Tamanhos de efeito
tab_model(mod_wing.1)

predic_wing <- ggpredict(mod_wing.1, type ="fixed" , terms = c("Tratamento"), ci_level = 0.95)

plot_wing <- ggplot(predic_wing, aes(x = x, y = predicted, fill = x)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, position = position_dodge(0.9), 
                size = 1) +
  geom_bar(stat = "identity", 
           position = "dodge",
           color = "black",
           show.legend = F,
           width = 0.75,
           size = 0.7) +
  # geom_jitter(data = df_Limpeza_clean, aes(y = Tempo_segundos, x = Tratamento, color = Tratamento),
  #             size = 3,
  #             position = position_jitterdodge(jitter.width = 0.15,
  #                                             dodge.width = 1),
  #             alpha = 0.3,
  #             show.legend = F,
  #             inherit.aes = F)+
  # geom_segment(data = predic_limpeza_clean, aes(x = as.numeric(x),
  #                                           xend = as.numeric(x) + 0.25,
  #                                           y = predicted, yend = predicted),
  #              size = 1.1, color = "black",
  #              linetype = 'dotted',
  #              inherit.aes = F,
  #              show.legend = F) +
  geom_label(data = predic_wing, aes(x = x, 
                                     y = predicted + ifelse(predic_wing$x != "CTR", y = -0.3,0),
                                     label = paste("", round(predicted, 2))),
             fill = NA, 
             # color = "black",
             size = 4, 
             fontface = "plain",
             label.padding = unit(0.15, "lines"),
             label.size = 0,
             position = position_nudge(x = 0, y = -4.16),
             color = ifelse(predic_wing$x == "COM", "white", "black"),
             show.legend = F)+
  geom_text(data = predic_wing, aes(x = x,
                                    y = conf.high + 6.5,
                                    # fill = group,
                                    label = c('a','a','a','a')),
            color = "black",
            size = 7,
            fontface = "plain",
            # label.padding = unit(0.3, "lines"), label.size = 0.5,
            position = position_dodge(width = 1.2),
            inherit.aes = F,
            show.legend = F)+
  theme_bw()+
  meu_tema+
  scale_color_manual(values = c('#000','#000','#000','#000','#928039','#928010'))+
  # scale_fill_manual(values = c('#8CA252','#BD9E39','#AD494A','#A55194','#928039','#928010'))+
  scale_fill_manual(values = c('#fff','#888','#888','#000','#ccc','#ccc'))+
  labs (x = '')+
  # scale_y_continuous(limits = c(0,5))+
  scale_x_discrete(labels = c("CTR" = "CTR \n(15)", "DEL" = "DEL \n(15)", "IMI" = "IMI \n(15)", 'COM' = 'COM \n(15)'))+
  labs (y = 'Wing fanning time (s)') 



# União de graficos -------------------------------------------------------
plot_dist +plot_speed +  plot_cons_barras+
  plot_layout(widths = c(0.35, 0.35, 0.3)) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 16, face = "bold"),
        plot.background = element_rect(fill = NA, color = NA),   # fundo do plot
        panel.background = element_rect(fill = NA, color = NA))

plot_grooming + plot_voo + plot_resting + plot_moving+
  plot_layout(widths = c(0.25, 0.25, 0.25,0.25)) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 16, face = "bold"),
        plot.background = element_rect(fill = NA, color = NA),   # fundo do plot
        panel.background = element_rect(fill = NA, color = NA))

plot_cons_barras + plot_cons_linhas  +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 16, face = "bold"),
        plot.background = element_rect(fill = NA, color = NA),   # fundo do plot
        panel.background = element_rect(fill = NA, color = NA))


## NOvos graficos com distribuição tweedie, e sem remoção dde outliers

plot_limpeza + plot_wing + plot_moving+
  plot_layout(widths = c(0.3, 0.3, 0.3)) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 16, face = "bold"),
        plot.background = element_rect(fill = NA, color = NA),   # fundo do plot
        panel.background = element_rect(fill = NA, color = NA))




