#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("Brak argumentów")
}

Dat <- read.csv2(file=args[1], header=TRUE)

#install.packages("knitr")
library(knitr)
library(dplyr)
library(Hmisc)
library(ggpubr)
library(dunn.test)
library(FSA)
library(car)
library(ggplot2)

#WYLICZENIE SREDNICH I UZUPELNIENIE NIMI BRAKOW
AvgFill <- function(Dataa)
{
  ValAvg <- data.frame(matrix(0,ncol = ncol(Dataa), nrow = length(unique(Dataa[,1])))) #Dataframe wartości do średniej
  colnames(ValAvg) <- colnames(Dataa)
  rownames(ValAvg) <- unique(Dataa[,1])
  GroupNum <- 1 #Numer obecnej grupy
  group_col <- which(names(Dat) == "grupa")
  CurGroup <- Dataa[1,group_col] #Obecnie sprawdzana grupa
  NumOfVal <- data.frame(matrix(0,ncol = ncol(Dataa), nrow = length(unique(Dataa[,1])))) #Dataframe przechowujący ilość dodanych wartości do dataframe ValAvg
  i <- 1 
  while(i <= nrow(Dataa))
  {
    CurCol<-1 #Obecna kolumna
    for(j in Dataa[i,])
    {
      if(CurGroup == Dataa[i, 1])
      {
        if(is.numeric(j)==TRUE && is.na(j)==FALSE) #Nie liczymy pustych pól i bierzemy pod uwagę wyłącznie wartości numeryczne
        {
            ValAvg[GroupNum, CurCol] <- ValAvg[GroupNum, CurCol]+j
            NumOfVal[GroupNum, CurCol] <- NumOfVal[GroupNum, CurCol] + 1
        }
      }
      CurCol <- CurCol + 1
    }
    if(CurGroup != Dataa[i,group_col])
    {
        CurGroup <- Dataa[i,group_col]
        GroupNum <- GroupNum +1
        i <- i-1
    }
    i <- i + 1
  }
  for(i in 1:nrow(NumOfVal)) #Podzielenie każdej komórki obu dataframe'ów
  {
    for(j in 1:ncol(NumOfVal))
    {
      if(NumOfVal[i,j] != 0)
      ValAvg[i,j] <- ValAvg[i,j]/NumOfVal[i,j]
    }
  }
  
  for(i in 1:nrow(Dataa))
  {
    for(j in 1:ncol(Dataa))
    {
      if(is.na(Dataa[i,j]))
      {
           for(k in 1:nrow(ValAvg))
           {
             for(l in 1:ncol(ValAvg))
             {
               if(colnames(Dataa)[j]==colnames(ValAvg)[l] && Dataa[i,group_col]==rownames(ValAvg)[k])
               {
                 Dataa[i,j] <- ValAvg[k,l]
                 cat("Umieszczono wartość ",ValAvg[k,l]," w komórce [",i,",",j,"]\n")
               }
             }
           }
      }
    }
  }
  return(Dataa)
}

Dat <- AvgFill(Dat)

#WARTOŚCI ODSTAJĄCE
odst <- Dat %>%
  group_by(grupa) %>%
reframe(across(where(is.numeric), ~toString(boxplot.stats(.)$out)))
odst[odst == ""] <- NA
cat("Tabela wartości odstających:")
odst

#       STATYSTYKI OPISOWE
#MEDIANA
mediana <- Dat %>%
  group_by(grupa) %>%
summarise(across(where(is.numeric),median))
print("MEDIANY:")
mediana
jpeg("mediany.jpeg", width=800, height=600, quality=100)
par(bg="#68E4A2")
boxplot(mediana[,2:ncol(mediana)],main="Mediany",col= "#E888FF")
dev.off()

#ŚREDNIA
srednia <- Dat %>%
  group_by(grupa) %>%
  summarise(across(where(is.numeric),mean))
print("ŚREDNIE:")
srednia
jpeg("srednie.jpeg", width=800, height=600, quality=100)
par(bg="#68E4A2")
boxplot(srednia[,2:ncol(srednia)], main="Średnie", col= "#E888FF")
dev.off()
#WARTOSC MAKSYMALNA
maksymalna <- Dat %>%
  group_by(grupa) %>%
  summarise(across(where(is.numeric),max))
print("MAKSYMALNE:")
maksymalna
jpeg("maksymalne.jpeg", width=800, height=600, quality=100)
par(bg="#68E4A2")
boxplot(maksymalna[,2:ncol(maksymalna)], main="Maksymalne",col= "#E888FF")
dev.off()
#WARTOSC MINIMALNA
minimalna <- Dat %>%
  group_by(grupa) %>%
  summarise(across(where(is.numeric),min))
print("MINIMALNE:")
minimalna
jpeg("minimalne.jpeg", width=800, height=600, quality=100)
par(bg="#68E4A2")
boxplot(minimalna[,2:ncol(minimalna)],main="Minimalne",col= "#E888FF")
dev.off()
#I KWARTYL
kwartyl1 <- Dat %>%
  group_by(grupa) %>%
  summarise(across(where(is.numeric),quantile, probs = 0.25))
print("I KWARTYLE:")
kwartyl1
jpeg("kwartyl1.jpeg", width=800, height=600, quality=100)
par(bg="#68E4A2")
boxplot(kwartyl1[,2:ncol(kwartyl1)],main="I kwartyle",col= "#E888FF")
dev.off()
#III KWARTYL
kwartyl3 <- Dat %>%
  group_by(grupa) %>%
  summarise(across(where(is.numeric),quantile, probs = 0.75))
print("III KWARTYLE:")
kwartyl3
jpeg("kwartyl3.jpeg", width=800, height=600, quality=100)
par(bg="#68E4A2")
boxplot(kwartyl3[,2:ncol(kwartyl3)], main="III kwartyle",col= "#E888FF")
dev.off()
######
#     PORÓWNIANIE GRUP
#ROZKLAD NORMALNY
pvalueShapiroTest <- Dat %>%
  group_by(grupa) %>%
  summarise_if(is.numeric, list(
    p.value = ~shapiro.test(.)$p.value
  ))

shapiros <- data.frame(grupa = pvalueShapiroTest$grupa)

for (i in 2:ncol(pvalueShapiroTest)) {
  col_name <- colnames(pvalueShapiroTest)[i]
  new_col <- ifelse(pvalueShapiroTest[, i] > 0.05, TRUE, FALSE)
  shapiros[, col_name] <- new_col
}

czy_normalny <- data.frame(matrix(nrow = 1,ncol = ncol(shapiros)))
colnames(czy_normalny) <- colnames(shapiros)

for(i in 2:ncol(shapiros)){
  if(all(shapiros[,i])){
    czy_normalny[1,i] <- TRUE
  }
  else{
    czy_normalny[1,i] <- FALSE
  }
}

czy_normalny <- subset(czy_normalny, select = -grupa)
#JEDNORODNOSC WARIANCJI
leveneTestresults <- Dat %>%
  ungroup() %>%
  summarise(across(where(is.numeric), ~leveneTest(.x, as.factor(grupa),data=Dat)$"Pr(>F)"[1]))

levenes <- data.frame(matrix(nrow=1,ncol=ncol(leveneTestresults)))
colnames(levenes) <- colnames(leveneTestresults)

for (i in 1:ncol(leveneTestresults)) {
  col_name <- colnames(leveneTestresults)[i]
  new_col <- ifelse(leveneTestresults[, i] > 0.05, TRUE, FALSE)
  levenes[, col_name] <- new_col
}

colnames(czy_normalny) <- colnames(levenes)

num_cols <- c()
for(i in 1:ncol(Dat)){
  if(is.numeric(Dat[,i]))
  {
      num_cols <- c(num_cols, i)
  }
}

if(length(unique(Dat$grupa))>2){
  j <- 1
  for (i in num_cols) {
    if (czy_normalny[,j] && levenes[,j]) {
        #ANOVA
        aov(Dat[,i] ~ grupa, data = Dat)
        summary(aov(Dat[,i] ~ grupa, data = Dat))
        result <- summary(aov(Dat[,i] ~ grupa, data = Dat))[[1]][["Pr(>F)"]]
      if(result[1] < 0.05){
          cat("ANOVA:\n")
          cat("Są różnice między grupami w kolumnie", colnames(Dat[i]), "\n\n")
          
          tukeyres <- TukeyHSD(aov(Dat[,i] ~ grupa, data = Dat))
          cat("Tukey:\n")
          print(tukeyres)
          cat("\nRóżnice występują między grupami:\n")
          for(i in 1:nrow(tukeyres$grupa)){
            if(tukeyres$grupa[i, "p adj"]<0.05)
            {
              print(rownames(tukeyres$grupa)[1])
            }
          }
          cat("\n\n")

         
        } else {
          cat("ANOVA:\n")
          cat("Brak różnic między grupami w kolumnie", colnames(Dat[i]), "\n\n")
        }
    } else {
      #Kruskal
      kruskal.test(Dat[,i] ~ grupa, data = Dat)
      result <- kruskal.test(Dat[,i] ~ grupa, data = Dat)$p.value
      if(result < 0.05) {
        cat("Kruskal:\n")
        cat("Są różnice między grupami w kolumnie", colnames(Dat[i]), "\n")
        
        cat("Dunn:\n")
        dunnres <- dunnTest(Dat[,i], Dat$grupa)
        print(dunnres)
        cat("\nRóżnice między występują między grupami:\n")
        for(i in 1:nrow(dunnres$res)){
          if(dunnres$res$P.adj[i]<0.05)
          {
            print(dunnres$res$Comparison[i])
          }
        }
        cat("\n\n")
        
      } else {
        cat("Kruskal:\n")
        cat("Brak różnic między grupami w kolumnie", colnames(Dat[i]), "\n\n")
      }
    }
    j <- j+1
  }
} else if(length(unique(Dat$grupa)) == 2) {
  j <- 1
  for(i in num_cols){
    if(czy_normalny[,j] && levenes[,j]){
      #t-student
      t.test(Dat[,i] ~ grupa, data = Dat)
      result <- t.test(Dat[,i] ~ grupa, data = Dat, var.equal = TRUE)$p.value
      if(result < 0.05){
        cat("T-student:\n")
        cat("Są różnice między grupami w kolumnie", colnames(Dat[i]), "\n")
        cat(result, "\n\n")
      } else {
        cat("T-student:\n")
        cat("Brak różnic między grupami w kolumnie", colnames(Dat[i]), "\n\n")
      }
    } else if (czy_normalny[,j] == TRUE && levenes[,j] == FALSE){
      #welch
      t.test(Dat[,i] ~ grupa, data = Dat, var.equal = FALSE)
      result <- t.test(Dat[,i] ~ grupa, data = Dat, var.equal = FALSE)$p.value
      if(result < 0.05){
        cat("Welch:\n")
        cat("Są różnice między grupami w kolumnie", colnames(Dat[i]), "\n")
        cat(result, "\n\n")
      } else {
        cat("Welch:\n")
        cat("Brak różnic między grupami w kolumnie", colnames(Dat[i]), "\n\n")
      }
    } else {
      #wilcoxon
      wilcox.test(Dat[,i] ~ grupa, data = Dat)
      result <- wilcox.test(Dat[,i] ~ grupa, data = Dat)$p.value
      if(result < 0.05){
        cat("Wilcoxon:\n")
        cat("Są różnice między grupami w kolumnie", colnames(Dat[i]), "\n")
        cat(result, "\n\n")
      } else {
        cat("Wilcoxon:\n")
        cat("Brak różnic między grupami w kolumnie", colnames(Dat[i]), "\n\n")
      }
    }
    j <- j+1
  }
}
colnames(shapiros) <- colnames(czy_normalny)
nazwy <-intersect(names(shapiros), names(Dat))

#korelacje
for(g in 1:length(unique(Dat$grupa))){
  for (i in nazwy) {
    for (j in nazwy) {
      if(i<j)
      {
        if(shapiros[g,i]==TRUE && shapiros[g,j]==TRUE){
          jaki <-1
          korelacje <- cor.test(Dat[Dat$grupa == unique(Dat$grupa)[g],i],Dat[Dat$grupa == unique(Dat$grupa)[g],j], method = "pearson")
        } else {
          jaki <- 2
          korelacje <- cor.test(Dat[Dat$grupa == unique(Dat$grupa)[g],i],Dat[Dat$grupa == unique(Dat$grupa)[g],j], method = "spearman")
        }
        danew<-subset(Dat,grupa==unique(Dat$grupa)[g])
        if(korelacje$p.value<0.05){
          if(jaki == 1){
            p <- ggplot(danew, aes_string(x = i, y = j)) +
              geom_point() +
              labs(title = paste("Porównanie kolumn: ", i, " oraz ", j, " w grupie ", unique(Dat$grupa)[g]))+
              geom_smooth(method = "lm", se = FALSE, color = "red")+
              theme(plot.background = element_rect(fill = "#68E4A2"))
            
            p
          } else {
            p <- ggplot(danew, aes_string(x = i, y = j)) +
              geom_point() +
              labs(title = paste("Porównanie kolumn: ", i, " oraz ", j, " w grupie ", unique(Dat$grupa)[g]))+
              theme(plot.background = element_rect(fill = "#68E4A2"))
              p
            
          }
          ggsave(paste("wykres_",i,"_",j,"_",unique(Dat$grupa)[g],".jpeg"), plot = p)
          cat("Test korelacji dla kolumn:", colnames(Dat[i]), " oraz ", colnames(Dat[j]), "w grupie:", unique(Dat$grupa)[g], "\n")
          cat("Istnieją korelacje między zmiennymi\n")
          cat("Współczynnik korelacji: ", korelacje$estimate, "\n")
          if(-1<korelacje$estimate && korelacje$estimate <= 0.7){
            cat("Jest to bardzo silna korelacja ujemna\n\n")
          } else if(-0.7<korelacje$estimate && korelacje$estimate <=-0.5) {
            cat("Jest to silna korelacja ujemna\n\n")
          } else if(-0.5<korelacje$estimate && korelacje$estimate <=-0.3) {
            cat("Jest to korelacja ujemna o średnim natężeniu\n\n")
          } else if(-0.3<korelacje$estimate && korelacje$estimate <=-0.2) {
            cat("Jest to słaba korelacja ujemna\n\n")
          } else if(-0.2<korelacje$estimate && korelacje$estimate <=0.2) {
            cat("Brak korelacji\n\n")
          } else {
            cat("Brak korelacji\n\n")
          }
        }
      }
    }
  }
}

