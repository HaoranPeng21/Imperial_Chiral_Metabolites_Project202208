#install.packages("openxlsx")
library(openxlsx)
setwd("/Users/mac/Desktop/Imperial\ College\ of\ London/7-Isabel\ group/1--data")
####get_adj_p####
get_adj_p <- function(data, .col, .grp = "Sample", comparisons = NULL,
                      method = "wilcox.test", p.adjust.method = "fdr", p.digits = 3L, ...) {
  # Compute p-values
  comparison.formula <- paste0(.col, "~", .grp) %>%
    as.formula()
  pvalues <- ggpubr::compare_means(
    formula = comparison.formula, data = data,
    method = method,
    p.adjust.method = p.adjust.method,
    ...
  )
  
  # If a comparison list is provided, extract the comparisons of interest for plotting
  if (!is.null(comparisons)) {
    pvalues <- purrr::map_df(comparisons, ~ pvalues %>% dplyr::filter(group1 == .x[1] & group2 == .x[2]))
  }
  
  # P-value y coordinates
  y.max <- data %>%
    dplyr::pull(.col) %>%
    max(na.rm = TRUE)
  p.value.y.coord <- rep(y.max, nrow(pvalues))
  
  step.increase <- (1:nrow(pvalues)) * (y.max / 10)
  p.value.y.coord <- p.value.y.coord + step.increase
  
  pvalues <- pvalues %>%
    dplyr::mutate(
      y.position = p.value.y.coord,
      p.adj = format.pval(.data$p.adj, digits = p.digits)
    )
  
  pvalues
}

####box_plot####
library(ggplot2)
library(ggplotify)
require(cowplot)
require(tidyverse)
require(ggsci)
require(ggpubr)

#stat_compare_means(method = "ANOVA")
data<- read.xlsx("concentration_result_new.xlsx", sheet = 5)

#data$Group.name<- as.character(data$Group.name)


#compare_means(L_valine ~ est_eco,  data = df)
my_comparisons <- list(c("Athlete","Control"))
#data$Group.name.3 <- gsub("Unhealthy_Control","UH_Control",data$Group.name.3)
#data$Group.name.3 <- gsub("Healthy_Control","H_Control",data$Group.name.3)
#data$Group.name.3 <- gsub("Healthy_Athletes","H_Athletes",data$Group.name.3)
my_comparisons <- list( c("Unhealthy_Control", "Healthy_Control"), c("Unhealthy_Control", "Healthy_Athletes"), c("Healthy_Control","Healthy_Athletes"))
data = data[-which(is.na(data$Group.name.3)),]

#colnames(data) <- gsub("D/(L+D)","ratio",colnames(data))
colnames(data) <- gsub("-","_",colnames(data))
colnames(data) <- gsub(" ","_",colnames(data))
col.name <- colnames(data)
result <- ''

####1####

#p_adj <- get_adj_p(data, .col = i, .grp = "Group.name.3")
#p_adj
#plot + stat_pvalue_manual(p_adj, label = "p.adj")
#plot <- apply(col.name[6:22],2,function(x){p})
for (i in col.name[8:length(col.name)]){
  nn <- paste0('p_', i)
  p_adj <- get_adj_p(data, .col = i, .grp = "Group.name.3")
  plot <- ggplot(data,aes_string(x= "Group.name.3" ,color="Group.name.3",y=i))+ geom_boxplot() + geom_jitter(width = 0.2,shape = 20, size =2.5)+
    scale_color_manual(values=c("lightgreen","lightblue"
                                ,"red"))+
    stat_boxplot(geom = "errorbar",size =0.5, width= 0.5)+ theme_classic()+ stat_pvalue_manual(p_adj, label = "p.adj")
  #+ stat_compare_means(comparisons = my_comparisons,method = "wilcox.test",p.adjust.method ="BH") 

  plot <- as.ggplot(plot)
  assign(nn, plot)
  result <- paste0(result,",`",nn,"`")
}
result

p_adj <- get_adj_p(data, .col = "XNLD_Gln", .grp = "Group.name.2")
p_adj
#plot <- ggplot(data,aes_string(x= "Group.name.2" ,color="Group.name.2",y="L_Cit"))+ geom_boxplot() + geom_jitter(width = 0.2,shape = 20, size =2.5)+
#  scale_color_manual(values=c("lightgreen"#,"lightblue"
#                              ,"red"))+
#  stat_boxplot(geom = "errorbar",size =0.5, width= 0.5)+ theme_classic()+ stat_pvalue_manual(p_adj, label = "p.adj")
#ggplot(data,aes(x=Group.name ,color=Group.name,y="D/(L+D)-Ala"))+ geom_boxplot() + geom_jitter(width = 0.2,shape = 20, size =2.5)+
#  stat_boxplot(geom = "errorbar",size =0.5, width= 0.5)+ stat_compare_means(comparisons = my_comparisons, label = "p.signif",p.adjust.method ="BH") + theme_classic()

#concentration L-AAs
plot_grid(`p_L_Ala`,`p_L_Ser`,`p_L_Ans`,`p_L_His`,`p_L_Cit`,`p_L_Pro`,`p_L2_ABA`,`p_L3_MeHis`,`p_L1_MeHis`,`p_L_Val`,`p_N_AcCys`,`p_L_Thr`,`p_L_Iso`,`p_L_Leu`,`p_L_Asn`,`p_O_PhEtAm`,`p_L_Gln`,`p_L_Arg`,`p_L_Trp`,`p_L_Orn`,`p_L_Lys`,`p_L_Tyr`
          ,nrow =6, axis = "l", align = "v",labels="AUTO")

#concentration D-AAs
plot_grid(`p_D_Ala`,`p_D_Ser`,`p_D_His`,`p_D_Cit`,`p_D_Pro`,`p_D2_ABA`,`p_D_Val`,`p_D_Thr`,`p_D_Iso`,`p_D_Leu`,`p_D_Asn`,`p_D_Gln`,`p_D_Arg`,`p_D_Trp`,`p_D_Orn`,`p_D_Lys`,`p_D_Tyr`
          ,nrow =5, axis = "l", align = "v",labels="AUTO")

#concentration (D/L+D)-AAs
plot_grid(`p_ratio_Ala`,`p_ratio_Ser`,`p_ratio_His`,`p_ratio_Cit`,`p_ratio_Pro`,`p_ratio_ABA`,`p_ratio_Val`,`p_ratio_Thr`,`p_ratio_Iso`,`p_ratio_Leu`,`p_ratio_Asn`,`p_ratio_Gln`,`p_ratio_Arg`,`p_ratio_Trp`,`p_ratio_Orn`,`p_ratio_Lys`,`p_ratio_Tyr`
          ,nrow =4, axis = "l", align = "v",labels="AUTO")

plot_grid(`p_Sum_Ala`,`p_Sum_Ser`,`p_Sum_His`,`p_Sum_Cit`,`p_Sum_Pro`,`p_Sum_ABA`,`p_Sum_Val`,`p_Sum_Thr`,`p_Sum_Iso`,`p_Sum_Leu`,`p_Sum_Asn`,`p_Sum_Gln`,`p_Sum_Arg`,`p_Sum_Trp`,`p_Sum_Orn`,`p_Sum_Lys`,`p_Sum_Tyr`
          ,nrow =4, axis = "l", align = "v",labels="AUTO")

##plot selected boxplot in two groups
plot_grid(`p_L_Ans`,`p_L_Cit`,`p_L_Asn`,`p_L_Orn`,`p_Ratio_Ser`,`p_Sum_Cit`,`p_Sum_Asn`,`p_XNLL_Cit`,`p_XNLL_Asn`,`p_XNLL_Tyr`,`p_XNLL_Trp`,`p_XNLD_Gln`,`p_XNRatio_Ala`
          ,nrow =4, axis = "l", align = "v",labels="AUTO")
##plot selected boxplot in 3 models
plot_grid(`p_L_Arg`,`p_L_Orn`,`p_Sum_Orn`,`p_XNLL_Cit`,`p_XNLL_Trp`,`p_XNLL_Tyr`,`p_XNLD2_ABA`,`p_XNRatio_ABA`
          ,nrow =3, axis = "l", align = "v",labels="AUTO")


####heatmap####

library(vegan)
library(corrplot)
library(pheatmap)
library(corrplot)
library(ppcor)
library(Hmisc)
library(genefilter)
library("fdrci")
library(psych)
library(tidyverse)
library(ggplotify)
library(cowplot)

data2 <- read.xlsx("/Users/mac/Desktop/Imperial\ College\ of\ London/7-Isabel\ group/1--data/Randomised\ urine\ metadata.xlsx",sheet = 1)

data3 <- data2
data3[data3==0] = NA

#data2 <- t(data2)
#data2 <- as.data.frame(data2)
flag <- apply(data3, 2, function(x) sum(is.na(x))/length(x)*100 <= 90)
data2 <- data2[, which(flag)]

##two group
data<- read.xlsx("concentration_result_new.xlsx", sheet = 3)

##model
data<- read.xlsx("concentration_result_new2.xlsx", sheet = 1)
data2 <- data2[-which(is.na(data$Group.name.3)),]
data <- data[-which(is.na(data$Group.name.3)),]
##Model1
data2 <- data2[-which(data$Group.name.3 == "Healthy_Athletes"),]
data <- data[-which(data$Group.name.3 == "Healthy_Athletes"),]
##Model2
data2 <- data2[-which(data$Group.name.3 == "Unhealthy_Control"),]
data <- data[-which(data$Group.name.3 == "Unhealthy_Control"),]
##Model3
data2 <- data2[-which(data$Group.name.3 == "Healthy_Control"),]
data <- data[-which(data$Group.name.3 == "Healthy_Control"),]

#data2

####heatmap cor_data####
cor_data <- corr.test(data[,8:ncol(data)],data2[,7:ncol(data2)],method = "spearman",adjust = "BH")

#cor_data$r
#cor_data$p
BH_value<- p.adjust(cor_data$p ,method = 'BH')
BH_value<-matrix(unlist(BH_value), nrow=(ncol(data)-7))
BH_value <- t(BH_value)
#BH_value<-cor_data$p
#cor_data <- rcorr(as.matrix(df),type = "spearman")
#cor_data <- rcorr(as.matrix(df[,c(1:5)]),as.matrix(df[,c(5:117)]),type="spearman")
r_value <- cor_data$r
r_value <- t(r_value)
rownames(BH_value) = colnames(data2[,7:ncol(data2)])
colnames(BH_value) = colnames(data[,8:ncol(data)])
rownames(r_value) = colnames(data2[,7:ncol(data2)])
colnames(r_value) = colnames(data[,8:ncol(data)])
subset = ifelse(BH_value < 0.1, r_value, 0)
subset = ifelse(is.na(subset) , 0, subset)

subset<-subset[which(rowSums(subset) != 0),]
subset<-t(subset)
subset<-subset[which(rowSums(subset) != 0),]


group <- rownames(subset)


rownames(BH_value) = colnames(data2[,7:ncol(data2)])
colnames(BH_value) = colnames(data[,8:ncol(data)])
rownames(r_value) = colnames(data2[,7:ncol(data2)])
colnames(r_value) = colnames(data[,8:ncol(data)])

subset = ifelse(BH_value < 0.1, r_value, 0)
subset = ifelse(is.na(subset) , 0, subset)

rownames(subset) = colnames(data2[,7:ncol(data2)])
colnames(subset) = colnames(data[,8:ncol(data)])


subset<-subset[which(rowSums(subset) != 0),]

subset<-t(subset)
subset<-subset[which(rowSums(subset) != 0),]
subset <- t(subset)
#rownames(subset) <- "ratio-Iso"
#annot_data <- as.data.frame(group)
#rownames(annot_data) <- rownames(subset)
#rownames(annot_data) <- colnames(subset)

paletteLength <- 256
mycolor=colorRampPalette(c("blue", "white", "red"))(paletteLength)

myBreaks <- c(seq(min(subset), 0, length.out=ceiling(paletteLength/2) + 1), 
              seq(max(subset)/paletteLength, max(subset), length.out=floor(paletteLength/2)))


####plot heatmap####
#write.csv(subset,"3--Correlation/1--core/core_spear_unadj.csv")
p3<-pheatmap(subset, treeheight_col=5,na_col = "white",color=mycolor,
               #breaks = myBreaks,
               #annotation_row=annot_data,
               #cluster_cols =T,
               angle_col = 315,
               display_numbers = T,
               fontsize_row=8,
               fontsize_col =8,
               #cluster_row=T,
               show_rownames=T)
#mycolor=colorRampPalette(c("blue","white"))(5)
#mycolor=colorRampPalette(c("white", "red"))(5)
p1 <- as.ggplot(p1)
p2 <- as.ggplot(p2)
p3 <- as.ggplot(p3)
plot_grid(p1,p2,p3,nrow =2, axis = "l", align = "v",labels="AUTO")

p4 <- as.ggplot(p4)
p5 <- as.ggplot(p5)
p6 <- as.ggplot(p6)
p7 <- as.ggplot(p7)
plot_grid(p1,p2,p7,p4,p5,p6,p3,nrow =3, axis = "l", align = "v",labels="AUTO")


p1_2<-pheatmap(subset, treeheight_col=5,na_col = "white",color=mycolor,
               breaks = myBreaks,
               #annotation_row=annot_data,
               cluster_cols =T,
               angle_col = 315,
               #display_numbers = T,
               fontsize_row=8,
               fontsize_col =8,
               cluster_row=T,show_rownames=T)
####venn plot####
library(VennDiagram)
#install.packages("ggVennDiagram")
library(ggVennDiagram)

dat <- read.csv("Venn.csv",header = TRUE,sep = ",")

venn_list <- list(Model1 = dat$Diet, Model2 = dat$Exercise, Model3 = dat$Diet_Exercise, Two_Groups = dat$Two_Groups)

#venn.diagram()
venn.plot <- venn.diagram(venn_list, filename = NULL, imagetype = 'png',height = 3000, width = 3000,
                          
                          fill = c('#F2ABBD', '#B3A9BC',"#BAC3AB","#CAB2A2"),
                          
                          cat.fontfamily = 'serif',
                          
                          col = F, fontfamily = 'serif')
grid.draw(venn.plot)

p <- ggVennDiagram(venn_list, edge_size = 0,set_size=6,label_size=6)
p
venn.diagram(venn_list, filename = 'venn5.png', imagetype = 'png',
             
             fill = c('red', 'blue', 'green', 'orange'),
             
             cat.fontfamily = 'serif',
             
             col = F, fontfamily = 'serif')


inter <- get.venn.partitions(venn_list)

for (i in 1:nrow(inter)) inter[i,'values'] <- paste(inter[[i,'..values..']], collapse = ', ')

write.table(inter[-c(5, 6)], 'venn_inter.txt', row.names = FALSE, sep = '\t', quote = FALSE)


####PLS-DA####
library(ropls)
library(ggplot2)
library(ggsci)
library(tidyverse)

plsda_3groups <- opls(data[,6:ncol(data)], data[,5], orthoI = 0,
                      predI =2,
                      permI=1000)
#PLS L
#83 samples x 22 variables and 1 response
#standard scaling of predictors and response(s)
#12 (1%) NAs
#R2X(cum) R2Y(cum) Q2(cum) RMSEE pre ort  pR2Y  pQ2
#Total    0.658    0.173  0.0234 0.463   2   0 0.153 0.12

#PLS D
#83 samples x 17 variables and 1 response
#standard scaling of predictors and response(s)
#6 (0%) NAs
#R2X(cum) R2Y(cum) Q2(cum) RMSEE pre ort  pR2Y   pQ2
#Total    0.524    0.127  -0.272 0.476   2   0 0.601 0.896

#PLS ratio
#PLS-DA
#83 samples x 17 variables and 1 response
#standard scaling of predictors and response(s)
#R2X(cum) R2Y(cum) Q2(cum) RMSEE pre ort  pR2Y   pQ2
#Total    0.167    0.219  -0.287  0.45   2   0 0.098 0.691


####age####
age <- df[,1]

#min(age)
#提取样本在 OPLS-DA 轴上的位置
sample.score = plsda_age@scoreMN %>%  #得分矩阵
  as.data.frame() %>%
  mutate(age =  age,
         o1 = plsda_age@orthoScoreMN[,1]) #正交矩阵
head(sample.score)#查看

x_lab <- plsda_age@modelDF[1, "R2Y(cum)"] * 100
y_lab <- plsda_age@modelDF[2, "R2Y(cum)"] * 100



p1 <- ggplot(sample.score, aes(p1, p2, color = factor(age_1))) +
  geom_hline(yintercept = 0, linetype = 'dashed', size = 0.5) + #横向虚线
  geom_vline(xintercept = 0, linetype = 'dashed', size = 0.5) +
  geom_point() +
  #geom_point(aes(-10,-10), color = 'white') +
  labs(x = paste0("p1(", x_lab, "%)"), y = paste0("p2(", y_lab, "%)")) +
  stat_ellipse(level = 0.95, linetype = 'solid', 
               size = 1, show.legend = FALSE) + #添加置信区间
  scale_color_manual(values = c('#E95B4E','#93C7A1','#90A3B9','#9788A3',"#CAB2A2")) +
  theme_bw() +
  theme(legend.position = c(0.1,0.85),
        legend.title = element_blank(),
        legend.text = element_text(color = 'black',size = 12),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_text(color = 'black',size = 15),
        axis.title = element_text(color = 'black',size = 15),
        axis.ticks = element_line(color = 'black'))
p1



