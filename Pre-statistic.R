setwd("/Users/mac/Desktop/Imperial College of London/7-Isabel group/network_test")
# load R graph library igraph
library(igraph)
pvals=read.table("sparcc_pvals_two_sided.txt",header=TRUE,sep="\t")
pvals.mat=pvals[,2:ncol(pvals)]
# set p-values of 0 to a non-zero, small p-value so we can take the logarithm
pvals.mat[pvals.mat==0]=0.000000001
# convert into significance
sig.mat=-1*log10(pvals.mat) 
# remove all edges with significance below 1
sig.mat[sig.mat<1]=0
sig.mat=as.matrix(sig.mat)
# convert adjacency matrix into a graph
sparcc.graph=graph.adjacency(sig.mat,mode="undirected")
# display the graph
layout=layout.spring
plot(sparcc.graph, layout=layout) # Spring layout was removed, we use Fruchterman-Reingold instead.

#观测值的相关矩阵
cor_sparcc <- read.delim('arctic_soils_sparcc.txt', row.names = 1, sep = '\t', check.names = FALSE)

#伪 p 值矩阵
pvals <- read.delim('sparcc_pvals_two_sided.txt', row.names = 1, sep = '\t', check.names = FALSE)

#保留 |相关性|≥0.8 且 p<0.01的值
cor_sparcc[abs(cor_sparcc) < 0.4] <- 0

pvals[pvals>=0.05 ] <- -1
pvals[pvals<0.05 & pvals>=0] <- 1
pvals[pvals==-1] <- 0

#筛选后的邻接矩阵
adj <- as.matrix(cor_sparcc) * as.matrix(pvals)
diag(adj) <- 0    #将相关矩阵中对角线中的值（表明了自相关）转为 0
library(matrixStats)
elim <- which(rowSds(adj) != 0)
adj <- adj[elim,elim]
diag(adj) <- 1
#write.table(data.frame(adj, check.names = FALSE), 'neetwork.adj.txt', col.names = NA, sep = '\t', quote = FALSE)

###convert correlations to distances
#library(psych)
#library(stats)
#library(vegan)
#BiocManager::install("WGCNA")
library(dendextend)
max(adj)
min(adj)
dist_adj <- as.dist(1-adj)
#dist_adj <- ifelse(adj >-2, 1-adj,1-adj)
max(dist_adj)
min(dist_adj)
write.table(data.frame(adj, check.names = FALSE), 'adj_filter.txt', col.names = NA, sep = '\t', quote = FALSE)

#dist_adj <- cor2dist(adj)
#dist_adj <- as.dist(dist_adj)
cluster_adj <- hclust(dist_adj, method = "ward.D2", members = NULL)
#cluster_adj$order
plot(cluster_adj, hang=-1)

class(cluster_adj)
dend1 <- as.dendrogram(cluster_adj)
class(dend1)
str(dend1, max = 2, last.str =  "'")

identity(cluster_adj)
plot(dend1,cex=0.8)

###
BiocManager::install("ctc")
library(TreeAndLeaf)
library(RedeR)
library(igraph)
library(RColorBrewer)
remotes::install_github("YuLab-SMU/tidytree")
library(ggtree)
library(treeio)
install.packages("ctc")
#library(dplyr)
#remove.packages("dplyr")
#library(remotes)
#install_version("dplyr","1.0.5")
#library(dplyr)
#ggtree(cluster_adj) # 转化为树和叶的形式，其实是转化为igraph对象

group_info<-read.csv("CAGs_group.csv",header=T,sep=",")

colnames(group_info)<-c("label","Species")
library("ctc")

write(hc2Newick(cluster_adj),file='hclust.newick')

tree <- read.tree("./hclust.newick")

tree1<-full_join(tree,group_info,by="label")

ggtree(cluster_adj)                          
ggtree(tree1,aes(color=Species),branch.length = "none")+
  layout_dendrogram()+
  theme(legend.position = "top")

rect.hclust(cluster_adj, k=9)
# 得到分为3类的数值
out.id = cutree(cluster_adj, k=6)
out.id
# 以矩阵的方式分辨名称对应的类
a <- table(out.id,paste("Name",1:171,""))


########网络
#输入数据，邻接矩阵
neetwork_adj <- read.delim('neetwork.adj.txt', row.names = 1, sep = '\t', check.names = FALSE)
head(neetwork_adj)[1:6]    #邻接矩阵类型的网络文件

#邻接矩阵 -> igraph 的邻接列表，得到含权的无向网络
g <- graph_from_adjacency_matrix(as.matrix(neetwork_adj), mode = 'undirected', weighted = TRUE, diag = FALSE)
g    #igraph 的邻接列表

#这种转换模式下，默认的边权重表明了 sparcc 计算的相关性（存在负值）
#因为边权重一般为正值，所以最好取个绝对值，相关性从新复制一列做为记录
E(g)$sparcc <- E(g)$weight
E(g)$weight <- abs(E(g)$weight)

#再转为其它类型的网络文件，例如
#再由 igraph 的邻接列表转换回邻接矩阵
adj_matrix <- as.matrix(get.adjacency(g, attr = 'sparcc'))
write.table(data.frame(adj_matrix, check.names = FALSE), 'network.adj_matrix.txt', col.names = NA, sep = '\t', quote = FALSE)

#graphml 格式，可以使用 gephi 软件打开并进行可视化编辑
write.graph(g, 'network.graphml', format = 'graphml')

#gml 格式，可以使用 cytoscape 软件打开并进行可视化编辑
write.graph(g, 'network.gml', format = 'gml')

#边列表，也能够直接导入至 gephi 或 cytoscape 等网络可视化软件中进行编辑
edge <- data.frame(as_edgelist(g))

edge_list <- data.frame(
  source = edge[[1]],
  target = edge[[2]],
  weight = E(g)$weight,
  sparcc = E(g)$sparcc
)
head(edge_list)

write.table(edge_list, 'network.edge_list.txt', sep = '\t', row.names = FALSE, quote = FALSE)

#节点属性列表，对应边列表，记录节点属性，例如
node_list <- data.frame(
  nodes_id = V(g)$name,    #节点名称
  degree = degree(g)    #节点度
)
head(node_list)

write.table(node_list, 'network.node_list.txt', sep = '\t', row.names = FALSE, quote = FALSE)
