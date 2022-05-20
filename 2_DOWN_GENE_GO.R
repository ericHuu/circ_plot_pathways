#https://my.oschina.net/shenweiyan/blog/4538188
require("RColorBrewer")
library(gplots)

a = c("2_DOWN_GENE_GO.txt"
      )

for (file in a){
  print(file)
  data<-read.delim(file, row.names=2)
  head(data)
  data <- data[-1]
  data[is.na(data)] <- 0
  data=0-data
  head(data)
  
  require(graphics); require(grDevices)
  
  dend <- as.dendrogram(hclust(dist((data))))
  # plot(dend)
  library(dendextend)
  n <- 4    # n 可自定义
  dend <- dend %>% set("branches_k_color", k = n) 
  
  # plot(dend)
  data=t(data)
  mat2 <- data[,order.dendrogram(dend)]
  lable1 <- row.names(mat2);
  lable1
  lable2 <- colnames(mat2);
  lable2
  
  require("circlize")
  
  col_fun <- colorRamp2(c(0,5,10), c("#E0E0E0","#C94A40","#67001F"))
  
  col_mat <- col_fun(mat2)
  col_mat[,1]
  col_mat[1,]
  
  op = paste(file,"pdf",sep = ".")
  
  pdf(op,width = 12,height = 12)
  # par(mar <- c(6,2,6,2))
  circos.clear();
  circos.par(canvas.xlim = c(-2.4,2.4),
             canvas.ylim = c(-2.4,2.4),
             cell.padding = c(0,0,0,0),
             gap.degree = 90)
  factors <- "a"
  nr <- nrow(mat2);nr
  nc <- ncol(mat2);nc
  
  circos.initialize(factors, xlim = c(0, ncol(mat2)))
  circos.track(ylim = c(0, nr),bg.border = NA,track.height = 0.1*nr,
               panel.fun = function(x, y) {
                 for(i in 1:nr) {
                   circos.rect(xleft = 1:nc - 1, ybottom = rep(nr - i, nc),
                               xright = 1:nc, ytop = rep(nr - i + 1, nc),
                               border = NA,
                               col = col_mat[i,])
                   circos.text(x = nc,
                               y = 4.2 -i,
                               offset=0.1,
                               labels = lable1[i],
                               facing = "downward", niceFacing = TRUE,
                               cex = 1.2,
                               adj = c(0, 0))
                 }
               })
  
  for(i in 1:nc){
    circos.text(x = i-0.1,
                y = 4.2,
                labels = lable2[i],
                facing = "clockwise", niceFacing = TRUE,
                cex = 1.0,adj = c(0, 0))
  }
  
  
  max_height <-max(attr(dend, "height"))
  circos.track(ylim = c(0, max_height),bg.border = NA,track.height = 0.3,
               panel.fun = function(x, y){
                 circos.dendrogram(dend = dend,
                                   max_height = max_height)
               })
  circos.clear()
  
  library(ComplexHeatmap)
  lgd <- Legend(at = c(0, 2,4,6,8, 10), col_fun = col_fun,
                title_position = "topcenter",title = "-log10(P)")
  draw(lgd, x = unit(0.65, "npc"), y = unit(0.65, "npc"))
  
  dev.off()
  # dev.off()
  
}
# dev.off()

# dev.off()

