# install.packages("G:/Programme/R/2.14/R.methodsS3_1.2.1.zip")

# Hannah-Kay for different countries, same theta, same sectors

source('\\\\oecdshare.oecd.org\\sti\\eas\\stan\\STAN_R\\progs\\0_executable.R')

## path.out <- c("//Asap5/sti/Progs/STAN/STANi4/Indicators/Scoreboard/HK/data/")
path.out <- c("\\\\oecdshare.oecd.org\\sti\\eas\\stan\\score13\\Chapter1_BW\\hk_cr4\\")
## path.out <- c("D:\\old_HK\\")

#***************** data
nameind <- c(STAN.INDA38,'D19T23','D26T28','D58T63','D69T75')
nameind <- nameind[!nameind%in%c('D01T03','D19','D20','D21','D22T23','D26','D27','D28','D58T60','D61','D62T63','D68','D69T71','D72','D73T75','D84','D85','D86','D87T88','D90T93','D94T96','D97T98','D99')]
nameind <- sort(nameind)
## Canada: D58T60, D72 missing
ind_id <- c("A38")         # ...replace with c("...") as name of industry vector
## names(STAN.IND)
## STAN.IND[STAN.IND$Ind%in%ind,colnames(STAN.IND)%in%c('Ind','LABEL_en.TEXT')]


#***************** parameters
theta <- 2
min_year <- 1980
max_year <- 2011

write_data=TRUE
create_plot=TRUE

## color palettes
dark.col <- c('#1B9E77','#D95F02','#7570B3','#E7298A','#66A61E','#E6AB02','#A6761D','#666666')
set2.col <- c('#66C2A5','#FC8D62','#8DA0CB','#E78AC3','#A6D854','#FFD92F','#E5C494','#B3B3B3')
pal.crN <- c(rev(set2.col),rev(dark.col))
pal.crN <- EXCEL.COL
pal.crN <- c('#95B3D7','#F79646','#8064A2','#4BACC6','#9BBB59','#C0504D','#DA9694','#C4D79B','#B1A0C7','#4F81BD','#92CDDC','#FABF8F')
topn <- 3

namevar <- c('VALU','EMPN','EMPE')
data.sql <- sqlQuery(SQL.STAN, paste0('SELECT * FROM STANPUBi4_PRE WHERE var IN (', toString(paste0("'",namevar, "'")), ') AND ind IN (', toString(paste0("'",nameind, "'")) ,')'))
data.sql <- data.sql[!data.sql$cou=='CHL',]

## print missing sectors for each country and year
## cous = c('CAN','JPN','USA')
years = c(2008:2010)
cat('Current list of sectors:')
nameind
cat('Missing sectors for each country and year:')
for (var in c('VALU','EMPN')) {
    data <- data.sql[data.sql$var==var,]
    print(var)
    for (cou in unique(data.sql$cou)) {
        for (year in years) {
            missing <- setdiff(nameind, unique(data$ind[data$cou==cou & data$year==year]))
            if (length(missing) > 0) { cat(cou, year); print(missing)}
        }
    }
}

## method 1: calculate average first
var.ind <- NULL # unique industries for variable
var.cou <- NULL # unique countries for varialbe
for (var in c(namevar,namevar)) {
    data <- data.sql[data.sql$var==var,]
    namecou <- unique(data$cou)
    nameyear <- unique(data$year)
    ## calculate average: stacked coulumn chart for one year
    years <- c(2008:2010)
    data.avg <- data[data$year%in%years,]
    data.avg.d <- dcast(data.avg, cou ~ ind, value.var = 'value', fun.aggregate = mean, na.rm = T)
    data.avg.m <- melt(data.avg.d, variable.name = 'ind')
    exclcou <- unique(data.avg.m$cou[is.nan(data.avg.m$value)])
    data.avg.m <- data.avg.m[!data.avg.m$cou%in%exclcou,]
    if (write_data==T) {
        data.avg.m.out <- data.avg.m[!data.avg.m$value==0,]
        data.avg.m.out <- data.avg.m.out[order(data.avg.m.out$cou,-data.avg.m.out$value),]
        write.csv(data.avg.m.out, file = paste0(path.out,'crN_column_v1\\',paste(ind_id,var,min(years),max(years),sep='_'),"_all.csv"), row.names=F)
    }
    ##
    crN.all <- NULL
    for (cou in unique(data.avg.m$cou)) {
        data.avg.m.cou <- data.avg.m[data.avg.m$cou==cou & data.avg.m$ind%in%nameind, ]
        pivot.cou <- acast(data.avg.m.cou, cou ~ ind, value.var='value')
        cou.share <- prop.table(pivot.cou,1)
        cou.sharem <- melt(cou.share)
        cou.order <- cou.sharem[order(-cou.sharem$value),]
        cou.order <- by(cou.order, cou.order["Var1"], head, n=topn)
        cou.order <- cou.order[[1]]
        ## cou.year <- cou.order[[match(toString(year), names(cou.order))]]
        if (length(cou.order) > 0) {
            names(cou.order) <- c('cou','ind','value')
            ## cou.year$cou <- cou
            crN.all <- rbind(crN.all, cou.order)
        }
    }
    ##
    crN.avg.m <- crN.all
    ## append industries to list of common industries (for all countries)
    var.ind <- as.factor(union(var.ind, unique(crN.avg.m$ind)))
    var.ind <- as.factor(sort(levels(var.ind)))
    if (length(setdiff(var.ind, unique(crN.avg.m$ind))) > 0) {
        couind <- merge(unique(crN.avg.m$cou), setdiff(var.ind, unique(crN.avg.m$ind)))
        names(couind) <- c('cou','ind')
        couind$value <- 0
        crN.avg.m <- rbind(crN.avg.m, couind)
    }
    crN.avg.m$ind <- factor(crN.avg.m$ind, levels = var.ind)
    ## append countries to list of common countries (for all variables)
    var.cou <- as.factor(union(var.cou, unique(crN.avg.m$cou)))
    var.cou <- as.factor(sort(levels(var.cou)))
    if (length(setdiff(var.cou, unique(crN.avg.m$cou))) > 0) {
        indcou <- merge(unique(crN.avg.m$ind), setdiff(var.cou, unique(crN.avg.m$cou)))
        names(indcou) <- c('ind','cou')
        indcou$value <- 0
        crN.avg.m <- rbind(crN.avg.m, indcou)
    }
    crN.avg.m$cou <- factor(crN.avg.m$cou, levels = var.cou)
    crN.avg.m.sum <- dcast(crN.avg.m, cou ~ ., fun.aggregate = sum)
    names(crN.avg.m.sum)[2] <- 'sum'
    ##
    order.cou <- crN.avg.m.sum$cou[order(-crN.avg.m.sum$sum)]
    order.cou <- order.cou[!order.cou%in%crN.avg.m.sum$cou[crN.avg.m.sum$sum==0]]
    crN.avg.m$cou <- factor(crN.avg.m$cou, levels = order.cou)
    ##
    ## crN.avg.m$value <- crN.avg.m$value * 100
    ## add industry labels
    ind.label <- STAN.LABEL$label[STAN.LABEL$ind%in%var.ind]
    ## crN.avg.m <- merge(crN.avg.m, STAN.LABEL[STAN.LABEL$ind%in%var.ind,])
    ## plot for average of years w/o facetting
    ##
    if (write_data==T) {
        crN.avg.m.out <- crN.avg.m[!crN.avg.m$value==0,]
        crN.avg.m.out$group <- paste0('top',c(1:topn))
        crN.avg.m.out.value <- dcast(crN.avg.m.out, cou ~ group, value.var = 'value')
        crN.avg.m.out.ind <- dcast(crN.avg.m.out, cou ~ group, value.var = 'ind')
        write.csv(crN.avg.m.out.value, file = paste0(path.out,'crN_column_v1\\',paste(ind_id,var,min(years),max(years),sep='_'),"_value.csv"), row.names=F)
        write.csv(crN.avg.m.out.ind, file = paste0(path.out,'crN_column_v1\\',paste(ind_id,var,min(years),max(years),sep='_'),"_ind.csv"), row.names=F)
    }

### 2000 data ###
    ## for (var in c(namevar)) {
    data.diamond <- data.sql[data.sql$var==var,]
    namecou.diamond <- unique(data.diamond$cou)
    nameyear.diamond <- unique(data.diamond$year)
    ## calculate average: stacked coulumn chart for one year
    years.diamond <- c(2000)
    data.avg.diamond <- data.diamond[data.diamond$year%in%years.diamond,]
    data.avg.diamond.d <- dcast(data.avg.diamond, cou ~ ind, value.var = 'value', fun.aggregate = mean, na.rm = T)
    data.avg.diamond.m <- melt(data.avg.diamond.d, variable.name = 'ind')
    exclcou <- unique(data.avg.diamond.m$cou[is.nan(data.avg.diamond.m$value)])
    data.avg.diamond.m <- data.avg.diamond.m[!data.avg.diamond.m$cou%in%exclcou,]
    ##
    crN.diamond.all <- NULL
    for (cou in unique(data.avg.diamond.m$cou)) {
        data.avg.diamond.m.cou <- data.avg.diamond.m[data.avg.diamond.m$cou==cou & data.avg.diamond.m$ind%in%nameind, ]
        pivot.cou <- acast(data.avg.diamond.m.cou, cou ~ ind, value.var='value')
        cou.share <- prop.table(pivot.cou,1)
        cou.sharem <- melt(cou.share)
        cou.order <- cou.sharem[order(-cou.sharem$value),]
        cou.order <- by(cou.order, cou.order["Var1"], head, n=topn)
        cou.order <- cou.order[[1]]
        ## cou.year <- cou.order[[match(toString(year), names(cou.order))]]
        if (length(cou.order) > 0) {
            names(cou.order) <- c('cou','ind','value')
            ## cou.year$cou <- cou
            crN.diamond.all <- rbind(crN.diamond.all, cou.order)
        }
    }
    ##
    crN.diamond.avg.m <- crN.diamond.all
    ## year 2000 different industries that are not in levels of 2008-2010 industries
    ## crN.avg.m$ind <- factor(crN.avg.m$ind, levels = var.ind)
    ##
    crN.diamond.avg.m$cou <- factor(crN.diamond.avg.m$cou, levels = var.cou)
    ## plot for average of years w/o facetting
    ## write data
    crN.diamond.avg.m.out <- crN.diamond.avg.m[!crN.diamond.avg.m$value==0,]
    crN.diamond.avg.m.out$group <- paste0('top',c(1:topn))
    crN.diamond.avg.m.out.value <- dcast(crN.diamond.avg.m.out, cou ~ group, value.var = 'value')
    crN.diamond.avg.m.out.ind <- dcast(crN.diamond.avg.m.out, cou ~ group, value.var = 'ind')
    if (write_data==T) {
        write.csv(crN.diamond.avg.m.out.value, file = paste0(path.out,'crN_column_v1\\',paste(ind_id,var,min(years),max(years),sep='_'),"_value.csv"), row.names=F)
        write.csv(crN.diamond.avg.m.out.ind, file = paste0(path.out,'crN_column_v1\\',paste(ind_id,var,min(years),max(years),sep='_'),"_ind.csv"), row.names=F)
    }
    ##
    crN.diamond.avg.m.sum <- dcast(crN.diamond.avg.m, cou ~ ., fun.aggregate = sum)
    names(crN.diamond.avg.m.sum)[2] <- 'sum'
    ##
    if (create_plot==T) {
        ## source('\\\\oecdshare.oecd.org\\sti\\eas\\stan\\STAN_R\\progs\\0_theme_sb.R')
        ## theme_set(theme_sb())
        ##
        ggplot() +
            geom_bar(data = crN.avg.m[!is.na(crN.avg.m$cou),],
                     aes(x = cou, y = value, fill = ind, colour = factor(1), order = -value),
                     position = 'stack', width = .4, stat = 'identity') +
                         ## facet_grid(year ~ .) +
                         scale_fill_manual(values=pal.crN, labels = ind.label) +
                             scale_y_continuous(labels=percent) +
                                 scale_color_manual(values = NA, guide = FALSE) +
                                     geom_point(data = crN.diamond.avg.m.sum, aes(x = cou, y = sum), colour = 'black', stat = 'identity', size = 3, shape = 18) +
                                         geom_point(data = crN.diamond.avg.m.sum, aes(x = cou, y = sum), stat = 'identity', size = 2.5, shape = 18, colour = 'white') +
                                             theme(axis.title.x = element_blank(),
                                                   axis.title.y = element_blank()) +
                                                       guides(fill = guide_legend(ncol = 2, title = NULL)) # + guides(shape = guide_legend(title = NULL))
        ## labs(title = paste('CRN using', var, 'for', length(nameind), 'sectors,', min(years), '-', max(years)))
        ## save plot
        ggsave(filename=paste0(path.out,'crN_column_v1\\',paste(ind_id,var,min(years),max(years),sep='_'),".emf"), width = 8.4, height = 4, units = c('in'), limitsize = )
    }
}


if (write_data==T) {
    industries <- STAN.LABEL[STAN.LABEL$ind%in%nameind,]
    write.csv(industries, file = paste0(path.out,'crN_column_v1\\','industry_labels.csv'), row.names=F)
}


##############################
##   CR4 time series plot   ##
##############################
##
#####   prepare data   #####
##
namevar <- c('VALU','EMPN','EMPE')
##
for (var in namevar) {
    data <- data.sql[data.sql$var==var,]
    namecou <- unique(data$cou)
    ##
    cr4.all <- NULL
    for (cou in unique(data$cou)) {
        for (year in unique(data$year)) {
            data.cou <- data[data$cou==cou & data$year >= min_year & data$year <= max_year & data$ind%in%nameind,]
            pivot.cou <- acast(data.cou, year ~ ind, value.var='value')
            cou.share <- prop.table(pivot.cou,1)
            cou.sharem <- melt(cou.share)
            cou.order <- cou.sharem[order(-cou.sharem$value),]
            cou.order <- by(cou.order, cou.order["Var1"], head, n=4)
            cou.year <- cou.order[[match(toString(year), names(cou.order))]]
            if (length(cou.year) > 0) {
                names(cou.year) <- c('year','ind','value')
                cou.year$cou <- cou
                cr4.all <- rbind(cr4.all, cou.year)
            }
        }
    }
    ## time series chart for all years, selected countries
    cr4.all.d <- dcast(cr4.all, cou ~ year, value.var = 'value', fun.aggregate = sum)
    cr4.all.m <- melt(cr4.all.d, id.vars = c('cou'), variable.name = 'year')
    cr4.all.m <- cr4.all.m[!cr4.all.m$value==0,] # DEU zero in 1990
    ##
    countries <- c('CAN','DEU','FRA','GBR','ITA','JPN','USA','KOR') # no employment data for Korea
    ##
    if (create_plot==T) {
        ggplot() +
            geom_line(data = cr4.all.m[cr4.all.m$cou%in%countries,], aes(x = year, y = value, colour = cou, linetype = cou, group = cou), size = 1, stat = 'identity') +
                theme(axis.title.x = element_blank(),
                      axis.title.y = element_blank()) +
                          labs(title = paste('CR4 using', var, 'for', length(nameind), 'sectors,', min_year, '-', max_year))
        ##
        ggsave(filename=paste0(path.out,'cr4_line_v1\\',paste(ind_id,var,min_year,max_year,sep='_'),".emf"), width = 12, height = 5)
    }
    if (write_data==T) {
        cr4.all.out <- dcast(cr4.all.m[!is.na(cr4.all.m$value),], year ~ cou, value.var = 'value', na.rm=T)
        write.csv(cr4.all.out, file = paste0(path.out,'cr4_line_v1\\',paste(ind_id,var,min_year,max_year,sep='_'),"_value.csv"), row.names=F)
    }
}


##########################
##   Hannah-Kay index   ##
##########################
######   prepare data   ######
var <- 'VALU'
data <- data.sql[data.sql$var==var,]
## test with ISIC Rev. 3 data ##
## nameind.i3 <- c('C01T05','C10T14','C15T16','C17T19','C20','C21T22','C23T25','C26','C27T28','C29','C30T33','C34T35','C36T37','C40T41','C45','C50T52','C55','C60T64','C65T67','C70T74')
## data.sql.i3 <- sqlQuery(SQL.STAN, paste0('SELECT * FROM STANPUB WHERE var IN (', toString(paste0("'",namevar, "'")), ') AND ind IN (', toString(paste0("'",nameind.i3, "'")) ,')'))
## data.sql.i3 <- data.sql.i3[!data.sql.i3$cou=='CHL',]
## data <- data.sql.i3[data.sql.i3$var==var,]
##
hk.all <- c()
for (cou in unique(data$cou)) {
    data_cou <- data[data$cou==cou & data$year >= min_year & data$year <= max_year & data$ind%in%nameind, ] # ind list : 'twodigit' or 'scrbd'
    pivot_cou <- acast(data_cou, year ~ ind, value_var='value')
    cou_share <- prop.table(pivot_cou,1)
    cou_share_power <- cou_share^theta
    cou_share_power_sum <- margin.table(cou_share_power,1)
    cou_hk <- cou_share_power_sum^(1/(1-theta))
    cou_hk <- as.data.frame(cou_hk)
    cou_hk$year <- row.names(cou_hk)
    cou_hk$cou <- cou
    hk.all <- rbind(hk.all, cou_hk)
}
names(hk.all)[1] <- 'value'
## hk.all$cou <- factor(hk.all$cou, levels = unique(hk.all$cou[order(hk.all$value)]))
hk.all$value <- hk.all$value / length(nameind) # normalize to 1
if (write_data==TRUE){
    hk.all.out <- hk.all
    hk.all.out$value <- hk.all.out$value * 100
    hk.all.out.d <- dcast(hk.all.out, year ~ cou, value.var = 'value')
    write.csv(hk.all.out.d, file = paste0(path.out,'hk_line_v1\\',paste(ind_id,var,min_year,max_year,sep='_'),"_value.csv"), row.names=F)
}
######   HK: create plots   ######
## horizontal bar plots with diamonds
years <- c(2008:2011)
hk1995 <- NULL
for(year in years) {
    hk1995y <- hk.all[hk.all$year==1995,]
    hk1995y$year <- year
    hk1995 <- rbind(hk1995, hk1995y)
}
##
if (create_plot==T) {
    ggplot() + geom_bar(data = hk.all[hk.all$year%in%years,], aes(x = cou, y = value, fill = factor(1), group = cou), stat = 'identity', width = 0.4) + scale_fill_manual(values = c('steelblue'), guide = 'none') +
        geom_point(data = hk1995, aes(x = cou, y = value, group = year), stat = 'identity', size = 3, shape = 18) +
            geom_point(data = hk1995, aes(x = cou, y = value, group = year), stat = 'identity', size = 2.5, colour = 'white', shape = 18) + coord_flip() + facet_grid(. ~ year) +
                theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
                    labs(title = paste0('HK value with theta = ', theta, ' divided by ', length(nameind), ' sectors, ', min(years), '-', max(years), ', 1995 diamond'))
    ##
    ggsave(filename=paste0(path.out,'hk_bar_v1\\',paste(ind_id,"theta",theta,min(years),max(years),sep='_'),".emf"), width = 10, height = 10)
}
