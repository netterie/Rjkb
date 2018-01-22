
###########################################################
# plot_categorical
###########################################################
#' Plot a categorical variable N and label with percent
#'
#' Goal: have an example with stack=FALSE vs TRUE, margin = 'no', 'yes', or 'only'
#' I think I should externalize the tabulation - that would be better for unit
#' testing. Then the plot function would only need to add the label.
#'
#' @param sum Set to TRUE if the variable is a weight and you want to view its sum rather than its count
#' Goal: be able to add a group and/or panel variable using standard ggplot syntax
#'
#' @examples
#' # Simple example with the nsfg edgelist - cut length into only 2 categories
#' # This is with the y-scale being N and no stacking
#' data(nsfg)
#' dfl <- reshape_edgelist(nsfg, delete_empty=c("active","once","rel","dfs","dls","len"))
#' d <- transform(dfl, lenf=cut(len,breaks=c(0,12,351)))
#' p <- plot_categorical(d, var='lenf',
#'                    group='sex', panel=NULL, yperc=TRUE, ylab='Percent')
#' plot_categorical(d, 'lenf')
#' plot_categorical(d, 'lenf', 'sex', NULL)
#' plot_categorical(d, 'lenf', 'sex', 'rel') + facet_grid(.~panel)
#' 
#' # Now there is stacking, with y-scale still N (note that coord is flipped)
#' plot_categorical(d, 'lenf', stack=TRUE)
#' plot_categorical(d, 'lenf', 'sex', stack=TRUE)
#' 
#' # Now make the y-scale percent for stack=FALSE, and add a panel
#' plot_categorical(d, 'lenf', 'sex', 'rel', yperc=TRUE, ylab='Percent') +
#'     facet_grid(panel~.)
#' 
#' # Stack with a panel
#' plot_categorical(d, 'lenf', 'sex', 'rel', stack=TRUE) + facet_grid(panel~.)
#' 
#' # Stack with a panel, but use the regular legend, since the special one 
#' # is too cluttered with paneling
#' plot_categorical(d, 'lenf', 'sex', 'rel', stack=TRUE,
#'                  regular_legend=TRUE) + facet_grid(panel~.)
#' 
#' # Adding exponential - not allowed for yperc=FALSE
#' plot_categorical(d, 'lenf', 'sex', 'rel', add_exponential=TRUE) + facet_grid(.~panel)
#' plot_categorical(d, 'lenf', 'sex', 'rel', add_exponential=TRUE,
#'                  yperc=TRUE) + facet_grid(.~panel)
#' @export

if (1==0) {
    library(devtools)
    library(egonet)
    library(plyr)
    library(ggplot2)
    load_all()

    #--------------------
    # Working - these have been transferred up to examples
    #--------------------
    # NSFG examples
    data(nsfg)
    dfl <- reshape_edgelist(nsfg, delete_empty=c("active","once","rel","dfs","dls","len"))
    d <- transform(dfl, lenf=cut(len,breaks=c(0,12,351)))
    p <- plot_categorical(d, var='lenf',
                       group='sex', panel=NULL, yperc=TRUE, ylab='Percent')
    plot_categorical(d, 'lenf')
    plot_categorical(d, 'lenf', 'sex', NULL)
    plot_categorical(d, 'lenf', 'sex', 'rel') + facet_grid(.~panel)
    # Now there is stacking, with y-scale still N (note that coord is flipped)
    plot_categorical(d, 'lenf', stack=TRUE)
    plot_categorical(d, 'lenf', 'sex', stack=TRUE)
    # Now make the y-scale percent for stack=FALSE, and add a panel
    plot_categorical(d, 'lenf', 'sex', 'rel', yperc=TRUE, ylab='Percent') +
        facet_grid(panel~.)
    # Stack with a panel
    plot_categorical(d, 'lenf', 'sex', 'rel', stack=TRUE) + facet_grid(panel~.)
    # Stack with a panel, but use the regular legend, since the special one 
    # is too cluttered with paneling
    plot_categorical(d, 'lenf', 'sex', 'rel', stack=TRUE,
                     regular_legend=TRUE) + facet_grid(panel~.)
    # Adding exponential - not allowed for yperc=FALSE
    plot_categorical(d, 'lenf', 'sex', 'rel', add_exponential=TRUE) + facet_grid(.~panel)
    plot_categorical(d, 'lenf', 'sex', 'rel', add_exponential=TRUE,
                     yperc=TRUE) + facet_grid(.~panel)

    #--------------------
    # Working - these still need to be transferred up to examples
    #--------------------
    
    #--------------------
    # To fix
    #--------------------
    library(devtools)
    library(HIVBackCalc)
    data(KCsim)
    load_all()
    # Total is wrong. Also Total should come at the bottom.
    plot_categorical(KCsim, 'fakeGroup', 'race', margin='yes', stack=TRUE)

    #--------------------
    # To develop
    #--------------------
}

plot_categorical <- function(d, var, group=NULL, panel=NULL,
                             stack=FALSE, flipcoord=TRUE, margin='no',
                             yperc=FALSE, ylab='Frequency',
                             labelPerc=TRUE,
                             add_exponential=FALSE,
                             regular_legend=FALSE,
                             sum=FALSE,
                             weighted=FALSE,
                             textsize=4) {
    # Define variables named 'var', 'group' and 'panel'
    # if they're not null
    # Could turn this into its own function, with default vals
    for (v in c('var', 'group', 'panel')) {
        if (!is.null(eval(parse(text=v))))
            d[,v] <- d[,eval(parse(text=v))] else d[,v] <- 'All'
    }
    # Specify plyr::summarize otherwise you could call summarize from 
    # another package like Hmisc or dplyr
    if (!sum & !weighted) {
        tab <- ddply(d, c('var', 'group', 'panel'), plyr::summarize, N=length(var))
    } else if (sum & !weighted) {
        tab <- ddply(d, c('var', 'group', 'panel'), plyr::summarize, N=sum(var))
    } else if (weighted) {
        tab <- ddply(d, c('var', 'group', 'panel'), plyr::summarize, N=sum(weight))
    }

    # Add margins
    tab <- rbind(tab,
                data.frame(group='Total',
                           ddply(tab, .(var, panel),
                                 summarise, N=sum(N)))
                )

    # For when there's a panel variable but you still want a total margin
    if (length(unique(d$panel))==1) {
        if (unique(d$panel)=='All') addmore <- FALSE else addmore <- TRUE
    } else addmore <- TRUE

    if (addmore) {
        tab <- rbind(tab,
                    data.frame(group='Total', panel='Total',
                               ddply(tab, .(var),
                                     summarise, N=sum(N)))
                    )
        if (!is.null(group)) {
            tab <- rbind(tab,
                        data.frame(panel='Total',
                                   ddply(tab, .(var, group),
                                         summarise, N=sum(N)))
                        )
        }
    }

    # Delete NA's and issue a warning - even this could be a function
    var.na <- is.na(tab$var)
    if (sum(var.na)!=0) {
        warning('\nDeleting ', sum(tab$N[var.na & tab$group!='Total' & tab$panel!='Total']), ' cases with ', var, ' =NA')
        tab <- tab[!var.na,]
    }

    # Calculate the percentages
    tab = ddply(tab, c('group', 'panel'),
               transform,
               Percent = round(100*N/sum(N)))
    # Check
    # ddply(tab, .(group, panel), summarise, Total=sum(Percent))

    # Format the labels and calculate their positions
    if (labelPerc) {
        tab$label = paste0(sprintf("%.0f", tab$Percent), "%")
    } else {
        tab$label = as.character(tab$N)
    }
    if (stack) {
        tab = ddply(tab, .(group, panel), transform, pos = (cumsum(N) - 0.5 * N))
    } else {
        # Turn the y-scale into percent, not N
        if (yperc) {
            tab <- transform(tab, N=Percent)
            adjust.lab <- 1
        } else adjust.lab <- max(tab$N)/75
        tab <- transform(tab, pos=N+adjust.lab)
    }

    # Choose dataset: include margins, exclude margins, or margins only
    if (margin=='no') {
        pdat <- subset(tab, group!='Total' & panel!='Total')
    } else if (margin=='only') {
        pdat <- subset(tab, group=='Total' | panel=='Total')
    } else pdat <- tab

    # The rev(var) appears necessary in ggplot2 built for R 3.3.2, as of
    # 11/30/16. Very annoying that the factor levels are treated differently
    # for file as opposed to label...?
    # Start with the bar plot, stacked or dodge
    # Stacked will x=group and colors=var values
    # Dodge will have x=var values and colors=group
    if (stack) {
        # The latest ggplot ordering is really annoying and has to be reversed
        if (!is.factor(pdat$var)) pdat$var <- factor(pdat$var)
        pdat <- transform(pdat, var=factor(var, levels=rev(levels(var)),
                                           labels=rev(levels(var))))
        p <- ggplot(pdat, aes(x=factor(group), y=N, fill=var)) +
            geom_bar(stat="identity", width=0.7)
        # Saving this in case the new code messes things up
#       p <- ggplot(pdat, aes(x=factor(group), y=N, fill=rev(var))) +
#            geom_bar(stat="identity", width=0.7)
        # Add text labels
        p <- p + geom_text(aes(y=pos, label=label), size = textsize)
        if (!regular_legend) {
            # Add special var labels, if regular_legend=FALSE
            p <- p + geom_label(aes(y=pos, fill=var, label=var),
                                colour = "white", fontface = "bold", vjust=-1.5)
#           p <- p + geom_label(aes(y=pos, fill=rev(var), label=var),
#                                colour = "white", fontface = "bold", vjust=-1.5)
            # Remove regular legend guide
            p <- p + guides(fill=FALSE)
        } else {
            # Regular legend needs flipping - again, annoying
            # nuance of the latest ggplot
            p <- p + scale_fill_discrete(name='', guide=guide_legend(reverse=TRUE))
        }
        # Flip coordinates?
        if (flipcoord) p <- p + coord_flip()
    } else {
        # Assumes fill guide is needed - but take off the name?
        p <- ggplot(pdat, aes(x=var, y=N, fill=group)) +
            geom_bar(stat='identity', position='dodge', width=0.7) +
            scale_fill_discrete(name='')
        # Does this need a fill=group in the aes? I got an
        # 'unknown aesethetic' warning
        p <- p + geom_text(aes(y=pos, label=label), size=textsize,
                       position=position_dodge(width=0.7))

        # Optional: add exponential
        if (add_exponential) {
            # Stop if not yperc
            if (!yperc) stop('In plot_categorical, must set yperc=TRUE if adding exponential')
            expdata <- expbin_by_group(d, group='group', panel='panel',
                                       binvar=var, ratevar='len')
            # Determine complete cases and issue warning
            if ('group'%in%colnames(expdata)) {
                check1 <- expdata$group
            } else check1 <- rep(1, nrow(expdata))
            if ('panel'%in%colnames(expdata)) {
                check2 <- expdata$panel
            } else check2 <- rep(1, nrow(expdata))
            complete <- complete.cases(check1, check2)
            if (sum(complete)!=length(complete)) {
                warning('Not displaying NA levels of group and panel vars')
                expdata <- expdata[complete, ]
            }
            # Plot
            p <- p + geom_point(data=expdata, aes(x=Bin, y=Percent, group=group),
                                shape=4, size=3,
                                inherit.aes=FALSE,
                                position=position_dodge(width=0.7))
        }
    }
    # Optional: add text percentages
    # Tidy up - all graphs - I'm not sure about the text size
    # and I want the scale name to be a function input
    p <- p + theme_bw() + scale_x_discrete(name='') +
        scale_y_continuous(name=ylab) +
        theme(text=element_text(size=15))
    # Option to panel - do this outside of the plot, so you can
    # control rows vs columns
    # p <- p + facet_grid(panel~.)
    return(p)
}
