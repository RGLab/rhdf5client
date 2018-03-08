#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr content
#' @importFrom rjson fromJSON
#' @importFrom utils capture.output
#' @importFrom base64enc base64encode

### Basic Object Definitions

#' @name H5S_file
#' @slot serverURL address and port of server
#' @slot name (usually) name of HDF5 file
#' @exportClass H5S_file
setClass("H5S_file", representation(serverURL="character", name="character", root="character"))

#' @name H5S_file
#' @export
H5S_file = function(serverURL, filename)  {

   rootuuid <- "" 
   req <- paste0(serverURL, "/")
   tryCatch(  {
       res <- GET(req, query=list('host'=filename))
   }, 
   error=function(cond)  {
       stop(paste0("error creating H5S_file object: ",cond))
   },
   warning=function(cond)  {
       warning(paste0("warning creating H5S_file object: ",cond))
   },
   finally={})
   rootuuid <- content(res)$root
   obj <- new("H5S_file", serverURL=serverURL, name=filename, root=rootuuid)
}

#' @name H5S_group
#' @slot uuid 
#' @slot file 
#' @export
setClass("H5S_group", representation(uuid="character", file="H5S_file"))

#' @name H5S_dset
#' @slot uuid 
#' @slot file 
#' @export
setClass("H5S_dset", representation(uuid="character", file="H5S_file"))

#' @name H5S_group
#' @export
H5S_group = function(uuid, file)  {
  obj <- new("H5S_group", uuid=uuid, file=file)
}

#' @name H5S_dset
#' @export
H5S_dset = function(uuid, file)  {
  obj <- new("H5S_dset", uuid=uuid, file=file)
}


### List datasets in a file: h5ls 

#' return a data frame of all links in a group
#' @param an object of type H5S_group
#' Private: used by visit and h5ls

linklst <- function(object)  {
   if (!class(object) == "H5S_group")  
     stop("error: object must be of type H5S_group")

   tryCatch({

     url <- object@file@serverURL
     uid <- object@uuid
     dmn <- object@file@name

     req <- paste0(url, "/groups/", uid, "/links")
     res <- GET(req, query=list('host'=dmn))
     lks <- content(res)$links

   },
   error=function(cond)  {
       stop(paste0("error requesting links: ", cond))
   },
   warning=function(cond)  {
       warning(paste0("warning requesting links: ", cond))
   },
   finally={})

   df = data.frame()
   nn = c("target", "title", "collection", "class", "href", "id")
   for (lk in lks)  {
     df <- rbind(df, as.data.frame(lk[nn], stringsAsFactors=FALSE))
   }
   df

}

#'  Get a data frame of all contents in a group
#'  Private: used by h5ls

#  TODO: put a limit on the number of rows retrieved
setGeneric("visit", function(object) standardGeneric("visit"))
setMethod("visit", "H5S_group", function(object)  {

  visited = c()
  tovisit = c(object@uuid)
  dd <- data.frame(stringsAsFactors=FALSE)
  pathlist <- list()
  pathlist[object@uuid] <- ""

  while (length(tovisit) > 0)  {

    v <- tovisit[1]
    tovisit = tovisit[-1]
    visited = c(visited,v)   # TODO: preallocate

    g <- H5S_group(v, object@file)
    df <- linklst(g)

    for (i in seq_len(nrow(df)))  {

      uuid <- as.character(df[i,"id"])
      path <- paste0(pathlist[v],"/",df[i,"title"])
      pathlist[uuid] = path
      df[i,"path"] = path

      if (df[i,"collection"] == "groups")  {
        if (!(uuid %in% visited))  {       # question: should this also check uuid %in% tovisit?
          tovisit <- c(tovisit,uuid)
        }
      }

    }
    dd <- rbind(dd, df, stringsAsFactors = F)
  }
  dd

})

#' Get a data frame of all datasets in file
#' @exportMethod h5ls
#  TODO: put a limit on the number of rows retrieved

setGeneric("h5ls", function(object) standardGeneric("h5ls"))
setMethod("h5ls", "H5S_file", function(object)  {

   rgp <- H5S_group(object@root, object)
   rdt <- visit(rgp)

   req <- paste0(object@serverURL, "/datasets")
   tryCatch(  {
       res <- GET(req, query=list('host'=object@name))
   }, 
   error=function(cond)  {
       stop(paste0("error listing datasets:", cond))
   },
   warning=function(cond)  {
       warning(paste0("warning listing datasets:", cond))
   },
   finally={})

   dsts <- content(res)$datasets
   df <- data.frame(path=c(), name=c(), dims=c(), base=c(), class=c(), stringsAsFactors=FALSE)
   for (dst in dsts)  {

     req <- paste0(object@serverURL, "/datasets/", dst)
     tryCatch(  {
         res <- GET(req, query=list('host'=object@name))
     }, 
     error=function(cond)  {
         stop(paste0("error looking up dataset:", cond))
     },
     warning=function(cond)  {
         warning(paste0("warning looking up dataset:", cond))
     },
     finally={})

     ir <- which(rdt$id == dst)
     dat <- content(res)

     ph <- rdt[ir,]$path
     tt <- rdt[ir,]$title
     dm <- unlist(content(res)$shape$dims)
     dm <- paste(dm, collapse=" ")
     bs <- content(res)$type$base
     cl <- content(res)$type$class
     dd <- data.frame(path=ph, name=tt, dims=dm, base=bs, class=cl, stringsAsFactors=FALSE)
     df <- rbind(df, dd, stringsAsFactors = F)

   }
   df

})


### h5read(file, dset) 

#' h5read
#' @exportMethod h5read
#' @return An object of type H5S_dset: no actual data is read in at this time
#' Possibly a good place to subclass H5DelayedArray

setGeneric("h5read", function(object, path) standardGeneric("h5read"))
setMethod("h5read", signature(object="H5S_file", path="character"), function(object, path)  {

  rgp <- H5S_group(object@root, object)
  rdt <- visit(rgp)

  iw <- which(rdt$path == path)
  if (length(iw) != 1L)
    stop(paste0("error: ", "no such dataset: ", path)) 

  uuid <- rdt$id[iw]
  ds <- H5S_dset(uuid, object)
  ds

})



### navigate group hierarchy with [

#' navigate file hierarchy
#' @exportMethod [
setMethod("[", signature(x="H5S_group", i="character"), function(x, i)  {

  lst <- unlist(strsplit(i, "/"))
  obj <- x

  for (lnk in lst)  {

    df <- linklst(obj)
    ir <- which(df[,"title"] == lnk)

    if (length(ir) == 0)  {
      warning(paste0("No such object ", lnk))
      return(NULL)
    }
    if (length(ir) > 1)  {
      warning(paste0("Multiple links (should not be possible)", lnk))
      ir <- ir[1]
    }
  
    if (df[ir,"collection"] == "groups")  {
      obj <- new("H5S_group", uuid=as.character(df[ir,"id"]), file=x@file)
    } else if(df[ir,"collection"] == "datasets")  {
      obj <- new("H5S_dset", uuid=as.character(df[ir,"id"]), file=x@file)
    } else  {
      warning(paste0("Unidentifiable collection type ", df[ir,"collection"]))
      return(NULL)
    }

  }
  obj

})

### retrieve data from a dataset with [
