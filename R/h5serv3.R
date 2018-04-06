#' Question: Where does this go?
library(reticulate)
library(abind)

#' Package environment to keep track of instantiated Python objects
pkg.env = new.env()
pkg.env$nfp <- 0       # next digit to use for File object python name
pkg.env$ngp <- 0       # next digit to use for Group object python name
pkg.env$nds <- 0       # next digit to use for Dataset object python name

#' @name H5S_py_source
#' @slot domain 
#' @slot endpoint 
#' @export
setClass("H5S_py_source", representation(domain="character", endpoint="character", pyname="character"))

#' @name H5S_py_source
#' @export
H5S_py_source = function(dm, ep)  {

  py_run_string("import h5pyd as h5py")

  pn <- paste0("fp", pkg.env$nfp)
  cmd <- paste0(pn, " = h5py.File('", dm, "', 'r', endpoint='", ep, "')")

  res <- NULL
  tryCatch(
    res <- py_run_string(cmd),
    error = function(e) { res <- NULL }
  )
    
  if (!is.null(res))  {
    obj <- new("H5S_py_source", pyname=pn, domain=dm, endpoint=ep)
    pkg.env$nfp <- pkg.env$nfp + 1
    return(obj)
  } else  {
    message("Error: Unable to instantiate HDF5 File object")
    return(NULL)
  }


}

#' @name H5S_py_dset
#' @slot source
#' @slot name
#' @slot pyname
#' @slot isopen
#' @export
setClass("H5S_py_dset", representation(name="character", pyname="character", 
  isopen="logical", source="H5S_py_source"))

#' @name H5S_py_dset
#' @export
H5S_py_dset = function(src, dn)  {

  pn <- paste0("ds", pkg.env$nds)
  fn <- src@pyname
  cmd <- paste0(pn, " = ", fn, "['", dn, "']")

  res <- NULL
  tryCatch(
    res <- py_run_string(cmd),
    error = function(e) { res <- NULL }
  )

  if (!is.null(res)) {
    obj <- new("H5S_py_dset", name=dn, pyname=pn, isopen=TRUE, source=src)
    pkg.env$nds <- pkg.env$nds + 1
    return(obj)
  } else  {
    return(NULL)
  }

}

#' @name shape
#' @param dset 
#' @export
shape <- function(dset)  {
  if (!class(dset) == "H5S_py_dset")
    stop("error: object is not a dataset")

  dn <- dset@pyname
  cmd <- paste0(dn,".shape")

  res <- NULL
  tryCatch(
    res <- py_eval(cmd),
    error = function(e) { res <- NULL }
  )

  if (!is.null(res)) {
    return(unlist(res))
  } else  {
    return(NULL)
  }

}

#' conversion of a vector into h5py slice notation
#' input: numeric vector, e.g., c(1, 4, 6)
#' TODO: other possible input formats
#' return value: a list of slices, e.g., "0:3:3", "6:7:1"
#' 
#' R: array indices begin at 1, ranges include last element 
#' python: array indices begin at 0, ranges exclude last element 

n2slice <- function(x)  {

  if (!is.numeric(x))  {
    stop("error: non-numeric subscripts not supported yet")
  }
  sproc(isplit(x)) 

}

#' get data from data set
#' @exportMethod [
setMethod("[", c(x="H5S_py_dset", i="ANY"), function(x,i) {
  
  if (!class(x) == "H5S_py_dset")
    stop("error: object is not a dataset")
  r <- shape(x)

  if (!is.list(i))
    stop("error: indices not a in a list") 

  if (!length(r) == length(i))
    stop("error: wrong number of indices")

  lll <- lapply(ll, n2slice)
  dn <- x@pyname

  if (length(lll) == 3)  {
    for (i in 1:length(lll[[1]]))  {
      for (j in 1:length(lll[[2]]))  {
        for (k in 1:length(lll[[3]]))  {

          lx <- lll[[1]][[i]]
          ly <- lll[[2]][[j]]
          lz <- lll[[3]][[k]]

          cmd <- paste0(dn, "[", lx, ",", ly, ",", lz, "]")
          res <- NULL
          tryCatch(
            res <- py_eval(cmd),
            error = function(e) { res <- NULL }
          )
          if (k == 1)  {
            Z <- res
          } else  {
            Z <- abind(Z, res, along=3)
          }

        }
        if (j == 1)  {
          Y <- Z
        } else  {
          Y <- abind(Y, Z, along=2)
        }
      }
      if (i == 1)  {
        X <- Y
      } else  {
        X <- abind(X, Y, along=1)
      }
    }
  }

  return(X) 

})

