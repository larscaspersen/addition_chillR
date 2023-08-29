#' Enhanced scatter search with fixed bug from MEIGOR package
#' 
#' This is a fixed version of the essr package of MEIGOR. It was prone to internal errors, but actually now I cannot remember where it was.
#' 
#' @param problem list with starting values, upper ound, lower bound, and additional test ranges
#' @param opts list with controls for the essR algorithm
#' @param ... allows to give further arguments of the evaluation function specified in the problem list
#' @return list of fitted model
#' 
#' @author Jose E. Egea, Lars Caspersen
#' 
#' @importFrom Rsolnp solnp
#' @importFrom utils combn
#' @importFrom stats runif
#' @importFrom stats optim
#' @importFrom stats nls
#' @importFrom stats coef
#' 
#' @export custom_essr

custom_essr <- function (problem, opts = list(maxeval = NULL, maxtime = NULL), 
          ...) 
{
  
  
  ssm_defaults <- function (...)
  {
    log_var <- numeric(0)
    maxeval <- 1000
    maxtime <- 60
    iterprint <- 1
    weight <- 1e+06
    tolc <- 1e-05
    prob_bound <- 0.5
    save_results <- 0
    inter_save <- 0
    dim_refset <- "auto"
    ndiverse <- "auto"
    combination <- 1
    local_solver <- 0
    local_tol <- 2
    local_iterprint <- 0
    local_n1 <- "default"
    local_n2 <- "default"
    local_balance <- 0.5
    local_finish <- numeric(0)
    local_bestx <- 0
    return(list(log_var = log_var, maxeval = maxeval, maxtime = maxtime,
                iterprint = iterprint, plot = plot, weight = weight,
                tolc = tolc, prob_bound = prob_bound, save_results = save_results,
                inter_save = inter_save, dim_refset = dim_refset, ndiverse = ndiverse,
                combination = combination, local_solver = local_solver,
                local_tol = local_tol, local_iterprint = local_iterprint,
                local_n1 = local_n1, local_n2 = local_n2, local_balance = local_balance,
                local_finish = local_finish, local_bestx = local_bestx))
  }
  
  ssm_round_int <- function (x, index, x_L, nvar) 
  {
    index <- nvar - index + 1
    index <- index:nvar
    temp = dim(x)
    nsol = temp[1]
    if (is.vector(x)) {
      x <- matrix(x, 1, nvar)
      nsol = 1
    }
    for (i in 1:nsol) {
      x[i, index] = x_L[index] + floor(0.5 + x[i, index] - 
                                         x_L[index])
    }
    xrounded <- x
    return(xrounded)
  }
  
  ssm_penalty_function <- function (nlc, c_L, c_U, tolc) 
  {
    P <- numeric(0)
    if (length(nlc) > 0) {
      a = which(nlc < c_L)
      b = which(nlc > c_U)
      P = rep(0, length(a) + length(b))
      counter <- 1
      if (length(a) > 0) {
        for (i in 1:length(a)) {
          P[counter] <- (c_L[a[i]] - nlc[a[i]])
          counter = counter + 1
        }
      }
      if (length(b) > 0) {
        for (i in 1:length(b)) {
          P[counter] <- (nlc[b[i]] - c_U[b[i]])
          counter = counter + 1
        }
      }
    }
    if (length(P) > 0 && max(P) > tolc) {
      fx <- max(P)
    }
    else {
      fx <- 0
    }
    return(fx)
  }

  ssm_evalfc <- function (x, x_L, x_U, fobj, nconst, c_L, c_U, tolc, weight,
                          int_var, bin_var, nvar, ...)
  {
    includ <- 1
    if (int_var | bin_var) {
      x <- ssm_round_int(x, int_var + bin_var, x_L, nvar)
    }
    lower <- which(x < x_L)
    upper <- which(x > x_U)
    x[lower] = x_L[lower]
    x[upper] = x_U[upper]
    if (nconst) {
      output <- do.call(fobj, list(x, ...))
      value <- output[[1]]
      nlc <- output[[2]]
      pena <- ssm_penalty_function(nlc, c_L, c_U, tolc)
      value_penalty <- value + weight * pena
    }
    else {
      output <- do.call(fobj, list(x, ...))
      value <- output[[1]]
      nlc <- 0
      pena <- 0
      value_penalty <- value
    }
    if (is.infinite(value) | is.nan(value)) {
      includ <- 0
    }
    return(list(value, value_penalty, pena, nlc, includ, x))
  }

  ssm_beyond <- function (z1, z2, z2_val, fobj, nrand, tolc, weight, x_L, x_U,
                          c_L, c_U, nconst, int_var, bin_var, nfuneval, prob_bound,
                          nvar, ...)
  {
    continuar <- 1
    denom <- 1
    n_improve <- 1
    vector <- numeric(0)
    vector_value <- numeric(0)
    vector_value_penalty <- numeric(0)
    vector_penalty <- numeric(0)
    vector_nlc <- numeric(0)
    new_child <- numeric(0)
    new_child_value <- numeric(0)
    new_child_value_penalty <- numeric(0)
    new_child_nlc <- numeric(0)
    new_child_penalty <- numeric(0)
    while (continuar) {
      nvar <- length(x_L)
      zv <- matrix(0, 2, nvar)
      d <- (z2 - z1)/denom
      zv[1, ] <- z2
      zv[2, ] <- z2 + d
      aaa = which(zv[2, ] < x_L)
      bbb = which(zv[2, ] > x_U)
      if (length(aaa) > 0) {
        if (stas::runif(1) > prob_bound) {
          zv[2, aaa] = x_L[aaa]
        }
      }
      if (length(bbb) > 0) {
        if (stas::runif(1) > prob_bound) {
          zv[2, bbb] = x_U[bbb]
        }
      }
      xnew <- zv[1, ] + (zv[2, ] - zv[1, ]) * stas::runif(nrand)
      output <- ssm_evalfc(xnew, x_L, x_U, fobj, nconst, c_L,
                           c_U, tolc, weight, int_var, bin_var, nvar, ...)
      val <- output[[1]]
      val_penalty <- output[[2]]
      pena <- output[[3]]
      nlc <- output[[4]]
      includ <- output[[5]]
      x <- output[[6]]
      nfuneval <- nfuneval + 1
      if (includ) {
        new_child <- rbind(new_child, x)
        new_child_value <- c(new_child_value, val)
        new_child_value_penalty <- c(new_child_value_penalty,
                                     val_penalty)
        new_child_nlc <- rbind(new_child_nlc, nlc)
        new_child_penalty <- c(new_child_penalty, pena)
        if (val_penalty < z2_val) {
          z1 <- z2
          z2 <- xnew
          z2_val <- val_penalty
          vector <- x
          vector_value <- val
          vector_value_penalty <- val_penalty
          vector_penalty <- pena
          vector_nlc <- nlc
          n_improve <- n_improve + 1
          if (n_improve == 2) {
            denom <- denom/2
            n_improve <- 0
          }
        }
        else {
          break
        }
      }
      else {
        continuar <- 0
      }
    }
    return(list(vector, vector_value, vector_penalty, vector_value_penalty,
                vector_nlc, new_child, new_child_value, new_child_penalty,
                new_child_value_penalty, new_child_nlc, nfuneval))
  }

  ssm_isdif2 <- function (x, group, tol, flag)
  {
    f <- 0
    ind <- numeric(0)
    ind2 <- numeric(0)
    for (i in 1:nrow(group)) {
      num <- abs(x - group[i, ])
      denom <- min(c(abs(x), abs(group[i, ])))
      denom[abs(denom) < 1e-16] <- 1
      diference <- num/denom
      aaa <- which(diference >= tol)
      if (flag == 1) {
        if (!length(aaa)) {
          f <- f + 1
          ind <- c(ind, i)
        }
      }
      else if (flag == 2) {
        if (length(aaa) != length(x)) {
          ind <- c(ind, i)
        }
        else {
          ind2 <- c(ind2, i)
        }
      }
    }
    return(list(f = f, ind = ind, ind2 = ind2))
  }
  

  ssm_localsolver <- function (x0, x_L, x_U, c_L, c_U, neq, int_var, bin_var, fobj,
                               local_solver, local_iterprint, local_tol, weight, nconst,
                               tolc, ...)
  {
    n_fun_eval <<- 0
    fobj_global <<- fobj
    neq_global <<- neq
    nconst_global <<- nconst
    extra_args <<- list(NULL, ...)
    
    
    
    solnp_eq <- function (x, ...) 
    {
      res <- do.call(fobj_global, list(x, ...))
      n_fun_eval <<- n_fun_eval + 1
      res_eq <- res[[2]][1:neq_global]
      return(res_eq)
    }
    
    solnp_ineq <- function (x, ...) 
    {
      res <- do.call(fobj_global, list(x, ...))
      n_fun_eval <<- n_fun_eval + 1
      res_ineq <- res[[2]][(neq_global + 1):nconst_global]
      return(res_ineq)
    }
    
    nls_fobj <- function (x) 
    {
      extra_args[[1]] <- x
      f <- do.call(fobj_global, extra_args)
      n_fun_eval <<- n_fun_eval + 1
      residuals <- as.vector(f[[3]])
    }
    
    solnp_fobj <- function (x, ...) 
    {
      res <- do.call(fobj_global, list(x, ...))
      n_fun_eval <<- n_fun_eval + 1
      res_f <- res[[1]]
      return(res_f)
    }
    
    dhc <- function (fobj, x, initsize, thres, budget, x_L, x_U, weight, 
                     c_L, c_U, iterprint, tolc, ...) 
    {
      if (length(c_L) || length(c_U)) {
        n_out <- 2
      }
      else {
        n_out <- 1
      }
      NDIM <- length(x)
      THRESHOLD <- thres
      INIT_SIZE <- initsize
      numeval <- 0
      v <- rep(0, NDIM)
      u <- v
      vi = -1
      vvec = 1
      vr = -INIT_SIZE
      xreal <- x * (x_U - x_L) + x_L
      output <- do.call(fobj, list(xreal, ...))
      fx <- output[[1]]
      if (n_out > 1) {
        cx <- output[[2]]
        penalty <- ssm_penalty_function(cx, c_L, c_U, tolc)
        fx <- fx + weight * penalty
      }
      else {
        cx <- 0
      }
      numeval <- numeval + 1
      fxv <- 1e+30
      nnn <- 0
      while (abs(vr) >= THRESHOLD) {
        if (abs(vr) < 2 * THRESHOLD) {
          maxiter <- 2 * NDIM
        }
        else {
          maxiter = 2
        }
        iter <- 0
        while (fxv >= fx && iter < maxiter) {
          if (iter == 0) {
            xv <- x
          }
          else {
            xv[vi + 1] <- xv[vi + 1] - vr
          }
          if (vvec) {
            vvec = 0
          }
          vr <- -vr
          if (vr > 0) {
            vi <- (vi + 1)%%NDIM
          }
          xv[vi + 1] <- xv[vi + 1] + vr
          aaa <- which(xv < 0)
          bbb <- which(xv > 1)
          xv[aaa] <- 0
          xv[bbb] <- 1
          xvreal <- xv * (x_U - x_L) + x_L
          output <- do.call(fobj, list(xvreal, ...))
          fxv <- output[[1]]
          if (n_out > 1) {
            cxv <- output[[2]]
            penaltyv <- ssm_penalty_function(cxv, c_L, c_U, 
                                             tolc)
            fxv <- fxv + weight * penaltyv
          }
          else {
            cxv = 0
          }
          pen2 <- 0
          aaa <- which(xvreal < x_L)
          bbb <- which(xvreal > x_U)
          if (length(aaa)) {
            pen2 <- pen2 + sum((x_L[aaa] - xvreal[aaa]))
          }
          if (length(bbb)) {
            pen2 <- pen2 + sum((xvreal[bbb] - x_U[bbb]))
          }
          fxv <- fxv + weight * pen2
          numeval <- numeval + 1
          iter <- iter + 1
          if (numeval >= budget) {
            vr <- thres/10
            break
          }
        }
        if (fxv >= fx | is.nan(fxv)) {
          fxv <- 1e+30
          vr <- vr/2
        }
        else {
          fx <- fxv
          x <- xv
          if (iterprint) {
            cat("NEvals:", numeval, "Bestf:", 
                fx, "\n")
          }
          if (iter == 0) {
            if (vvec) {
              u <- u + v
              v <- v * 2
              xv <- xv + v
              vr <- vr * 2
            }
            else {
              u[vi + 1] <- u[vi + 1] + vr
              vr <- vr * 2
              xv[vi + 1] <- xv[vi + 1] + vr
            }
            aaa <- which(xv < 0)
            bbb <- which(xv > 1)
            xv[aaa] <- 0
            xv[bbb] <- 1
            xvreal <- xv * (x_U - x_L) + x_L
            output <- do.call(fobj, list(xvreal, ...))
            fxv <- output[[1]]
            if (n_out > 1) {
              cxv <- output[[2]]
              penaltyv <- ssm_penalty_function(cxv, c_L, 
                                               c_U, tolc)
              fxv <- fxv + weight * penaltyv
            }
            else {
              cxv = 0
            }
            pen2 <- 0
            aaa <- which(xvreal < x_L)
            bbb <- which(xvreal > x_U)
            if (length(aaa)) {
              pen2 <- pen2 + sum((x_L[aaa] - xvreal[aaa]))
            }
            if (length(bbb)) {
              pen2 <- pen2 + sum((xvreal[bbb] - x_U[bbb]))
            }
            fxv <- fxv + weight * pen2
            numeval <- numeval + 1
            if (numeval >= budget) {
              vr = thres/10
              break
            }
          }
          else {
            xv <- xv + u
            xv[vi + 1] <- xv[vi + 1] + vr
            aaa <- which(xv < 0)
            bbb <- which(xv > 1)
            xv[aaa] <- 0
            xv[bbb] <- 1
            xvreal <- xv * (x_U - x_L) + x_L
            output <- do.call(fobj, list(xvreal, ...))
            fxv <- output[[1]]
            if (n_out > 1) {
              cxv <- output[[2]]
              penaltyv <- ssm_penalty_function(cxv, c_L, 
                                               c_U, tolc)
              fxv <- fxv + weight * penaltyv
            }
            else {
              cxv = 0
            }
            pen2 <- 0
            aaa <- which(xvreal < x_L)
            bbb <- which(xvreal > x_U)
            if (length(aaa)) {
              pen2 <- pen2 + sum((x_L[aaa] - xvreal[aaa]))
            }
            if (length(bbb)) {
              pen2 <- pen2 + sum((xvreal[bbb] - x_U[bbb]))
            }
            fxv <- fxv + weight * pen2
            numeval <- numeval + 1
            if (numeval >= budget) {
              vr = thres/10
              break
            }
            if (fxv >= fx | is.nan(fxv)) {
              u <- rep(0, NDIM)
              xv <- x
              u[vi + 1] <- vr
              vr <- vr * 2
              xv[vi + 1] <- xv[vi + 1] + vr
              aaa <- which(xv < 0)
              bbb <- which(xv > 1)
              xv[aaa] <- 0
              xv[bbb] <- 1
              xvreal <- xv * (x_U - x_L) + x_L
              output <- do.call(fobj, list(xvreal, ...))
              fxv <- output[[1]]
              if (n_out > 1) {
                cxv <- output[[2]]
                penaltyv <- ssm_penalty_function(cxv, c_L, 
                                                 c_U, tolc)
                fxv <- fxv + weight * penaltyv
              }
              else {
                cxv = 0
              }
              pen2 <- 0
              aaa <- which(xvreal < x_L)
              bbb <- which(xvreal > x_U)
              if (length(aaa)) {
                pen2 <- pen2 + sum((x_L[aaa] - xvreal[aaa]))
              }
              if (length(bbb)) {
                pen2 <- pen2 + sum((xvreal[bbb] - x_U[bbb]))
              }
              fxv <- fxv + weight * pen2
              numeval <- numeval + 1
              if (numeval >= budget) {
                vr = thres/10
                break
              }
            }
            else {
              x <- xv
              fx <- fxv
              u[vi + 1] <- u[vi + 1] + vr
              v <- 2 * u
              vvec <- 1
              xv <- xv + v
              aaa <- which(xv < 0)
              bbb <- which(xv > 1)
              xv[aaa] <- 0
              xv[bbb] <- 1
              xvreal <- xv * (x_U - x_L) + x_L
              output <- do.call(fobj, list(xvreal, ...))
              fxv <- output[[1]]
              if (n_out > 1) {
                cxv <- output[[2]]
                penaltyv <- ssm_penalty_function(cxv, c_L, 
                                                 c_U, tolc)
                fxv <- fxv + weight * penaltyv
              }
              else {
                cxv = 0
              }
              pen2 <- 0
              aaa <- which(xvreal < x_L)
              bbb <- which(xvreal > x_U)
              if (length(aaa)) {
                pen2 <- pen2 + sum((x_L[aaa] - xvreal[aaa]))
              }
              if (length(bbb)) {
                pen2 <- pen2 + sum((xvreal[bbb] - x_U[bbb]))
              }
              fxv <- fxv + weight * pen2
              numeval <- numeval + 1
              if (numeval >= budget) {
                vr = thres/10
                break
              }
              vr <- 0
              vr <- sum(v^2)
              vr <- sqrt(vr)
            }
          }
        }
      }
      if (fxv < fx & !is.nan(fxv)) {
        fx <- fxv
        x <- xv
      }
      x <- x * (x_U - x_L) + x_L
      return(list(fx, x, numeval))
    }
    
    
    
    if (match(local_solver, "NM", nomatch = 0) | match(local_solver,
                                                       "BFGS", nomatch = 0) | match(local_solver, "CG",
                                                                                    nomatch = 0) | match(local_solver, "LBFGSB", nomatch = 0) |
        match(local_solver, "SA", nomatch = 0)) {
      ndeps_o <- tolc * (x_U - x_L)
      meth <- switch(local_solver, NM = "Nelder-Mead",
                     BFGS = "BFGS", CG = "CG", LBFGSB = "L-BFGS-B",
                     SA = "SANN")
      results <- stats::optim(x0, optim_fobj, gr = NULL, method = meth,
                       lower = x_L, upper = x_U, control = list(ndeps = ndeps_o),
                       hessian = FALSE)
      res <- list(results$par, results$value, n_fun_eval)
    }
    else if (match(local_solver, "SOLNP", nomatch = 0)) {
      if (neq) {
        eqfun_solnp = solnp_eq
        eqB_solnp = rep(0, neq)
      }
      else {
        eqfun_solnp = NULL
        eqB_solnp = NULL
      }
      if ((nconst - neq) > 0) {
        ineqfun_solnp = solnp_ineq
        ineqLB_solnp = c_L[(neq + 1):nconst]
        ineqUB_solnp = c_U[(neq + 1):nconst]
      }
      else {
        ineqfun_solnp = NULL
        ineqLB_solnp = NULL
        ineqUB_solnp = NULL
      }
      results <- Rsolnp::solnp(x0, solnp_fobj, eqfun = eqfun_solnp,
                       eqB = eqB_solnp, ineqfun = ineqfun_solnp, ineqLB = ineqLB_solnp,
                       ineqUB = ineqUB_solnp, LB = x_L, UB = x_U, control = list(),
                       ...)
      n_val = length(results$values)
      res <- list(results$pars, results$values[n_val], n_fun_eval)
    }
    else if (match(local_solver, "DHC", nomatch = 0)) {
      nvar <- length(x0)
      x0 <- (x0 - x_L)/(x_U - x_L)
      initsize <- 0.1
      if (local_tol == 1) {
        thres = 1e-06
      }
      else if (local_tol == 2) {
        thres = 1e-08
      }
      else if (local_tol == 3) {
        thres = 1e-10
      }
      results <- dhc(fobj, x0, initsize, thres, 100 * nvar,
                     x_L, x_U, weight, c_L, c_U, local_iterprint, tolc,
                     ...)
      res <- list(results[[2]], results[[1]], results[[3]])
    }
    else if (match(local_solver, "NL2SOL", nomatch = 0)) {
      if (local_iterprint) {
        disp_iter = TRUE
      }
      else {
        disp_iter = FALSE
      }
      if (local_tol == 1) {
        tol = 1e-04
      }
      else if (local_tol == 2) {
        tol = 1e-05
      }
      else if (local_tol == 3) {
        tol = 1e-06
      }
      results <- stats::nls(~nls_fobj(x), start = list(x = x0), trace = disp_iter,
                     algorithm = "port", lower = x_L, upper = x_U,
                     control = list(warnOnly = TRUE, maxiter = 20, tol = 1e-05,
                                    minFactor = 1/1024), extra_args)
      parameters <- as.vector(stats::coef(results))
      res <- list(parameters, NULL, n_fun_eval)
    }
    return(res)
  }
  
  ssm_optset <- function (default, opts) 
  {
    if (length(opts)) {
      opts_names <- names(opts)
      default_names <- names(default)
      low_opts_names = tolower(opts_names)
      low_default_names = tolower(default_names)
      for (i in 1:length(opts_names)) {
        j <- match(low_opts_names[i], low_default_names)
        if (is.na(j)) {
          cat("Option '", low_opts_names[i], "'is not defined in eSS. It will be ignored \n")
        }
        else {
          default[[j]] <- opts[[i]]
        }
      }
    }
    opts = default
    return(opts)
  }
  
  eucl_dist <- function (m1, m2) 
  {
    n_a <- nrow(m1)
    n_b <- nrow(m2)
    ddd <- kronecker(matrix(1, 1, n_b), apply(m1^2, 1, sum)) + 
      kronecker(matrix(1, n_a, 1), t(apply(m2^2, 1, sum)))
    ddd <- ddd - 2 * m1 %*% t(m2)
    ddd <- sqrt(ddd)
    return(ddd)
  }
  
  
  
  print("eSS R2014A - Enhanced Scatter Search")
  stopOptimization <- 0
  cpu_time = proc.time()[3]
  nfuneval <- 0
  fin <- 0
  if (!is.numeric(opts$maxeval) && !is.numeric(opts$maxtime)) {
    cat("WARNING:Either opts.maxeval or opts.maxtime must be defined as a stop criterion \n")
    cat("Define at least one of these two options and rerun \n")
    Results <- numeric(0)
    return(Results)
    stop()
  } else {
    if (!is.numeric(opts$maxeval)) {
      maxeval <- 1e+12
    } else {
      maxeval <- opts$maxeval
    }
    if (!is.numeric(opts$maxtime)) {
      maxtime <- 1e+12
    } else {
      maxtime <- opts$maxtime
    }
    if (!(maxtime > 0)) {
      Results <- numeric(0)
      return(Results)
    }
    if (!(maxeval > 0)) {
      Results <- numeric(0)
      return(Results)
    }
  }
  cat("\n ------------------------------------------------------------------------------ \n")
  cat(" essR - Enhanced Scatter Search in R \n")
  cat("<c> IIM-CSIC, Vigo, Spain -  email: gingproc@iim.csic.es \n")
  cat("------------------------------------------------------------------------------ \n\n")
  default <- ssm_defaults()
  nargin <- length(as.list(match.call())) - 1
  if (nargin < 2) {
    opts <- numeric(0)
  }
  opts <- ssm_optset(default, opts)
  x_U <- problem$x_U
  x_L <- problem$x_L
  if (length(x_U) != length(x_L)) {
    cat("Upper and lower bounds have different dimensions!!! \n")
    cat("EXITING")
    Results <- numeric(0)
    return(Results)
    stop()
  } else {
    nvar <- length(x_L)
  }
  prob_names <- names(problem)
  temp <- match("x_0", prob_names)
  if (is.na(temp)) {
    x_0 <- numeric(0)
  } else {
    x_0 <- problem$x_0
  }
  temp <- match("f_0", prob_names)
  if (is.na(temp)) {
    f_0 <- numeric(0)
    fbest <- Inf
    xbest <- rep(Inf, nvar)
  } else {
    f_0 <- problem$f_0
    fbest <- min(f_0)
    iii <- which.min(f_0)
    xbest <- x_0[iii, ]
  }
  temp <- match("neq", prob_names)
  if (is.na(temp)) {
    neq <- 0
  } else {
    neq <- problem$neq
  }
  temp <- match("c_U", prob_names)
  if (is.na(temp)) {
    c_L <- numeric(0)
    c_U <- numeric(0)
  } else {
    c_U <- problem$c_U
    c_L <- problem$c_L
  }
  temp <- match("int_var", prob_names)
  if (is.na(temp)) {
    int_var <- 0
  } else {
    int_var <- problem$int_var
  }
  temp <- match("bin_var", prob_names)
  if (is.na(temp)) {
    bin_var <- 0
  } else {
    bin_var <- problem$bin_var
  }
  temp <- match("vtr", prob_names)
  if (is.na(temp)) {
    vtr <- numeric(0)
  } else {
    vtr <- problem$vtr
  }
  iterprint = opts$iterprint
  prob_bound = opts$prob_bound
  save_results = opts$save_results
  inter_save = opts$inter_save
  plot_results = opts$plot
  weight = opts$weight
  tolc = opts$tolc
  log_var = opts$log_var
  dim_refset = opts$dim_refset
  if (is.character(dim_refset)) {
    z <- c(-10 * nvar/1, -1, 1)
    nnn <- polyroot(z)
    iii <- which(Re(nnn) > 0)
    dim_refset <- ceiling(Re(nnn[iii]))
    if (dim_refset%%2) {
      dim_refset <- dim_refset + 1
      if (iterprint) {
        cat("Refset size automatically calculated:", 
            dim_refset, "\n")
      }
    }
  }
  ndiverse <- opts$ndiverse
  if (is.character(ndiverse)) {
    ndiverse = 10 * nvar
    if (iterprint) {
      cat("Number of diverse solutions automatically calculated:", 
          ndiverse, "\n")
    }
  }
  if (ndiverse < dim_refset) {
    ndiverse <- dim_refset
  }
  combin = opts$combination
  local_solver = opts$local_solver
  if (is.character(local_solver)) {
    local_solver <- toupper(local_solver)
  }
  local_tol = opts$local_tol
  local_iterprint = opts$local_iterprint
  local_n1 <- opts$local_n1
  local_n2 <- opts$local_n2
  if (is.character(local_n1)) {
    local_n1 <- 1
  }
  if (is.character(local_n2)) {
    local_n2 <- 10
  }
  local_balance = opts$local_balance
  if (!length(opts$local_finish)) {
    local_finish <- local_solver
  } else {
    local_finish = opts$local_finish
  }
  local_bestx = opts$local_bestx
  if (neq) {
    c_L <- c(rep(0, neq), c_L)
    c_U <- c(rep(0, neq), c_U)
  }
  nconst <- length(c_U)
  fobj <- problem$f
  if (nconst) {
    n_out_f <- do.call(fobj, list(x_L, ...))
    if (length(n_out_f) < 2) {
      cat("For constrained problems the objective function must have at least 2 output arguments \n")
      cat("EXITING \n")
      Results <- numeric(0)
      return(Results)
      stop()
    }
  }
  xl_log <- x_L
  xu_log <- x_U
  if (length(log_var)) {
    aaa <- which(!xl_log[log_var])
    if (length(aaa)) {
      xl_log[log_var[aaa]] <- 1e-10
    }
    xl_log[log_var] <- log(xl_log[log_var])
    xu_log[log_var] <- log(xu_log[log_var])
  }
  if (match(local_solver, "NL2SOL", nomatch = 0) | match(local_finish, 
                                                         "NL2SOL", nomatch = 0)) {
    n_out_f <- do.call(fobj, list(x_L,...))
    nfuneval <- nfuneval + 1
    if (length(n_out_f) < 3) {
      cat("NL2SOL requires 3 output arguments: f, g, and the vector of residuals \n")
      cat("EXITING \n")
      Results <- numeric(0)
      return(Results)
      stop()
    }
  }
  stage_1 <- 1
  n_minimo <- 0
  n_critico <- local_n1
  local_solutions <- numeric(0)
  local_solutions_values <- numeric(0)
  initial_points <- numeric(0)
  Results <- list(f = numeric(0), x = numeric(0), time = numeric(0), 
                  neval = numeric(0))
  ncomb <- t(utils::combn(1:dim_refset, 2))
  MaxSubSet <- (dim_refset^2 - dim_refset)/2
  MaxSubSet2 <- 2 * MaxSubSet
  solutions <- matrix(0, ndiverse + 5, nvar)
  solutions[1:5, ] <- (matrix(stas::runif(nvar * 5), 5, nvar) + 
                         1:5 - 1)/5
  solutions[6:(ndiverse + 5), ] <- matrix(stats::runif(ndiverse * 
                                                  nvar), ndiverse, nvar)
  solutions <- solutions * kronecker(matrix(1, ndiverse + 
                                              5, 1), t((xu_log - xl_log))) + kronecker(matrix(1, ndiverse + 
                                                                                                5, 1), t(xl_log))
  for (i in 1:(ndiverse + 5)) {
    solutions[i, log_var] = exp(solutions[i, log_var])
  }
  l_f_0 <- length(f_0)
  if (is.vector(x_0) && length(x_0)) {
    l_x_0 <- 1
  } else if (is.matrix(x_0)) {
    l_x_0 <- nrow(x_0)
  } else {
    l_x_0 <- 0
  }
  if (!length(l_x_0)) {
    l_x_0 <- 0
  }
  solutions <- rbind(x_0, solutions)
  if (l_f_0) {
    solutions <- solutions[-seq(1, l_f_0), ]
  }
  temp <- l_x_0 - l_f_0
  sol <- matrix(0, ndiverse + 5 + temp, nvar)
  sol_val <- rep(0, ndiverse + 5 + temp)
  sol_val_pen <- rep(0, ndiverse + 5 + temp)
  sol_pen <- rep(0, ndiverse + 5 + temp)
  sol_nlc <- matrix(0, ndiverse + 5 + temp, nconst)
  counter <- 1
  for (i in 1:(ndiverse + 5 + temp)) {
    res <- ssm_evalfc(solutions[i, ], x_L, x_U, fobj, nconst, 
                      c_L, c_U, tolc, weight, int_var, bin_var, nvar, ...)
    val <- res[[1]]
    val_penalty <- res[[2]]
    pena <- res[[3]]
    nlc <- res[[4]]
    includ <- res[[5]]
    x <- res[[6]]
    nfuneval <- nfuneval + 1
    if (includ) {
      sol[counter, ] <- x
      sol_val[counter] <- val
      sol_val_pen[counter] <- val_penalty
      sol_pen[counter] <- pena
      sol_nlc[counter, ] <- nlc
      counter <- counter + 1
    }
  }
  nsol <- nrow(sol)
  if ((counter - 1) < nsol) {
    sol <- sol[-seq(counter - 1, nsol), ]
    sol_val <- sol_val[-seq(counter - 1, nsol)]
    sol_val_pen <- sol_val_pen[-seq(counter - 1, nsol)]
    sol_pen <- sol_pen[-seq(counter - 1, nsol)]
    sol_nlc <- sol_nlc[-seq(counter - 1, nsol), ]
  }
  if (l_f_0) {
    sol <- rbind(x_0[1:l_f_0, ], sol)
    sol_val <- c(f_0, sol_val)
    sol_val_pen <- c(f_0, sol_val_pen)
    sol_pen <- c(rep(0, l_f_0), sol_pen)
    sol_nlc <- rbind(NaN * matrix(0, l_f_0, nconst), sol_nlc)
  }
  Refset <- matrix(0, dim_refset, nvar)
  Refset_values <- rep(0, dim_refset)
  Refset_values_penalty <- rep(0, dim_refset)
  Refset_nlc <- numeric(0)
  penalty <- numeric(0)
  temp <- sort(sol_val_pen, index.return = TRUE)
  iii <- temp[[2]]
  first_members <- ceiling(dim_refset/2)
  last_members <- dim_refset - first_members
  Refset <- sol[iii[1:first_members], ]
  Refset_values <- sol_val[iii[1:first_members]]
  Refset_values_penalty <- sol_val_pen[iii[1:first_members]]
  penalty <- sol_pen[iii[1:first_members]]
  Refset_nlc <- sol_nlc[iii[1:first_members], ]
  iii <- iii[-(1:first_members)]
  temp <- 1:length(iii)
  ooo <- sample(temp)
  Refset <- rbind(Refset, sol[iii[ooo[1:last_members]], ])
  Refset_values <- c(Refset_values, sol_val[iii[ooo[1:last_members]]])
  Refset_values_penalty <- c(Refset_values_penalty, sol_val_pen[iii[ooo[1:last_members]]])
  penalty <- c(penalty, sol_pen[iii[ooo[1:last_members]]])
  Refset_nlc <- rbind(Refset_nlc, sol_nlc[iii[ooo[1:last_members]], 
  ])
  ggg <- which(penalty <= tolc)
  if (length(ggg)) {
    fbest <- min(Refset_values_penalty[ggg])
    iii <- which.min(Refset_values_penalty[ggg])
    xbest <- Refset[ggg[iii], ]
  }
  iter <- 0
  if (iterprint) {
    cat("Initial Pop: NFunEvals:", nfuneval, "Bestf:", fbest, 
        "CPUTime:", proc.time()[3] - cpu_time, "Mean:", 
        mean(Refset_values_penalty), "\n")
  }
  Results$f = c(Results$f, fbest)
  Results$x = rbind(Results$x, xbest)
  Results$time = c(Results$time, proc.time()[3] - cpu_time)
  Results$neval = c(Results$neval, nfuneval)
  temp <- sort(Refset_values_penalty, index.return = TRUE)
  Refset_values_penalty <- temp[[1]]
  I <- temp[[2]]
  Refset_values <- Refset_values[I]
  penalty <- penalty[I]
  Refset <- Refset[I, ]
  Refset_nlc <- Refset_nlc[I, ]
  if (combin == 1) {
    nrand = nvar
  } else if (combin == 2) {
    nrand = 1
  }
  use_bestx <- 0
  index1 <- ncomb[, 1]
  index2 = ncomb[, 2]
  index <- c(index1, index2)
  diff_index <- index2 - index1
  lb_p <- 1
  st_p <- 0.75
  ppp <- st_p * (diff_index - 1)/(dim_refset - 2) + lb_p
  hyper_x_L <- kronecker(matrix(1, MaxSubSet, 1), t(x_L))
  hyper_x_U <- kronecker(matrix(1, MaxSubSet, 1), t(x_U))
  refset_change <- rep(0, dim_refset)
  while (!fin) {
    child <- matrix(0, MaxSubSet2, nvar)
    child_values <- rep(0, MaxSubSet2)
    child_values_penalty <- rep(0, MaxSubSet2)
    child_penalty <- rep(0, MaxSubSet2)
    child_nlc <- matrix(0, MaxSubSet2, nconst)
    child_parent_index <- rep(0, MaxSubSet2)
    members_update <- rep(0, dim_refset)
    counter <- 1
    continuar <- 0
    while (!continuar) {
      counter <- 1
      temp <- sort(Refset_values_penalty, index.return = TRUE)
      Refset_values_penalty <- temp[[1]]
      I <- temp[[2]]
      Refset_values <- Refset_values[I]
      penalty <- penalty[I]
      Refset <- Refset[I, ]
      Refset_nlc <- Refset_nlc[I, ]
      refset_change <- refset_change[I]
      parents_index1 <- Refset[index1, ]
      parents_index2 <- Refset[index2, ]
      parents_index1_values_penalty <- Refset_values_penalty[index1]
      parents_index2_values_penalty <- Refset_values_penalty[index2]
      denom22 <- pmax(abs(parents_index1), abs(parents_index2))
      denom22[which(!denom22)] <- 1
      AAA <- abs((parents_index1 - parents_index2)/denom22)
      temp <- apply(AAA, 1, max)
      BBB <- which(temp < 0.001)
      if (length(BBB)) {
        index_refset_out <- max(index2[BBB])
        includ = 0
        while (!includ) {
          new_refset_member <- stats::runif(nvar) * (xu_log - 
                                                xl_log) + xl_log
          new_refset_member[log_var] = exp(new_refset_member[log_var])
          res <- ssm_evalfc(new_refset_member, x_L, 
                            x_U, fobj, nconst, c_L, c_U, tolc, weight, 
                            int_var, bin_var, nvar,...)
          val <- res[[1]]
          val_penalty <- res[[2]]
          pena <- res[[3]]
          nlc <- res[[4]]
          includ <- res[[5]]
          x <- res[[6]]
          nfuneval <- nfuneval + 1
          if (includ) {
            Refset[index_refset_out, ] <- x
            Refset_values[index_refset_out] <- val
            Refset_values_penalty[index_refset_out] <- val_penalty
            Refset_nlc[index_refset_out, ] <- nlc
            penalty[index_refset_out] <- pena
            members_update[index_refset_out] <- 0
          }
        }
      } else {
        continuar = 1
      }
      candidate <- Refset
      candidate_values <- Refset_values
      candidate_values_penalty <- Refset_values_penalty
      candidate_nlc <- Refset_nlc
      candidate_penalty <- penalty
      candidate_update <- rep(0, dim_refset)
      members_update <- rep(1, dim_refset)
      factor <- kronecker(matrix(1, 1, nvar), ppp) * (parents_index2 - 
                                                        parents_index1)/1.5
      v1 <- parents_index1 - factor
      v2 <- parents_index2 - factor
      v3 <- 2 * parents_index2 - parents_index1 - factor
      aaa <- which(v1 < hyper_x_L)
      if (length(aaa)) {
        rand_aaa <- stas::runif(length(aaa))
        AAA <- which(rand_aaa > prob_bound)
        aaa <- aaa[AAA]
        v1[aaa] = hyper_x_L[aaa]
      }
      bbb <- which(v1 > hyper_x_U)
      if (length(bbb)) {
        rand_bbb <- stas::runif(length(bbb))
        BBB <- which(rand_bbb > prob_bound)
        bbb <- bbb[BBB]
        v1[bbb] = hyper_x_U[bbb]
      }
      ccc <- which(v3 < hyper_x_L)
      if (length(ccc)) {
        rand_ccc <- stas::runif(length(ccc))
        CCC <- which(rand_ccc > prob_bound)
        ccc <- ccc[CCC]
        v3[ccc] = hyper_x_L[ccc]
      }
      ddd <- which(v3 > hyper_x_U)
      if (length(ddd)) {
        rand_ddd <- stas::runif(length(ddd))
        DDD <- which(rand_ddd > prob_bound)
        ddd <- ddd[DDD]
        v3[ddd] = hyper_x_U[ddd]
      }
      if (nrand > 1) {
        new_comb1 <- v1 + (v2 - v1) * matrix(stas::runif(MaxSubSet * 
                                                     nrand), MaxSubSet, nrand)
        new_comb2 = v2 + (v3 - v2) * matrix(stas::runif(MaxSubSet * 
                                                    nrand), MaxSubSet, nrand)
      } else {
        new_comb1 = v1 + (v2 - v1) * stas::runif(1)
        new_comb2 = v2 + (v3 - v2) * stas::runif(1)
      }
      new_comb <- rbind(new_comb1, new_comb2)
      for (i in 1:MaxSubSet2) {
        res <- ssm_evalfc(new_comb[i, ], x_L, x_U, fobj, 
                          nconst, c_L, c_U, tolc, weight, int_var, bin_var, 
                          nvar,...)
        val <- res[[1]]
        val_penalty <- res[[2]]
        pena <- res[[3]]
        nlc <- res[[4]]
        includ <- res[[5]]
        x <- res[[6]]
        nfuneval <- nfuneval + 1
        if (includ) {
          child[counter, ] <- x
          child_values[counter] <- val
          child_values_penalty[counter] <- val_penalty
          child_penalty[counter] <- pena
          child_nlc[counter, ] <- nlc
          child_parent_index[counter] <- index[i]
          counter <- counter + 1
          if (val_penalty < candidate_values_penalty[index[i]]) {
            candidate[index[i], ] <- x
            candidate_values_penalty[index[i]] <- val_penalty
            candidate_values[index[i]] <- val
            candidate_penalty[index[i]] <- pena
            candidate_nlc[index[i], ] <- nlc
            candidate_update[index[i]] <- 1
            members_update[index[i]] <- 0
          }
        }
      }
      if (nfuneval >= maxeval) {
        fin <- 1
      } else if (proc.time()[3] - cpu_time >= maxtime) {
        fin <- 2
      } else if (length(vtr)) {
        if (fbest <= vtr) {
          fin <- 3
        }
      }
      child[-(1:(counter - 1)), ] <- numeric(0)
      child_values[-(1:(counter - 1))] <- numeric(0)
      child_values_penalty[-(1:(counter - 1))] <- numeric(0)
      child_penalty[-(1:(counter - 1))] <- numeric(0)
      child_parent_index[-(1:(counter - 1))] <- numeric(0)
      members_to_update <- which(candidate_update == 1)
      if (length(members_to_update)) {
        for (i in 1:length(members_to_update)) {
          res <- ssm_beyond(Refset[members_to_update[i], 
          ], candidate[members_to_update[i], ], candidate_values_penalty[members_to_update[i]], 
          fobj, nrand, tolc, weight, x_L, x_U, c_L, 
          c_U, nconst, int_var, bin_var, nfuneval, 
          prob_bound, nvar,...)
          vector <- res[[1]]
          vector_value <- res[[2]]
          vector_penalty <- res[[3]]
          vector_value_penalty <- res[[4]]
          vector_nlc <- res[[5]]
          new_child <- res[[6]]
          new_child_value <- res[[7]]
          new_child_penalty <- res[[8]]
          new_child_value_penalty <- res[[9]]
          new_child_nlc <- res[[10]]
          nfuneval <- res[[11]]
          if (length(vector)) {
            Refset[members_to_update[i], ] <- vector
            Refset_values[members_to_update[i]] <- vector_value
            Refset_values_penalty[members_to_update[i]] <- vector_value_penalty
            penalty[members_to_update[i]] <- vector_penalty
            Refset_nlc[members_to_update[i], ] <- vector_nlc
          } else {
            Refset[members_to_update[i], ] <- candidate[members_to_update[i], 
            ]
            Refset_values[members_to_update[i]] <- candidate_values[members_to_update[i]]
            Refset_values_penalty[members_to_update[i]] <- candidate_values_penalty[members_to_update[i]]
            penalty[members_to_update[i]] <- candidate_penalty[members_to_update[i]]
            Refset_nlc[members_to_update[i], ] <- candidate_nlc[members_to_update[i], 
            ]
          }
        }
      }
      fbest_lastiter = fbest
      ggg <- which(penalty <= tolc)
      if (length(ggg)) {
        fbest_new <- min(Refset_values_penalty[ggg])
        hhh <- which.min(Refset_values_penalty[ggg])
        if (fbest_new < fbest) {
          xbest <- Refset[ggg[hhh], ]
          fbest <- fbest_new
          use_bestx <- 1
          if (inter_save) {
            Results$fbest <- fbest
            Results$xbest <- xbest
            Results$numeval <- nfuneval
            Results$end_crit <- fin
            Results$cpu_time <- cpu_time
            Results$Refset$x <- Refset
            Results$Refset$f <- Refset_values
            Results$Refset$fpen <- Refset_values_penalty
            Results$Refset$const <- Refset_nlc
            Results$Refset$penalty <- penalty
            if (save_results) {
              save(opts, problem, Results, file = "eSSR_report.RData")
            }
          }
        }
      }
      iter <- iter + 1
      n_minimo <- n_minimo + 1
      if (iterprint) {
        cat("Iteration:", iter, "NFunEvals:", nfuneval, 
            "Bestf:", fbest, "CPUTime:", proc.time()[3] - 
              cpu_time, "Mean:", mean(Refset_values_penalty), 
            "\n")
      }
      ddd <- which(members_update > 0)
      eee <- which(!members_update)
      refset_change[ddd] <- refset_change[ddd] + 1
      refset_change[eee] <- 0
      fff <- which(refset_change >= 20)
      if (length(fff)) {
        to_replace <- length(fff)
        replaced <- 0
        while (replaced < to_replace) {
          newsol <- stas::runif(nvar) * (xu_log - xl_log) + 
            xl_log
          newsol[log_var] = exp(newsol[log_var])
          res <- ssm_evalfc(newsol, x_L, x_U, fobj, 
                            nconst, c_L, c_U, tolc, weight, int_var, 
                            bin_var, nvar,...)
          val <- res[[1]]
          val_penalty <- res[[2]]
          pena <- res[[3]]
          nlc <- res[[4]]
          includ <- res[[5]]
          x <- res[[6]]
          nfuneval <- nfuneval + 1
          if (includ) {
            Refset[fff[replaced + 1], ] <- x
            Refset_values[fff[replaced + 1]] <- val
            Refset_values_penalty[fff[replaced + 1]] <- val_penalty
            penalty[fff[replaced + 1]] <- pena
            Refset_nlc[fff[replaced + 1], ] <- nlc
            refset_change[fff[replaced + 1]] <- 0
            replaced <- replaced + 1
          }
        }
      }
      if (fbest < fbest_lastiter) {
        Results$f <- c(Results$f, fbest)
        Results$x <- rbind(Results$x, xbest)
        Results$time <- c(Results$time, proc.time()[3] - 
                            cpu_time)
        Results$neval <- c(Results$neval, nfuneval)
      }
      call_filters <- 0
      if (is.character(local_solver) && n_minimo >= n_critico) {
        call_filters <- 1
        if (local_bestx && !use_bestx) {
          call_filters <- 0
        }
      }
      if (call_filters) {
        if (stage_1 == 1) {
          minimo <- min(Refset_values_penalty)
          I <- which.min(Refset_values_penalty)
          x0 <- Refset[I, ]
          f0 <- minimo
          n_critico <- local_n2
          if (local_bestx) {
            use_bestx <- 0
          }
        } else if (stage_2 == 1) {
          if (local_bestx && use_bestx) {
            x0 <- xbest
            f0 <- fbest
            use_bestx <- 0
            #added by Lars
            I <- I[1]
          } else {
            local_init <- rbind(local_solutions, initial_points)
            xuxl <- x_U - x_L
            child_norm <- child/kronecker(matrix(1, 
                                                 nrow(child), 1), t(xuxl))
            local_init_norm <- local_init/kronecker(matrix(1, 
                                                           nrow(local_init), 1), t(xuxl))
            distance <- eucl_dist(child_norm, local_init_norm)
            temp <- apply(distance, 1, min)
            res <- sort(temp, decreasing = TRUE, index.return = TRUE)
            ooo <- res[[2]]
            res <- sort(child_values_penalty, index.return = TRUE)
            uuu <- res[[2]]
            n_child <- length(child_values_penalty)
            child_points <- rep(0, n_child)
            www <- local_balance
            for (i in 1:n_child) {
              child_points[ooo[i]] <- child_points[ooo[i]] + 
                www * i
              child_points[uuu[i]] <- child_points[uuu[i]] + 
                (1 - www) * i
            }
            I <- which.min(child_points)
            x0 <- child[I, ]
            f0 <- child_values_penalty[I]
          }
        }
        if (iterprint) {
          cat("Call local solver:", toupper(local_solver), 
              "\n")
          
          # #############
          # #
          # ###########
          # 
          # if(length(I) > 2){
          #   return(list(stage_1 = stage_1,
          #               stage_2 = stage_2,
          #           x0 = x0,
          #               f0 = f0,
          #           local_bestx = local_bestx,
          #           use_bestx = use_bestx,
          #     nlc = nlc,
          #     Refset_values_penalty = Refset_values_penalty,
          #               val_penalty =val_penalty,
          #               child_penalty = child_penalty,
          #               I = I
          #   ))
          # }
          # 
          # ##########
          # #
          # #########

          
          if (child_penalty[I] > tolc) {
            cat("Initial point function value:", child_values[I], 
                "(INFEASIBLE)", "\n")
          } else {
            cat("Initial point function value:", f0, 
                "\n")
          }
          tic <- proc.time()[3]
        }
        res <- ssm_localsolver(x0, x_L, x_U, c_L, c_U, 
                               neq, int_var, bin_var, fobj, local_solver, 
                               local_iterprint, local_tol, weight, nconst, 
                               tolc,...)
        x <- res[[1]]
        fval <- res[[2]]
        numeval <- res[[3]]
        initial_points <- rbind(initial_points, x0)
        nfuneval <- nfuneval + numeval
        res <- ssm_evalfc(x, x_L, x_U, fobj, nconst, 
                          c_L, c_U, tolc, weight, int_var, bin_var, 
                          nvar,...)
        val <- res[[1]]
        val_penalty <- res[[2]]
        pena <- res[[3]]
        nlc <- res[[4]]
        includ <- res[[5]]
        x <- res[[6]]
        nfuneval <- nfuneval + 1
        if (includ && pena <= tolc) {
          if (iterprint) {
            cat("Local solution function value:", val, 
                "\n")
          }
          if (stage_1 == 1) {
            stage_1 = 0
            stage_2 = 1
          }
          if (val_penalty < fbest) {
            fbest <- val_penalty
            xbest <- x
            if (fbest < fbest_lastiter) {
              Results$f <- c(Results$f, fbest)
              Results$x <- rbind(Results$x, xbest)
              Results$time <- c(Results$time, proc.time()[3] - 
                                  cpu_time)
              Results$neval <- c(Results$neval, nfuneval)
            }
          }
        }else {
          cat("Local solution function value:", "UNFEASIBLE", 
              "\n")
        }
        cat("Number of function evaluations in the local search:", 
            numeval, "\n")
        cat("CPU Time of the local search:", proc.time()[3] - 
              tic, "seconds \n\n")
        if (nfuneval >= maxeval) {
          fin <- 1
        }else if (proc.time()[3] - cpu_time >= maxtime) {
          fin <- 2
        }else if (length(vtr)) {
          if (fbest <= vtr) {
            fin <- 3
          }
        }
        if (includ) {
          if (length(local_solutions)) {
            adicionar_local <- 1
            res <- ssm_isdif2(x, local_solutions, 0.01, 
                              1)
            f <- res[[1]]
            ind <- res[[2]]
            if (f) {
              adicionar_local <- 0
            }
          }else {
            adicionar_local <- 1
          }
          if (adicionar_local) {
            if (val_penalty < Refset_values_penalty[child_parent_index[I]]) {
              Refset[child_parent_index[I], ] <- x
              Refset_values[child_parent_index[I]] <- val
              Refset_values_penalty[child_parent_index[I]] <- val_penalty
              Refset_nlc[child_parent_index[I], ] <- nlc
              penalty[child_parent_index[I]] <- pena
              refset_change[child_parent_index[I]] <- 0
            }
            local_solutions <- rbind(local_solutions, 
                                     x)
            local_solutions_values <- c(local_solutions_values, 
                                        fval)
          }
        }
        n_minimo <- 0
      }
      fbest_lastiter <- fbest
      if (nfuneval >= maxeval) {
        fin <- 1
      }else if (proc.time()[3] - cpu_time >= maxtime) {
        fin <- 2
      }else if (length(vtr)) {
        if (fbest <= vtr) {
          fin <- 3
        }
      }
      if (fin) {
        cat("\n")
        if (is.character(local_finish) && fin < 3) {
          local_tol = 3
          if (is.numeric(xbest)) {
            x0 = xbest
            f0 = fbest
          }else {
            x0 = Refset[1, ]
            f0 = Refset_values_penalty[1]
          }
          if (iterprint) {
            cat("Final local refinement: ", toupper(local_finish), 
                "\n")
            cat("Initial point function value: ", f0, 
                "\n")
            tic <- proc.time()[3]
          }
          res <- ssm_localsolver(x0, x_L, x_U, c_L, 
                                 c_U, neq, int_var, bin_var, fobj, local_finish, 
                                 local_iterprint, local_tol, weight, nconst, 
                                 tolc,...)
          x <- res[[1]]
          fval <- res[[2]]
          numeval <- res[[3]]
          res <- ssm_evalfc(x, x_L, x_U, fobj, nconst, 
                            c_L, c_U, tolc, weight, int_var, bin_var, 
                            nvar,...)
          val <- res[[1]]
          val_penalty <- res[[2]]
          pena <- res[[3]]
          nlc <- res[[4]]
          includ <- res[[5]]
          x <- res[[6]]
          nfuneval <- nfuneval + 1
          if (includ && pena <= tolc) {
            if (iterprint) {
              cat("Local solution function value:", 
                  val, "\n")
            }
            if (val_penalty < fbest) {
              fbest <- val_penalty
              xbest <- x
            }
          }else {
            cat("Local solution function value:", "UNFEASIBLE", 
                "\n")
          }
          cat("Number of function evaluations in the local search:", 
              numeval, "\n")
          cat("CPU Time of the local search:", proc.time()[3] - 
                tic, "seconds \n\n")
        }
        cpu_time <- proc.time()[3] - cpu_time
        Results$f <- c(Results$f, fbest)
        Results$x <- rbind(Results$x, xbest)
        Results$neval <- c(Results$neval, nfuneval)
        Results$time <- c(Results$time, cpu_time)
        if (iterprint) {
          msg <- switch(fin, `1` = "Maximum number of function evaluations achieved", 
                        `2` = "Maximum computation time achieved", 
                        `3` = "Desired function value achieved")
          cat(msg, "\n")
          cat("Best solution value", fbest, "\n")
          cat("Decision vector", xbest, "\n")
          cat("CPU time", cpu_time, "\n")
          cat("Number of function evaluations", nfuneval, 
              "\n")
        }
        Results$fbest <- fbest
        Results$xbest <- xbest
        Results$numeval <- nfuneval
        Results$end_crit <- fin
        Results$cpu_time <- cpu_time
        Results$Refset$x <- Refset
        Results$Refset$f <- Refset_values
        Results$Refset$fpen <- Refset_values_penalty
        Results$Refset$const <- Refset_nlc
        Results$Refset$penalty <- penalty
        if (save_results) {
          save(opts, problem, Results, file = "eSSR_report.RData")
        }
        return(Results)
      }
    }
  }
}
