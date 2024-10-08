
K <- function(v)
{
  return(gamma((v+1) / 2) / ( sqrt(pi*v) * gamma(v/2) ))
}

B_Ast <-function(nu1,nu2, alpha)
{
  return(alpha*K(nu1)+(1-alpha)*K(nu2))
}

Alpha_star <- function(nu1, nu2, alpha)
{
  return(alpha*K(nu1)/ (alpha*K(nu1)+(1-alpha)*K(nu2)))
}

E_Y_Ast <- function(nu1, nu2, alpha)
{
  As = Alpha_star(nu1, nu2, alpha)
  
  return (4*B_Ast( (-As*As*nu1/(nu1-1))  +  ((1-As)*(1-As)*nu2/(nu2-1)) ))
}

Variance_Y_Ast <- function(nu1, nu2, alpha)
{
  As = Alpha_star(nu1, nu2, alpha)
  w = E_Y_Ast(nu1,nu2,alpha)
  
  s = 4*( ( (alpha*As*As*nu1)/(nu1-2) ) + ((1-alpha)*(1-As)*(1-As)*nu2) / (nu2-2)  )  - (w*w)
  
  return(s)
}

LL_Ast_Garch <- function(m, b0, b1, b2, c , nu1, nu2, alpha)
{
  
 omega = E_Y_Ast(nu1, nu2, alpha)
  
 delta = sqrt(Variance_Y_Ast(nu1, nu2, alpha))
 
 log(delta) - log(sigma) + log(dast(omega + delta * (x-m)/sigma , nu1=nu1, nu2 = nu2, alpha)) 
}