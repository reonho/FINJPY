
# Quadratic Programming
# Long Only Markowitz
param nAssets;
param mu{1..nAssets};
param Sigma{1..nAssets, 1..nAssets};
param targetReturn;
var x{1..nAssets} >= 0;
minimize Risk: sum {i in 1..nAssets} sum{j in 1..nAssets} x[i]*Sigma[i,j]*x[j];
subject to Return: sum{i in 1..nAssets} mu[i]*x[i] = targetReturn;
subject to Budget: sum{i in 1..nAssets} x[i] = 1;
