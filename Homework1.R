# Jonathon Ferreira
# MTH499
# Data Imports
library(UsingR);
data(father.son);

# Plot Scatterplot
plot(father.son$fheight, father.son$sheight, main="Pearson's Father-Son Regression", xlab="Father's height (in)", ylab="Son's height (in)", xlim=c(58,80), ylim=c(58,80), pch=20);

# Plot Regression Line
abline(lm(father.son$sheight~father.son$fheight), col="red", lwd=3);

# Calculate Sample X&Y Mean
xMean <- mean(father.son$fheight);
yMean <- mean(father.son$sheight);

# Calculate SD Line Slope $ Int
b <- 1.025441;
a <- (yMean-(xMean*b));

# Plot SD Line
abline(a=a,b=b,col="blue", lty=2, lwd=3);

# Plot Center of Regression
points(xMean, yMean, col="green");
abline(v=xMean, col="green", lty=3, lwd=3);
abline(h=yMean, col="green", lty=3, lwd=3);

# Plot Graph Legend
legend("bottomright", 
  legend = c("Regression Line", "SD Line", "Center of Regression"), 
  col = c("red", "blue", "green"), 
  lty=1:3, cex=0.8);

# Output Regression Summary
summary(lm(father.son$sheight~father.son$fheight))