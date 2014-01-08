#!/bin/bash
# mortgage payment calculator
#
#### http://www.jeacle.ie/mortgage/instructions.html
#
# First you must define some variables to make it easier to set up:
#
# P = principal, the initial amount of the loan
#
# I = the annual interest rate (from 1 to 100 percent)
#
# L = length, the length (in years) of the loan, or at least the length over
# which the loan is amortized.
#
# The following assumes a typical conventional loan where the interest is
# compounded monthly. First I will define two more variables to make the
# calculations easier:
#
# J = monthly interest in decimal form = I / (12 x 100)
#
# N = number of months over which loan is amortized = L x 12
# 
# Okay now for the big monthly payment (M) formula, it is:
# 
#                               J
#          M  =  P  x ------------------------
#                       1  - ( 1 + J ) ^ -N
# 
#  where 1 is the number one (it does not appear too clearly on some browsers)
#
# So to calculate it, you would first calculate 1 + J then take that
# to the -N (minus N) power, subtract that from the number 1. Now take
# the inverse of that (if you have a 1/X button on your calculator
# push that). Then multiply the result times J and then times P.
# Sorry, for the long way of explaining it, but I just wanted to be
# clear for everybody. 
#
# The one-liner for a program would be (adjust for your favorite
# language):
# 
#          M = P * ( J / (1 - (1 + J) ** -N))
#
# So now you should be able to calculate the monthly payment, M.
#
# To calculate the amortization table you need to do some iteration
# (i.e. a simple loop). I will tell you the simple steps:
#
# Step 1: Calculate H = P x J, this is your current monthly interest
#
# Step 2: Calculate C = M - H, this is your monthly payment minus your
# monthly interest, so it is the amount of principal you pay for that
# month
#
# Step 3: Calculate Q = P - C, this is the new balance of your
# principal of your loan.
#
# Step 4: Set P equal to Q and go back to Step 1: You thusly loop
# around until the value Q (and hence P) goes to zero.
# 

export PATH=$PATH:.:$HOME/lib
source real-utils.sh

# input variables
P='172000.00'           # principle of the loan in dollars
L=15                    # length of the loan in years
I='5.5'                 # annual interest rate on the loan

# derived variables
n=$(( 12 * L ))                 # number of payments
j=`real_eval "$I/1200" 10`      # monthly interest rate

# Now, compute the monthly payment, in dollars
M=`real_eval "$P * ( $j / (1 - (1 + $j) ^ -$n))" 10`
M=`round $M 2`                  # round to the nearest penny

printf "Total Principal: %10.2f\n" "$P"
printf "Monthly Payment: %10.2f\n\n" "$M"
printf "%-8s  %-8s  %-10s  %-10s\n" 'Payment' 'Interest' 'Principal' 'Balance'

ti='0.0' p="$P"
for ((k=0; k<n; k++)) do
  h=`real_eval "round($p * $j,2)"`
  ti=`real_eval "$ti + $h" 2`           # accumulate interest
  c=`real_eval "$M - $h" 2`
  p=`real_eval "$p - $c" 2`             # decreasing principal
  printf "  %4d  %8.2f   %10.2f  %10.2f\n" $k "$h" "$c" "$p"
done
echo ''
printf "Total Principal: %10.2f\n" "$P"
printf "Monthly Payment: %10.2f\n" "$M"
printf " Total Interest: %10.2f\n" "$ti"
printf "   Total Amount: %10.2f\n" `real_eval "$P + $ti" 2`
printf "    Total Int %%: %10.1f%%\n" `real_eval "100*($ti/($P+$ti))" 2`
exit
