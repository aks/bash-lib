#! /bin/sh

# http://www.jeacle.ie/mortgage/instructions.html

awk 'BEGIN{
  n = 180
  p = 172000.0
  i = 5.5
  j = i / 1200
  m = p * ( j / (1 - (1 + j) ^ -n))
  # computation in pennies
  m = int(100*m)
  p = int(100*p)
  printf "Total P+I: %5d.%02d\n\n",int(m/100),m%100
  printf "Payment\t Interest\t Principal\tBalance\n"
  for (k=0; n>k; k++){
    h = int(p*j + 0.5)
    c = m - h
    p = p - c
    printf "%d\t%5d.%02d\t%5d.%02d\t%6d.%02d\n",
           k,int(h/100),h%100,int(c/100),c%100,int(p/100),p%100
  }
}'

