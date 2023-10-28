#include<stdio.h>

int main () {
  int x = 3;
  int y = x++ + x++ + x++ + x++;
  printf("%i\n",y);
}
