#include "test.h"

Graph::Graph(int n){
  this -> adj = new Neighbors*[n];
  for(int i = 0;i < n;i++)
    adj[i] = new Neighbors((int*) 0);
}

