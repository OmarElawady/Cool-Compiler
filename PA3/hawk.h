#ifndef _H_GRAPH
#define _H_GRAPH
template<class T>
class List{
public:
  List(){}
  List(int*){}
};
class Graph{
  typedef List<int> Neighbors;
  typedef Neighbors** AdjacencyList;

  private:
    AdjacencyList adj;
  public:
    Graph(int n);
};

#endif
