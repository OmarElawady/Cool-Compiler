#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"
#define TRUE 1
#define FALSE 0
class ClassEntry{
private:
  Symbol name;
  int in;
public:
  int get_index(){return in;};
  Symbol get_name(){return name;};
  ClassEntry(Symbol name, int index):name(name), in(index){}
};

class ClassTable;
typedef ClassTable *ClassTableP;

class Graph{ 
  typedef List<int> Neighbors;
  typedef Neighbors** AdjacencyList;
  private:
    int n;
    AdjacencyList adj;
    int* parent;
    int* depth;
    int root;
    void dfs(int u);
    void print_tree(int u, ClassTable* ct, int in);
    Classes get_classes_dfs(int u, ClassTable* ct);
  public:
    Graph(int n);
    void set_root(int u);
    void add_edge(int u, int v);
    bool is_parent(int u, int v);
    int lca(int u, int v);
    void construct();
    List<int>* cycle(int u);
    void print_tree(ClassTable* ct);
    Classes get_classes_in_order(ClassTable* ct);
};
 

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.
class ClassTable {
friend class Graph;
typedef List<ClassEntry> SymId;
typedef List<int> Neighbors;
typedef List<Neighbors> AdhacenyList;
protected:
  Symbol get_name(int index);
private:
  int semant_errors;

  void install_basic_classes();
  void construct_classes_graph();
  int get_index(Symbol);
  void indexify_classes();
  void assert_classes_recorded();
  void add_class_entry(Symbol name, int index);
  ostream& error_stream;

  Classes classes;
  Graph graph;
  SymId* symId;
public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
  bool is_parent(Class_ parent, Class_ child);
  bool is_parent(Symbol parent, Symbol child);
  Symbol lca(Symbol c1, Symbol c2);
  Class_ get_class(Symbol name);
  Classes get_classes();
  Classes get_classes_in_order();
  bool exists(Symbol class_name);
  void report_class_not_found(Symbol class_name, int line_number);
  void dump_out();
  ~ClassTable();
};
typedef List<Entry> ClassList;
class MethodEntry{
  private:
    Symbol class_name;
    Symbol method_name;
    ClassList* signature;
  public:
    MethodEntry(Symbol c, Symbol n, ClassList* s);
    Symbol get_method_name();
    Symbol get_class_name();
    ClassList* get_signature();
};

typedef List<MethodEntry> MethodEnvironment;

class Environment{
  private:
   Symbol class_name;
   ClassTable* ct;
   MethodEnvironment* methods;
   SymbolTable<Symbol, Entry> *symbolTable;
   void build_method_environment();
   void add_class_methods(class__class* c);
   void add_method_entry(Symbol class_name, method_class* method);
   ClassList* extract_signature(method_class* method);
   bool lists_equal(ClassList* cs1, ClassList* cs2);
  public:
    Environment(ClassTable* ct);
    ClassList* get_signature(Symbol class_name, Symbol method_name);
    ClassList* get_signature_in_class(Symbol class_name, Symbol method_name);
    Environment* enter_scope();
    Environment* add_symtab_entry(Symbol name, Symbol type);
    Environment* set_current_class(Symbol class_name);
    Environment* exit_scope();
    Symbol get_current_class();
    Symbol get_type(Symbol obj);
    Symbol probe(Symbol e);
};
class SemanticAnalyzer{
   private:
     ClassTable* ct;
     Environment* env;
     void annotate_classes(Classes cs);
     void annotate_class(Class_ c);
     void extract_attrs();
     void add_attr(attr_class* attr);
     void add_formals(Formals formals);
     void annotate_method(method_class* method);
     template <class T>
     int find(List<T>* lst, T* elem);
     Symbol annotate_expression(Expression exp);
     Symbol annotate_assign_expression(assign_class* e);
     Symbol annotate_static_dispatch_expression(static_dispatch_class* e);
     Symbol annotate_dispatch_expression(dispatch_class* e);
     Symbol annotate_conditional_expression(cond_class* e);
     Symbol annotate_loop_expression(loop_class* e);
     Symbol annotate_block_expression(block_class* e);
     Symbol annotate_let_expression(let_class* e);
     Symbol annotate_typcase_expression(typcase_class* e);
     Symbol annotate_new_expression(new__class* e);
     Symbol annotate_isvoid_expression(isvoid_class* e);
     Symbol annotate_add_expression(plus_class* e);
     Symbol annotate_sub_expression(sub_class* e);
     Symbol annotate_mul_expression(mul_class* e);
     Symbol annotate_divide_expression(divide_class* e);
     Symbol annotate_neg_expression(neg_class* e);
     Symbol annotate_lt_expression(lt_class* e);
     Symbol annotate_leq_expression(leq_class* e);
     Symbol annotate_eq_expression(eq_class* e);
     Symbol annotate_comp_expression(comp_class* e);
     Symbol annotate_object_expression(object_class* e);
     Symbol annotate_int_expression(int_const_class* e);
     Symbol annotate_string_expression(string_const_class* e);
     Symbol annotate_bool_expression(bool_const_class* e);
     Symbol annotate_noexpr_expression(no_expr_class* e); 

  public:
    SemanticAnalyzer(ClassTable* c, Environment* e);     
};
#endif


