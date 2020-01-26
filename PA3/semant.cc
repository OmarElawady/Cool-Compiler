

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}

// Graph implementation
Graph::Graph(int n) : n(n), root(-1){
  this -> adj = new Neighbors*[n];
  for(int i = 0;i < n;i++)
    adj[i] = new Neighbors((int*) 0);
  depth = new int[n];
  parent = new int[n];
  for(int i = 0;i < n;i++)
    depth[i] = 0;
}
void Graph::set_root(int id){root = id;}
void Graph::add_edge(int u, int v){
  adj[u] = new Neighbors(new int(v), adj[u]);
  parent[v] = u;
}
bool Graph::is_parent(int p, int c){
  while(depth[p] < depth[c])
    c = parent[c];
  return p == c;
}
int Graph::lca(int u, int v){
  while(depth[u] > depth[v])
    u = parent[u];
  while(depth[v] > depth[u])
    v = parent[v];
  while(u != v)
    u = parent[u], v = parent[v];
  return u;
}
void Graph::dfs(int u){
  if(u == root)
    depth[u] = 1;
 
  for(Neighbors* i = adj[u];i -> hd();i = i -> tl()){
    int v = *(i -> hd());
    if(depth[v]){
      throw cycle(v);
    }
    depth[v] = depth[u] + 1;;
    dfs(v);
  }
}
Classes Graph::get_classes_dfs(int u, ClassTable* ct){
	Classes res = single_Classes(ct -> get_class(ct -> get_name(u)));
  for(Neighbors* i = adj[u];i -> hd();i = i -> tl()){
    int v = *(i -> hd());
    res = append_Classes(res, get_classes_dfs(v, ct));
  }
  return res;
}
Classes Graph::get_classes_in_order(ClassTable* ct){
  return get_classes_dfs(root, ct);
}

void Graph::construct(){
  //for(int i = 0;i < n;i++)
	//	cout << parent[i] << endl;
  dfs(root);
  for(int i = 0;i < n;i++)
    if(depth[i] == 0)
			dfs(i);
}
List<int>* Graph::cycle(int v){
	List<int>* lst = new List<int>(new int(v));
	int u = parent[v];
	while(u != v){
		lst = new List<int>(new int(u), lst);
		u = parent[u];
	}
	return lst;
}
void Graph::print_tree(int u, ClassTable* ct, int in){
  cout << pad(in) << ct -> get_name(u) -> get_string() << endl;
  for(Neighbors* i = adj[u];i -> hd();i = i -> tl())
    print_tree(*(i -> hd()), ct, in + 2);
}
void Graph::print_tree(ClassTable* ct){
  print_tree(root, ct, 0);
}

ClassTable::ClassTable(Classes classes_) : semant_errors(0) , error_stream(cerr), classes(classes_), graph(classes -> len() + 5), symId(new SymId((ClassEntry*) NULL)) {
	install_basic_classes();
	assert_classes_recorded();
  indexify_classes();
  if(semant_errors)
		return;
	construct_classes_graph();
  if(get_class(idtable.lookup_string("Main")) == NULL)
    semant_error() << "Class Main is not defined.\n";
  //dump_out();
}
void ClassTable::assert_classes_recorded(){
	for(int i = classes -> first();classes -> more(i);i = classes -> next(i)){
		class__class* current = (class__class*) (classes -> nth(i));
    if(current -> get_name() != idtable.lookup_string("Object") && get_class(current -> get_parent()) == NULL){
			semant_error(current) << "Class " << current -> get_parent() -> get_string() << " is not defined\n";
		}
	}
}
void ClassTable::indexify_classes(){
  bool rooted = false;
	for(int i = classes -> first();classes -> more(i);i = classes -> next(i)){
		class__class* current = (class__class*) (classes -> nth(i));
    //cout << current -> get_name() << endl;
		if(current -> get_name() == SELF_TYPE || get_index(current -> get_name()) != -1){
			semant_error((Class_)current) << "Multiple definitions of class " << current -> get_name() -> get_string() << endl;
			continue;
		}
    if(current -> get_parent() == Bool || current -> get_parent() == Int || current -> get_parent() == Str){
      semant_error(current) << "Can't inherit from class " << current -> get_parent() -> get_string() << endl;
			continue;
    }
		add_class_entry(current -> get_name(), i);
		if(current -> get_name() == idtable.lookup_string("Object"))
			graph.set_root(i), rooted = true;
	}
	if(!rooted){
		semant_error() << curr_filename <<  ":1: Object class is not defined\n";
	}
}
void ClassTable::add_class_entry(Symbol name, int index){
  symId = new SymId(new ClassEntry(name, index), symId); 
}
void ClassTable::construct_classes_graph(){
	for(int i = classes -> first();classes -> more(i);i = classes -> next(i)){
		class__class* current = (class__class*) (classes -> nth(i));
		//cout << current -> get_parent() -> get_string() << ' ' << current -> get_name() -> get_string() << endl;
		if(current -> get_name() != idtable.lookup_string("Object"))
    	graph.add_edge(get_index(current -> get_parent()), get_index(current -> get_name()));
	}
  try{
    graph.construct();
  }catch(List<int>* cyc){
    ostream& out = semant_error(get_class(get_name(*cyc -> hd())));
		out << "Classes: ";
		for(;cyc;cyc = cyc -> tl()){
			out << get_name(*cyc -> hd()) << ' ';
		}
		out << "forms a cycle\n";
  }
}
Symbol ClassTable::get_name(int index){
  SymId* cp = symId;
  for(ClassEntry* ce = cp-> hd();ce;cp = cp -> tl(), ce = cp -> hd()){
		if(ce -> get_index() == index)
			return ce -> get_name();
	} 
  return NULL; 
}
Class_ ClassTable::get_class(Symbol name){
	for(int i = classes -> first();classes -> more(i);i = classes -> next(i)){
		class__class* current = (class__class*) classes -> nth(i);
    if(current -> get_name() == name)
			return current;
  }
	return NULL;
}
int ClassTable::get_index(Symbol name){
  SymId* cp = symId;
  for(ClassEntry* ce = cp-> hd();ce;cp = cp -> tl(), ce = cp -> hd()){
		if(ce -> get_name() == name)
			return ce -> get_index();
	} 
  return -1; 

}
bool ClassTable::is_parent(Class_ p, Class_ c){
  return graph.is_parent(get_index(((class__class*)p) -> get_name()), get_index(((class__class*)c) -> get_name()));
}
bool ClassTable::is_parent(Symbol p, Symbol c){
  return is_parent(get_class(p), get_class(c));
}

bool ClassTable::exists(Symbol class_name){
	return get_class(class_name) != NULL;
}

void ClassTable::report_class_not_found(Symbol class_name, int line_number){
  semant_error(get_class(class_name)) << "Class " << class_name << " not declared.\n";
}

Symbol ClassTable::lca(Symbol c1, Symbol c2){
  return get_name(graph.lca(get_index(c1), get_index(c2)));
}
void ClassTable::dump_out(){
  graph.print_tree(this);
}
void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);
  classes = append_Classes(single_Classes(Object_class), classes);
  classes = append_Classes(single_Classes(Str_class), classes);
  classes = append_Classes(single_Classes(IO_class), classes);
  classes = append_Classes(single_Classes(Int_class), classes);
  classes = append_Classes(single_Classes(Bool_class), classes);
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
}

Classes ClassTable::get_classes(){return classes;}
Classes ClassTable::get_classes_in_order(){
  return graph.get_classes_in_order(this);
}
// MethodEntry implementation

MethodEntry::MethodEntry(Symbol c, Symbol n, ClassList* s): class_name(c), method_name(n), signature(s){}
Symbol MethodEntry::get_class_name(){return class_name;}
Symbol MethodEntry::get_method_name(){return method_name;}
ClassList* MethodEntry::get_signature(){return signature;}

// Environment implementation

Environment::Environment(ClassTable* ct): ct(ct), methods(new List<MethodEntry>((MethodEntry*)NULL)), symbolTable(new SymbolTable<Symbol, Entry>()){
  build_method_environment();
}
ClassList* Environment::get_signature_in_class(Symbol class_name, Symbol method_name){
  for(MethodEnvironment* mv = methods;mv -> hd();mv = mv -> tl()){
    MethodEntry* me = mv -> hd();
    if(me -> get_method_name() == method_name && me -> get_class_name() == class_name)
      return me -> get_signature();
  }
  return NULL;
}
ClassList* Environment::get_signature(Symbol class_name, Symbol method_name){
  ClassList* try_this_class = get_signature_in_class(class_name, method_name);
  if(try_this_class != NULL)
    return try_this_class;
  else if(class_name == Object)
    return NULL;
  else{
    return get_signature(((class__class*)ct -> get_class(class_name)) -> get_parent(), method_name);
	}
}

void Environment::build_method_environment(){
  Classes classes = ct -> get_classes_in_order();
  for(int i = classes -> first();classes -> more(i);i = classes -> next(i)){
    class__class* current = (class__class*) classes -> nth(i);
    add_class_methods(current);
  } 
}
void Environment::add_class_methods(class__class* c){
  Features fs = c -> get_features();
  for(int i = fs -> first();fs -> more(i);i = fs -> next(i)){
    Feature f = fs -> nth(i);
    if(f -> get_constructor() == F_METHOD){
			Symbol f_name = ((method_class*)f) -> get_name();
			ClassList* signature_in_class = get_signature_in_class(c -> get_name(), f_name);
		  if(signature_in_class != NULL)
				ct -> semant_error(idtable.add_string(curr_filename), f) << "Redefinition of method " << f_name -> get_string() << endl;
			else
      	add_method_entry(c -> get_name(), ((method_class*)f));
		}
  }
}
void Environment::add_method_entry(Symbol class_name, method_class* mc){
  ClassList* signature = get_signature(class_name, mc -> get_name());
  ClassList* cs = extract_signature(mc);
  //cout << class_name << ' ' << mc -> get_name() << ' ' << cs << ' ' << signature << endl;
  if(signature != NULL && !lists_equal(cs, signature))
    ct -> semant_error(idtable.add_string(curr_filename), mc) << "Redifinition of method " << mc -> get_name() -> get_string() << " With different signature" << endl;
	else
  	methods = new List<MethodEntry>(new MethodEntry(class_name, mc -> get_name(), extract_signature(mc)), methods); 
}

bool Environment::lists_equal(ClassList* cs1, ClassList* cs2){
  
  for(;cs1 || cs2;cs1 = cs1 -> tl(), cs2 = cs2 -> tl()){
    if(!cs1 || !cs2 || cs1 -> hd() != cs2 -> hd())
      return false;
 	}
  return true;

}

ClassList* Environment::extract_signature(method_class* mc){
  ClassList* cs = new List<Entry>((Symbol)NULL);

  Formals fs = mc -> get_formals();
  for(int i = fs -> first();fs -> more(i);i = fs -> next(i)){
    formal_class* f = (formal_class*)fs -> nth(i);

		if(!ct -> exists(f -> get_type_decl())){
			ct -> semant_error(idtable.add_string(curr_filename), mc) << "class " << f -> get_type_decl() << " is not declared.\n";
			cs = new List<Entry>(Object, cs);
		}else{
      cs = new List<Entry>(f -> get_type_decl(), cs);
		}
  }
  ClassList* rev = new List<Entry>((Symbol)NULL);
  for(ClassList* cp = cs;cp -> hd();cp = cp -> tl()){
    rev = new List<Entry>(cp -> hd(), rev);
  }
  rev = new List<Entry>(mc -> get_return_type(), rev);
  return rev;
}
Environment* Environment::enter_scope(){
  symbolTable -> enterscope();
  return this;
}
Environment* Environment::add_symtab_entry(Symbol name, Symbol type){
  symbolTable -> addid(name, type);
  return this;
}
Environment* Environment::set_current_class(Symbol c){
  class_name = c;
  return this;
}
Environment* Environment::exit_scope(){
  symbolTable -> exitscope();
  return this;
}
Symbol Environment::get_current_class(){
  return class_name;
}
Symbol Environment::get_type(Symbol obj){
  return symbolTable -> lookup(obj);
}

Symbol Environment::probe(Symbol e){
  return symbolTable -> probe(e);
}
/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
   curr_filename = classtable -> get_class(idtable.lookup_string("Main")) -> get_filename() -> get_string();
    Environment* env = new Environment(classtable);
    /* some semantic analysis code may go here */

   SemanticAnalyzer sa(classtable, env);
    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}
SemanticAnalyzer::SemanticAnalyzer(ClassTable* c, Environment* e): ct(c), env(e){
  annotate_classes(ct -> get_classes_in_order());
}
void SemanticAnalyzer::annotate_classes(Classes cs){
  Symbol current = No_class;
  for(int i = cs -> first();cs -> more(i);i = cs -> next(i)){
    class__class* next = (class__class*)cs -> nth(i);


    while(current != next -> get_parent()){
      env -> exit_scope();
			current = ((class__class*)ct -> get_class(current)) -> get_parent();
      //cout << current << endl;
    }
    env -> enter_scope();
    if(next -> get_name() != Object && next -> get_name() != Bool && next -> get_name() != Int && next -> get_name() != IO && next -> get_name() != Str)
    	annotate_class(cs -> nth(i));
		current = next -> get_name();
	}
}
void SemanticAnalyzer::annotate_class(Class_ c){
  class__class* cl = (class__class*) c;
	env -> set_current_class(cl -> get_name());
  Features fs = cl -> get_features();
  for(int i = fs -> first();fs -> more(i);i = fs -> next(i)){
    Feature f = fs -> nth(i);
    if(f -> get_constructor() == F_ATTR)
      add_attr((attr_class*)f);
  }
  for(int i = fs -> first();fs -> more(i);i = fs -> next(i)){
    Feature f = fs -> nth(i);
    if(f -> get_constructor() == F_METHOD)
      annotate_method((method_class*)f);
  }
}
void SemanticAnalyzer::add_attr(attr_class* attr){
	Expression init = attr -> get_init();
	Symbol type = annotate_expression(init);
  if(type == SELF_TYPE)
		 type = env -> get_current_class();
  if(type != No_type && !ct -> is_parent(attr -> get_type_decl(), type))
		ct -> semant_error(idtable.add_string(curr_filename), attr) << "Type conformance error in assignment expression.\n";

  if(attr -> get_name() == self)
		ct -> semant_error(idtable.add_string(curr_filename), attr) << "Can't name an attribute self\n";
	else if(env -> get_type(attr -> get_name()) != NULL)
		ct -> semant_error(idtable.add_string(curr_filename), attr) << "Overriding preciously defined attribute\n";
  else
		env -> add_symtab_entry(attr -> get_name(), attr -> get_type_decl());
}
void SemanticAnalyzer::annotate_method(method_class* method){
	Expression expr = method -> get_expr();
	Symbol return_type = method -> get_return_type();

	Formals formals = method -> get_formals();
  env -> enter_scope();
	add_formals(formals);
	Symbol expr_type = annotate_expression(expr);
  //cout << return_type << ' ' << expr_type << endl;
  if(return_type != SELF_TYPE && ct -> get_class(return_type) == NULL){
		ct -> semant_error(idtable.add_string(curr_filename), method) << "Class " << return_type << " is not defined\n";
  }else if(return_type != expr_type && (return_type == SELF_TYPE) || return_type != SELF_TYPE && expr_type != SELF_TYPE && !ct -> is_parent(return_type, expr_type)){
		ct -> semant_error(idtable.add_string(curr_filename), method) << "Non conforming return type\n";
  }
  env -> exit_scope();
}

void SemanticAnalyzer::add_formals(Formals formals){
  for(int i = formals -> first();formals -> more(i);i = formals -> next(i)){
    formal_class* f = (formal_class*)formals -> nth(i);
    Symbol name = f -> get_name();
    Symbol type = f -> get_type_decl();
    if(name == self)
			ct -> semant_error(idtable.add_string(curr_filename), f) << "Can't name a parameter self\n";
		else if(env -> probe(name) != NULL)
			ct -> semant_error(idtable.add_string(curr_filename), f) << "Redifinition of a parameter\n";	
		else
    	env -> add_symtab_entry(name, type);
  }
}

Symbol SemanticAnalyzer::annotate_expression(Expression e){
	//cout << "In annotating expression: " << e -> get_constructor() << endl;
  switch(e -> get_constructor()){
		case E_NO_EXPR:
			return annotate_noexpr_expression((no_expr_class*)e);
		case E_ASSIGN:
			return annotate_assign_expression((assign_class*)e);
		case E_STATIC_DISPATCH:
			return annotate_static_dispatch_expression((static_dispatch_class*)e);
		case E_DISPATCH:
			return annotate_dispatch_expression((dispatch_class*)e);
		case E_CONDITIONAL:
			return annotate_conditional_expression((cond_class*)e);
		case E_LOOP:
			return annotate_loop_expression((loop_class*)e);
		case E_BLOCK:
			return annotate_block_expression((block_class*)e);
		case E_LET:
			return annotate_let_expression((let_class*)e);
		case E_TYPCASE:
			return annotate_typcase_expression((typcase_class*)e);
		case E_NEW:
			return annotate_new_expression((new__class*)e);
		case E_ISVOID:
			return annotate_isvoid_expression((isvoid_class*)e);
		case E_ADD:
			return annotate_add_expression((plus_class*)e);
		case E_SUB:
			return annotate_sub_expression((sub_class*)e);
		case E_MUL:
			return annotate_mul_expression((mul_class*)e);
		case E_DIVIDE:
			return annotate_divide_expression((divide_class*)e);
		case E_NEG:
			return annotate_neg_expression((neg_class*)e); 
		case E_LT:
			return annotate_lt_expression((lt_class*)e);
		case E_LEQ:
			return annotate_leq_expression((leq_class*)e);
		case E_EQ:
			return annotate_eq_expression((eq_class*)e);
		case E_COMP:
			return annotate_comp_expression((comp_class*)e);
		case E_OBJECT:
			return annotate_object_expression((object_class*)e);
		case E_INT:
			return annotate_int_expression((int_const_class*)e);
		case E_STRING:
			return annotate_string_expression((string_const_class*)e);
		case E_BOOL:
			return annotate_bool_expression((bool_const_class*)e);
		default:
			assert(false);
	}

}

Symbol SemanticAnalyzer::annotate_assign_expression(assign_class* e){
	Expression expr = e -> get_expr();
	Symbol id    = e -> get_name();
  if(id == self)
		ct -> semant_error(idtable.add_string(curr_filename), e) << "Can't assign to self.\n";
 
  Symbol id_type;
  if(id == self)
    id_type = Object;
  else
    id_type = env -> get_type(id);
  Symbol expr_type = annotate_expression(expr);
  if(expr_type == SELF_TYPE)
		ct -> semant_error(idtable.add_string(curr_filename), e) << "Can't assign self type.\n"; 
  else if(!ct -> is_parent(id_type, expr_type))
		ct -> semant_error(idtable.add_string(curr_filename), e) << "Type conformance error in assignment expression.\n";
  e -> set_type(expr_type);
	return expr_type;
}

Symbol SemanticAnalyzer::annotate_static_dispatch_expression(static_dispatch_class* e){
  Expression expr = e -> get_expr();
  Symbol name = e -> get_name();
	Symbol type_name = e -> get_type_name();
  Expressions actual = e -> get_actual();
	Symbol expr_type = annotate_expression(expr);
  if(expr_type == SELF_TYPE)
    expr_type = env -> get_current_class();
	if(!ct -> is_parent(type_name, expr_type)){
		ct -> semant_error(idtable.add_string(curr_filename), e) << "Type conformance error\n";
	}
	ClassList* signature = env -> get_signature(type_name, name);	
	if(signature == NULL){
    ct -> semant_error(idtable.add_string(curr_filename), e) << "can't find method " << name << endl;
		e -> set_type(Object);
		return Object;
  }
	Symbol return_type = signature -> hd();
	signature = signature -> tl();
	if(!ct -> exists(return_type) && return_type != SELF_TYPE){
		ct -> report_class_not_found(return_type, e -> get_line_number());
		e -> set_type(Object);
		return Object;
	}
  for(int i = actual -> first();actual -> more(i);i = actual -> next(i)){
		if(signature == NULL){
      ct -> semant_error(idtable.add_string(curr_filename), expr) << "number of arguments is different from the number of parameters";
			e -> set_type(Object);
			return Object;
    }
    Expression arg = actual -> nth(i);
    Symbol arg_type = annotate_expression(arg);
		Symbol param_type = signature -> hd();
		signature = signature -> tl();
		if(!ct -> is_parent(param_type, arg_type)){
			ct -> semant_error(idtable.add_string(curr_filename), expr) << "argument type doesn't conform to the parameter type.\n";
		}
  }

	e -> set_type(return_type);
  return return_type;	
}

Symbol SemanticAnalyzer::annotate_dispatch_expression(dispatch_class* e){
  Expression expr = e -> get_expr();
  Symbol name = e -> get_name();
  Expressions actual = e -> get_actual();
	Symbol expr_type = annotate_expression(expr);
	//cout << expr_type << ' ' << name << endl;
	ClassList* signature;
  if(expr_type != SELF_TYPE)
		signature = env -> get_signature(expr_type, name);	
	else
		signature = env -> get_signature(env -> get_current_class(), name);
	//return Object;
	if(signature == NULL){
    ct -> semant_error(idtable.add_string(curr_filename), e) << "can't find method " << name << endl;
		e -> set_type(Object);
		return Object;
  }
	Symbol return_type = signature -> hd();
	signature = signature -> tl();
	if(!ct -> exists(return_type) && return_type != SELF_TYPE){
		ct -> report_class_not_found(return_type, e -> get_line_number());
		e -> set_type(Object);
		return Object;
	}

  for(int i = actual -> first();actual -> more(i);i = actual -> next(i)){
		if(signature == NULL){
      ct -> semant_error(idtable.add_string(curr_filename), expr) << "number of arguments is different from the number of parameters\n";
			e -> set_type(Object);
			return Object;
    }
    Expression arg = actual -> nth(i);
    Symbol arg_type = annotate_expression(arg);
    if(arg_type == SELF_TYPE)
			arg_type = env -> get_current_class();
		Symbol param_type = signature -> hd();
		signature = signature -> tl();
		if(!ct -> is_parent(param_type, arg_type)){
			ct -> semant_error(idtable.add_string(curr_filename), expr) << "argument type doesn't conform to the parameter type.\n";
		}
  }
  if(return_type == SELF_TYPE)
    return_type = expr_type;

	e -> set_type(return_type);
	//cout << (return_type) << endl;
	return return_type;
}

Symbol SemanticAnalyzer::annotate_conditional_expression(cond_class* e){
	Expression pred = e -> get_pred();
  Expression then_exp = e -> get_then_exp();
  Expression else_exp = e -> get_else_exp();
  Symbol pred_type = annotate_expression(pred);
	Symbol then_exp_type = annotate_expression(then_exp);
	Symbol else_exp_type = annotate_expression(else_exp);
	if(pred_type != idtable.lookup_string("Bool")){
    ct -> semant_error(idtable.add_string(curr_filename), pred) << "The type of the predicate is not Bool.\n"; 
  }
  if(else_exp_type == SELF_TYPE)
    else_exp_type = env -> get_current_class();
  if(then_exp_type == SELF_TYPE)
    then_exp_type = env -> get_current_class();
  Symbol type = ct -> lca(then_exp_type, else_exp_type);
  e -> set_type(type);
  return type;
}

Symbol SemanticAnalyzer::annotate_loop_expression(loop_class* e){
	Expression pred = e -> get_pred();
  Expression body = e -> get_body();
  Symbol pred_type = annotate_expression(pred);
  annotate_expression(body);
  if(pred_type != idtable.lookup_string("Bool")){
    ct -> semant_error(idtable.add_string(curr_filename), pred) << "The type of the predicate is not Bool.\n";
	}
	Symbol type = idtable.lookup_string("Object");
  e -> set_type(type);
	return type;
}

Symbol SemanticAnalyzer::annotate_block_expression(block_class* e){
	Expressions body = e -> get_body();
  Symbol type;
  for(int i = body -> first();body -> more(i);i = body -> next(i)){
		Expression current_e = body -> nth(i);
    type = annotate_expression(current_e);
	}
  e -> set_type(type);
  return type;
}

Symbol SemanticAnalyzer::annotate_let_expression(let_class* e){
  Symbol id = e -> get_identifier();
  if(id == self){
    ct -> semant_error(idtable.add_string(curr_filename), e) << "Can't bind to self\n"; 
  }
  Symbol id_type = e -> get_type_decl();
  Symbol check_type = id_type;
  if(check_type == SELF_TYPE)
    check_type = env -> get_current_class();
  Expression init = e -> get_init();
  Expression body = e -> get_body();
  Symbol init_type = annotate_expression(init);
  Symbol init_check_type = init_type;
  if(init_check_type == SELF_TYPE)
    init_check_type = env -> get_current_class();
	if(init_type != No_type && !ct -> is_parent(check_type, init_check_type))
    ct -> semant_error(idtable.add_string(curr_filename), init) << "Type conformance error in let initialization expression.\n";
  env -> enter_scope();
  env -> add_symtab_entry(id, id_type);
  Symbol body_type = annotate_expression(body);
  env -> exit_scope();
  e -> set_type(body_type);
  return body_type;
}

Symbol SemanticAnalyzer::annotate_typcase_expression(typcase_class* e){
  Expression typ_expr = e -> get_expr();
	annotate_expression(typ_expr);
  Cases typ_cases     = e -> get_cases();
  List<Entry>* types = new List<Entry>((Symbol)NULL);
  Symbol type = 0;
  for(int i = typ_cases -> first();typ_cases -> more(i);i = typ_cases -> next(i)){
    branch_class* branch = (branch_class*)typ_cases -> nth(i);
    Symbol branch_name = branch -> get_name();
    Symbol branch_type_decl = branch -> get_type_decl();
    if(find(types, branch_type_decl) != -1)
			ct -> semant_error(idtable.add_string(curr_filename), e) << "Same branch type declared twice\n";
		else
			types = new List<Entry>(branch_type_decl, types);
    Expression branch_expr = branch -> get_expr();
    env -> enter_scope();
    env -> add_symtab_entry(branch_name, branch_type_decl);
    Symbol branch_expr_type = annotate_expression(branch_expr);
    if(type == 0)
      type = branch_expr_type;
    else
			type = ct -> lca(type, branch_expr_type);
    env -> exit_scope();
  }
  e -> set_type(type);
  return type;
}
template <class T>
int SemanticAnalyzer::find(List<T>* lst, T* elem){
  for(int i = 0;lst && lst -> hd();i++, lst = lst -> tl()){
    if(elem == lst -> hd())
      return i;
  }
  return -1;
}

Symbol SemanticAnalyzer::annotate_new_expression(new__class* e){
	Symbol type = e -> get_type_name();
  e -> set_type(type);
	return type;
}

Symbol SemanticAnalyzer::annotate_isvoid_expression(isvoid_class* e){
  annotate_expression(e -> get_expr());
  e -> set_type(Bool);
  return Bool;
}

Symbol SemanticAnalyzer::annotate_add_expression(plus_class* e){
  Expression e1 = e -> get_e1();
  Expression e2 = e -> get_e2();
  Symbol e1_type = annotate_expression(e1);
  Symbol e2_type = annotate_expression(e2);
  if(e1_type != Int)
    ct -> semant_error(idtable.add_string(curr_filename), e1) << "Type of left operand of arithmetic expression is not an int.\n";
  if(e2_type != Int)
    ct -> semant_error(idtable.add_string(curr_filename), e1) << "Type of right operand of arithmetic expression is not an int.\n";
  
  e -> set_type(Int);
  return Int;
}

Symbol SemanticAnalyzer::annotate_sub_expression(sub_class* e){
  Expression e1 = e -> get_e1();
  Expression e2 = e -> get_e2();
  Symbol e1_type = annotate_expression(e1);
  Symbol e2_type = annotate_expression(e2);
  if(e1_type != Int)
    ct -> semant_error(idtable.add_string(curr_filename), e1) << "Type of left operand of arithmetic expression is not an int.\n";
  if(e2_type != Int)
    ct -> semant_error(idtable.add_string(curr_filename), e1) << "Type of right operand of arithmetic expression is not an int.\n";
  
  e -> set_type(Int);
  return Int;

}

Symbol SemanticAnalyzer::annotate_mul_expression(mul_class* e){
  Expression e1 = e -> get_e1();
  Expression e2 = e -> get_e2();
  Symbol e1_type = annotate_expression(e1);
  Symbol e2_type = annotate_expression(e2);
  if(e1_type != Int)
    ct -> semant_error(idtable.add_string(curr_filename), e1) << "Type of left operand of arithmetic expression is not an int.\n";
  if(e2_type != Int)
    ct -> semant_error(idtable.add_string(curr_filename), e1) << "Type of right operand of arithmetic expression is not an int.\n";
  
  e -> set_type(Int);
  return Int;

}

Symbol SemanticAnalyzer::annotate_divide_expression(divide_class* e){
  Expression e1 = e -> get_e1();
  Expression e2 = e -> get_e2();
  Symbol e1_type = annotate_expression(e1);
  Symbol e2_type = annotate_expression(e2);
  if(e1_type != Int)
    ct -> semant_error(idtable.add_string(curr_filename), e1) << "Type of left operand of arithmetic expression is not an int.\n";
  if(e2_type != Int)
    ct -> semant_error(idtable.add_string(curr_filename), e1) << "Type of right operand of arithmetic expression is not an int.\n";
  
  e -> set_type(Int);
  return Int;

}

Symbol SemanticAnalyzer::annotate_neg_expression(neg_class* e){
  Expression e1 = e -> get_e1();
  Symbol e1_type = annotate_expression(e1);
  if(e1_type != Int)
    ct -> semant_error(idtable.add_string(curr_filename), e1) << "Can not negate non Int.\n";
  
  e -> set_type(Int);
  return Int;
}

Symbol SemanticAnalyzer::annotate_lt_expression(lt_class* e){
  Expression e1 = e -> get_e1();
  Expression e2 = e -> get_e2();
  Symbol e1_type = annotate_expression(e1);
  Symbol e2_type = annotate_expression(e2);
  if(e1_type != Int)
    ct -> semant_error(idtable.add_string(curr_filename), e1) << "Type of left operand of comparison expression is not an int.\n";
  if(e2_type != Int)
    ct -> semant_error(idtable.add_string(curr_filename), e1) << "Type of right operand of comparison expression is not an int.\n";
  
  e -> set_type(Bool);
  return Bool;

}

Symbol SemanticAnalyzer::annotate_leq_expression(leq_class* e){
  Expression e1 = e -> get_e1();
  Expression e2 = e -> get_e2();
  Symbol e1_type = annotate_expression(e1);
  Symbol e2_type = annotate_expression(e2);
  if(e1_type != Int)
    ct -> semant_error(idtable.add_string(curr_filename), e1) << "Type of left operand of comparison expression is not an int.\n";
  if(e2_type != Int)
    ct -> semant_error(idtable.add_string(curr_filename), e1) << "Type of right operand of comparison expression is not an int.\n";
  
  e -> set_type(Bool);
  return Bool;

}

Symbol SemanticAnalyzer::annotate_eq_expression(eq_class* e){
  Expression e1 = e -> get_e1();
  Expression e2 = e -> get_e2();
  Symbol e1_type = annotate_expression(e1);
  Symbol e2_type = annotate_expression(e2);
  if((e1_type == Int || e1_type == Bool || e1_type == Str || e2_type == Int || e2_type == Bool || e2_type == Str) && e1_type != e2_type)
    ct -> semant_error(idtable.add_string(curr_filename), e1) << "Can not check equality of non conforming primitive types.\n";
  
  e -> set_type(Bool);
  return Bool; 
}

Symbol SemanticAnalyzer::annotate_comp_expression(comp_class* e){
  Expression e1 = e -> get_e1();
  Symbol e1_type = annotate_expression(e1);
  if(e1_type != Bool)
    ct -> semant_error(idtable.add_string(curr_filename), e1) << "Can not complement non bool.\n";
  
  e -> set_type(Bool);
  return Bool;
	
}

Symbol SemanticAnalyzer::annotate_object_expression(object_class* e){
	Symbol type;
	if(e -> get_name() == self)
		type = SELF_TYPE;
	else{
		type = env -> get_type(e -> get_name());
    if(type == NULL){
      ct -> semant_error(idtable.add_string(curr_filename), e) << e -> get_name() << " is not defined\n";
      type = Object;
    }
  }
	e -> set_type(type);
	return type;
}

Symbol SemanticAnalyzer::annotate_int_expression(int_const_class* e){
	e -> set_type(Int);
	return Int;
}

Symbol SemanticAnalyzer::annotate_string_expression(string_const_class* e){
	e -> set_type(Str);
	return Str;
}

Symbol SemanticAnalyzer::annotate_bool_expression(bool_const_class* e){
	e -> set_type(Bool);
	return Bool;
}

Symbol SemanticAnalyzer::annotate_noexpr_expression(no_expr_class* e){
	e -> set_type(No_type);
	return No_type;
}
