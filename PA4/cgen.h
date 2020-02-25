#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness
{
   Basic,
   NotBasic
};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol, CgenNode>
{
private:
   List<CgenNode> *nds;
   ostream &str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;
   int class_count;
   // The following methods emit code for
   // constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();
   void code_classes();
   // The following creates an inheritance graph from
   // a list of classes.  The graph is implemented as
   // a tree of `CgenNode', and class names are placed
   // in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
   void code_basic_pro_obj();
   void code_basic_init();
   void code_nametab();
   void code_methods();

public:
   CgenClassTable(Classes, ostream &str);
   void code();
   CgenNodeP root();
   CgenNodeP lookup_node(Symbol name);
   CgenNodeP lookup_index(int index);
   int get_class_count();
   // debug
   void print_nodes();
   void print_needed_debugging();
};

class CgenNode : public class__class
{
private:
   CgenNodeP parentnd;       // Parent of class
   List<CgenNode> *children; // Children of class
   Basicness basic_status;   // `Basic' if class is basic
                             // `NotBasic' otherwise
   int length;
   List<attr_class> *attrs;
   List<method_class> *methods;
   int class_tag;
   CgenClassTableP ct;

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table,
            int class_tag);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
   void code_class(ostream &os);
   int get_length();
   int get_class_tag();

   void differentiate_attrs_and_methods();
   void code_dispatch_table(ostream &os);
   void code_prototype_object(ostream &os);
   void code_init_method(ostream &os);
   void code_children(ostream &os);
   void code_methods(ostream &os);
   void code_children_methods(ostream &os);
   void default_value(ostream &os);
   void code_new_methods(ostream &os);
   void code_method(method_class *method, ostream &os);
   void init_attr(attr_class *attr, ostream &os);
   void add_attrs_to_symtab();
   void add_formals(Formals formals);
   void add_attr(attr_class *attr);
   void add_method(method_class *attr);
   int calc_init_locals();
   int method_index(Symbol name);
   int attr_index(Symbol name);
   bool is_class(Symbol name);
   CgenNodeP get_parent_node();

   // debugging
   void print_class_name();
   void print_attrs();
   void print_methods();
   void print_class_tag();
   void print_needed_debugging();
};
class BoolConst
{
private:
   int val;

public:
   BoolConst(int);
   void code_def(ostream &, int boolclasstag);
   void code_ref(ostream &) const;
};

class IDLocation
{
private:
   char *reg;
   int offset;

public:
   IDLocation(char *reg, int offset);
   char *get_register();
   int get_offset();
};
