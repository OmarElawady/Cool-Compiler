
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream &str, char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol
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

SymbolTable<Symbol, IDLocation> symbol_table;
int label_id = 2;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
  arg = idtable.add_string("arg");
  arg2 = idtable.add_string("arg2");
  Bool = idtable.add_string("Bool");
  concat = idtable.add_string("concat");
  cool_abort = idtable.add_string("abort");
  copy = idtable.add_string("copy");
  Int = idtable.add_string("Int");
  in_int = idtable.add_string("in_int");
  in_string = idtable.add_string("in_string");
  IO = idtable.add_string("IO");
  length = idtable.add_string("length");
  Main = idtable.add_string("Main");
  main_meth = idtable.add_string("main");
  //   _no_class is a symbol that can't be the name of any
  //   user-defined class.
  No_class = idtable.add_string("_no_class");
  No_type = idtable.add_string("_no_type");
  Object = idtable.add_string("Object");
  out_int = idtable.add_string("out_int");
  out_string = idtable.add_string("out_string");
  prim_slot = idtable.add_string("_prim_slot");
  self = idtable.add_string("self");
  SELF_TYPE = idtable.add_string("SELF_TYPE");
  Str = idtable.add_string("String");
  str_field = idtable.add_string("_str_field");
  substr = idtable.add_string("substr");
  type_name = idtable.add_string("type_name");
  val = idtable.add_string("_val");
}

static char *gc_init_names[] =
    {"_NoGC_Init", "_GenGC_Init", "_ScnGC_Init"};
static char *gc_collect_names[] =
    {"_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect"};

//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os)
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes, os);

  os << "\n# end of generated code\n";
}

//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream &s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")"
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream &s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
    << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream &s)
{
  s << LI << dest_reg << " " << val << endl;
}

static void emit_load_address(char *dest_reg, char *address, ostream &s)
{
  s << LA << dest_reg << " " << address << endl;
}

static void emit_partial_load_address(char *dest_reg, ostream &s)
{
  s << LA << dest_reg << " ";
}

static void emit_load_bool(char *dest, const BoolConst &b, ostream &s)
{
  emit_partial_load_address(dest, s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream &s)
{
  emit_partial_load_address(dest, s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream &s)
{
  emit_partial_load_address(dest, s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream &s)
{
  s << MOVE << dest_reg << " " << source_reg << endl;
}

static void emit_neg(char *dest, char *src1, ostream &s)
{
  s << NEG << dest << " " << src1 << endl;
}

static void emit_add(char *dest, char *src1, char *src2, ostream &s)
{
  s << ADD << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addu(char *dest, char *src1, char *src2, ostream &s)
{
  s << ADDU << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addiu(char *dest, char *src1, int imm, ostream &s)
{
  s << ADDIU << dest << " " << src1 << " " << imm << endl;
}

static void emit_div(char *dest, char *src1, char *src2, ostream &s)
{
  s << DIV << dest << " " << src1 << " " << src2 << endl;
}

static void emit_mul(char *dest, char *src1, char *src2, ostream &s)
{
  s << MUL << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sub(char *dest, char *src1, char *src2, ostream &s)
{
  s << SUB << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sll(char *dest, char *src1, int num, ostream &s)
{
  s << SLL << dest << " " << src1 << " " << num << endl;
}

static void emit_jalr(char *dest, ostream &s)
{
  s << JALR << "\t" << dest << endl;
}

static void emit_jal(char *address, ostream &s)
{
  s << JAL << address << endl;
}

static void emit_return(ostream &s)
{
  s << RET << endl;
}
static void emit_gc_assign(ostream &s)
{
  s << JAL << "_GenGC_Assign" << endl;
}

static void emit_disptable_ref(Symbol sym, ostream &s)
{
  s << sym << DISPTAB_SUFFIX;
}

static void emit_init_ref(Symbol sym, ostream &s)
{
  s << sym << CLASSINIT_SUFFIX;
}

static void emit_label_ref(int l, ostream &s)
{
  s << "label" << l;
}

static void emit_protobj_ref(Symbol sym, ostream &s)
{
  s << sym << PROTOBJ_SUFFIX;
}

static void emit_method_ref(Symbol classname, Symbol methodname, ostream &s)
{
  s << classname << METHOD_SEP << methodname;
}

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l, s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_bnez(char *src1, int label, ostream &s)
{
  s << BNEZ << src1 << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_branch(int l, ostream &s)
{
  s << BRANCH;
  emit_label_ref(l, s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream &str)
{
  emit_store(reg, 0, SP, str);
  emit_addiu(SP, SP, -4, str);
}

//
// Pop a register off the stack. The stack grows towards smaller addresses.
//
static void emit_pop(ostream &str)
{
  emit_addiu(SP, SP, 4, str);
}

static void emit_load_pro(char *dest, Symbol name, ostream &str)
{
  str << LA << dest << ' ' << name->get_string() << PROTOBJ_SUFFIX << endl;
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream &s)
{
  emit_load(dest, DEFAULT_OBJFIELDS, source, s);
}

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream &s)
{
  emit_store(source, DEFAULT_OBJFIELDS, dest, s);
}

static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s);  // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP, SP, 4, s);
  emit_load(ACC, 0, SP, s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char *)A1)
    emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}

static void emit_copy(ostream &s)
{
  s << JAL << "Object.copy" << endl;
}

static void emit_load_default(char *dest, Symbol name, ostream &s)
{

  if (name->equal_string("String", 6))
  {
    emit_partial_load_address(dest, s);
    stringtable.lookup_string("")->code_ref(s);
    s << endl;
  }
  else if (name->equal_string("Int", 3))
  {
    emit_partial_load_address(dest, s);
    inttable.lookup_string("0")->code_ref(s);
    s << endl;
  }
  else if (name->equal_string("Bool", 4))
  {
    emit_partial_load_address(dest, s);
    falsebool.code_ref(s);
    s << endl;
  }
  else
  {
    emit_load_imm(dest, 0, s);
  }
}

void advance_label()
{
  label_id += 1;
}
///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream &s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream &s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);
  s << LABEL                                                              // label
    << WORD << stringclasstag << endl                                     // tag
    << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len + 4) / 4) << endl // size
    << WORD;

  emit_disptable_ref(Str, s);
  /***** done! Add dispatch information for class String ******/

  s << endl; // dispatch table
  s << WORD;
  lensym->code_ref(s);
  s << endl;                    // string length
  emit_string_constant(s, str); // ascii string
  s << ALIGN;                   // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the
// stringtable.
//
void StrTable::code_string_table(ostream &s, int stringclasstag)
{
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s, stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);
  s << LABEL                                           // label
    << WORD << intclasstag << endl                     // class tag
    << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl // object size
    << WORD;

  emit_disptable_ref(Int, s);
  /***** done! Add dispatch information for class Int ******/

  s << endl;                // dispatch table
  s << WORD << str << endl; // integer value
}

//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s, intclasstag);
}

//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream &s) const
{
  s << BOOLCONST_PREFIX << val;
}

//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream &s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);
  s << LABEL                                            // label
    << WORD << boolclasstag << endl                     // class tag
    << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl // object size
    << WORD;

  emit_disptable_ref(Bool, s);
  /***** done! Add dispatch information for class Bool ******/

  s << endl;                // dispatch table
  s << WORD << val << endl; // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main = idtable.lookup_string(MAINNAME);
  Symbol string = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n"
      << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL;
  emit_protobj_ref(main, str);
  str << endl;
  str << GLOBAL;
  emit_protobj_ref(integer, str);
  str << endl;
  str << GLOBAL;
  emit_protobj_ref(string, str);
  str << endl;
  str << GLOBAL;
  falsebool.code_ref(str);
  str << endl;
  str << GLOBAL;
  truebool.code_ref(str);
  str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL
      << WORD << stringclasstag << endl;
}

//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Int"), str);
  str << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("String"), str);
  str << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"), str);
  str << endl
      << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str, boolclasstag);
  truebool.code_def(str, boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}

//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str, stringclasstag);
  inttable.code_string_table(str, intclasstag);
  code_bools(boolclasstag);
}

CgenClassTable::CgenClassTable(Classes classes, ostream &s) : nds(NULL), str(s)
{
  stringclasstag = 4 /* Change to your String class tag here */;
  intclasstag = 2 /* Change to your Int class tag here */;
  boolclasstag = 3 /* Change to your Bool class tag here */;

  enterscope();
  if (cgen_debug)
    cout << "Building CgenClassTable" << endl;
  install_basic_classes();
  install_classes(classes);
  build_inheritance_tree();

  code();
  exitscope();
}

void CgenClassTable::install_basic_classes()
{

  // The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

  //
  // A few special class names are installed in the lookup table but not
  // the class list.  Thus, these classes exist, but are not part of the
  // inheritance hierarchy.
  // No_class serves as the parent of Object and the other special classes.
  // SELF_TYPE is the self class; it cannot be redefined or inherited.
  // prim_slot is a class known to the code generator.
  //
  addid(No_class,
        new CgenNode(class_(No_class, No_class, nil_Features(), filename),
                     Basic, this, -1));
  addid(SELF_TYPE,
        new CgenNode(class_(SELF_TYPE, No_class, nil_Features(), filename),
                     Basic, this, -1));
  addid(prim_slot,
        new CgenNode(class_(prim_slot, No_class, nil_Features(), filename),
                     Basic, this, -1));

  //
  // The Object class has no parent class. Its methods are
  //        cool_abort() : Object    aborts the program
  //        type_name() : Str        returns a string representation of class name
  //        copy() : SELF_TYPE       returns a copy of the object
  //
  // There is no need for method bodies in the basic classes---these
  // are already built in to the runtime system.
  //
  install_class(
      new CgenNode(
          class_(Object,
                 No_class,
                 append_Features(
                     append_Features(
                         single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                         single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                     single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
                 filename),
          Basic, this, class_count++));

  //
  // The IO class inherits from Object. Its methods are
  //        out_string(Str) : SELF_TYPE          writes a string to the output
  //        out_int(Int) : SELF_TYPE               "    an int    "  "     "
  //        in_string() : Str                    reads a string from the input
  //        in_int() : Int                         "   an int     "  "     "
  //
  install_class(
      new CgenNode(
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
                 filename),
          Basic, this, class_count++));

  //
  // The Int class has no methods and only a single attribute, the
  // "val" for the integer.
  //
  install_class(
      new CgenNode(
          class_(Int,
                 Object,
                 single_Features(attr(val, prim_slot, no_expr())),
                 filename),
          Basic, this, class_count++));

  //
  // Bool also has only the "val" slot.
  //
  install_class(
      new CgenNode(
          class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename),
          Basic, this, class_count++));

  //
  // The class Str has a number of slots and operations:
  //       val                                  ???
  //       str_field                            the string itself
  //       length() : Int                       length of the string
  //       concat(arg: Str) : Str               string concatenation
  //       substr(arg: Int, arg2: Int): Str     substring
  //
  install_class(
      new CgenNode(
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
                 filename),
          Basic, this, class_count++));
}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
  {
    return;
  }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd, nds);
  addid(name, nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for (int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i), NotBasic, this, class_count++));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for (List<CgenNode> *l = nds; l; l = l->tl())
    set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n, children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}

void CgenClassTable::code()
{
  if (cgen_debug)
    cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug)
    cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug)
    cout << "coding constants" << endl;
  this->code_constants();
  this->code_basic_pro_obj();
  this->code_classes();
  this->code_nametab();
  this->print_needed_debugging();
  //                 Add your code to emit
  //                   - prototype objects
  //                   - class_nameTab
  //                   - dispatch tables
  // done!

  if (cgen_debug)
    cout << "coding global text" << endl;
  code_global_text();

  code_basic_init();
  this->code_methods();
  //                 Add your code to emit
  //                   - object initializer
  //                   - the class methods
  //                   - etc...
}

void CgenClassTable::code_methods()
{
  root()->code_methods(str);
}

void CgenClassTable::code_classes()
{
  CgenNodeP root_class = root();
  root_class->code_class(str);
}

void CgenClassTable::print_nodes()
{
  for (List<CgenNode> *nds = this->nds; nds; nds = nds->tl())
  {
    nds->hd()->print_class_name();
    nds->hd()->print_class_tag();
  }
}
void CgenClassTable::print_needed_debugging()
{
}
CgenNodeP CgenClassTable::root()
{
  return probe(Object);
}

CgenNodeP CgenClassTable::lookup_node(Symbol name)
{

  for (List<CgenNode> *l = nds; l; l = l->tl())
    if (l->hd()->get_name()->equal_string(name->get_string(), name->get_len()))
      return l->hd();
  cout << name->get_string() << endl;
  assert(0 && "CgenNode not found"[0]);
}

CgenNodeP CgenClassTable::lookup_index(int index)
{

  for (List<CgenNode> *l = nds; l; l = l->tl())
    if (l->hd()->get_class_tag() == index)
      return l->hd();
  cout << index << endl;
  assert(0 && "CgenNode with the given index not found"[0]);
}

void CgenClassTable::code_basic_pro_obj()
{
  CgenNodeP object_node = lookup_node(Object);
  str << WORD << "-1\n";
  emit_protobj_ref(Object, str);
  str << LABEL;
  str << WORD << object_node->get_class_tag() << endl;
  str << WORD << 3 << endl;
  str << WORD;
  emit_disptable_ref(Object, str);
  str << endl;
  CgenNodeP string_node = lookup_node(Str);
  str << WORD << "-1\n";
  emit_protobj_ref(Str, str);
  str << LABEL;
  str << WORD << string_node->get_class_tag() << endl;
  str << WORD << 5 << endl;
  str << WORD;
  emit_disptable_ref(Str, str);
  str << endl;

  str << WORD;
  inttable.lookup_string("0")->code_ref(str);
  str << endl;
  str << BYTE << 0 << endl;
  str << ALIGN;

  CgenNodeP int_node = lookup_node(Int);
  str << WORD << "-1\n";
  emit_protobj_ref(Int, str);
  str << LABEL;
  str << WORD << int_node->get_class_tag() << endl;
  str << WORD << 4 << endl;
  str << WORD;
  emit_disptable_ref(Int, str);
  str << endl;

  str << WORD << 0 << endl;

  CgenNodeP bool_node = lookup_node(Bool);
  str << WORD << "-1\n";
  emit_protobj_ref(Bool, str);
  str << LABEL;
  str << WORD << bool_node->get_class_tag() << endl;
  str << WORD << 4 << endl;
  str << WORD;
  emit_disptable_ref(Bool, str);
  str << endl;

  str << WORD << 0 << endl;
}
void CgenClassTable::code_basic_init()
{
  emit_init_ref(Str, str);
  str << LABEL;
  emit_return(str);

  emit_init_ref(Int, str);
  str << LABEL;
  emit_return(str);

  emit_init_ref(Bool, str);
  str << LABEL;
  emit_return(str);
}

void CgenClassTable::code_nametab()
{
  str << CLASSNAMETAB << LABEL;
  for (int i = 0; i < class_count; i++)
  {
    str << WORD;
    CgenNodeP node = lookup_index(i);
    stringtable.lookup_string(node->get_name()->get_string())->code_ref(str);
    str << endl;
  }
}

int CgenClassTable::get_class_count()
{
  return class_count;
}

#define HEADER_SIZE 3
///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct, int class_tag) : class__class((const class__class &)*nd),
                                                                                      parentnd(NULL),
                                                                                      children(NULL),
                                                                                      basic_status(bstatus),
                                                                                      attrs(NULL),
                                                                                      methods(NULL),
                                                                                      class_tag(class_tag),
                                                                                      ct(ct)
{
  stringtable.add_string(name->get_string()); // Add class name to string table
}

void CgenNode::differentiate_attrs_and_methods()
{
  Features features = this->features;
  if (this->parentnd)
  {
    this->attrs = this->parentnd->attrs;
    this->methods = this->parentnd->methods;
  }
  for (int i = features->first(); features->more(i); i = features->next(i))
  {
    Feature feature = features->nth(i);
    if (feature->is_attr())
      add_attr((attr_class *)feature);
    else
      add_method((method_class *)feature);
  }
}

void CgenNode::add_attr(attr_class *attr)
{
  attr->set_host_class(name);
  this->attrs = new List<attr_class>(attr, this->attrs);
}

void CgenNode::add_method(method_class *method)
{
  method->set_host_class(name);
  method_class *overrided_method = 0;
  List<method_class> *reversed_before = NULL;
  List<method_class> *methods_after = NULL;
  for (List<method_class> *methods = this->methods; methods; methods = methods->tl())
  {
    method_class *old_method = methods->hd();
    if (old_method->get_name() == method->get_name())
    {
      overrided_method = old_method;
      methods_after = methods->tl();
      break;
    }
    else
    {
      reversed_before = new List<method_class>(old_method, reversed_before);
    }
  }
  if (overrided_method)
  {
    this->methods = new List<method_class>(method, methods_after);
    while (reversed_before)
    {
      this->methods = new List<method_class>(reversed_before->hd(), this->methods);
      reversed_before = reversed_before->tl();
    }
  }
  else
  {
    this->methods = new List<method_class>(method, this->methods);
  }
}
void CgenNode::print_attrs()
{
  for (List<attr_class> *attrs = this->attrs; attrs; attrs = attrs->tl())
  {
    cout << "  Attribute: \n";
    cout << "    name: " << attrs->hd()->get_name() << endl;
    cout << "    index: " << attr_index(attrs->hd()->get_name()) << endl;
    cout << "    host class: " << attrs->hd()->get_host_class()->get_string() << endl;
  }
}
void CgenNode::print_methods()
{
  for (List<method_class> *methods = this->methods; methods; methods = methods->tl())
  {
    cout << "  Method: \n";
    cout << "    name: " << methods->hd()->get_name() << endl;
    cout << "    index: " << method_index(methods->hd()->get_name()) << endl;
    cout << "    host class: " << methods->hd()->get_host_class()->get_string() << endl;
  }
}
void CgenNode::print_class_tag()
{
  cout << class_tag << endl;
}
void CgenNode::print_class_name()
{
  cout << "Class: " << this->get_name()->get_string() << endl;
}
void CgenNode::print_needed_debugging()
{
}
void CgenNode::default_value(ostream &os)
{
  if (name->equal_string("String", 6))
    stringtable.lookup_string("")->code_ref(os);
  else if (name->equal_string("Int", 3))
    inttable.lookup_string("0")->code_ref(os);
  else if (name->equal_string("Bool", 4))
    falsebool.code_ref(os);
  else
    os << 0;
}
void CgenNode::code_class(ostream &os)
{
  this->differentiate_attrs_and_methods();
  this->length = list_length(attrs) + HEADER_SIZE;
  this->print_needed_debugging();
  this->code_dispatch_table(os);
  this->code_prototype_object(os);
  this->code_children(os);
}

void CgenNode::code_children_methods(ostream &os)
{
  for (List<CgenNode> *descendants = children; descendants; descendants = descendants->tl())
    descendants->hd()->code_methods(os);
}

void CgenNode::code_methods(ostream &os)
{
  if (basic_status)
  {
    symbol_table.enterscope();
    this->add_attrs_to_symtab();
    this->code_init_method(os);
    this->code_new_methods(os);
    symbol_table.exitscope();
  }
  this->code_children_methods(os);
}

void CgenNode::add_attrs_to_symtab()
{
  List<attr_class> *ordered_attrs = list_reverse(attrs);
  int index = HEADER_SIZE;
  for (; ordered_attrs; ordered_attrs = ordered_attrs->tl())
  {
    attr_class *attr = ordered_attrs->hd();
    symbol_table.addid(attr->get_name(), new IDLocation("$a0", index++));
  }
}

void CgenNode::code_dispatch_table(ostream &os)
{
  emit_disptable_ref(this->name, os);
  os << LABEL;

  os << WORD;
  if (!this->basic())
    emit_init_ref(name, os);
  else
    os << 0;
  os << endl;
  List<method_class> *ordered_methods = list_reverse(this->methods);
  for (; ordered_methods; ordered_methods = ordered_methods->tl())
  {
    method_class *method = ordered_methods->hd();
    os << WORD << method->get_host_class() << METHOD_SEP << method->get_name() << endl;
  }
}
void CgenNode::code_prototype_object(ostream &os)
{
  if (name == Str || name == Bool || name == Int || name == Object)
    return;
  os << WORD << "-1\n";
  emit_protobj_ref(this->name, os);
  os << LABEL;
  os << WORD << this->class_tag << endl;
  os << WORD << this->length << endl;
  os << WORD;
  emit_disptable_ref(this->name, os);
  os << endl;
  List<attr_class> *ordered_attrs = list_reverse(attrs);

  for (; ordered_attrs; ordered_attrs = ordered_attrs->tl())
  {
    attr_class *attr = ordered_attrs->hd();
    os << WORD;
    Symbol attr_type = attr->get_type_decl();
    if (attr_type == SELF_TYPE)
      attr_type = name;
    this->ct->lookup_node(attr_type)->default_value(os);
    os << endl;
  }
}
void CgenNode::code_init_method(ostream &os)
{
  os << this->name->get_string() << CLASSINIT_SUFFIX << LABEL;
  emit_push(FP, os);
  emit_move(FP, SP, os);
  emit_addiu(FP, FP, 4, os);
  emit_push(RA, os);
  emit_push(ACC, os);
  int locals = this->calc_init_locals();
  emit_addiu(SP, SP, locals * -4, os);
  emit_load(T1, SELF_OFFSET, FP, os);
  for (List<attr_class> *attrs = list_reverse(this->attrs); attrs; attrs = attrs->tl())
  {

    attr_class *attr = attrs->hd();
    emit_load_default(ACC, attr->get_type_decl(), os);
    if (attr->get_type_decl() != prim_slot)
      emit_store(ACC, attr_index(attr->get_name()) + HEADER_SIZE, T1, os);
  }
  for (List<attr_class> *attrs = list_reverse(this->attrs); attrs; attrs = attrs->tl())
  {
    attr_class *attr = attrs->hd();
    if (attr->get_expression()->get_type() != 0 && attr->get_type_decl() != prim_slot)
      this->init_attr(attr, os);
  }
  //cout << endl;
  emit_addiu(SP, SP, (3 + locals) * 4, os);
  emit_load(FP, FP_OFFSET, SP, os);
  emit_load(RA, RA_OFFSET, SP, os);
  emit_load(ACC, SELF_OFFSET, SP, os);
  emit_return(os);
}

int CgenNode::calc_init_locals()
{
  int res = 0;
  for (List<attr_class> *attrs = list_reverse(this->attrs); attrs; attrs = attrs->tl())
  {
    attr_class *attr = attrs->hd();
    int attr_locals = attr->get_expression()->calc_locals();
    if (attr_locals > res)
      res = attr_locals;
  }
  return res;
}
void CgenNode::init_attr(attr_class *attr, ostream &os)
{
  int attribute_index = attr_index(attr->get_name());
  attr->get_expression()->code(ct, this, LOCALS_OFFSET, os);
  emit_load(T1, SELF_OFFSET, FP, os);
  emit_store(ACC, attribute_index + HEADER_SIZE, T1, os);
}
void CgenNode::code_new_methods(ostream &os)
{

  for (List<method_class> *methods = this->methods; methods; methods = methods->tl())
  {
    method_class *method = methods->hd();
    if (method->get_host_class() == this->name)
      this->code_method(method, os);
    else
    {
      os << name->get_string() << METHOD_SEP << method->get_name() << LABEL;
      os << BRANCH;
      emit_method_ref(method->get_host_class(), method->get_name(), os);
      os << endl;
    }
  }
}

void CgenNode::code_method(method_class *method, ostream &os)
{
  os << name->get_string() << METHOD_SEP << method->get_name() << LABEL;
  symbol_table.enterscope();
  this->add_formals(method->get_formals());
  emit_push(FP, os);
  emit_move(FP, SP, os);
  emit_addiu(FP, FP, 4, os);
  emit_push(RA, os);
  emit_push(ACC, os);
  int number_of_locals = method->get_expression()->calc_locals();
  emit_addiu(SP, SP, -4 * number_of_locals, os);
  method->get_expression()->code(ct, this, -3, os);
  emit_addiu(SP, SP, (HEADER_SIZE + number_of_locals) * 4, os);
  emit_load(RA, RA_OFFSET, SP, os);
  emit_load(FP, FP_OFFSET, FP, os);
  emit_addiu(SP, SP, (method->get_formals()->len()) * 4, os);
  emit_return(os);
  symbol_table.exitscope();
}

void CgenNode::add_formals(Formals formals)
{
  int index = formals->len();
  for (int i = formals->first(); formals->more(i); i = formals->next(i))
  {
    formal_class *fc = (formal_class *)formals->nth(i);
    symbol_table.addid(fc->get_name(), new IDLocation("$fp", index--));
  }
}

void CgenNode::code_children(ostream &os)
{

  for (List<CgenNode> *descendants = children; descendants; descendants = descendants->tl())
    descendants->hd()->code_class(os);
}

int CgenNode::get_length()
{
  return this->length;
}
int CgenNode::get_class_tag()
{
  return this->class_tag;
}

int CgenNode::method_index(Symbol name)
{
  int index = list_length(this->methods) - 1;
  for (List<method_class> *methods = this->methods; methods; methods = methods->tl())
    if (methods->hd()->get_name() == name)
      return index;
    else
      index--;
  assert(false);
  return -1;
}

int CgenNode::attr_index(Symbol name)
{
  int index = list_length(this->attrs) - 1;
  for (List<attr_class> *attrs = this->attrs; attrs; attrs = attrs->tl())
    if (attrs->hd()->get_name() == name)
      return index;
    else
      index--;
  assert(false);
  return -1;
}

CgenNodeP CgenNode::get_parent_node()
{
  return parentnd;
}

bool CgenNode::is_class(Symbol name)
{
  return this->name == name;
}

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(CgenClassTableP ct, CgenNodeP nd, int next_local, ostream &s)
{
  this->expr->code(ct, nd, next_local, s);
  IDLocation *assignee = symbol_table.lookup(this->name);
  emit_move(T1, ACC, s);
  emit_load(ACC, SELF_OFFSET, FP, s);
  emit_store(T1, assignee->get_offset(), assignee->get_register(), s);
  emit_move(ACC, T1, s);
}

void static_dispatch_class::code(CgenClassTableP ct, CgenNodeP nd, int next_local, ostream &s)
{
  for (int i = actual->first(); actual->more(i); i = actual->next(i))
  {
    actual->nth(i)->code(ct, nd, next_local, s);
    emit_push(ACC, s);
  }
  expr->code(ct, nd, next_local, s);
  int nonvoid_label = label_id++;
  emit_bnez(ACC, nonvoid_label, s);
  emit_load_string(ACC, (StringEntryP)nd->filename, s);
  emit_load_imm(T1, line_number, s);
  emit_jal("_dispatch_abort", s);
  emit_label_def(nonvoid_label, s);
  s << JAL;
  emit_method_ref(this->type_name, this->name, s);
  s << endl;
}

void dispatch_class::code(CgenClassTableP ct, CgenNodeP nd, int next_local, ostream &s)
{
  for (int i = actual->first(); actual->more(i); i = actual->next(i))
  {
    actual->nth(i)->code(ct, nd, next_local, s);
    emit_push(ACC, s);
  }
  Symbol static_type;
  if (expr->get_type() == SELF_TYPE)
    static_type = nd->name;
  else
    static_type = expr->get_type();
  CgenNodeP class_node = ct->lookup_node(static_type);
  int method_index = class_node->method_index(name);
  int expression_position = actual->len() + 1;
  expr->code(ct, nd, next_local, s);
  int nonvoid_label = label_id++;
  emit_bnez(ACC, nonvoid_label, s);
  emit_load_string(ACC, (StringEntryP)nd->filename, s);
  emit_load_imm(T1, line_number, s);
  emit_jal("_dispatch_abort", s);
  emit_label_def(nonvoid_label, s);
  emit_load(T1, DISPTABLE_OFFSET, ACC, s);
  emit_load(T1, method_index + METHODS_OFFSET, T1, s);
  emit_jalr(T1, s);
}

void cond_class::code(CgenClassTableP ct, CgenNodeP nd, int next_local, ostream &s)
{
  int true_branch = label_id++;
  int false_branch = label_id++;
  int end_branch = label_id++;
  this->pred->code(ct, nd, next_local, s);
  emit_fetch_int(ACC, ACC, s);
  emit_bgti(ACC, 0, true_branch, s);
  emit_label_def(false_branch, s);
  this->else_exp->code(ct, nd, next_local, s);
  emit_branch(end_branch, s);
  emit_label_def(true_branch, s);
  this->then_exp->code(ct, nd, next_local, s);
  emit_label_def(end_branch, s);
}

void loop_class::code(CgenClassTableP ct, CgenNodeP nd, int next_local, ostream &s)
{
  int loop_label = label_id++;
  int end_label = label_id++;
  emit_label_def(loop_label, s);
  this->pred->code(ct, nd, next_local, s);
  emit_fetch_int(ACC, ACC, s);
  emit_beqz(ACC, end_label, s);
  this->body->code(ct, nd, next_local, s);
  emit_branch(loop_label, s);
  emit_label_def(end_label, s);
  emit_load_imm(ACC, 0, s);
}

void typcase_class::code(CgenClassTableP ct, CgenNodeP nd, int next_local, ostream &s)
{
  this->expr->code(ct, nd, next_local, s);
  int good_label = label_id++;
  emit_bnez(ACC, good_label, s);
  emit_load_string(ACC, (StringEntry *)nd->filename, s);
  emit_load_imm(T1, line_number, s);
  emit_jal("_case_abort2", s);
  emit_label_def(good_label, s);
  emit_load(T1, TAG_OFFSET, ACC, s);
  int class_len = ct->get_class_count();
  int branch_len = this->cases->len();
  for (int i = 0; i < class_len; i++)
  {
    CgenNodeP cls = ct->lookup_index(i);
    CgenNodeP orig = cls;
    bool done = false;
    while (cls)
    {
      for (int j = 0; j < branch_len; j++)
      {
        branch_class *branch = (branch_class *)this->cases->nth(j);
        if (cls->is_class(branch->get_type_decl()))
        {
          emit_load_imm(T2, orig->get_class_tag(), s);
          emit_beq(T1, T2, label_id + j, s);
        }
      }
      if (done)
        break;
      cls = cls->get_parent_node();
    }
  }
  int end_label = label_id + this->cases->len();
  int orig_label = label_id;
  label_id = end_label + 1;
  emit_load(ACC, SELF_OFFSET, FP, s);
  emit_jal("_case_abort", s);
  for (int i = 0; i < branch_len; i++)
  {
    branch_class *branch = (branch_class *)this->cases->nth(i);
    emit_label_def(i + orig_label, s);
    symbol_table.enterscope();
    IDLocation *idloc = new IDLocation(FP, next_local--);
    symbol_table.addid(branch->get_name(), idloc);
    emit_store(ACC, idloc->get_offset(), idloc->get_register(), s);
    branch->get_expression()->code(ct, nd, next_local, s);
    emit_branch(end_label, s);
    next_local++;
    symbol_table.exitscope();
  }
  emit_label_def(end_label, s);
}

void block_class::code(CgenClassTableP ct, CgenNodeP nd, int next_local, ostream &s)
{
  for (int i = this->body->first(); this->body->more(i); i = this->body->next(i))
  {
    this->body->nth(i)->code(ct, nd, next_local, s);
  }
}

void let_class::code(CgenClassTableP ct, CgenNodeP nd, int next_local, ostream &s)
{
  if (this->init->get_type() == 0)
  {
    emit_load_default(ACC, this->type_decl, s);
  }
  else
  {
    this->init->code(ct, nd, next_local, s);
  }
  symbol_table.enterscope();
  IDLocation *idloc = new IDLocation(FP, next_local--);
  symbol_table.addid(this->identifier, idloc);
  emit_store(ACC, idloc->get_offset(), idloc->get_register(), s);
  this->body->code(ct, nd, next_local, s);
  next_local++;
  symbol_table.exitscope();
}

void plus_class::code(CgenClassTableP ct, CgenNodeP nd, int next_local, ostream &s)
{
  this->e1->code(ct, nd, next_local, s);
  emit_push(ACC, s);
  this->e2->code(ct, nd, next_local, s);
  emit_copy(s);
  emit_load(T1, 1, SP, s);
  emit_fetch_int(T1, T1, s);
  emit_fetch_int(T2, ACC, s);
  emit_add(T1, T1, T2, s);
  emit_store_int(T1, ACC, s);
  emit_pop(s);
}

void sub_class::code(CgenClassTableP ct, CgenNodeP nd, int next_local, ostream &s)
{

  this->e1->code(ct, nd, next_local, s);
  emit_push(ACC, s);
  this->e2->code(ct, nd, next_local, s);
  emit_copy(s);
  emit_load(T1, 1, SP, s);
  emit_fetch_int(T1, T1, s);
  emit_fetch_int(T2, ACC, s);
  emit_sub(T1, T1, T2, s);
  emit_store_int(T1, ACC, s);
  emit_pop(s);
}

void mul_class::code(CgenClassTableP ct, CgenNodeP nd, int next_local, ostream &s)
{
  this->e1->code(ct, nd, next_local, s);
  emit_push(ACC, s);
  this->e2->code(ct, nd, next_local, s);
  emit_copy(s);
  emit_load(T1, 1, SP, s);
  emit_fetch_int(T1, T1, s);
  emit_fetch_int(T2, ACC, s);
  emit_mul(T1, T1, T2, s);
  emit_store_int(T1, ACC, s);
  emit_pop(s);
}

void divide_class::code(CgenClassTableP ct, CgenNodeP nd, int next_local, ostream &s)
{
  this->e1->code(ct, nd, next_local, s);
  emit_push(ACC, s);
  this->e2->code(ct, nd, next_local, s);
  emit_copy(s);
  emit_load(T1, 1, SP, s);
  emit_fetch_int(T1, T1, s);
  emit_fetch_int(T2, ACC, s);
  emit_div(T1, T1, T2, s);
  emit_store_int(T1, ACC, s);
  emit_pop(s);
}

void neg_class::code(CgenClassTableP ct, CgenNodeP nd, int next_local, ostream &s)
{
  this->e1->code(ct, nd, next_local, s);
  emit_copy(s);
  emit_fetch_int(T1, ACC, s);
  emit_neg(T1, T1, s);
  emit_store_int(T1, ACC, s);
}

void lt_class::code(CgenClassTableP ct, CgenNodeP nd, int next_local, ostream &s)
{
  int true_branch = label_id++;
  int end_branch = label_id++;
  this->e1->code(ct, nd, next_local, s);
  emit_push(ACC, s);
  this->e2->code(ct, nd, next_local, s);
  emit_load(T1, 1, SP, s);
  emit_move(T2, ACC, s);
  emit_fetch_int(T1, T1, s);
  emit_fetch_int(T2, T2, s);
  emit_load_bool(ACC, falsebool, s);
  emit_blt(T1, T2, true_branch, s);
  emit_branch(end_branch, s);
  emit_label_def(true_branch, s);
  emit_load_bool(ACC, truebool, s);
  emit_label_def(end_branch, s);
  emit_pop(s);
}

void eq_class::code(CgenClassTableP ct, CgenNodeP nd, int next_local, ostream &s)
{
  this->e1->code(ct, nd, next_local, s);
  emit_push(ACC, s);
  this->e2->code(ct, nd, next_local, s);
  emit_load(T1, 1, SP, s);
  emit_move(T2, ACC, s);
  emit_load_bool(ACC, truebool, s);
  emit_load_bool(A1, falsebool, s);
  if (this->e1->type == Int || this->e1->type == Bool || this->e1->type == Str)
    emit_jal("equality_test", s);
  else
  {
    int equal_label = label_id++;
    emit_beq(T1, T2, equal_label, s);
    emit_move(ACC, A1, s);
    emit_label_def(equal_label, s);
  }

  emit_pop(s);
}

void leq_class::code(CgenClassTableP ct, CgenNodeP nd, int next_local, ostream &s)
{
  int true_branch = label_id++;
  int end_branch = label_id++;
  this->e1->code(ct, nd, next_local, s);
  emit_push(ACC, s);
  this->e2->code(ct, nd, next_local, s);
  emit_load(T1, 1, SP, s);
  emit_move(T2, ACC, s);
  emit_fetch_int(T1, T1, s);
  emit_fetch_int(T2, T2, s);
  emit_load_bool(ACC, falsebool, s);
  emit_bleq(T1, T2, true_branch, s);
  emit_branch(end_branch, s);
  emit_label_def(true_branch, s);
  emit_load_bool(ACC, truebool, s);
  emit_label_def(end_branch, s);
  emit_pop(s);
}

void comp_class::code(CgenClassTableP ct, CgenNodeP nd, int next_local, ostream &s)
{
  this->e1->code(ct, nd, next_local, s);
  emit_copy(s);
  emit_fetch_int(T1, ACC, s);
  emit_neg(T1, T1, s);
  emit_addiu(T1, T1, 1, s);
  emit_store_int(T1, ACC, s);
}

void int_const_class::code(CgenClassTableP ct, CgenNodeP nd, int next_local, ostream &s)
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC, inttable.lookup_string(token->get_string()), s);
}

void string_const_class::code(CgenClassTableP ct, CgenNodeP nd, int next_local, ostream &s)
{
  emit_load_string(ACC, stringtable.lookup_string(token->get_string()), s);
}

void bool_const_class::code(CgenClassTableP ct, CgenNodeP nd, int next_local, ostream &s)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(CgenClassTableP ct, CgenNodeP nd, int next_local, ostream &s)
{
  if (type_name == SELF_TYPE)
  {
    emit_load(ACC, SELF_OFFSET, FP, s);
    emit_copy(s);
    emit_load(T1, DISPTABLE_OFFSET, ACC, s);
    emit_load(T1, INIT_OFFSET, T1, s);
    emit_jalr(T1, s);
  }
  else
  {
    emit_load_pro(ACC, type_name, s);
    emit_copy(s);
    if (!ct->lookup_node(type_name)->basic())
    {
      s << JAL;
      emit_init_ref(type_name, s);
      s << endl;
    }
  }
}

void isvoid_class::code(CgenClassTableP ct, CgenNodeP nd, int next_local, ostream &s)
{
  this->e1->code(ct, nd, next_local, s);
  emit_move(T1, ACC, s);
  emit_load_bool(ACC, falsebool, s);
  emit_bgti(T1, 0, label_id, s);
  emit_load_bool(ACC, truebool, s);
  emit_label_def(label_id, s);
  label_id++;
}

void no_expr_class::code(CgenClassTableP ct, CgenNodeP nd, int next_local, ostream &s)
{
}

void object_class::code(CgenClassTableP ct, CgenNodeP nd, int next_local, ostream &s)
{
  emit_load(ACC, SELF_OFFSET, FP, s);
  if (this->name != self)
  {
    IDLocation *loc = symbol_table.lookup(this->name);
    emit_load(ACC, loc->get_offset(), loc->get_register(), s);
  }
}

IDLocation::IDLocation(char *reg, int offset) : reg(reg), offset(offset) {}

int IDLocation::get_offset()
{
  return offset;
}
char *IDLocation::get_register()
{
  return reg;
}