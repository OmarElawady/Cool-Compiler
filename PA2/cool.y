/*
*  cool.y
*              Parser definition for the COOL language.
*
*/
%{
  #include <iostream>
  #include "cool-tree.h"
  #include "stringtab.h"
  #include "utilities.h"
  
  extern char *curr_filename;
  
  
  /* Locations */
  #define YYLTYPE int              /* the type of locations */
  #define cool_yylloc curr_lineno  /* use the curr_lineno from the lexer
  for the location of tokens */
    
    extern int node_lineno;          /* set before constructing a tree node
    to whatever you want the line number
    for the tree node to be */
      
      
      #define YYLLOC_DEFAULT(Current, Rhs, N)         \
      Current = Rhs[1];                             \
      node_lineno = Current;
    
    
    #define SET_NODELOC(Current)  \
    node_lineno = Current;
    
    /* IMPORTANT NOTE ON LINE NUMBERS
    *********************************
    * The above definitions and macros cause every terminal in your grammar to 
    * have the line number supplied by the lexer. The only task you have to
    * implement for line numbers to work correctly, is to use SET_NODELOC()
    * before constructing any constructs from non-terminals in your grammar.
    * Example: Consider you are matching on the following very restrictive 
    * (fictional) construct that matches a plus between two integer constants. 
    * (SUCH A RULE SHOULD NOT BE  PART OF YOUR PARSER):
    
    plus_consts	: INT_CONST '+' INT_CONST 
    
    * where INT_CONST is a terminal for an integer constant. Now, a correct
    * action for this rule that attaches the correct line number to plus_const
    * would look like the following:
    
    plus_consts	: INT_CONST '+' INT_CONST 
    {
      // Set the line number of the current non-terminal:
      // ***********************************************
      // You can access the line numbers of the i'th item with @i, just
      // like you acess the value of the i'th exporession with $i.
      //
      // Here, we choose the line number of the last INT_CONST (@3) as the
      // line number of the resulting expression (@$). You are free to pick
      // any reasonable line as the line number of non-terminals. If you 
      // omit the statement @$=..., bison has default rules for deciding which 
      // line number to use. Check the manual for details if you are interested.
      @$ = @3;
      
      
      // Observe that we call SET_NODELOC(@3); this will set the global variable
      // node_lineno to @3. Since the constructor call "plus" uses the value of 
      // this global, the plus node will now have the correct line number.
      SET_NODELOC(@3);
      
      // construct the result node:
      $$ = plus(int_const($1), int_const($3));
    }
    
    */
    
    
    
    void yyerror(char *s);        /*  defined below; called for each parse error */
    extern int yylex();           /*  the entry point to the lexer  */
    
    /************************************************************************/
    /*                DONT CHANGE ANYTHING IN THIS SECTION                  */
    
    Program ast_root;	      /* the result of the parse  */
    Classes parse_results;        /* for use in semantic analysis */
    int omerrs = 0;               /* number of errors in lexing and parsing */

	class accessible_attr_class : public attr_class{
	public:
		accessible_attr_class(Symbol a, Symbol b, Expression c) : attr_class(a, b, c) {}
		Symbol get_name(){return this -> name;}
		Symbol get_type_decl(){return this -> type_decl;}
		Expression get_init(){return this -> init;}
	};
    %}
    
    /* A union of all the types that can be the result of parsing actions. */
    %union {
      Boolean boolean;
      Symbol symbol;
      Program program;
      Class_ class_;
      Classes classes;
      Feature feature;
      Features features;
      Formal formal;
      Formals formals;
      Case case_;
      Cases cases;
      Expression expression;
      Expressions expressions;
      char *error_msg;
    }
    
    /* 
    Declare the terminals; a few have types for associated lexemes.
    The token ERROR is never used in the parser; thus, it is a parse
    error when the lexer returns it.
    
    The integer following token declaration is the numeric constant used
    to represent that token internally.  Typically, Bison generates these
    on its own, but we give explicit numbers to prevent version parity
    problems (bison 1.25 and earlier start at 258, later versions -- at
    257)
    */
    %token CLASS 258 ELSE 259 FI 260 IF 261 IN 262 
    %token INHERITS 263 LET 264 LOOP 265 POOL 266 THEN 267 WHILE 268
    %token CASE 269 ESAC 270 OF 271 DARROW 272 NEW 273 ISVOID 274
    %token <symbol>  STR_CONST 275 INT_CONST 276 
    %token <boolean> BOOL_CONST 277
    %token <symbol>  TYPEID 278 OBJECTID 279 
    %token ASSIGN 280 NOT 281 LE 282 ERROR 283
    
    /*  DON'T CHANGE ANYTHING ABOVE THIS LINE, OR YOUR PARSER WONT WORK       */
    /**************************************************************************/
    
    /* Complete the nonterminal list below, giving a type for the semantic
    value of each non terminal. (See section 3.6 in the bison 
    documentation for details). */
    
    /* Declare types for the grammar's non-terminals. */
    %type <program> program
    %type <classes> class_list
    %type <class_> class
    
    /* You will want to change the following line. */
    %type <features> feature_list
    %type <feature> method
    %type <feature> attribute
    %type <feature> accessible_attribute
    %type <formal> formal
    %type <formals> formal_list
    %type <features> let_definition_list
	%type <expressions> block_expression_list
	%type <expressions> argument_list
	%type <expression> expression
    %type <expression> assignment_expression
    %type <expression> static_dispatch_expresssion
    %type <expression> dispatch_expression
    %type <expression> conditional_expression
    %type <expression> loop_expression
    %type <expression> block_expression
    %type <expression> let_expression
    %type <expression> typecase_expression
    %type <expression> new_expression
    %type <expression> isvoid_expression
    %type <expression> add_expression
    %type <expression> sub_expression
    %type <expression> mul_expression
    %type <expression> divide_expression
    %type <expression> neg_expression
    %type <expression> less_expression
    %type <expression> less_or_equal_expression
    %type <expression> equal_expression
    %type <expression> comp_expression
    %type <expression> object_expression
    %type <expression> int_expression
    %type <expression> string_expression
    %type <expression> bool_expression

    %type <case_> case
    %type <cases> case_list



    /* Precedence declarations go here. */
    %nonassoc IN
    %right ASSIGN
    %nonassoc NOT
    %nonassoc LE  '<'  '='
    %left '+' '-'
    %left '*' '/'
    %nonassoc ISVOID
    %nonassoc '~'
    %nonassoc '@'
    %left '.'

	%nonassoc THEN
	%nonassoc ELSE

	%%
    /* 
    Save the root of the abstract syntax tree in a global variable.
    */
    program	: class_list	{ @$ = @1; ast_root = program($1);}
    ;
    
    class_list
    : class			/* single class */
    { $$ = single_Classes($1);
    parse_results = $$; }
    | class_list class	/* several classes */
    { $$ = append_Classes($1,single_Classes($2)); 
    parse_results = $$; }
    ;
    
    /* If no parent is specified, the class inherits from the Object class. */
    class	: CLASS TYPEID '{' feature_list '}' ';'
    { $$ = class_($2,idtable.add_string("Object"),$4,
    stringtable.add_string(curr_filename)); }
	| error ';' {}
    | CLASS TYPEID INHERITS TYPEID '{' feature_list '}' ';'
    { $$ = class_($2,$4,$6,stringtable.add_string(curr_filename)); }
    ;
    
    /* Feature list may be empty, but no empty features in list. */
    feature_list:		/* empty */
    {  $$ = nil_Features(); }
    | feature_list method ';'
    { $$ = append_Features($1, single_Features($2)); }
	| error ';' {}
    | feature_list attribute ';'
    { $$ = append_Features($1, single_Features($2)); }
    ;

    method
    : OBJECTID '(' formal_list ')' ':' TYPEID '{' expression '}'
    { $$ = method($1, $3, $6, $8); }
    ;

    attribute
    : OBJECTID ':' TYPEID
    { $$ = attr($1, $3, no_expr()); }
	| OBJECTID ':' TYPEID ASSIGN expression
	{ $$ = attr($1, $3, $5); }
	;

	expression
	: assignment_expression
    | static_dispatch_expresssion
    | dispatch_expression
    | conditional_expression
    | loop_expression
    | block_expression
    | let_expression
    | typecase_expression
    | new_expression
    | isvoid_expression
    | add_expression
    | sub_expression
    | mul_expression
    | divide_expression
    | neg_expression
    | less_expression
    | less_or_equal_expression
    | equal_expression
    | comp_expression
    | object_expression
    | int_expression
    | string_expression
    | bool_expression
	| '(' expression ')'
	{ $$ = $2; }
	;

	assignment_expression
	: OBJECTID ASSIGN expression
	{ $$ = assign($1, $3); }
	;

	static_dispatch_expresssion
	: expression '@' TYPEID '.' OBJECTID '(' argument_list ')'
	{ $$ = static_dispatch($1, $3, $5, $7); }
	;

	dispatch_expression
	: OBJECTID '(' argument_list ')'
	{ $$ = dispatch(object(idtable.add_string("self")), $1, $3); }
	| expression '.' OBJECTID '(' argument_list ')'
	{ $$ = dispatch($1, $3, $5); }
	;

	conditional_expression
	: IF expression THEN expression ELSE expression FI
	{ $$ = cond($2, $4, $6); }
	;

	loop_expression
	: WHILE expression LOOP expression POOL
	{ $$ = loop($2, $4); }
	;

	block_expression
	: '{' block_expression_list '}'
	{ $$ = block($2); }
	;

	let_expression
	: LET let_definition_list IN expression 
	{ accessible_attr_class* f = (accessible_attr_class*)$2 -> nth($2 -> first());
	  $$ = let(f -> get_name(), f -> get_type_decl(), f -> get_init(), $4);
	  for(int i = $2 -> next($2 -> first());$2 -> more(i);i = $2 -> next(i)){
		accessible_attr_class* c = (accessible_attr_class*)$2 -> nth(i);
		$$ = let(c -> get_name(), c -> get_type_decl(), c -> get_init(), $$);
	  }
	}
	;

	typecase_expression
	: CASE expression OF case_list ESAC
	{ $$ = typcase($2, $4); }
	;

	new_expression
	: NEW TYPEID
	{ $$ = new_($2); }
	;

	isvoid_expression
	: ISVOID expression
	{ $$ = isvoid($2); }
	;

	add_expression
	: expression '+' expression
	{ $$ = plus($1, $3); }
	;

	sub_expression
	: expression '-' expression
	{ $$ = sub($1, $3); }
	;

	mul_expression
	: expression '*' expression
	{ $$ = mul($1, $3); }
	;

	divide_expression
	: expression '/' expression
	{ $$ = divide($1, $3); }
	;

	neg_expression
	: '~' expression
	{ $$ = neg($2); }
	;

	less_expression
	: expression '<' expression
	{ $$ = lt($1, $3); }
	;

	less_or_equal_expression
	: expression LE expression
	{ $$ = leq($1, $3); }
	;

	equal_expression
	: expression '=' expression
	{ $$ = eq($1, $3); }
	;

	comp_expression
	: NOT expression
	{ $$ = comp($2); }
	;

	object_expression
	: OBJECTID
	{ $$ = object($1); }
	;

	int_expression
	: INT_CONST
	{ $$ = int_const($1); }
	;

	string_expression
	: STR_CONST
	{ $$ = string_const($1); }
	;

	bool_expression
	: BOOL_CONST
	{ $$ = bool_const($1); }
	;
	
	argument_list
	: 
	{ $$ = nil_Expressions(); }
	| expression
	{ $$ = single_Expressions($1); }
	| argument_list ',' expression
	{ $$ = append_Expressions($1, single_Expressions($3)); }
	;
	
	accessible_attribute
    : OBJECTID ':' TYPEID
    { $$ = (Feature)new accessible_attr_class($1, $3, no_expr()); }
    | OBJECTID ':' TYPEID ASSIGN expression
    { $$ = (Feature)new accessible_attr_class($1, $3, $5); }
    ;
	
	let_definition_list
    : accessible_attribute
	{ $$ = single_Features($1); }
	| error ',' let_definition_list {}
	| accessible_attribute ',' let_definition_list
	{ $$ = append_Features($1, single_Features($3)); }
	;

	block_expression_list
	: expression ';'
	{ $$ = single_Expressions($1); }
	| error ';' {}
	| block_expression_list expression ';'
	{ $$ = append_Expressions($1, single_Expressions($2)); }
	;

	case
	: OBJECTID ':' TYPEID DARROW expression ';'
	{ $$ = branch($1, $3, $5); }
	;

	case_list
	: case
	{ $$ = single_Cases($1); }
	| case_list case
	{ $$ = append_Cases($1, single_Cases($2)); }
	;

	formal
	: OBJECTID ':' TYPEID
	{ $$ = formal($1, $3); }
	;
	
	formal_list
	: 
	{ $$ = nil_Formals(); }
	| formal
	{ $$ = single_Formals($1); }
	| formal_list ',' formal
	{ $$ = append_Formals($1, single_Formals($3)); }
	;
    /* end of grammar */ 
    %%
    
    /* This function is called automatically when Bison detects a parse error. */
    void yyerror(char *s)
    {
      extern int curr_lineno;
      
      cerr << "\"" << curr_filename << "\", line " << curr_lineno << ": " \
      << s << " at or near ";
      print_cool_token(yychar);
      cerr << endl;
      omerrs++;
      
      if(omerrs>50) {fprintf(stdout, "More than 50 errors\n"); exit(1);}
    }
    
    
