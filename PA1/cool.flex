/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

%}

/*
 * Define names for regular expressions here.
 */

DARROW          =>
CLASS (c|C)(l|L)(a|A)(s|S)(s|S)
ELSE (e|E)(l|L)(s|S)(e|E)
FALSE f(a|A)(l|L)(s|S)(e|E)
FI (f|F)(i|I)
IF (i|I)(f|F)
IN (i|I)(n|N)
INHERITS (i|I)(n|N)(h|H)(e|E)(r|R)(i|I)(t|T)(s|S)
ISVOID (i|I)(s|S)(v|V)(o|O)(i|I)(d|D)
LET (l|L)(e|E)(t|T)
LOOP (l|L)(o|O)(o|O)(p|P)
POOL (p|P)(o|O)(o|O)(l|L)
THEN (t|T)(h|H)(e|E)(n|N)
WHILE (w|W)(h|H)(i|I)(l|L)(e|E)
CASE (c|C)(a|A)(s|S)(e|E)
ESAC (e|E)(s|S)(a|A)(c|C)
NEW (n|N)(e|E)(w|W)
OF (o|O)(f|F)
NOT (n|N)(o|O)(t|T)
TRUE t(r|R)(u|U)(e|E)
LE <=
BOOL_CONST {TRUE}|{FALSE}
TYPEID [A-Z][A-Za-z0-9_]*
OBJECTID [a-z][A-Za-z0-9_]*
ASSIGN <-
INT_CONST [0-9]+
%x str
%%
	char string_buf[1025];
	bool too_long, null_char;
	int cur_length;
 /*
  *  Nested comments
  */
"(*" {
  int cnt = 1;
  char c, pc;
  pc = 0;
  while(1){
    c = yyinput();
    if(c == EOF){
      cool_yylval.error_msg = "EOF in comment";
      return(ERROR);
    }
    if(c == '\n')curr_lineno++;
    if(c == ')' && pc == '*')
      cnt--;
    if(c == '*' && pc == '(')
      cnt++;
    if(cnt == 0)
      break;
    pc = c;
  }
}

 /*
  * one line Comments
  */

--.* {}

 /*
  * single character symbols
  */

"," {return(',');}
"+" {return('+');}
"-" {return('-');}
"*" {return('*');}
"/" {return('/');}
";" {return(';');}
"=" {return('=');}
")" {return(')');}
"(" {return('(');}
"}" {return('}');}
"{" {return('{');}
":" {return(':');}
"<" {return('<');}
"@" {return('@');}
"~" {return('~');}
"." {return('.');}

 /*
  * whitespaces
  */

[ \t\n\r\f\v]+ {
  int i = 0;
  while(yytext[i]){
    if(yytext[i] == '\n')curr_lineno++;
    i++;
  }
}

 /*
  *  The multiple-character operators.
  */

{DARROW}		{ return (DARROW); }
"<=" {return(LE);}
{ASSIGN} {return(ASSIGN);}

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */

{CLASS}                 {return (CLASS); }
{ELSE} {return(ELSE);}
{FALSE} {cool_yylval.boolean = false;return(BOOL_CONST);}
{FI} {return(FI);}
{IF} {return(IF);}
{IN} {return(IN);}
{INHERITS} {return(INHERITS);}
{ISVOID} {return(ISVOID);}
{LET} {return(LET);}
{LOOP} {return(LOOP);}
{POOL} {return(POOL);}
{THEN} {return(THEN);}
{WHILE} {return(WHILE);}
{CASE} {return(CASE);}
{ESAC} {return(ESAC);}
{NEW} {return(NEW);}
{OF} {return(OF);}
{NOT} {return(NOT);}
{TRUE} {cool_yylval.boolean = true;return(BOOL_CONST);}

 /*
  * Identifiers
  */ 

{TYPEID} {cool_yylval.symbol = idtable.add_string(yytext);return(TYPEID);}
{OBJECTID} {cool_yylval.symbol = idtable.add_string(yytext);return(OBJECTID);}

 /*
  * Int constant
  */

{INT_CONST} {cool_yylval.symbol = inttable.add_string(yytext);return(INT_CONST);}

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */
\" {
  cur_length = 0;
  too_long = null_char = false;
  BEGIN(str);
}
<str>{
  \\n {
    string_buf[cur_length++] = '\n';
    if(cur_length > 1024){
      too_long = true;
      cur_length = 0;
    }
  }
  \\t {
    string_buf[cur_length++] = '\t';
    if(cur_length > 1024){
      too_long = true;
      cur_length = 0;
    }
  }
  \\b {
    string_buf[cur_length++] = '\b';
    if(cur_length > 1024){
      too_long = true;
      cur_length = 0;
    }
  }
  \\f {
    string_buf[cur_length++] = '\f';
    if(cur_length > 1024){
      too_long = true;
      cur_length = 0;
    }
  }
  \\\0 {null_char = true;}
  \0 {
    null_char = true;
  }
  \n {
    curr_lineno++;
    BEGIN(INITIAL);
    cool_yylval.error_msg = "Unterminated string constant";
    too_long = false;
    return(ERROR);
  }
  \" {
    BEGIN(INITIAL);
    if(too_long){
      cool_yylval.error_msg = "Too long string";
      return(ERROR);
    }else if(null_char){
      cool_yylval.error_msg = "String contains null character";
      return(ERROR);
    }
    string_buf[cur_length++] = 0;
    cool_yylval.symbol = stringtable.add_string(string_buf);
    return(STR_CONST);
  }
  \\[^\0] {
    string_buf[cur_length++] = yytext[1];
    if(cur_length > 1024){
      too_long = true;
      cur_length = 0;
    }
  }
  <<EOF>> {
    BEGIN(INITIAL);
    cool_yylval.error_msg = "EOF in string constant";
    return(ERROR);
  }
  [^\\\n\"\0]+ {
    int i = 0;
    while(i < yyleng){
      string_buf[cur_length++] = yytext[i++];
      if(cur_length > 1024){
        too_long = true;
        cur_length = 0;
      }
    }
  }
}
 /*
  * error in matching
  */
. {cool_yylval.error_msg = yytext;return(ERROR);}
"*)" {cool_yylval.error_msg = "Unmatched *)";return(ERROR);}
%%
