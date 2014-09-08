/* File:  scanner.l
 * ----------------
 * Lex inupt file to generate the scanner for the compiler.
 */

%{
#include <string.h>
#include "scanner.h"
#include "utility.h" // for PrintDebug()
#include "errors.h"
#include "list.h"

int num_row=1;
int num_col=1;
List<char*> stringLiterals;

YYSTYPE yylval;  // manually declared for pp1, later Yacc provides
struct yyltype yylloc; // manually dclared for pp1, later Yacc provides
static void DoBeforeEachAction(); 
#define YY_USER_ACTION DoBeforeEachAction();

%}

DIGIT [0-9]
HEXDIGIT [0-9A-Fa-f]
ALPHABET [A-Za-z]
EXPONENT [Ee][\+\-]?{DIGIT}+

%s comment

%%
<comment>{
\*\/		BEGIN(0);
[^\*\n+]+|\*
\n
}
\/\/(.*)	/* Discard one -line comment */
\/\*		BEGIN(comment);
void		return T_Void;
int		return T_Int;
double		return T_Double;
bool		return T_Bool;
string		return T_String;
class		return T_Class;
interface	return T_Interface;
null		return T_Null;
this		return T_This;
extends		return T_Extends;
implements	return T_Implements;
for		return T_For;
while		return T_While;
if		return T_If;
else		return T_Else;
return		return T_Return;
break		return T_Break;
New		return T_New;
NewArray	return T_NewArray;
\+		return '+';
-		return '-';
\*		return '*';
\/		return '/';
%		return '%';
\<		return '<';
\<=		return T_LessEqual;
>		return '>';
>=		return T_GreaterEqual;
=		return '=';
==		return T_Equal;
!=		return T_NotEqual;
&&		return T_And;
\|\|		return T_Or;
!		return '!';
;		return ';';
,		return ',';
\.		return '.';
\[		return '[';
\]		return ']';
\(		return '(';
\)		return ')';
\{		return '{';
\}		return '}';
\"[^\"\n]*\"				{
					  yylval.stringConstant=strdup(yytext);
					  stringLiterals.Append(yylval.stringConstant);
					  return T_StringConstant;
					}
({DIGIT}+)|((0x|0X){HEXDIGIT}+)		sscanf(yytext,"%i",&yylval.integerConstant); return T_IntConstant;
{DIGIT}+\.{DIGIT}*{EXPONENT}?		sscanf(yytext,"%lf",&yylval.doubleConstant); return T_DoubleConstant;
true|false				yylval.boolConstant=strcmp(yytext,"true")==0?true:false; return T_BoolConstant;
{ALPHABET}({ALPHABET}|{DIGIT}|_)*	{
					  if (strlen(yytext)>31){
					    ReportError::LongIdentifier(&yylloc,yytext);
					    strncpy(yylval.identifier,yytext,31);
					  }
					  else
					    strcpy(yylval.identifier,yytext);
					  return T_Identifier;
					}
[ \t\n]
.					ReportError::UnrecogChar(&yylloc,yytext[0]);

%%

/* Function: InitScanner
 * ---------------------
 * This function will be called before any calls to yylex().  It is designed
 * to give you an opportunity to do anything that must be done to initialize
 * the scanner (set global variables, configure starting state, etc.). One
 * thing it already does for you is assign the value of the global variable
 * yy_flex_debug that controls whether flex prints debugging information
 * about each token and what rule was matched. If set to false, no information
 * is printed. Setting it to true will give you a running trail that might
 * be helpful when debugging your scanner. Please be sure the variable is
 * set to false when submitting your final version.
 */
void InitScanner() {
    PrintDebug("lex", "Initializing scanner");
    yy_flex_debug = false;
}


/* Function: DoBeforeEachAction()
 * ------------------------------
 * This function is installed as the YY_USER_ACTION. This is a place
 * to group code common to all actions.
 */
static void DoBeforeEachAction() {
  yylloc.first_line = num_row;
  yylloc.first_column = num_col;
  if (strlen(yytext) == 1 && yytext[0] == '\n') {
    num_row++;
    num_col = 1;
  }
  else
    num_col += strlen(yytext);

  yylloc.last_line = num_row;
  yylloc.last_column = num_col-1;
}
