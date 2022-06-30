%{
    #include "parser.tab.h"
    int countn=1;
%}

%option yylineno

alpha [a-zA-Z]
digit [0-9]

%%

"print"                     { strcpy(yylval.nd_obj.name,(yytext)); return PRINTFF; }
"INT"                       { strcpy(yylval.nd_obj.name,(yytext)); return INT; }
"REAL"                      { strcpy(yylval.nd_obj.name,(yytext)); return REAL; }
"CHAR"                      { strcpy(yylval.nd_obj.name,(yytext)); return CHAR; }
"BOOL" 			    { strcpy(yylval.nd_obj.name,(yytext)); return BOOL; }	

"FOR"	                    { strcpy(yylval.nd_obj.name,(yytext)); return FOR; }
"FROM"			    { strcpy(yylval.nd_obj.name,(yytext)); return FROM; }
"TO"                        { strcpy(yylval.nd_obj.name,(yytext)); return TO; }
"DO"			    { strcpy(yylval.nd_obj.name,(yytext)); return DO; }
"OD"			    { strcpy(yylval.nd_obj.name,(yytext)); return OD; }

"IF"	                    { strcpy(yylval.nd_obj.name,(yytext)); return IF; }
"ELIF"			    { strcpy(yylval.nd_obj.name,(yytext)); return ELIF; }
"ELSE"	                    { strcpy(yylval.nd_obj.name,(yytext)); return ELSE; }
"THEN"			    { strcpy(yylval.nd_obj.name,(yytext)); return THEN; }
"FI"			    { strcpy(yylval.nd_obj.name,(yytext)); return FI; }

"TRUE"                      { strcpy(yylval.nd_obj.name,(yytext)); return TRUE; }
"FALSE"                     { strcpy(yylval.nd_obj.name,(yytext)); return FALSE; }

"<="                        { strcpy(yylval.nd_obj.name,(yytext)); return LE; }
">="                        { strcpy(yylval.nd_obj.name,(yytext)); return GE; }
"=="                        { strcpy(yylval.nd_obj.name,(yytext)); return EQ; }
"EQ"                        { strcpy(yylval.nd_obj.name,(yytext)); return EQ; }
"!="                        { strcpy(yylval.nd_obj.name,(yytext)); return NE; }
"NE"                        { strcpy(yylval.nd_obj.name,(yytext)); return NE; }
">"	                    { strcpy(yylval.nd_obj.name,(yytext)); return GT; }
"<"	                    { strcpy(yylval.nd_obj.name,(yytext)); return LT; }
"&&"	                    { strcpy(yylval.nd_obj.name,(yytext)); return AND; }
"AND"	                    { strcpy(yylval.nd_obj.name,(yytext)); return AND; }
"||"	                    { strcpy(yylval.nd_obj.name,(yytext)); return OR; }
"OR"	                    { strcpy(yylval.nd_obj.name,(yytext)); return OR; }
"+"                         { strcpy(yylval.nd_obj.name,(yytext)); return ADD; }
"-"                         { strcpy(yylval.nd_obj.name,(yytext)); return SUBTRACT; }
"/"                         { strcpy(yylval.nd_obj.name,(yytext)); return DIVIDE; }
"*"                         { strcpy(yylval.nd_obj.name,(yytext)); return MULTIPLY; }
":="			    { strcpy(yylval.nd_obj.name,(yytext)); return ASSIGN; }

[-]?{digit}+                { strcpy(yylval.nd_obj.name,(yytext)); return NUMBER; }
[-]?{digit}+\.{digit}{1,6}  { strcpy(yylval.nd_obj.name,(yytext)); return REAL_NUM; }
{alpha}({alpha}|{digit})*   { strcpy(yylval.nd_obj.name,(yytext)); return ID; }


\/\/.*                      { ; }
\/\*(.*\n)*.*\*\/           { ; }
#.*#			    { ; } 
[ \t]*                      { ; }
[\n]                        { countn++; }
.	                    { return *yytext; }
["].*["]                    { strcpy(yylval.nd_obj.name,(yytext)); return STR; }
['].[']                     { strcpy(yylval.nd_obj.name,(yytext)); return CHARACTER; }

%%

int yywrap() {
    return 1;
}