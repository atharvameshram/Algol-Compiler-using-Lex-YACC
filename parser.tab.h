
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton interface for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     CHARACTER = 258,
     PRINTFF = 259,
     INT = 260,
     REAL = 261,
     CHAR = 262,
     BOOL = 263,
     FOR = 264,
     FROM = 265,
     TO = 266,
     DO = 267,
     OD = 268,
     IF = 269,
     ELIF = 270,
     ELSE = 271,
     THEN = 272,
     FI = 273,
     TRUE = 274,
     FALSE = 275,
     NUMBER = 276,
     REAL_NUM = 277,
     ID = 278,
     LE = 279,
     GE = 280,
     EQ = 281,
     NE = 282,
     GT = 283,
     LT = 284,
     ASSIGN = 285,
     AND = 286,
     OR = 287,
     STR = 288,
     ADD = 289,
     MULTIPLY = 290,
     DIVIDE = 291,
     SUBTRACT = 292
   };
#endif



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 1676 of yacc.c  */
#line 52 "parser.y"
 
	struct var_name { 
		char name[100]; 
		struct node* nd;
	} nd_obj; 
	
	struct var_name2 { 
		char name[100]; 
		struct node* nd;
		char type[5];
	} nd_obj2;

	struct var_name3 {
		char name[100];
		struct node* nd;
		char if_body[5];
		char else_body[5];
	} nd_obj3;



/* Line 1676 of yacc.c  */
#line 111 "parser.tab.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE yylval;


