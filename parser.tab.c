
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C
   
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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.4.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Copy the first part of user declarations.  */

/* Line 189 of yacc.c  */
#line 1 "parser.y"

    #include<stdio.h>
    #include<string.h>
    #include<stdlib.h>
    #include<ctype.h>
    #include"lex.yy.c"
    
    void yyerror(const char *s);
    int yylex();
    int yywrap();
    void add(char);
    void insert_type();
    int search(char *);
    void insert_type();
    void printtree(struct node*);
    void printInorder(struct node *);
    void check_declaration(char *);
    void check_return_type(char *);
    int check_types(char *, char *);
    char *get_type(char *);
    struct node* mknode(struct node *left, struct node *right, char *token);

    struct dataType {
        char * id_name;
        char * data_type;
        char * type;
        int line_no;
    } symbol_table[30];

    int count=0;
    int q;
    char type[10];
    extern int countn;
    struct node *head;
    int sem_errors=0;
    int ic_idx=0;
    int temp_var=0;
    int label=0;
    int is_for=0;
    char buff[100];
    char errors[10][100];
    char reserved[14][10] = {"INT", "REAL", "CHAR", "BOOL", "IF", "ELIF", "THEN", "ELSE", "FI", "FOR", "FROM", "TO", "DO", "OD"};
    char icg[50][100];

    struct node { 
	struct node *left; 
	struct node *right; 
	char *token; 
    };


/* Line 189 of yacc.c  */
#line 125 "parser.tab.c"

/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif


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

/* Line 214 of yacc.c  */
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



/* Line 214 of yacc.c  */
#line 220 "parser.tab.c"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 264 of yacc.c  */
#line 232 "parser.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  18
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   91

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  41
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  32
/* YYNRULES -- Number of rules.  */
#define YYNRULES  62
/* YYNRULES -- Number of states.  */
#define YYNSTATES  95

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   292

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      39,    40,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,    38,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint8 yyprhs[] =
{
       0,     0,     3,     5,     7,     9,    11,    13,    14,    15,
      16,    25,    26,    27,    28,    29,    40,    43,    46,    47,
      54,    56,    58,    59,    60,    61,    62,    72,    73,    77,
      78,    79,    80,    88,    94,    96,    98,    99,   100,   105,
     106,   111,   112,   117,   120,   121,   125,   127,   129,   131,
     133,   135,   137,   139,   141,   143,   145,   147,   149,   151,
     153,   155,   157
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      42,     0,    -1,    44,    -1,     5,    -1,     6,    -1,     7,
      -1,     8,    -1,    -1,    -1,    -1,     9,    45,    60,    12,
      46,    44,    13,    47,    -1,    -1,    -1,    -1,    -1,    14,
      48,    63,    49,    17,    50,    44,    51,    54,    18,    -1,
      64,    38,    -1,    44,    44,    -1,    -1,     4,    52,    39,
      33,    40,    38,    -1,    21,    -1,    22,    -1,    -1,    -1,
      -1,    -1,    15,    55,    63,    56,    17,    57,    44,    58,
      54,    -1,    -1,    16,    59,    44,    -1,    -1,    -1,    -1,
      23,    10,    61,    53,    11,    62,    53,    -1,    39,    72,
      71,    72,    40,    -1,    19,    -1,    20,    -1,    -1,    -1,
      43,    23,    65,    68,    -1,    -1,    23,    66,    30,    69,
      -1,    -1,    23,    67,    71,    69,    -1,    30,    72,    -1,
      -1,    69,    70,    69,    -1,    72,    -1,    34,    -1,    37,
      -1,    35,    -1,    36,    -1,    29,    -1,    28,    -1,    24,
      -1,    25,    -1,    26,    -1,    27,    -1,    21,    -1,    22,
      -1,     3,    -1,    23,    -1,    19,    -1,    20,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    79,    79,    85,    86,    87,    88,    91,    91,    91,
      91,    99,    99,    99,    99,    99,   104,   105,   106,   106,
     109,   110,   113,   113,   113,   113,   113,   118,   118,   119,
     122,   122,   122,   165,   178,   179,   180,   183,   183,   213,
     213,   249,   249,   255,   260,   263,   304,   311,   312,   313,
     314,   317,   318,   319,   320,   321,   322,   325,   326,   327,
     328,   329,   330
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "CHARACTER", "PRINTFF", "INT", "REAL",
  "CHAR", "BOOL", "FOR", "FROM", "TO", "DO", "OD", "IF", "ELIF", "ELSE",
  "THEN", "FI", "TRUE", "FALSE", "NUMBER", "REAL_NUM", "ID", "LE", "GE",
  "EQ", "NE", "GT", "LT", "ASSIGN", "AND", "OR", "STR", "ADD", "MULTIPLY",
  "DIVIDE", "SUBTRACT", "';'", "'('", "')'", "$accept", "program",
  "datatype", "body", "$@1", "$@2", "$@3", "$@4", "$@5", "$@6", "$@7",
  "$@8", "num_value", "else", "$@9", "$@10", "$@11", "$@12", "$@13",
  "for_condn", "$@14", "$@15", "condition", "statement", "$@16", "$@17",
  "$@18", "init", "expression", "arithmetic", "relop", "value", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,    59,    40,
      41
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    41,    42,    43,    43,    43,    43,    45,    46,    47,
      44,    48,    49,    50,    51,    44,    44,    44,    52,    44,
      53,    53,    55,    56,    57,    58,    54,    59,    54,    54,
      61,    62,    60,    63,    63,    63,    63,    65,    64,    66,
      64,    67,    64,    68,    68,    69,    69,    70,    70,    70,
      70,    71,    71,    71,    71,    71,    71,    72,    72,    72,
      72,    72,    72
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     1,     1,     1,     1,     0,     0,     0,
       8,     0,     0,     0,     0,    10,     2,     2,     0,     6,
       1,     1,     0,     0,     0,     0,     9,     0,     3,     0,
       0,     0,     7,     5,     1,     1,     0,     0,     4,     0,
       4,     0,     4,     2,     0,     3,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,    18,     3,     4,     5,     6,     7,    11,    41,     0,
       0,     2,     0,     0,     0,    36,     0,     0,     1,    37,
      17,    16,     0,     0,     0,    34,    35,     0,    12,     0,
      53,    54,    55,    56,    52,    51,     0,    44,     0,    30,
       8,    59,    61,    62,    57,    58,    60,     0,     0,    40,
      46,    42,     0,    38,     0,     0,     0,     0,    13,    47,
      49,    50,    48,     0,    43,    19,    20,    21,     0,     0,
       0,     0,    45,    31,     9,    33,    14,     0,    10,    29,
      32,    22,    27,     0,    36,     0,    15,    23,    28,     0,
      24,     0,    25,    29,    26
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     9,    10,    20,    14,    56,    78,    15,    48,    71,
      79,    13,    68,    83,    84,    89,    91,    93,    85,    24,
      55,    77,    28,    12,    37,    16,    17,    53,    49,    63,
      36,    50
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -35
static const yytype_int8 yypact[] =
{
      28,   -35,   -35,   -35,   -35,   -35,   -35,   -35,   -27,     7,
      -4,    28,   -22,   -11,    17,   -14,    11,    19,   -35,   -35,
      28,   -35,    -3,    39,    38,   -35,   -35,     1,   -35,     1,
     -35,   -35,   -35,   -35,   -35,   -35,     1,    27,    20,   -35,
     -35,   -35,   -35,   -35,   -35,   -35,   -35,    19,    41,    18,
     -35,    18,     1,   -35,    21,    -7,    28,     1,   -35,   -35,
     -35,   -35,   -35,     1,   -35,   -35,   -35,   -35,    50,     4,
      22,    28,    18,   -35,   -35,   -35,    28,    -7,   -35,    23,
     -35,   -35,   -35,    45,   -14,    28,   -35,   -35,    28,    47,
     -35,    28,    28,    23,   -35
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -35,   -35,   -35,     0,   -35,   -35,   -35,   -35,   -35,   -35,
     -35,   -35,   -12,   -25,   -35,   -35,   -35,   -35,   -35,   -35,
     -35,   -35,   -18,   -35,   -35,   -35,   -35,   -35,   -34,   -35,
      25,   -26
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -40
static const yytype_int8 yytable[] =
{
      11,    47,    51,   -39,    41,    25,    26,    18,     1,     2,
       3,     4,     5,     6,    66,    67,    21,    74,     7,    19,
      42,    43,    44,    45,    46,    27,    64,     8,    22,    72,
      38,    70,     1,     2,     3,     4,     5,     6,    81,    82,
      23,    29,     7,    30,    31,    32,    33,    34,    35,    39,
      40,     8,    59,    60,    61,    62,    69,    52,    58,    65,
      54,    73,    75,    86,    90,    80,    87,     0,    94,     0,
       0,    76,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    88,     0,     0,     0,     0,
       0,    92
};

static const yytype_int8 yycheck[] =
{
       0,    27,    36,    30,     3,    19,    20,     0,     4,     5,
       6,     7,     8,     9,    21,    22,    38,    13,    14,    23,
      19,    20,    21,    22,    23,    39,    52,    23,    39,    63,
      33,    57,     4,     5,     6,     7,     8,     9,    15,    16,
      23,    30,    14,    24,    25,    26,    27,    28,    29,    10,
      12,    23,    34,    35,    36,    37,    56,    30,    17,    38,
      40,    11,    40,    18,    17,    77,    84,    -1,    93,    -1,
      -1,    71,    47,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    85,    -1,    -1,    -1,    -1,
      -1,    91
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     4,     5,     6,     7,     8,     9,    14,    23,    42,
      43,    44,    64,    52,    45,    48,    66,    67,     0,    23,
      44,    38,    39,    23,    60,    19,    20,    39,    63,    30,
      24,    25,    26,    27,    28,    29,    71,    65,    33,    10,
      12,     3,    19,    20,    21,    22,    23,    72,    49,    69,
      72,    69,    30,    68,    40,    61,    46,    71,    17,    34,
      35,    36,    37,    70,    72,    38,    21,    22,    53,    44,
      72,    50,    69,    11,    13,    40,    44,    62,    47,    51,
      53,    15,    16,    54,    55,    59,    18,    63,    44,    56,
      17,    57,    44,    58,    54
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}

/* Prevent warnings from -Wmissing-prototypes.  */
#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */


/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*-------------------------.
| yyparse or yypush_parse.  |
`-------------------------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{


    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.

       Refer to the stacks thru separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */
  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss_alloc, yyss);
	YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:

/* Line 1455 of yacc.c  */
#line 79 "parser.y"
    {  
    (yyval.nd_obj).nd = mknode(NULL, (yyvsp[(1) - (1)].nd_obj).nd, "program"); 
    head = (yyval.nd_obj).nd; 
;}
    break;

  case 3:

/* Line 1455 of yacc.c  */
#line 85 "parser.y"
    { insert_type(); ;}
    break;

  case 4:

/* Line 1455 of yacc.c  */
#line 86 "parser.y"
    { insert_type(); ;}
    break;

  case 5:

/* Line 1455 of yacc.c  */
#line 87 "parser.y"
    { insert_type(); ;}
    break;

  case 6:

/* Line 1455 of yacc.c  */
#line 88 "parser.y"
    { insert_type(); ;}
    break;

  case 7:

/* Line 1455 of yacc.c  */
#line 91 "parser.y"
    { add('K'); is_for = 1; ;}
    break;

  case 8:

/* Line 1455 of yacc.c  */
#line 91 "parser.y"
    { add('K'); ;}
    break;

  case 9:

/* Line 1455 of yacc.c  */
#line 91 "parser.y"
    { add('K'); ;}
    break;

  case 10:

/* Line 1455 of yacc.c  */
#line 91 "parser.y"
    {

	(yyval.nd_obj).nd = mknode((yyvsp[(3) - (8)].nd_obj3).nd, (yyvsp[(6) - (8)].nd_obj).nd, (yyvsp[(1) - (8)].nd_obj).name);	

	sprintf(icg[ic_idx++], buff);
	sprintf(icg[ic_idx++], "JUMP to %s\n", (yyvsp[(3) - (8)].nd_obj3).if_body);
    	sprintf(icg[ic_idx++], "\nLABEL %s:\n", (yyvsp[(3) - (8)].nd_obj3).else_body);
;}
    break;

  case 11:

/* Line 1455 of yacc.c  */
#line 99 "parser.y"
    { add('K');  is_for = 0; ;}
    break;

  case 12:

/* Line 1455 of yacc.c  */
#line 99 "parser.y"
    { sprintf(icg[ic_idx++], "\nLABEL %s:\n", (yyvsp[(3) - (3)].nd_obj3).if_body); ;}
    break;

  case 13:

/* Line 1455 of yacc.c  */
#line 99 "parser.y"
    { add('K'); ;}
    break;

  case 14:

/* Line 1455 of yacc.c  */
#line 99 "parser.y"
    { sprintf(icg[ic_idx++], "\nLABEL %s:\n", (yyvsp[(3) - (7)].nd_obj3).else_body); ;}
    break;

  case 15:

/* Line 1455 of yacc.c  */
#line 99 "parser.y"
    {
	struct node *iff = mknode((yyvsp[(3) - (10)].nd_obj3).nd, (yyvsp[(7) - (10)].nd_obj).nd, (yyvsp[(1) - (10)].nd_obj).name);
	(yyval.nd_obj).nd = mknode(iff, (yyvsp[(9) - (10)].nd_obj).nd, "if-else");
	sprintf(icg[ic_idx++], "GOTO next\n");
;}
    break;

  case 16:

/* Line 1455 of yacc.c  */
#line 104 "parser.y"
    { (yyval.nd_obj).nd = (yyvsp[(1) - (2)].nd_obj).nd ;}
    break;

  case 17:

/* Line 1455 of yacc.c  */
#line 105 "parser.y"
    { (yyval.nd_obj).nd = mknode((yyvsp[(1) - (2)].nd_obj).nd, (yyvsp[(2) - (2)].nd_obj).nd, "statements"); ;}
    break;

  case 18:

/* Line 1455 of yacc.c  */
#line 106 "parser.y"
    { add('K'); ;}
    break;

  case 19:

/* Line 1455 of yacc.c  */
#line 106 "parser.y"
    { (yyval.nd_obj).nd = mknode(NULL, NULL, "printf"); ;}
    break;

  case 20:

/* Line 1455 of yacc.c  */
#line 109 "parser.y"
    { strcpy((yyval.nd_obj2).name, (yyvsp[(1) - (1)].nd_obj).name); sprintf((yyval.nd_obj2).type, "INT"); add('C'); (yyval.nd_obj2).nd = mknode(NULL, NULL, (yyvsp[(1) - (1)].nd_obj).name); ;}
    break;

  case 21:

/* Line 1455 of yacc.c  */
#line 110 "parser.y"
    { strcpy((yyval.nd_obj2).name, (yyvsp[(1) - (1)].nd_obj).name); sprintf((yyval.nd_obj2).type, "REAL"); add('C'); (yyval.nd_obj2).nd = mknode(NULL, NULL, (yyvsp[(1) - (1)].nd_obj).name); ;}
    break;

  case 22:

/* Line 1455 of yacc.c  */
#line 113 "parser.y"
    { add('K'); is_for = 0; ;}
    break;

  case 23:

/* Line 1455 of yacc.c  */
#line 113 "parser.y"
    { sprintf(icg[ic_idx++], "\nLABEL %s:\n", (yyvsp[(3) - (3)].nd_obj3).if_body); ;}
    break;

  case 24:

/* Line 1455 of yacc.c  */
#line 113 "parser.y"
    { add('K'); ;}
    break;

  case 25:

/* Line 1455 of yacc.c  */
#line 113 "parser.y"
    { sprintf(icg[ic_idx++], "\nLABEL %s:\n", (yyvsp[(3) - (7)].nd_obj3).else_body); ;}
    break;

  case 26:

/* Line 1455 of yacc.c  */
#line 113 "parser.y"
    {
	struct node *elif = mknode((yyvsp[(3) - (9)].nd_obj3).nd, (yyvsp[(7) - (9)].nd_obj).nd, (yyvsp[(1) - (9)].nd_obj).name);
	(yyval.nd_obj).nd = mknode(elif, (yyvsp[(9) - (9)].nd_obj).nd, "elif-else");
	sprintf(icg[ic_idx++], "GOTO next\n");
;}
    break;

  case 27:

/* Line 1455 of yacc.c  */
#line 118 "parser.y"
    { add('K'); ;}
    break;

  case 28:

/* Line 1455 of yacc.c  */
#line 118 "parser.y"
    { (yyval.nd_obj).nd = mknode(NULL, (yyvsp[(3) - (3)].nd_obj).nd, (yyvsp[(1) - (3)].nd_obj).name); ;}
    break;

  case 29:

/* Line 1455 of yacc.c  */
#line 119 "parser.y"
    { (yyval.nd_obj).nd = NULL; ;}
    break;

  case 30:

/* Line 1455 of yacc.c  */
#line 122 "parser.y"
    { add('K'); ;}
    break;

  case 31:

/* Line 1455 of yacc.c  */
#line 122 "parser.y"
    { add('K'); ;}
    break;

  case 32:

/* Line 1455 of yacc.c  */
#line 122 "parser.y"
    {
	
	//declaration of variable
	(yyvsp[(1) - (7)].nd_obj).nd = mknode(NULL, NULL, (yyvsp[(1) - (7)].nd_obj).name);
	struct var_name nd1;
	nd1.nd = mknode((yyvsp[(1) - (7)].nd_obj).nd, (yyvsp[(4) - (7)].nd_obj2).nd, "declaration");
	sprintf(icg[ic_idx++], "%s = %s\n", (yyvsp[(1) - (7)].nd_obj).name, (yyvsp[(4) - (7)].nd_obj2).name);
	
	//for condition
	struct node *new1 = mknode(NULL, NULL, (yyvsp[(1) - (7)].nd_obj).name);
	struct var_name newID1;
	strcpy(newID1.name, (yyvsp[(1) - (7)].nd_obj).name);
	newID1.nd = new1;
	struct var_name3 nd2;
	nd2.nd = mknode(newID1.nd, (yyvsp[(7) - (7)].nd_obj2).nd, "<"); 
	sprintf(nd2.if_body, "L%d", label++);
	sprintf(icg[ic_idx++], "\nLABEL %s:\n", nd2.if_body);
	sprintf(icg[ic_idx++], "\nif NOT (%s %s %s) GOTO L%d\n", newID1.name, "<", (yyvsp[(7) - (7)].nd_obj2).name, label);
	sprintf(nd2.else_body, "L%d", label++);

	//increment
	struct node *new1_1 = mknode(NULL, NULL, (yyvsp[(1) - (7)].nd_obj).name);
	struct var_name newID1_1;
	strcpy(newID1_1.name, (yyvsp[(1) - (7)].nd_obj).name);
	newID1_1.nd = new1_1;
	struct var_name nd3;
	nd3.nd = mknode(newID1_1.nd, NULL, "iterator");
	if(atoi((yyvsp[(4) - (7)].nd_obj2).name) <= atoi((yyvsp[(7) - (7)].nd_obj2).name)){
		sprintf(buff, "t%d = %s + 1\n%s = t%d\n", temp_var, (yyvsp[(1) - (7)].nd_obj).name, (yyvsp[(1) - (7)].nd_obj).name, temp_var);
		temp_var++;
	}
	else{
		sprintf(buff, "t%d = %s - 1\n%s = t%d\n", temp_var, (yyvsp[(1) - (7)].nd_obj).name, (yyvsp[(1) - (7)].nd_obj).name, temp_var);
		temp_var++;
	}
	
	struct node *nd4 = mknode(nd2.nd, nd3.nd, "constraints");
	(yyval.nd_obj3).nd = mknode(nd1.nd, nd4, "condition");
	sprintf((yyval.nd_obj3).if_body, nd2.if_body);
	sprintf((yyval.nd_obj3).else_body, nd2.else_body);
;}
    break;

  case 33:

/* Line 1455 of yacc.c  */
#line 165 "parser.y"
    {
	(yyval.nd_obj3).nd = mknode((yyvsp[(2) - (5)].nd_obj2).nd, (yyvsp[(4) - (5)].nd_obj2).nd, (yyvsp[(3) - (5)].nd_obj).name);
	if(is_for) {
		sprintf((yyval.nd_obj3).if_body, "L%d", label++);
		sprintf(icg[ic_idx++], "\nLABEL %s:\n", (yyval.nd_obj3).if_body);
		sprintf(icg[ic_idx++], "\nif NOT (%s %s %s) GOTO L%d\n", (yyvsp[(2) - (5)].nd_obj2).name, (yyvsp[(3) - (5)].nd_obj).name, (yyvsp[(4) - (5)].nd_obj2).name, label);
		sprintf((yyval.nd_obj3).else_body, "L%d", label++);
	} else {
		sprintf(icg[ic_idx++], "\nif (%s %s %s) GOTO L%d else GOTO L%d\n", (yyvsp[(2) - (5)].nd_obj2).name, (yyvsp[(3) - (5)].nd_obj).name, (yyvsp[(4) - (5)].nd_obj2).name, label, label+1);
		sprintf((yyval.nd_obj3).if_body, "L%d", label++);
		sprintf((yyval.nd_obj3).else_body, "L%d", label++);
	}
;}
    break;

  case 34:

/* Line 1455 of yacc.c  */
#line 178 "parser.y"
    { add('K'); (yyval.nd_obj3).nd = NULL; ;}
    break;

  case 35:

/* Line 1455 of yacc.c  */
#line 179 "parser.y"
    { add('K'); (yyval.nd_obj3).nd = NULL; ;}
    break;

  case 36:

/* Line 1455 of yacc.c  */
#line 180 "parser.y"
    { (yyval.nd_obj3).nd = NULL; ;}
    break;

  case 37:

/* Line 1455 of yacc.c  */
#line 183 "parser.y"
    { add('V'); ;}
    break;

  case 38:

/* Line 1455 of yacc.c  */
#line 183 "parser.y"
    {
	(yyvsp[(2) - (4)].nd_obj).nd = mknode(NULL, NULL, (yyvsp[(2) - (4)].nd_obj).name);
	int t = check_types((yyvsp[(1) - (4)].nd_obj).name, (yyvsp[(4) - (4)].nd_obj2).type);
	if(t>0){
		struct node *temp;
		if(t == 1){
			struct node *temp = mknode(NULL, (yyvsp[(4) - (4)].nd_obj2).nd, "realtoint");
		}
		else if(t == 2){
			temp = mknode(NULL, (yyvsp[(4) - (4)].nd_obj2).nd, "inttoreal");
		}
		else if(t == 3){
			temp = mknode(NULL, (yyvsp[(4) - (4)].nd_obj2).nd, "chartoint");
		}
		else if(t == 4){
			temp = mknode(NULL, (yyvsp[(4) - (4)].nd_obj2).nd, "inttochar");
		}
		else if(t == 5){
			temp = mknode(NULL, (yyvsp[(4) - (4)].nd_obj2).nd, "chartoreal");
		}
		else{
			temp = mknode(NULL, (yyvsp[(4) - (4)].nd_obj2).nd, "realtochar");
		}
		(yyval.nd_obj).nd = mknode((yyvsp[(2) - (4)].nd_obj).nd, temp, "declaration");
	}
	else{
		(yyval.nd_obj).nd = mknode((yyvsp[(2) - (4)].nd_obj).nd, (yyvsp[(4) - (4)].nd_obj2).nd, "declaration");	
	}
	sprintf(icg[ic_idx++], "%s = %s\n", (yyvsp[(2) - (4)].nd_obj).name, (yyvsp[(4) - (4)].nd_obj2).name);
;}
    break;

  case 39:

/* Line 1455 of yacc.c  */
#line 213 "parser.y"
    { check_declaration((yyvsp[(1) - (1)].nd_obj).name); ;}
    break;

  case 40:

/* Line 1455 of yacc.c  */
#line 213 "parser.y"
    {
	(yyvsp[(1) - (4)].nd_obj).nd = mknode(NULL, NULL, (yyvsp[(1) - (4)].nd_obj).name);
	char *id_type = get_type((yyvsp[(1) - (4)].nd_obj).name);
	if(strcmp(id_type, (yyvsp[(4) - (4)].nd_obj2).type)){
		struct node *temp;
		if(!strcmp(id_type, "INT")){
			if(!strcmp(id_type, "REAL")){
				temp = mknode(NULL, (yyvsp[(4) - (4)].nd_obj2).nd, "realtoint");
			}
			else{
				temp = mknode(NULL, (yyvsp[(4) - (4)].nd_obj2).nd, "chartoint");	
			}
		}
		else if(!strcmp(id_type, "REAL")) {
			if(!strcmp((yyvsp[(4) - (4)].nd_obj2).type, "INT")){
				temp = mknode(NULL, (yyvsp[(4) - (4)].nd_obj2).nd, "inttoreal");
			}
			else{
				temp = mknode(NULL, (yyvsp[(4) - (4)].nd_obj2).nd, "chartoreal");
			}	 
		}
		else{
			if(!strcmp((yyvsp[(4) - (4)].nd_obj2).type, "INT")){
				temp = mknode(NULL, (yyvsp[(4) - (4)].nd_obj2).nd, "inttochar");
			}
			else{
				temp = mknode(NULL, (yyvsp[(4) - (4)].nd_obj2).nd, "realtochar");
			}
		}
		(yyval.nd_obj).nd = mknode((yyvsp[(1) - (4)].nd_obj).nd, temp, (yyvsp[(3) - (4)].nd_obj).name);
	}
	else{
		(yyval.nd_obj).nd = mknode((yyvsp[(1) - (4)].nd_obj).nd, (yyvsp[(4) - (4)].nd_obj2).nd, (yyvsp[(3) - (4)].nd_obj).name);
	}
	sprintf(icg[ic_idx++], "%s = %s\n", (yyvsp[(1) - (4)].nd_obj).name, (yyvsp[(4) - (4)].nd_obj2).name);
;}
    break;

  case 41:

/* Line 1455 of yacc.c  */
#line 249 "parser.y"
    { check_declaration((yyvsp[(1) - (1)].nd_obj).name); ;}
    break;

  case 42:

/* Line 1455 of yacc.c  */
#line 249 "parser.y"
    {
	(yyvsp[(1) - (4)].nd_obj).nd = mknode(NULL, NULL, (yyvsp[(1) - (4)].nd_obj).name); 
    	(yyval.nd_obj).nd = mknode((yyvsp[(1) - (4)].nd_obj).nd, (yyvsp[(4) - (4)].nd_obj2).nd, (yyvsp[(3) - (4)].nd_obj).name);
;}
    break;

  case 43:

/* Line 1455 of yacc.c  */
#line 255 "parser.y"
    {
	(yyval.nd_obj2).nd = (yyvsp[(2) - (2)].nd_obj2).nd;
	sprintf((yyval.nd_obj2).type, (yyvsp[(2) - (2)].nd_obj2).type);
	strcpy((yyval.nd_obj2).name, (yyvsp[(2) - (2)].nd_obj2).name);
;}
    break;

  case 44:

/* Line 1455 of yacc.c  */
#line 260 "parser.y"
    {  sprintf((yyval.nd_obj2).type, "null"); (yyval.nd_obj2).nd = mknode(NULL, NULL, "NULL"); strcpy((yyval.nd_obj2).name, "NULL"); ;}
    break;

  case 45:

/* Line 1455 of yacc.c  */
#line 263 "parser.y"
    {
	if(!strcmp((yyvsp[(1) - (3)].nd_obj2).type, (yyvsp[(3) - (3)].nd_obj2).type)) {
		sprintf((yyval.nd_obj2).type, (yyvsp[(1) - (3)].nd_obj2).type);
		(yyval.nd_obj2).nd = mknode((yyvsp[(1) - (3)].nd_obj2).nd, (yyvsp[(3) - (3)].nd_obj2).nd, (yyvsp[(2) - (3)].nd_obj).name); 
	}
	else {
		if(!strcmp((yyvsp[(1) - (3)].nd_obj2).type, "INT") && !strcmp((yyvsp[(3) - (3)].nd_obj2).type, "REAL")) {
			struct node *temp = mknode(NULL, (yyvsp[(1) - (3)].nd_obj2).nd, "inttoreal");
			sprintf((yyval.nd_obj2).type, (yyvsp[(3) - (3)].nd_obj2).type);
			(yyval.nd_obj2).nd = mknode(temp, (yyvsp[(3) - (3)].nd_obj2).nd, (yyvsp[(2) - (3)].nd_obj).name);
		}
		else if(!strcmp((yyvsp[(1) - (3)].nd_obj2).type, "REAL") && !strcmp((yyvsp[(3) - (3)].nd_obj2).type, "INT")) {
			struct node *temp = mknode(NULL, (yyvsp[(3) - (3)].nd_obj2).nd, "inttoreal");
			sprintf((yyval.nd_obj2).type, (yyvsp[(1) - (3)].nd_obj2).type);
			(yyval.nd_obj2).nd = mknode((yyvsp[(1) - (3)].nd_obj2).nd, temp, (yyvsp[(2) - (3)].nd_obj).name);
		}
		else if(!strcmp((yyvsp[(1) - (3)].nd_obj2).type, "INT") && !strcmp((yyvsp[(3) - (3)].nd_obj2).type, "CHAR")) {
			struct node *temp = mknode(NULL, (yyvsp[(3) - (3)].nd_obj2).nd, "chartoint");
			sprintf((yyval.nd_obj2).type, (yyvsp[(1) - (3)].nd_obj2).type);
			(yyval.nd_obj2).nd = mknode((yyvsp[(1) - (3)].nd_obj2).nd, temp, (yyvsp[(2) - (3)].nd_obj).name);
		}
		else if(!strcmp((yyvsp[(1) - (3)].nd_obj2).type, "CHAR") && !strcmp((yyvsp[(3) - (3)].nd_obj2).type, "INT")) {
			struct node *temp = mknode(NULL, (yyvsp[(1) - (3)].nd_obj2).nd, "chartoint");
			sprintf((yyval.nd_obj2).type, (yyvsp[(3) - (3)].nd_obj2).type);
			(yyval.nd_obj2).nd = mknode(temp, (yyvsp[(3) - (3)].nd_obj2).nd, (yyvsp[(2) - (3)].nd_obj).name);
		}
		else if(!strcmp((yyvsp[(1) - (3)].nd_obj2).type, "REAL") && !strcmp((yyvsp[(3) - (3)].nd_obj2).type, "CHAR")) {
			struct node *temp = mknode(NULL, (yyvsp[(3) - (3)].nd_obj2).nd, "chartoreal");
			sprintf((yyval.nd_obj2).type, (yyvsp[(1) - (3)].nd_obj2).type);
			(yyval.nd_obj2).nd = mknode((yyvsp[(1) - (3)].nd_obj2).nd, temp, (yyvsp[(2) - (3)].nd_obj).name);
		}
		else {
			struct node *temp = mknode(NULL, (yyvsp[(1) - (3)].nd_obj2).nd, "chartoreal");
			sprintf((yyval.nd_obj2).type, (yyvsp[(3) - (3)].nd_obj2).type);
			(yyval.nd_obj2).nd = mknode(temp, (yyvsp[(3) - (3)].nd_obj2).nd, (yyvsp[(2) - (3)].nd_obj).name);
		}
	}
	sprintf((yyval.nd_obj2).name, "t%d", temp_var);
	temp_var++;
	sprintf(icg[ic_idx++], "%s = %s %s %s\n",  (yyval.nd_obj2).name, (yyvsp[(1) - (3)].nd_obj2).name, (yyvsp[(2) - (3)].nd_obj).name, (yyvsp[(3) - (3)].nd_obj2).name);
;}
    break;

  case 46:

/* Line 1455 of yacc.c  */
#line 304 "parser.y"
    { 
	strcpy((yyval.nd_obj2).name, (yyvsp[(1) - (1)].nd_obj2).name);
	sprintf((yyval.nd_obj2).type, (yyvsp[(1) - (1)].nd_obj2).type);
	(yyval.nd_obj2).nd = (yyvsp[(1) - (1)].nd_obj2).nd;
;}
    break;

  case 57:

/* Line 1455 of yacc.c  */
#line 325 "parser.y"
    { strcpy((yyval.nd_obj2).name, (yyvsp[(1) - (1)].nd_obj).name); sprintf((yyval.nd_obj2).type, "INT"); add('C'); (yyval.nd_obj2).nd = mknode(NULL, NULL, (yyvsp[(1) - (1)].nd_obj).name); ;}
    break;

  case 58:

/* Line 1455 of yacc.c  */
#line 326 "parser.y"
    { strcpy((yyval.nd_obj2).name, (yyvsp[(1) - (1)].nd_obj).name); sprintf((yyval.nd_obj2).type, "REAL"); add('C'); (yyval.nd_obj2).nd = mknode(NULL, NULL, (yyvsp[(1) - (1)].nd_obj).name); ;}
    break;

  case 59:

/* Line 1455 of yacc.c  */
#line 327 "parser.y"
    { strcpy((yyval.nd_obj2).name, (yyvsp[(1) - (1)].nd_obj).name); sprintf((yyval.nd_obj2).type, "CHAR"); add('C'); (yyval.nd_obj2).nd = mknode(NULL, NULL, (yyvsp[(1) - (1)].nd_obj).name); ;}
    break;

  case 60:

/* Line 1455 of yacc.c  */
#line 328 "parser.y"
    { strcpy((yyval.nd_obj2).name, (yyvsp[(1) - (1)].nd_obj).name); char *id_type = get_type((yyvsp[(1) - (1)].nd_obj).name); sprintf((yyval.nd_obj2).type, id_type); check_declaration((yyvsp[(1) - (1)].nd_obj).name); (yyval.nd_obj2).nd = mknode(NULL, NULL, (yyvsp[(1) - (1)].nd_obj).name); ;}
    break;

  case 61:

/* Line 1455 of yacc.c  */
#line 329 "parser.y"
    { add('K'); (yyval.nd_obj2).nd = NULL; ;}
    break;

  case 62:

/* Line 1455 of yacc.c  */
#line 330 "parser.y"
    { add('K'); (yyval.nd_obj2).nd = NULL; ;}
    break;



/* Line 1455 of yacc.c  */
#line 2056 "parser.tab.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined(yyoverflow) || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}



/* Line 1675 of yacc.c  */
#line 333 "parser.y"


int main() {
    yyparse();

    printf("\n\n");
	printf("\t\tPHASE 1: LEXICAL ANALYSIS \n");
	printf("\n\tSYMBOL   DATATYPE   TYPE   LINE NUMBER \n");
	printf("\t_______________________________________\n\n");
	int i=0;
	for(i=0; i<count; i++) {
		printf("\t%s\t%s\t%s\t%d\t\n", symbol_table[i].id_name, symbol_table[i].data_type, symbol_table[i].type, symbol_table[i].line_no);
	}
	for(i=0;i<count;i++) {
		free(symbol_table[i].id_name);
		free(symbol_table[i].type);
	}
	printf("\n\n");
	printf("\t\t\t PHASE 2: SYNTAX ANALYSIS");
	printtree(head); 
	printf("\n\n\n");
	printf("\t\t\t PHASE 3: SEMANTIC ANALYSIS \n\n");
	if(sem_errors>0) {
		printf("Semantic analysis completed with %d errors\n", sem_errors);
		for(int i=0; i<sem_errors; i++){
			printf("\t - %s", errors[i]);
		}
	} else {
		printf("Semantic analysis completed with no errors");
	}
	printf("\n\n\n");
	printf("\t\t\t PHASE 4: INTERMEDIATE CODE GENERATION \n\n");
	for(int i=0; i<ic_idx; i++){
		printf("%s", icg[i]);
	}
	printf("\n\n");
}

int search(char *type) {
	int i;
	for(i=count-1; i>=0; i--) {
		if(strcmp(symbol_table[i].id_name, type)==0) {
			return -1;
			break;
		}
	}
	return 0;
}

void check_declaration(char *c) {    
    q = search(c);    
    if(!q) {        
        sprintf(errors[sem_errors], "Line %d: Variable \"%s\" not declared before usage!\n", countn+1, c);  
        sem_errors++;    
    }
}

void check_return_type(char *value) {
	char *main_datatype = get_type("main");
	char *return_datatype = get_type(value);
	if((!strcmp(main_datatype, "INT") && !strcmp(return_datatype, "CONST")) || !strcmp(main_datatype, return_datatype)){
		return ;
	}
	else {
		sprintf(errors[sem_errors], "Line %d: Return type mismatch\n", countn+1);
		sem_errors++;
	}
}

int check_types(char *type1, char *type2){
	// declaration with no init
	if(!strcmp(type2, "null"))
		return -1;
	// both datatypes are same
	if(!strcmp(type1, type2))
		return 0;
	// both datatypes are different
	if(!strcmp(type1, "INT") && !strcmp(type2, "REAL"))
		return 1;
	if(!strcmp(type1, "REAL") && !strcmp(type2, "INT"))
		return 2;
	if(!strcmp(type1, "INT") && !strcmp(type2, "CHAR"))
		return 3;
	if(!strcmp(type1, "CHAR") && !strcmp(type2, "INT"))
		return 4;
	if(!strcmp(type1, "REAL") && !strcmp(type2, "CHAR"))
		return 5;
	if(!strcmp(type1, "CHAR") && !strcmp(type2, "FLOAT"))
		return 6;
}

char *get_type(char *var){
	for(int i=0; i<count; i++) {
		// Handle case of use before declaration
		if(!strcmp(symbol_table[i].id_name, var)) {
			return symbol_table[i].data_type;
		}
	}
}

void add(char c) {
  if(c == 'V'){
	for(int i=0; i<10; i++){
		if(!strcmp(reserved[i], strdup(yytext))){
        		sprintf(errors[sem_errors], "Line %d: Variable name \"%s\" is a reserved keyword!\n", countn+1, yytext);
			sem_errors++;
			return;
		}
	}
  }
  q=search(yytext);
  if(!q) {
    if(c == 'K') {
		symbol_table[count].id_name=strdup(yytext);
		symbol_table[count].data_type=strdup("N/A");
		symbol_table[count].line_no=countn;
		symbol_table[count].type=strdup("Keyword\t");
		count++;
	}
	else if(c == 'V') {
		symbol_table[count].id_name=strdup(yytext);
		symbol_table[count].data_type=strdup(type);
		symbol_table[count].line_no=countn;
		symbol_table[count].type=strdup("Variable");
		count++;
	}
	else if(c == 'C') {
		symbol_table[count].id_name=strdup(yytext);
		symbol_table[count].data_type=strdup("CONST");
		symbol_table[count].line_no=countn;
		symbol_table[count].type=strdup("Constant");
		count++;
	}
  }
  else if(c == 'V' && q){
  	sprintf(errors[sem_errors], "Line %d: Multiple declarations of \"%s\" not allowed!\n", countn+1, yytext);
	sem_errors++;
  }
}

struct node* mknode(struct node *left, struct node *right, char *token) {	
	struct node *newnode = (struct node *)malloc(sizeof(struct node));
	char *newstr = (char *)malloc(strlen(token)+1);
	strcpy(newstr, token);
	newnode->left = left;
	newnode->right = right;
	newnode->token = newstr;
	return(newnode);
}

void printtree(struct node* tree) {
	printf("\n\n Inorder traversal of the Parse Tree: \n\n");
	printInorder(tree);
	printf("\n\n");
}

void printInorder(struct node *tree) {
	int i;
	if (tree->left) {
		printInorder(tree->left);
	}
	printf("%s, ", tree->token);
	if (tree->right) {
		printInorder(tree->right);
	}
}

void insert_type() {
	strcpy(type, yytext);
}


void yyerror(const char* msg) {
    fprintf(stderr, "Line number = %d %s\n", countn, msg);
}
