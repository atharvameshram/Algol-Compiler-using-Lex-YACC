%{
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
%}

%union { 
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
}

%token <nd_obj> CHARACTER PRINTFF INT REAL CHAR BOOL FOR FROM TO DO OD IF ELIF ELSE THEN FI TRUE FALSE NUMBER REAL_NUM ID LE GE EQ NE GT LT ASSIGN AND OR STR ADD MULTIPLY DIVIDE SUBTRACT
%type <nd_obj> program datatype body else statement arithmetic relop
%type <nd_obj2> init value expression num_value
%type <nd_obj3> condition for_condn

%%

program: body {  
    $$.nd = mknode(NULL, $1.nd, "program"); 
    head = $$.nd; 
}
;

datatype: INT { insert_type(); }
| REAL { insert_type(); }
| CHAR { insert_type(); }
| BOOL { insert_type(); }
;

body: FOR { add('K'); is_for = 1; } for_condn DO { add('K'); } body OD { add('K'); } {

	$$.nd = mknode($3.nd, $6.nd, $1.name);	

	sprintf(icg[ic_idx++], buff);
	sprintf(icg[ic_idx++], "JUMP to %s\n", $3.if_body);
    	sprintf(icg[ic_idx++], "\nLABEL %s:\n", $3.else_body);
}
| IF { add('K');  is_for = 0; } condition { sprintf(icg[ic_idx++], "\nLABEL %s:\n", $3.if_body); } THEN { add('K'); } body { sprintf(icg[ic_idx++], "\nLABEL %s:\n", $3.else_body); } else FI {
	struct node *iff = mknode($3.nd, $7.nd, $1.name);
	$$.nd = mknode(iff, $9.nd, "if-else");
	sprintf(icg[ic_idx++], "GOTO next\n");
}
| statement ';' { $$.nd = $1.nd }
| body body { $$.nd = mknode($1.nd, $2.nd, "statements"); }
| PRINTFF { add('K'); } '(' STR ')' ';' { $$.nd = mknode(NULL, NULL, "printf"); }
;

num_value: NUMBER { strcpy($$.name, $1.name); sprintf($$.type, "INT"); add('C'); $$.nd = mknode(NULL, NULL, $1.name); }
| REAL_NUM { strcpy($$.name, $1.name); sprintf($$.type, "REAL"); add('C'); $$.nd = mknode(NULL, NULL, $1.name); }
;

else: ELIF { add('K'); is_for = 0; } condition { sprintf(icg[ic_idx++], "\nLABEL %s:\n", $3.if_body); } THEN { add('K'); } body { sprintf(icg[ic_idx++], "\nLABEL %s:\n", $3.else_body); } else {
	struct node *elif = mknode($3.nd, $7.nd, $1.name);
	$$.nd = mknode(elif, $9.nd, "elif-else");
	sprintf(icg[ic_idx++], "GOTO next\n");
}
| ELSE { add('K'); } body { $$.nd = mknode(NULL, $3.nd, $1.name); }
| { $$.nd = NULL; }
;

for_condn: ID FROM { add('K'); } num_value TO { add('K'); } num_value {
	
	//declaration of variable
	$1.nd = mknode(NULL, NULL, $1.name);
	struct var_name nd1;
	nd1.nd = mknode($1.nd, $4.nd, "declaration");
	sprintf(icg[ic_idx++], "%s = %s\n", $1.name, $4.name);
	
	//for condition
	struct node *new1 = mknode(NULL, NULL, $1.name);
	struct var_name newID1;
	strcpy(newID1.name, $1.name);
	newID1.nd = new1;
	struct var_name3 nd2;
	nd2.nd = mknode(newID1.nd, $7.nd, "<"); 
	sprintf(nd2.if_body, "L%d", label++);
	sprintf(icg[ic_idx++], "\nLABEL %s:\n", nd2.if_body);
	sprintf(icg[ic_idx++], "\nif NOT (%s %s %s) GOTO L%d\n", newID1.name, "<", $7.name, label);
	sprintf(nd2.else_body, "L%d", label++);

	//increment
	struct node *new1_1 = mknode(NULL, NULL, $1.name);
	struct var_name newID1_1;
	strcpy(newID1_1.name, $1.name);
	newID1_1.nd = new1_1;
	struct var_name nd3;
	nd3.nd = mknode(newID1_1.nd, NULL, "iterator");
	if(atoi($4.name) <= atoi($7.name)){
		sprintf(buff, "t%d = %s + 1\n%s = t%d\n", temp_var, $1.name, $1.name, temp_var);
		temp_var++;
	}
	else{
		sprintf(buff, "t%d = %s - 1\n%s = t%d\n", temp_var, $1.name, $1.name, temp_var);
		temp_var++;
	}
	
	struct node *nd4 = mknode(nd2.nd, nd3.nd, "constraints");
	$$.nd = mknode(nd1.nd, nd4, "condition");
	sprintf($$.if_body, nd2.if_body);
	sprintf($$.else_body, nd2.else_body);
}
;

condition: '(' value relop value ')' {
	$$.nd = mknode($2.nd, $4.nd, $3.name);
	if(is_for) {
		sprintf($$.if_body, "L%d", label++);
		sprintf(icg[ic_idx++], "\nLABEL %s:\n", $$.if_body);
		sprintf(icg[ic_idx++], "\nif NOT (%s %s %s) GOTO L%d\n", $2.name, $3.name, $4.name, label);
		sprintf($$.else_body, "L%d", label++);
	} else {
		sprintf(icg[ic_idx++], "\nif (%s %s %s) GOTO L%d else GOTO L%d\n", $2.name, $3.name, $4.name, label, label+1);
		sprintf($$.if_body, "L%d", label++);
		sprintf($$.else_body, "L%d", label++);
	}
}
| TRUE { add('K'); $$.nd = NULL; }
| FALSE { add('K'); $$.nd = NULL; }
| { $$.nd = NULL; }
;

statement: datatype ID { add('V'); } init {
	$2.nd = mknode(NULL, NULL, $2.name);
	int t = check_types($1.name, $4.type);
	if(t>0){
		struct node *temp;
		if(t == 1){
			struct node *temp = mknode(NULL, $4.nd, "realtoint");
		}
		else if(t == 2){
			temp = mknode(NULL, $4.nd, "inttoreal");
		}
		else if(t == 3){
			temp = mknode(NULL, $4.nd, "chartoint");
		}
		else if(t == 4){
			temp = mknode(NULL, $4.nd, "inttochar");
		}
		else if(t == 5){
			temp = mknode(NULL, $4.nd, "chartoreal");
		}
		else{
			temp = mknode(NULL, $4.nd, "realtochar");
		}
		$$.nd = mknode($2.nd, temp, "declaration");
	}
	else{
		$$.nd = mknode($2.nd, $4.nd, "declaration");	
	}
	sprintf(icg[ic_idx++], "%s = %s\n", $2.name, $4.name);
}
| ID { check_declaration($1.name); } ASSIGN expression {
	$1.nd = mknode(NULL, NULL, $1.name);
	char *id_type = get_type($1.name);
	if(strcmp(id_type, $4.type)){
		struct node *temp;
		if(!strcmp(id_type, "INT")){
			if(!strcmp(id_type, "REAL")){
				temp = mknode(NULL, $4.nd, "realtoint");
			}
			else{
				temp = mknode(NULL, $4.nd, "chartoint");	
			}
		}
		else if(!strcmp(id_type, "REAL")) {
			if(!strcmp($4.type, "INT")){
				temp = mknode(NULL, $4.nd, "inttoreal");
			}
			else{
				temp = mknode(NULL, $4.nd, "chartoreal");
			}	 
		}
		else{
			if(!strcmp($4.type, "INT")){
				temp = mknode(NULL, $4.nd, "inttochar");
			}
			else{
				temp = mknode(NULL, $4.nd, "realtochar");
			}
		}
		$$.nd = mknode($1.nd, temp, $3.name);
	}
	else{
		$$.nd = mknode($1.nd, $4.nd, $3.name);
	}
	sprintf(icg[ic_idx++], "%s = %s\n", $1.name, $4.name);
}
| ID { check_declaration($1.name); } relop expression{
	$1.nd = mknode(NULL, NULL, $1.name); 
    	$$.nd = mknode($1.nd, $4.nd, $3.name);
}
;

init: ASSIGN value {
	$$.nd = $2.nd;
	sprintf($$.type, $2.type);
	strcpy($$.name, $2.name);
}
| {  sprintf($$.type, "null"); $$.nd = mknode(NULL, NULL, "NULL"); strcpy($$.name, "NULL"); }
;

expression: expression arithmetic expression {
	if(!strcmp($1.type, $3.type)) {
		sprintf($$.type, $1.type);
		$$.nd = mknode($1.nd, $3.nd, $2.name); 
	}
	else {
		if(!strcmp($1.type, "INT") && !strcmp($3.type, "REAL")) {
			struct node *temp = mknode(NULL, $1.nd, "inttoreal");
			sprintf($$.type, $3.type);
			$$.nd = mknode(temp, $3.nd, $2.name);
		}
		else if(!strcmp($1.type, "REAL") && !strcmp($3.type, "INT")) {
			struct node *temp = mknode(NULL, $3.nd, "inttoreal");
			sprintf($$.type, $1.type);
			$$.nd = mknode($1.nd, temp, $2.name);
		}
		else if(!strcmp($1.type, "INT") && !strcmp($3.type, "CHAR")) {
			struct node *temp = mknode(NULL, $3.nd, "chartoint");
			sprintf($$.type, $1.type);
			$$.nd = mknode($1.nd, temp, $2.name);
		}
		else if(!strcmp($1.type, "CHAR") && !strcmp($3.type, "INT")) {
			struct node *temp = mknode(NULL, $1.nd, "chartoint");
			sprintf($$.type, $3.type);
			$$.nd = mknode(temp, $3.nd, $2.name);
		}
		else if(!strcmp($1.type, "REAL") && !strcmp($3.type, "CHAR")) {
			struct node *temp = mknode(NULL, $3.nd, "chartoreal");
			sprintf($$.type, $1.type);
			$$.nd = mknode($1.nd, temp, $2.name);
		}
		else {
			struct node *temp = mknode(NULL, $1.nd, "chartoreal");
			sprintf($$.type, $3.type);
			$$.nd = mknode(temp, $3.nd, $2.name);
		}
	}
	sprintf($$.name, "t%d", temp_var);
	temp_var++;
	sprintf(icg[ic_idx++], "%s = %s %s %s\n",  $$.name, $1.name, $2.name, $3.name);
}
| value { 
	strcpy($$.name, $1.name);
	sprintf($$.type, $1.type);
	$$.nd = $1.nd;
}
;

arithmetic: ADD 
| SUBTRACT 
| MULTIPLY
| DIVIDE
;

relop: LT
| GT
| LE
| GE
| EQ
| NE
;

value: NUMBER { strcpy($$.name, $1.name); sprintf($$.type, "INT"); add('C'); $$.nd = mknode(NULL, NULL, $1.name); }
| REAL_NUM { strcpy($$.name, $1.name); sprintf($$.type, "REAL"); add('C'); $$.nd = mknode(NULL, NULL, $1.name); }
| CHARACTER { strcpy($$.name, $1.name); sprintf($$.type, "CHAR"); add('C'); $$.nd = mknode(NULL, NULL, $1.name); }
| ID { strcpy($$.name, $1.name); char *id_type = get_type($1.name); sprintf($$.type, id_type); check_declaration($1.name); $$.nd = mknode(NULL, NULL, $1.name); }
| TRUE { add('K'); $$.nd = NULL; }
| FALSE { add('K'); $$.nd = NULL; }
;

%%

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