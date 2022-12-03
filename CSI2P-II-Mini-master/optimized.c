#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

/*
For the language grammar, please refer to Grammar section on the github page:
  https://github.com/lightbulb12294/CSI2P-II-Mini1#grammar
*/

#define MAX_LENGTH 200
typedef enum {
	IDY, NUM, COM
} data_kind; 
typedef enum {
	ASSIGN, ADD, SUB, MUL, DIV, REM, PREINC, PREDEC, POSTINC, POSTDEC, IDENTIFIER, CONSTANT, LPAR, RPAR, PLUS, MINUS, END
} Kind;
typedef enum {
	STMT, EXPR, ASSIGN_EXPR, ADD_EXPR, MUL_EXPR, UNARY_EXPR, POSTFIX_EXPR, PRI_EXPR
} GrammarState;
typedef struct TokenUnit {
	Kind kind;
	int val; // record the integer value or variable name
	struct TokenUnit *next;
} Token;
typedef struct ASTUnit {
	Kind kind;
	int val; // record the integer value or variable name
	struct ASTUnit *lhs, *mid, *rhs;
} AST;
typedef struct data_{
	data_kind kind;
	int reg; 
	int val; 
} data; 
typedef struct result_
{
	Kind kind; 
	int val; 
	int reg; 
	int flag_dec; 
	int flag_inc; 

} result;


/// utility interfaces

// err marco should be used when a expression error occurs.
#define err(x) {\
	puts("Compile Error!");\
	if(DEBUG) {\
		fprintf(stderr, "Error at line: %d\n", __LINE__);\
		fprintf(stderr, "Error message: %s\n", x);\
	}\
	exit(0);\
}
// You may set DEBUG=1 to debug. Remember setting back to 0 before submit.
#define DEBUG 0
// Split the input char array into token linked list.
Token *lexer(const char *in);
// Create a new token.
Token *new_token(Kind kind, int val);
// Translate a token linked list into array, return its length.
size_t token_list_to_arr(Token **head);
// Parse the token array. Return the constructed AST.
AST *parser(Token *arr, size_t len);
// Parse the token array. Return the constructed AST.
AST *parse(Token *arr, int l, int r, GrammarState S);
// Create a new AST node.
AST *new_AST(Kind kind, int val);
// Find the location of next token that fits the condition(cond). Return -1 if not found. Search direction from start to end.
int findNextSection(Token *arr, int start, int end, int (*cond)(Kind));
// Return 1 if kind is ASSIGN.
int condASSIGN(Kind kind);
// Return 1 if kind is ADD or SUB.
int condADD(Kind kind);
// Return 1 if kind is MUL, DIV, or REM.
int condMUL(Kind kind);
// Return 1 if kind is RPAR.
int condRPAR(Kind kind);
// Check if the AST is semantically right. This function will call err() automatically if check failed.
void semantic_check(AST *now);
// Generate ASM code.
int codegen(AST *root);

int codegen_file(AST *root);
// Free the whole AST.
void freeAST(AST *now);

/// debug interfaces

// Print token array.
void token_print(Token *in, size_t len);
// Print AST tree.
void AST_print(AST *head);

char input[MAX_LENGTH];

int find_reg(void); 
result *new_result (Kind k, int v, int r); 
result *oper(result *r1, result *r2, Kind k); 
result *codegen2(AST *root); 
void init_reg(void); 
int reg[256]; 
int cur_reg = 0; 

int postinc_x = 0; 
int postinc_y = 0; 
int postinc_z = 0; 

int postdec_x = 0; 
int postdec_y = 0; 
int postdec_z = 0; 

int used_x = 1;
int used_y = 1;
int used_z = 1;

int pri = 0;
// FILE *fin; 
// FILE *fout; 

void traverse (AST *root); 
data eval (AST *root); 
data* new_data(data_kind k, int r, int v); 

int main() {
	int x = 5, y = 99, z = 99; 

 	printf("%d %d %d\n", x, y, z); 
	while (fgets(input, MAX_LENGTH, stdin) != NULL) {  
		if (!pri) {
		printf("load r0 [0]\n"); 
		printf("load r1 [4]\n"); 
		printf("load r2 [8]\n"); 
		pri = 1;
		}
		
		Token *content = lexer(input);
		size_t len = token_list_to_arr(&content);
		if (len == 0) continue;
		AST *ast_root = parser(content, len);
		semantic_check(ast_root);


		// if (ast_root) codegen(ast_root); 

		// if (ast_root) {
		// 	init_reg(); 
		// 	result *r = codegen2(ast_root); 
		// }
		if (ast_root) {
			init_reg(); 
			traverse(ast_root); 
		}
		// check postfix
		{
			if (postinc_x) {
			printf("add r0 r0 1\n"); 
			postinc_x = 0; 
			} 
			if (postinc_y) {
			printf("add r1 r1 1\n"); 
			postinc_y = 0; 
			}
			if (postinc_z)
			{
			printf("add r2 r2 1\n"); 
			postinc_z = 0; 
			}
			if (postdec_x) {
			printf("sub r0 r0 1\n"); 
			postdec_x = 0;
			}
			if (postdec_y) {
			printf("sub r1 r1 1\n"); 
			postdec_y = 0;
			}
			if (postdec_z) {
			printf("sub r2 r2 1\n"); 
			postdec_z = 0;
			}
		}
 		// AST_print(ast_root); 

		free(content);
		freeAST(ast_root);
		cur_reg = 0;
	}

	if (used_x) printf("store [0] r0\n"); 
	if (used_y) printf("store [4] r1\n"); 
	if (used_z) printf("store [8] r2\n"); 

	// fclose(fin); 
	// fclose(fout); 

	return 0;
}

Token *lexer(const char *in) {
	Token *head = NULL;
	Token **now = &head;
	for (int i = 0; in[i]; i++) {
		if (isspace(in[i])) // ignore space characters
			continue;
		else if (isdigit(in[i])) {
			(*now) = new_token(CONSTANT, atoi(in + i));
			while (in[i+1] && isdigit(in[i+1])) i++;
		}
		else if ('x' <= in[i] && in[i] <= 'z') // variable
			(*now) = new_token(IDENTIFIER, in[i]);
		else switch (in[i]) {
			case '=':
				(*now) = new_token(ASSIGN, 0);
				break;
			case '+':
				if (in[i+1] && in[i+1] == '+') {
					i++;
					// In lexer scope, all "++" will be labeled as PREINC.
					(*now) = new_token(PREINC, 0);
				}
				// In lexer scope, all single "+" will be labeled as PLUS.
				else (*now) = new_token(PLUS, 0);
				break;
			case '-':
				if (in[i+1] && in[i+1] == '-') {
					i++;
					// In lexer scope, all "--" will be labeled as PREDEC.
					(*now) = new_token(PREDEC, 0);
				}
				// In lexer scope, all single "-" will be labeled as MINUS.
				else (*now) = new_token(MINUS, 0);
				break;
			case '*':
				(*now) = new_token(MUL, 0);
				break;
			case '/':
				(*now) = new_token(DIV, 0);
				break;
			case '%':
				(*now) = new_token(REM, 0);
				break;
			case '(':
				(*now) = new_token(LPAR, 0);
				break;
			case ')':
				(*now) = new_token(RPAR, 0);
				break;
			case ';':
				(*now) = new_token(END, 0);
				break;
			default:
				err("Unexpected character.");
		}
		now = &((*now)->next);
	}
	return head;
}

Token *new_token(Kind kind, int val) {
	Token *res = (Token*)malloc(sizeof(Token));
	res->kind = kind;
	res->val = val;
	res->next = NULL;
	return res;
}

size_t token_list_to_arr(Token **head) {
	size_t res;
	Token *now = (*head), *del;
	for (res = 0; now != NULL; res++) now = now->next;
	now = (*head);
	if (res != 0) (*head) = (Token*)malloc(sizeof(Token) * res);
	for (int i = 0; i < res; i++) {
		(*head)[i] = (*now);
		del = now;
		now = now->next;
		free(del);
	}
	return res;
}

AST *parser(Token *arr, size_t len) {
	for (int i = 1; i < len; i++) {
		// correctly identify "ADD" and "SUB"
		if (arr[i].kind == PLUS || arr[i].kind == MINUS) {
			switch (arr[i - 1].kind) {
				case PREINC:
				case PREDEC:
				case IDENTIFIER:
				case CONSTANT:
				case RPAR:
					arr[i].kind = arr[i].kind - PLUS + ADD;
				default: break;
			}
		}
	}
	return parse(arr, 0, len - 1, STMT);
}

AST *parse(Token *arr, int l, int r, GrammarState S) {
	AST *now = NULL;
	if (l > r)
		err("Unexpected parsing range.");
	int nxt;
	switch (S) {
		case STMT:
			if (l == r && arr[l].kind == END)
				return NULL;
			else if (arr[r].kind == END)
				return parse(arr, l, r - 1, EXPR);
			else err("Expected \';\' at the end of line.");
		case EXPR:
			return parse(arr, l, r, ASSIGN_EXPR);
		case ASSIGN_EXPR:
			if ((nxt = findNextSection(arr, l, r, condASSIGN)) != -1) {
				now = new_AST(arr[nxt].kind, 0);
				now->lhs = parse(arr, l, nxt - 1, UNARY_EXPR);
				now->rhs = parse(arr, nxt + 1, r, ASSIGN_EXPR);
				return now;
			}
			return parse(arr, l, r, ADD_EXPR);
		case ADD_EXPR:
			if((nxt = findNextSection(arr, r, l, condADD)) != -1) {
				now = new_AST(arr[nxt].kind, 0);
				now->lhs = parse(arr, l, nxt - 1, ADD_EXPR);
				now->rhs = parse(arr, nxt + 1, r, MUL_EXPR);
				return now;
			}
			return parse(arr, l, r, MUL_EXPR);
		case MUL_EXPR:
			// TODO: Implement MUL_EXPR.
			// hint: Take ADD_EXPR as reference.
			if((nxt = findNextSection(arr, r, l, condMUL)) != -1) {
				now = new_AST(arr[nxt].kind, 0);
				now->lhs = parse(arr, l, nxt - 1, MUL_EXPR);
				now->rhs = parse(arr, nxt + 1, r, UNARY_EXPR);
				return now;
			}
			return parse(arr, l, r, UNARY_EXPR);
		case UNARY_EXPR:
			// TODO: Implement UNARY_EXPR.
			// hint: Take POSTFIX_EXPR as reference.
			if (arr[l].kind == PREINC || arr[l].kind == PREDEC || arr[l].kind == PLUS || arr[l].kind == MINUS)  {
				now = new_AST(arr[l].kind, 0);
				now->mid = parse(arr, l + 1, r, UNARY_EXPR);
				return now;
			} 
			return parse(arr, l, r, POSTFIX_EXPR);
		case POSTFIX_EXPR:
			if (arr[r].kind == PREINC || arr[r].kind == PREDEC) {
				// translate "PREINC", "PREDEC" into "POSTINC", "POSTDEC"
				now = new_AST(arr[r].kind - PREINC + POSTINC, 0);
				now->mid = parse(arr, l, r - 1, POSTFIX_EXPR);
				return now;
			}
			return parse(arr, l, r, PRI_EXPR);
		case PRI_EXPR:
			if (findNextSection(arr, l, r, condRPAR) == r) {
				now = new_AST(LPAR, 0);
				now->mid = parse(arr, l + 1, r - 1, EXPR);
				return now;
			}
			if (l == r) {
				if (arr[l].kind == IDENTIFIER || arr[l].kind == CONSTANT)
					return new_AST(arr[l].kind, arr[l].val);
				err("Unexpected token during parsing.");
			}
			err("No token left for parsing.");
		default:
			err("Unexpected grammar state.");
	}
}

AST *new_AST(Kind kind, int val) {
	AST *res = (AST*)malloc(sizeof(AST));
	res->kind = kind;
	res->val = val;
	res->lhs = res->mid = res->rhs = NULL;
	return res;
}


int findNextSection(Token *arr, int start, int end, int (*cond)(Kind)) {
	int par = 0;
	int d = (start < end) ? 1 : -1;
	for (int i = start; (start < end) ? (i <= end) : (i >= end); i += d) {
		if (arr[i].kind == LPAR) par++;
		if (arr[i].kind == RPAR) par--;
		if (par == 0 && cond(arr[i].kind) == 1) return i;
	}
	return -1;
}

int condASSIGN(Kind kind) {
	return kind == ASSIGN;
}

int condADD(Kind kind) {
	return kind == ADD || kind == SUB;
}

int condMUL(Kind kind) {
	return kind == MUL || kind == DIV || kind == REM;
}

int condRPAR(Kind kind) {
	return kind == RPAR;
}

void semantic_check(AST *now) {
	if (now == NULL) return;
	// Left operand of '=' must be an identifier or identifier with one or more parentheses.
	if (now->kind == ASSIGN) {
		AST *tmp = now->lhs;
		while (tmp->kind == LPAR) tmp = tmp->mid;
		if (tmp->kind != IDENTIFIER)
			err("Lvalue is required as left operand of assignment.");
	} 
	

	// Operand of INC/DEC must be an identifier or identifier with one or more parentheses.
	// TODO: Implement the remaining semantic_check code.
	// hint: Follow the instruction above and ASSIGN-part code to implement.
	// hint: Semantic of each node needs to be checked recursively (from the current node to lhs/mid/rhs node).
	if (now->kind == PREINC || now->kind == POSTINC || now->kind == PREDEC || now->kind == POSTDEC) {
		AST *tmp = now->mid; 
		while (tmp->kind == LPAR) tmp = tmp->mid; 
		if (tmp->kind != IDENTIFIER) 
			err("Operand of INC/DEC must be an identifier.");
	}
	semantic_check(now->lhs); 
	semantic_check(now->mid); 
	semantic_check(now->rhs); 
}

result* codegen2(AST *root) {
	if (root->kind == ASSIGN) {
		AST *tmp = root->lhs; 
			while (tmp->kind == LPAR) tmp = tmp->mid; 
			result *r1 = codegen2(tmp); 
			result *r2 = codegen2(root->rhs);
			if (r2->kind == CONSTANT) {
				if (r2->val > 0) {
					printf("add r%d 0 %d\n", r1->reg, r2->val); 
				} else {
					printf("add r%d 0 %d\n", r1->reg, -(r2->val)); 
					printf("sub r%d 0 r%d\n", r1->reg, r1->reg); 
				}
			} else if (r2->kind == IDENTIFIER) {
				printf("add r%d 0 r%d\n", r1->reg, r2->reg); 
			}
			return r1; 
	} else if (root->kind == ADD || root->kind == SUB || root->kind == MUL || root->kind == DIV || root->kind == REM) {
		AST *l = root->lhs; 
			AST *r = root->rhs; 
			while (l->kind == LPAR) l = l->mid; 
			while (r->kind == LPAR) r = r->mid; 
			result *r1 = codegen2(l); 
			result *r2 = codegen2(r); 
			return oper(r1, r2, root->kind); 
	} else if (root->kind == PREINC) {
AST *tmp = root->mid; 
			while (tmp->kind == LPAR) tmp = tmp->mid; 
	
			if (tmp->val == 'x') {
				printf("add r0 r0 1\n"); 
				result* r = new_result(IDENTIFIER, tmp->val, 0); 
				return r; 
			} else if (tmp->val == 'y') {
				printf("add r1 r1 1\n"); 
				result* r = new_result(IDENTIFIER, tmp->val, 1); 
				return r; 
			} else {
				printf("add r2 r2 1\n"); 
				result* r = new_result(IDENTIFIER, tmp->val, 2); 
				return r; 
			}

	} else if (root->kind == PREDEC) {

			AST *tmp = root->mid; 
			while (tmp->kind == LPAR) tmp = tmp->mid; 
	
			if (tmp->val == 'x') {
				printf("sub r0 r0 1\n"); 
				result* r = new_result(IDENTIFIER, tmp->val, 0); 
				return r; 
			} else if (tmp->val == 'y') {
				printf("sub r1 r1 1\n"); 
				result* r = new_result(IDENTIFIER, tmp->val, 1); 
				return r; 
			} else {
				printf("sub r2 r2 1\n"); 
				result* r = new_result(IDENTIFIER, tmp->val, 2); 
				return r; 
			}
	} else if (root->kind == POSTINC) {
AST *tmp = root->mid; 
			while (tmp->kind == LPAR) tmp = tmp->mid; 
	
			if (tmp->val == 'x') {
				result* r = new_result(IDENTIFIER, tmp->val, 0); 
				r->flag_inc = 1; 
				postinc_x = 1;
				return r; 
			} else if (tmp->val == 'y') { 
				result* r = new_result(IDENTIFIER, tmp->val, 1); 
				r->flag_inc = 1; 
				postinc_y = 1;
				return r; 
			} else {
				result* r = new_result(IDENTIFIER, tmp->val, 2); 
				r->flag_inc = 1; 
				postinc_z = 1;
				return r; 
			}
	} else if (root->kind == POSTDEC) {
AST *tmp = root->mid; 
			while (tmp->kind == LPAR) tmp = tmp->mid; 
			if (tmp->val == 'x') {
				result* r = new_result(IDENTIFIER, tmp->val, 0); 
				r->flag_dec = 1; 
				postdec_x = 1;
				return r; 
			} else if (tmp->val == 'y') { 
				result* r = new_result(IDENTIFIER, tmp->val, 1); 
				r->flag_dec = 1; 
				postdec_y = 1;
				return r; 
			} else {
				result* r = new_result(IDENTIFIER, tmp->val, 2); 
				r->flag_dec = 1; 
				postdec_z = 1;
				return r; 
			}
	} else if (root->kind == PLUS) {
AST *tmp = root->mid; 
			while (tmp->kind == LPAR) tmp = tmp->mid; 
			result *r2 = codegen2(tmp); 
			return r2; 
	} else if (root->kind == MINUS) {	
AST *tmp = root->mid; 
			while (tmp->kind == LPAR) tmp = tmp->mid; 
			result *r2 = codegen2(tmp); 
			if (r2->kind == CONSTANT) {
				r2->val = -(r2->val); 
				return r2; 
			} else if (r2->kind == IDENTIFIER) {
				if (r2->reg != 0 & r2->reg != 1 && r2->reg != 2) {
					printf("sub r%d 0 r%d\n", r2->reg, r2->reg); 
					return r2; 
				}
				else { 
					int new_reg = find_reg();
					reg[new_reg] = 1; 
					printf("sub r%d 0 r%d\n", new_reg, r2->reg); 
					r2->reg = new_reg; 
					return r2; 
				}

			}
			return r2;  
	} else if (root->kind == IDENTIFIER) {
if (root->val == 'x') {
				result *r = new_result(IDENTIFIER, root->val, 0); 
				return r; 
			} else if (root->val == 'y') {
				result *r = new_result(IDENTIFIER, root->val, 1); 
				return r; 
			} else {
				result *r = new_result(IDENTIFIER, root->val, 2); 
				return r; 
			}
	} else if (root->kind == CONSTANT) {
result *r = new_result(CONSTANT, root->val, -1); 
			return r; 
	} else if (root->kind == LPAR) {
AST *tmp = root->mid; 
			while (tmp->kind == LPAR) tmp = tmp->mid; 
			return codegen2(tmp); 
	} else {
return 0;
	}
}

int codegen(AST *root) {
	// TODO: Implement your codegen in your own way.
	// You may modify the function parameter or the return type, even the whole structure as you wish.
	
	switch (root->kind) {
		case ASSIGN: 
		{
			AST *tmp = root->lhs; 
			while (tmp->kind == LPAR) tmp = tmp->mid; 
			int r2 = codegen(root->rhs);
			if (tmp->val == 'x') {
				printf("store [0] r%d\n", r2); 
			} else if (tmp->val == 'y') {
				printf("store [4] r%d\n", r2); 
			} else {
				printf("store [8] r%d\n", r2); 
			}
			return r2; 
		}
		case ADD:
		{
			int r1 = codegen(root->lhs); 
			int r2 = codegen(root->rhs); 
			printf("add r%d r%d r%d\n", r1, r1, r2); 
			return r1; 
		}
		case SUB:
		{
			int r1 = codegen(root->lhs); 
			int r2 = codegen(root->rhs); 
			printf("sub r%d r%d r%d\n", r1, r1, r2); 
			return r1; 
		}
		case MUL:
		{
			int r1 = codegen(root->lhs); 
			int r2 = codegen(root->rhs); 
			printf("mul r%d r%d r%d\n", r1, r1, r2); 
			return r1; 
		}
		case DIV:
		{
			int r1 = codegen(root->lhs); 
			int r2 = codegen(root->rhs); 
			printf("div r%d r%d r%d\n", r1, r1, r2); 
			return r1; 
		}
		case REM:
		{	
			int r1 = codegen(root->lhs); 
			int r2 = codegen(root->rhs); 
			printf("rem r%d r%d r%d\n", r1, r1, r2); 
			return r1; 
		}
		case PREINC:
		{
			AST *tmp = root->mid; 
			while (tmp->kind == LPAR) tmp = tmp->mid; 
	
			if (tmp->val == 'x') {
				printf("load r%d [0]\n", cur_reg); 
				printf("add r%d r%d 1\n", cur_reg, cur_reg); 
				printf("store [0] r%d\n", cur_reg); 
			} else if (tmp->val == 'y') {
				printf("load r%d [4]\n", cur_reg); 
				printf("add r%d r%d 1\n", cur_reg, cur_reg); 
				printf("store [4] r%d\n", cur_reg); 
			} else {
				printf("load r%d [8]\n", cur_reg); 
				printf("add r%d r%d 1\n", cur_reg, cur_reg); 
				printf("store [8] r%d\n", cur_reg); 
			}
			cur_reg++; 
			return cur_reg-1; 
		}
		case PREDEC:
		{
			AST *tmp = root->mid; 
			while (tmp->kind == LPAR) tmp = tmp->mid; 
	
			if (tmp->val == 'x') {
				printf("load r%d [0]\n", cur_reg); 
				printf("sub r%d r%d 1\n", cur_reg, cur_reg); 
				printf("store [0] r%d\n", cur_reg); 
			} else if (tmp->val == 'y') {
				printf("load r%d [4]\n", cur_reg); 
				printf("sub r%d r%d 1\n", cur_reg, cur_reg); 
				printf("store [4] r%d\n", cur_reg); 
			} else {
				printf("load r%d [8]\n", cur_reg); 
				printf("sub r%d r%d 1\n", cur_reg, cur_reg); 
				printf("store [8] r%d\n", cur_reg); 
			}
			cur_reg++; 
			return cur_reg-1; 
		}
		case POSTINC:
		{
			AST *tmp = root->mid; 
			while (tmp->kind == LPAR) tmp = tmp->mid; 
	
			if (tmp->val == 'x') {
				printf("load r%d [0]\n", cur_reg); 
				printf("add r%d r%d 1\n", cur_reg + 1, cur_reg); 
				printf("store [0] r%d\n", cur_reg + 1);
			} else if (tmp->val == 'y') {
				printf("load r%d [4]\n", cur_reg); 
				printf("add r%d r%d 1\n", cur_reg + 1, cur_reg); 
				printf("store [4] r%d\n", cur_reg + 1);
			} else {
				printf("load r%d [8]\n", cur_reg); 
				printf("add r%d r%d 1\n", cur_reg + 1, cur_reg); 
				printf("store [8] r%d\n", cur_reg + 1);
			}
			cur_reg++; 
			return cur_reg - 1; 
		}
		case POSTDEC:
		{
			AST *tmp = root->mid; 
			while (tmp->kind == LPAR) tmp = tmp->mid; 
	
			if (tmp->val == 'x') {
				printf("load r%d [0]\n", cur_reg); 
				printf("sub r%d r%d 1\n", cur_reg + 1, cur_reg); 
				printf("store [0] r%d\n", cur_reg + 1);
			} else if (tmp->val == 'y') {
				printf("load r%d [4]\n", cur_reg); 
				printf("sub r%d r%d 1\n", cur_reg + 1, cur_reg); 
				printf("store [4] r%d\n", cur_reg + 1);
			} else {
				printf("load r%d [8]\n", cur_reg); 
				printf("sub r%d r%d 1\n", cur_reg + 1, cur_reg); 
				printf("store [8] r%d\n", cur_reg + 1);
			}
			cur_reg++; 
			return cur_reg - 1; 
		}
		case LPAR:
		{
			return codegen(root->mid); 
		}
		case RPAR:
		case PLUS:
		{
			AST *tmp = root->mid; 
			while (tmp->kind == LPAR) tmp = tmp->mid; 
			int r2 = codegen(tmp); 
			return r2; 
		}
		case MINUS:
		{
			AST *tmp = root->mid; 
			while (tmp->kind == LPAR) tmp = tmp->mid; 
			int r2 = codegen(tmp); 
			printf("sub r%d 0 r%d\n", r2, r2);
			return r2;  
		}
		case IDENTIFIER:
		{
			if (root->val == 'x') {
				printf("load r%d [0]\n", cur_reg);
			} else if (root->val == 'y') {
				printf("load r%d [4]\n", cur_reg);
			} else {
				printf("load r%d [8]\n", cur_reg);
			}
			cur_reg++; 
			return cur_reg - 1;
		}
		case CONSTANT:
		{	
			printf("add r%d 0 %d\n", cur_reg++, root->val) ; 
			return cur_reg - 1; 
		}
		default:
		{	
			return cur_reg; 
		}
	}
}


void freeAST(AST *now) {
	if (now == NULL) return;
	freeAST(now->lhs);
	freeAST(now->mid);
	freeAST(now->rhs);
	free(now);
}

void token_print(Token *in, size_t len) {
	const static char KindName[][20] = {
		"Assign", "Add", "Sub", "Mul", "Div", "Rem", "Inc", "Dec", "Inc", "Dec", "Identifier", "Constant", "LPar", "RPar", "Plus", "Minus", "End"
	};
	const static char KindSymbol[][20] = {
		"'='", "'+'", "'-'", "'*'", "'/'", "'%'", "\"++\"", "\"--\"", "\"++\"", "\"--\"", "", "", "'('", "')'", "'+'", "'-'"
	};
	const static char format_str[] = "<Index = %3d>: %-10s, %-6s = %s\n";
	const static char format_int[] = "<Index = %3d>: %-10s, %-6s = %d\n";
	for(int i = 0; i < len; i++) {
		switch(in[i].kind) {
			case LPAR:
			case RPAR:
			case PREINC:
			case PREDEC:
			case ADD:
			case SUB:
			case MUL:
			case DIV:
			case REM:
			case ASSIGN:
			case PLUS:
			case MINUS:
				printf(format_str, i, KindName[in[i].kind], "symbol", KindSymbol[in[i].kind]);
				break;
			case CONSTANT:
				printf(format_int, i, KindName[in[i].kind], "value", in[i].val);
				break;
			case IDENTIFIER:
				printf(format_str, i, KindName[in[i].kind], "name", (char*)(&(in[i].val)));
				break;
			case END:
				printf("<Index = %3d>: %-10s\n", i, KindName[in[i].kind]);
				break;
			default:
				puts("=== unknown token ===");
		}
	}
}

void AST_print(AST *head) {
	static char indent_str[MAX_LENGTH] = "  ";
	static int indent = 2;
	const static char KindName[][20] = {
		"Assign", "Add", "Sub", "Mul", "Div", "Rem", "PreInc", "PreDec", "PostInc", "PostDec", "Identifier", "Constant", "Parentheses", "Parentheses", "Plus", "Minus"
	};
	const static char format[] = "%s\n";
	const static char format_str[] = "%s, <%s = %s>\n";
	const static char format_val[] = "%s, <%s = %d>\n";
	if (head == NULL) return;
	char *indent_now = indent_str + indent;
	indent_str[indent - 1] = '-';
	printf("%s", indent_str);
	indent_str[indent - 1] = ' ';
	if (indent_str[indent - 2] == '`')
		indent_str[indent - 2] = ' ';
	switch (head->kind) {
		case ASSIGN:
		case ADD:
		case SUB:
		case MUL:
		case DIV:
		case REM:
		case PREINC:
		case PREDEC:
		case POSTINC:
		case POSTDEC:
		case LPAR:
		case RPAR:
		case PLUS:
		case MINUS:
			printf(format, KindName[head->kind]);
			break;
		case IDENTIFIER:
			printf(format_str, KindName[head->kind], "name", (char*)&(head->val));
			break;
		case CONSTANT:
			printf(format_val, KindName[head->kind], "value", head->val);
			break;
		default:
			puts("=== unknown AST type ===");
	}
	indent += 2;
	strcpy(indent_now, "| ");
	AST_print(head->lhs);
	strcpy(indent_now, "` ");
	AST_print(head->mid);
	AST_print(head->rhs);
	indent -= 2;
	(*indent_now) = '\0';
}

int find_reg(void) {
	for (int i = 3; i < 256; i++) {
		if (reg[i] == 0) {
			reg[i] = 1;	return i; 
		}
	}	
	return -1; 
}

result *oper (result *r1, result *r2, Kind k) {
	char cmd[5]; 
	if (k == ADD) {
		strcpy(cmd, "add "); 
	} else if (k == SUB) {
		strcpy(cmd, "sub "); 
	} else if (k == MUL) {
		strcpy(cmd, "mul "); 
	} else if (k == DIV) {
		strcpy(cmd, "div "); 
	} else {	
		strcpy(cmd, "rem "); 
	}
	int new_reg; 
	if (r1->kind == CONSTANT && r2->kind == CONSTANT) {
		if (k == ADD) {
			r1->val = r1->val + r2->val; 
		} else if (k == SUB) {
			r1->val = r1->val - r2->val; 
		} else if (k == MUL) {
			r1->val = r1->val * r2->val; 
		} else if (k == DIV) {
			r1->val = r1->val / r2->val; 
		}
		else {	
			r1->val = r1->val % r2->val; 
		}
		return r1; 
	} else if (r1->kind == CONSTANT) {
		if (r2->reg == 0 || r2->reg == 1 || r2->reg == 2) {
			new_reg = find_reg(); 
		} else {
			new_reg = r2->reg; 
		}
		if (r1->val > 0)  { 
			printf("%s", cmd); 
			printf("r%d %d r%d\n", new_reg, r1->val, r2->reg); 
		} else {
			if (k == ADD) {
				printf("sub r%d r%d %d\n", new_reg, r2->reg, -(r1->val)); 
			} else if (k == SUB) {
				printf("sub r%d 0 r%d\n", new_reg, r2->reg); 
				printf("sub r%d r%d %d\n", new_reg, new_reg, -(r1->val)); 
			} else if (k == MUL) {
				printf("mul r%d %d r%d\n", new_reg, -(r1->val), r2->reg); 
				printf("sub r%d 0 r%d\n", new_reg, new_reg); 
			} else if (k == DIV) {
				printf("div r%d %d r%d\n", new_reg, -(r1->val), r2->reg); 
				printf("sub r%d 0 r%d\n", new_reg, new_reg); 
			} else {	
				printf("rem r%d %d r%d\n", new_reg, -(r1->val), r2->reg); 
				printf("sub r%d 0 r%d\n", new_reg, new_reg); 
			}
		}
	} else if (r2->kind == CONSTANT) {
		if (r1->reg == 0 || r1->reg == 1 || r1->reg == 2) {
			new_reg = find_reg(); 
		} else {
			new_reg = r1->reg;
		}
		if (r2->val > 0)  {
			printf("%s", cmd); 
			printf("r%d r%d %d\n", new_reg, r1->reg, r2->val); 
		} else {
			if (k == ADD) {
				printf("sub r%d r%d %d\n", new_reg, r1->reg, -(r2->val)); 
			} else if (k == SUB) {
				printf("add r%d r%d %d\n", new_reg, r1->reg, -(r2->val)); 
			} else if (k == MUL) {
				printf("mul r%d r%d %d\n", new_reg, r1->reg, -(r2->val)); 
				printf("sub r%d 0 r%d\n", new_reg, new_reg); 
			} else if (k == DIV) {
				printf("div r%d r%d %d\n", new_reg, r1->reg, -(r2->val)); 
				printf("sub r%d 0 r%d\n", new_reg, new_reg); 
			} else {	
				printf("rem r%d r%d %d\n", new_reg, r1->reg, -(r2->val)); 
				printf("sub r%d 0 r%d\n", new_reg, new_reg); 
			}
		}
	} else {
		if (r1->reg != 0 && r1->reg != 1 && r1->reg != 2) {
			new_reg = r1->reg; 
		} else if (r2->reg != 0 && r2->reg != 1 && r2->reg != 2) {
			new_reg = r2->reg; 
		} else {
			new_reg = find_reg(); 
		}
		printf("%s", cmd); 
		printf("r%d r%d r%d\n", new_reg, r1->reg, r2->reg); 
		if (r2->reg > 2) reg[r2->reg] = 0; 
	}
	result *r3 = new_result(IDENTIFIER, r1->val, new_reg); 
	if (r1->kind != IDENTIFIER) r3->val= r2->val; 
	return r3; 	
}

result *new_result (Kind k, int v, int r) {
	result *res = malloc(sizeof(result)); 
	res->kind = k; 
	res->val = v; 
	res->reg = r; 
	res->flag_dec = 0; 
	res->flag_inc = 0; 
	return res; 
}

void init_reg(void) { 
	for (int i = 3; i < 256; i++) reg[i] = 0; 
}

void traverse (AST *root) {
	if (root->kind == ASSIGN) {
		AST *tmp = root->lhs; 
		while (tmp->kind == LPAR) tmp = tmp->mid; 
		data val = eval(root->rhs); 
		if (tmp->val == 'x') {
			if (!used_x) used_x = 1; 
			if (val.kind == IDY || val.kind == COM) {
				printf("add r0 0 r%d\n", val.reg); 
			} else {
				if (val.val >= 0) {
					printf("add r0 0 %d\n", val.val); 
				} else {
					printf("sub r0 0 %d\n", -val.val); 
				}
			}
		} else if (tmp->val == 'y') {
			if (!used_y) used_y = 1; 
			if (val.kind == IDY || val.kind == COM) {
				printf("add r1 0 r%d\n", val.reg); 
			} else {
				if (val.val >= 0) {
					printf("add r1 0 %d\n", val.val); 
				} else {
					printf("sub r1 0 %d\n", -val.val); 
				}
			}
		} else {
			if (!used_z) used_z = 1; 
			if (val.kind == IDY || val.kind == COM) {
				printf("add r2 0 r%d\n", val.reg); 
			} else {
				if (val.val >= 0) {
					printf("add r2 0 %d\n", val.val); 
				} else {
					printf("sub r2 0 %d\n", -val.val); 
				}
			}
		}
	} else if (root->kind == ADD || root->kind == SUB || root->kind == MUL || root->kind == DIV || root->kind == REM) {
		traverse(root->lhs); 
		traverse(root->rhs); 
	} else if (root->kind == PREINC) {
		AST *tmp = root->mid; 
		while (tmp->kind == LPAR) tmp = tmp->mid; 
		if (tmp->val == 'x') {
			if (!used_x) {
				printf("load r0 [0]\n"); 
				printf("add r0 r0 1\n"); 
				used_x = 1; 
			} else {
				printf("add r0 r0 1\n"); 
			}
		} else if (tmp->val == 'y') {
			if (!used_y) {
				printf("load r1 [4]\n"); 
				printf("add r1 r1 1\n"); 
				used_y = 1; 
			} else {
				printf("add r1 r1 1\n");  
			}
		} else {
			if (!used_z) {
				printf("load r2 [8]\n"); 
				printf("add r2 r2 1\n"); 
				used_z = 1; 
			} else {
				printf("add r2 r2 1\n");  
			}
		}

	} else if (root->kind == PREDEC) {
		AST *tmp = root->mid; 
		while (tmp->kind == LPAR) tmp = tmp->mid; 

		if (tmp->val == 'x') {
			if (!used_x) {
				printf("load r0 [0]\n"); 
				printf("sub r0 r0 1\n"); 
				used_x = 1; 
			} else {
				printf("sub r0 r0 1\n"); 
			}
		} else if (tmp->val == 'y') {
			if (!used_y) {
				printf("load r1 [4]\n"); 
				printf("sub r1 r1 1\n"); 
				used_y = 1; 
			} else {
				printf("sub r1 r1 1\n");  
			}
		} else {
			if (!used_z) {
				printf("load r2 [8]\n"); 
				printf("sub r2 r2 1\n"); 
				used_z = 1; 
			} else {
				printf("sub r2 r2 1\n");  
			}
		}	
	} else if (root->kind == POSTINC) {
AST *tmp = root->mid; 
			while (tmp->kind == LPAR) tmp = tmp->mid; 
	
			if (tmp->val == 'x') {
				if (!used_x) {
					printf("load r0 [0]\n"); 
					used_x = 1; 
				} 
				postinc_x = 1;
			} else if (tmp->val == 'y') {
					if (!used_y) {
					printf("load r1 [4]\n"); 
					used_y = 1; 
				} 
				postinc_y = 1;
			} else {
				if (!used_z) {
					printf("load r2 [8]\n"); 
					printf("add r2 r2 1\n"); 
					used_z = 1; 
				} 
				postinc_z = 1;
			}
	} else if (root->kind == POSTDEC) {
AST *tmp = root->mid; 
			while (tmp->kind == LPAR) tmp = tmp->mid; 
	
			if (tmp->val == 'x') {
				if (!used_x) {
					printf("load r0 [0]\n"); 
					used_x = 1; 
				} 
			postdec_x = 1; 
			} else if (tmp->val == 'y') {
					if (!used_y) {
					printf("load r1 [4]\n"); 
					used_y = 1; 
				} 
			postdec_y = 1; 
			} else {
				if (!used_z) {
					printf("load r2 [8]\n"); 
					printf("add r2 r2 1\n"); 
					used_z = 1; 
				} 
		postdec_z = 1; 
			}
	} else if (root->kind == PLUS) {
AST *tmp = root->mid; 
			while (tmp->kind == LPAR) tmp = tmp->mid; 
			traverse(tmp); 
	} else if (root->kind == MINUS) {	
AST *tmp = root->mid; 
			while (tmp->kind == LPAR) tmp = tmp->mid; 
			traverse(tmp); 
	} else if (root->kind == IDENTIFIER) {
		return;
	} else if (root->kind == CONSTANT) {
return; 
	} else if (root->kind == LPAR) {
		AST *tmp = root->mid; 
		while (tmp->kind == LPAR) tmp = tmp->mid; 
		traverse(tmp); 
	} else {
		return; 
	}

}

data helper (data l, data r, Kind kind) {
	if (l.kind == NUM && r.kind == NUM) {
		if (kind == ADD) l.val = l.val + r.val; 
		else if (kind == SUB) l.val = l.val - r.val; 
		else if (kind == MUL) l.val = l.val * r.val;
		else if (kind == DIV) l.val = l.val / r.val; 
		else l.val = l.val % r.val; 
		return l; 
	} else if (l.kind == COM) {
		if (r.kind == COM || r.kind == IDY) {
			int new_reg = l.reg; 
			if (kind == ADD) printf("add r%d r%d r%d\n", new_reg, l.reg, r.reg); 
			else if (kind == SUB) printf("sub r%d r%d r%d\n", new_reg, l.reg, r.reg); 
			else if (kind == MUL) printf("mul r%d r%d r%d\n", new_reg, l.reg, r.reg); 
			else if (kind == DIV) printf("div r%d r%d r%d\n", new_reg, l.reg, r.reg); 
			else printf("rem r%d r%d r%d\n", new_reg, l.reg, r.reg); 

			if (r.kind == COM) reg[r.reg] = 0; 
			return l; 
		} else { // r.kind == NUM
			int new_reg = l.reg; 
			if (r.val < 0) {
				if (kind == ADD) printf("sub r%d r%d %d\n", new_reg, l.reg, -r.val); 
				else if (kind == SUB) printf("add r%d r%d %d\n", new_reg, l.reg, -r.val); 
				else if (kind == MUL) printf("mul r%d r%d %d\n", new_reg, l.reg, -r.val); 
				else if (kind == DIV) printf("div r%d r%d %d\n", new_reg, l.reg, r.val); 
				else printf("rem r%d r%d %d\n", new_reg, l.reg, -r.val); 
				
				if (kind == MUL || kind == DIV || kind == REM) printf("sub r%d 0 r%d\n", new_reg, new_reg); 
			} else {
				if (kind == ADD) printf("add r%d r%d %d\n", new_reg, l.reg, r.val); 
				else if (kind == SUB) printf("sub r%d r%d %d\n", new_reg, l.reg, r.val); 
				else if (kind == MUL) printf("mul r%d r%d %d\n", new_reg, l.reg, r.val); 
				else if (kind == DIV) printf("div r%d r%d %d\n", new_reg, l.reg, r.val); 
				else printf("rem r%d r%d %d\n", new_reg, l.reg, r.val); 
			}
			return l; 
		}
	} else if (r.kind == COM) {
		int new_reg = r.reg; 
		if (l.kind == NUM) {
			if (l.val < 0) {
				if (kind == ADD) printf("sub r%d r%d %d\n", new_reg, r.reg, -l.val); 
				else if (kind == SUB) {
					printf("sub r%d 0 r%d\n", new_reg, r.reg); 
					printf("sub r%d r%d %d\n", new_reg, new_reg, l.val); 
				}
				else if (kind == MUL) printf("mul r%d %d r%d\n", new_reg, -l.val, r.reg); 
				else if (kind == DIV) printf("div r%d %d r%d\n", new_reg, -l.val, r.reg); 
				else printf("rem r%d %d r%d\n", new_reg, -l.val, r.reg); 
				if (kind == MUL || kind == DIV || kind == REM) {
					printf("sub r%d 0 r%d\n", new_reg, new_reg); 
				}
			}
			else {
				if (kind == ADD) printf("add r%d %d r%d\n", new_reg, l.val, r.reg); 
				else if (kind == SUB) printf("sub r%d %d r%d\n", new_reg, l.val, r.reg); 
				else if (kind == MUL) printf("mul r%d %d r%d\n", new_reg, l.val, r.reg); 
				else if (kind == DIV) printf("div r%d %d r%d\n", new_reg, l.val, r.reg); 
				else printf("rem r%d %d r%d\n", new_reg, l.val, r.reg); 
			}
		} else { // l.kind == IDY
			if (kind == ADD) printf("add r%d r%d r%d\n", new_reg, l.reg, r.reg); 
			else if (kind == SUB) printf("sub r%d r%d r%d\n", new_reg, l.reg, r.reg); 
			else if (kind == MUL) printf("mul r%d r%d r%d\n", new_reg, l.reg, r.reg); 
			else if (kind == DIV) printf("div r%d r%d r%d\n", new_reg, l.reg, r.reg); 
			else printf("rem r%d r%d r%d\n", new_reg, l.reg, r.reg); 
		}
		return r; 
	} else if (l.kind == IDY && r.kind == IDY) {
		int new_reg = find_reg(); 
		if (kind == ADD) printf("add r%d r%d r%d\n", new_reg, l.reg, r.reg); 
		else if (kind == SUB) printf("sub r%d r%d r%d\n", new_reg, l.reg, r.reg); 
		else if (kind == MUL) printf("mul r%d r%d r%d\n", new_reg, l.reg, r.reg); 
		else if (kind == DIV) printf("div r%d r%d r%d\n", new_reg, l.reg, r.reg); 
		else printf("rem r%d r%d r%d\n", new_reg, l.reg, r.reg); 
		data *res = new_data(COM, new_reg, -1); 
		return *res; 
	} 
	else {
		int new_reg = find_reg(); 
		if (l.kind == NUM) {
			if (l.val < 0) {
				if (kind == ADD) printf("sub r%d r%d %d\n", new_reg, r.reg, -l.val); 
				else if (kind == SUB) {
					printf("sub r%d 0 r%d\n", new_reg, r.reg); 
					printf("sub r%d r%d %d\n", new_reg, new_reg, l.val); 
				}
				else if (kind == MUL) printf("mul r%d %d r%d\n", new_reg, -l.val, r.reg); 
				else if (kind == DIV) printf("div r%d %d r%d\n", new_reg, -l.val, r.reg); 
				else printf("rem r%d %d r%d\n", new_reg, -l.val, r.reg); 
				if (kind == MUL || kind == DIV || kind == REM) {
					printf("sub r%d 0 r%d\n", new_reg, new_reg); 
				}
			}
			else {
				if (kind == ADD) printf("add r%d %d r%d\n", new_reg, l.val, r.reg); 
				else if (kind == SUB) printf("sub r%d %d r%d\n", new_reg, l.val, r.reg); 
				else if (kind == MUL) printf("mul r%d %d r%d\n", new_reg, l.val, r.reg); 
				else if (kind == DIV) printf("div r%d %d r%d\n", new_reg, l.val, r.reg); 
				else printf("rem r%d %d r%d\n", new_reg, l.val, r.reg); 
			}
		} else {
			if (r.val < 0) {
				if (kind == ADD) printf("sub r%d r%d %d\n", new_reg, l.reg, -r.val); 
				else if (kind == SUB) printf("add r%d r%d %d\n", new_reg, l.reg, -r.val); 
				else if (kind == MUL) printf("mul r%d r%d %d\n", new_reg, l.reg, -r.val); 
				else if (kind == DIV) printf("div r%d r%d %d\n", new_reg, l.reg, r.val); 
				else printf("rem r%d r%d %d\n", new_reg, l.reg, -r.val); 
				
				if (kind == MUL || kind == DIV || kind == REM) printf("sub r%d 0 r%d\n", new_reg, new_reg); 
			} else {
				if (kind == ADD) printf("add r%d r%d %d\n", new_reg, l.reg, r.val); 
				else if (kind == SUB) printf("sub r%d r%d %d\n", new_reg, l.reg, r.val); 
				else if (kind == MUL) printf("mul r%d r%d %d\n", new_reg, l.reg, r.val); 
				else if (kind == DIV) printf("div r%d r%d %d\n", new_reg, l.reg, r.val); 
				else printf("rem r%d r%d %d\n", new_reg, l.reg, r.val); 
			}
		}
		data *res = new_data(COM, new_reg, -1); 
		return *res; 
	}
}
data eval (AST *root) {
	if (root->kind == ASSIGN) {
		AST *tmp = root->lhs; 
		while (tmp->kind == LPAR) tmp = tmp->mid; 
		data val = eval(root->rhs); 
		if (tmp->val == 'x') {
			if (val.kind == IDY || val.kind == COM) {
				printf("add r0 0 r%d\n", val.reg); 
			} else {
				if (val.val >= 0) {
				printf("add r0 0 %d\n", val.val); 
				} else {
					printf("sub r0 0 %d\n", -val.reg); 
				}
			}
			data *res = new_data(IDY, 0, -1); 
			return *res;
		} else if (tmp->val == 'y') {
			if (val.kind == IDY || val.kind == COM) {
			printf("add r1 0 r%d\n", val.reg); 
			} else {
			if (val.val >= 0) {
			printf("add r1 0 %d\n", val.val); 
			} else {
			printf("sub r1 0 %d\n", -val.reg); 
			}
			}
			data *res = new_data(IDY, 1, -1); 
			return *res;
		} else {
			if (val.kind == IDY || val.kind == COM) {
			printf("add r2 0 r%d\n", val.reg); 
			} else {
				if (val.val >= 0) {
			printf("add r2 0 %d\n", val.val); 
				} else {
			printf("sub r2 0 %d\n", -val.reg); 
				}
			}
			data *res = new_data(IDY, 2, -1); 
			return *res;
		}

	} else if (root->kind == ADD || root->kind == SUB || root->kind == MUL || root->kind == DIV || root->kind == REM) {
		data l = eval(root->lhs); 
		data r = eval(root->rhs); 
		return helper(l, r, root->kind); 
	} else if (root->kind == PREINC) {
	AST *tmp = root->mid; 
	while (tmp->kind == LPAR) tmp = tmp->mid; 
	if (tmp->val == 'x') {
		if (!used_x) {
		printf("load r0 [0]\n"); 
		printf("add r0 r0 1\n"); 
		used_x = 1; 
		} else {
		printf("add r0 r0 1\n"); 
		}
		data *res = new_data(IDY, 0, -1); 
		return *res;
	} else if (tmp->val == 'y') {
		if (!used_y) {
		printf("load r1 [4]\n"); 
		printf("add r1 r1 1\n"); 
		used_y = 1; 
		} else {
		printf("add r1 r1 1\n");  
		}
		data *res = new_data(IDY, 1, -1); 
		return *res;
	} else {
			if (!used_z) {
			printf("load r2 [8]\n"); 
			printf("add r2 r2 1\n"); 
			used_z = 1; 
			} else {
			printf("add r2 r2 1\n");  
			}
			data *res = new_data(IDY, 2, -1); 
			return *res;
		}
	} else if (root->kind == PREDEC) {
		AST *tmp = root->mid; 
		while (tmp->kind == LPAR) tmp = tmp->mid; 
		if (tmp->val == 'x') {
			if (!used_x) {
			printf("load r0 [0]\n"); 
			printf("sub r0 r0 1\n"); 
			used_x = 1; 
			} else {
			printf("sub r0 r0 1\n"); 
			}
			data *res = new_data(IDY, 0, -1); 
			return *res;
		} else if (tmp->val == 'y') {
			if (!used_y) {
			printf("load r1 [4]\n"); 
			printf("sub r1 r1 1\n"); 
			used_y = 1; 
			} else {
			printf("sub r1 r1 1\n");  
			}
			data *res = new_data(IDY, 1, -1); 
			return *res;
		} else {
			if (!used_z) {
			printf("load r2 [8]\n"); 
			printf("sub r2 r2 1\n"); 
			used_z = 1; 
			} else {
			printf("sub r2 r2 1\n");  
			}
			data *res = new_data(IDY, 2, -1); 
			return *res;
		}
	} else if (root->kind == POSTINC) {
		AST *tmp = root->mid; 
		while (tmp->kind == LPAR) tmp = tmp->mid; 
		if (tmp->val == 'x') {
			if (!used_x) {
				printf("load r0 [0]\n"); 
				used_x = 1; 
			} 
			postinc_x = 1;
			data *res = new_data(IDY, 0, -1); 
			return *res;
		} else if (tmp->val == 'y') {
			if (!used_y) {
				printf("load r1 [4]\n"); 
				used_y = 1; 
			} 
			postinc_y = 1;
			data *res = new_data(IDY, 1, -1); 
			return *res;
		} else {
			if (!used_z) {
				printf("load r2 [8]\n"); 
				printf("add r2 r2 1\n"); 
				used_z = 1; 
			} 
			postinc_z = 1;
			data *res = new_data(IDY, 2, -1); 
			return *res;
		}
	} else if (root->kind == POSTDEC) {
		AST *tmp = root->mid; 
		while (tmp->kind == LPAR) tmp = tmp->mid; 
		if (tmp->val == 'x') {
			if (!used_x) {
				printf("load r0 [0]\n"); 
				used_x = 1; 
			} 
			postdec_x = 1; 
			data *res = new_data(IDY, 0, -1); 
			return *res;
		} else if (tmp->val == 'y') {
			if (!used_y) {
				printf("load r1 [4]\n"); 
				used_y = 1; 
			} 
			postdec_y = 1; 
			data *res = new_data(IDY, 1, -1); 
			return *res;
		} else {
				if (!used_z) {
					printf("load r2 [8]\n"); 
					printf("add r2 r2 1\n"); 
					used_z = 1; 
				} 
				postdec_z = 1; 
				data *res = new_data(IDY, 0, -1); 
				return *res;
			}
	} else if (root->kind == PLUS) {
		AST *tmp = root->mid; 
		while (tmp->kind == LPAR) tmp = tmp->mid; 
		return eval(tmp); 
	} else if (root->kind == MINUS) {	
		AST *tmp = root->mid; 
		while (tmp->kind == LPAR) tmp = tmp->mid; 
		data val = eval(tmp); 
		if (val.kind == NUM) {
			val.val = -val.val; 
			return val; 
		} else if (val.kind == IDY) {
			int new_reg = find_reg(); 
			printf("sub r%d 0 r%d\n", new_reg, val.reg); 
			val.kind = COM; 
			val.reg = new_reg; 
			return val; 
		} else {
			printf("sub r%d 0 r%d\n", val.reg, val.reg); 
			return val; 
		}
	} else if (root->kind == IDENTIFIER) {
		data *res = new_data(IDY, 0, -1); 
		if (root->val == 'x') {
			if (!used_x) {
				printf("load r0 [0]\n"); 
				used_x = 1; 
			}
		}
		if (root->val == 'y') {
			res->reg = 1; 
			if (!used_y) {
				printf("load r1 [4]\n"); 
				used_y = 1; 
			}
		} else if (root->val == 'z') {
			res->reg = 2; 
			if (!used_z) {
				printf("load r2 [8]\n"); 
				used_z = 1; 
			}
		}
		return *res; 
	} else if (root->kind == CONSTANT) {
		data *res = new_data(NUM, -1, root->val); 
		return *res; 
	} else if (root->kind == LPAR) {
		AST *tmp = root->mid; 
		while (tmp->kind == LPAR) tmp = tmp->mid; 
		return eval(tmp); 
	} else {
		AST *tmp = root->mid; 
		while (tmp->kind == LPAR) tmp = tmp->mid; 
		return eval(tmp); 
	}

}

data* new_data(data_kind k, int r, int v) {
	data* res = malloc(sizeof(data)); 
	res->kind = k; res->reg = r; res->val = v;
	return res; 
}
