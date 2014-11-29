/* File: semantic.h
 * ----------------
 * Defines routines for semantic analysis.
 */

#ifndef _H_semantic
#define _H_semantic

#include <map>
#include <set>
#include <vector>

#include "ast_stmt.h"
#include "ast_type.h"
#include "ast_decl.h"
#include "ast_expr.h"
#include "codegen.h"
using namespace std;

typedef enum {
  rootScope,
  fnScope,
  classScope,
  interfaceScope,
  blockScope
} ScopeType;

class Scope;

class Symbol {
  friend class Semantic;

 public:
  explicit Symbol(Decl *d): decl(d), scope(NULL), location(NULL) {}
  explicit Symbol(Decl *d, Scope *s): decl(d), scope(s), location(NULL) {}
  void set_location_fp_relative(int offset);
  void set_location_gp_relative(int offset);
  void set_location_class_member(int offset);

 protected:
  Decl *decl;
  Scope *scope; // NULL for Variable Declarations
  Location *location;
};

class Scope {
  friend class Semantic;

 public:
  Scope(Scope *parent, ScopeType type, Decl *decl = NULL);

 protected:
  map<string, Symbol> symbols;
  Scope *parent;
  ScopeType type;
  Decl *decl;
  
  map<FnDecl*, int> vtable_index; // For ClassScope
};

class Semantic {
 protected:
  Program *program;
  set<Scope*> scopes;
  Scope *current, *root;
  map<string, NamedType> id_to_type;
  set<Decl*> loaded;
  vector<ArrayType> array_types;
  Symbol *fn_array_length;
  
  CodeGenerator instructions;
  int gp_offset;

  // Common Internal
  void enter_scope(ScopeType type);
  void enter_scope(ScopeType type, Decl *decl);
  void enter_scope(Scope *scope);
  void exit_scope();
  void insert_symbol(string ident, Symbol symbol);
  void override(string ident, FnDecl* fnDecl);
  Symbol* lookup(Scope *scope, string s) const;
  Symbol* lookup(string s) const;
  Symbol* local_lookup(string s) const;

  // Semantic Analyzer Internal

  void build(Program* program);
  void build(Decl *decl);

  void build_id_to_type_map();

  void check(Program *program);
  void check(VarDecl *varDecl, bool symbol_only, bool suppress_dup_error);
  void check(FnDecl *fnDecl, bool symbol_only, bool suppress_dup_error);
  void check(ClassDecl *classDecl, bool load_only);
  void check(InterfaceDecl *interfaceDecl);

  void check(Stmt *stmt);
  void check(StmtBlock *stmtBlock);
  void check(ForStmt *forStmt);
  void check(WhileStmt *whileStmt);
  void check(IfStmt *ifStmt);
  void check(BreakStmt *breakStmt);
  void check(ReturnStmt *returnStmt);
  void check(PrintStmt *printStmt);

  const Type* check(Expr *expr);
  const Type* check(CompoundExpr *expr);
  const Type* check(FieldAccess *field_access);
  const Type* check(ArrayAccess *array_access);
  const Type* check(This *expr);
  const Type* check(NewExpr *expr);
  const Type* check(NewArrayExpr *expr);
  const Type* check(Call *expr);

  bool has_undefined_named_type(const Type *type); // Returns true when the core type is a NamedType that is undefined, false otherwise.
  const Type* check_compatibility(const Operator *op, const Type* rhs);
  const Type* check_compatibility(const Operator *op, const Type* lhs, const Type* rhs);
  const Type* check_compatibility(const Type* lhs, const Type* rhs);
  bool is_compatible_inheritance(const Type* parent, const Type *derived);
  bool find_parent(const NamedType *derived, string parent);
  bool is_matched_prototype(FnDecl *d1, FnDecl *d2);

  // Code Generator Internal
  bool check_main();
  void emit(Program *program);
  void emit(VarDecl *varDecl, bool is_global, int *offset);
  void emit(FnDecl *fnDecl);
  void emit(ClassDecl *classDecl);

  void init_current_class(ClassDecl *classDecl, vector<FnDecl*> &vtable, int *field_offset);
  void emit_vtable(char *class_name, const vector<FnDecl*> &vtable);
  
  void emit(Stmt *stmt, int *fp_offset);
  void emit(StmtBlock *stmtBlock, int *fp_offset);
  void emit(ForStmt *forStmt, int *fp_offset);
  void emit(WhileStmt *whileStmt, int *fp_offset);
  void emit(IfStmt *ifStmt, int *fp_offset);
  void emit(BreakStmt *breakStmt, int *fp_offset);
  void emit(ReturnStmt *returnStmt, int *fp_offset);
  void emit(PrintStmt *printStmt, int *fp_offset);

  Location* emit(Expr *expr, int *fp_offset, bool assignable_location);
  Location* emit(CompoundExpr *expr, int *fp_offset);
  Location* emit(Operator *op, Location *rhs, int *fp_offset);
  Location* emit(Operator *op, Expr *left, Expr *right, int *fp_offset);
  Location* emit_assignment(Expr *left, Expr *right, int *fp_offset);
  Location* emit(FieldAccess *expr, int *fp_offset);
  Location* emit(ArrayAccess *expr, int *fp_offset);
  Location* emit(NewExpr *expr, int *fp_offset);
  Location* emit(NewArrayExpr *expr, int *fp_offset);
  Location* emit(Call *expr, int *fp_offset);
  Location* emit_array_length(Location *array_base, int *fp_offset);

  char* get_function_label(FnDecl *fnDecl);

 public:
  explicit Semantic(Program *program);
  void analyze();
  void generate();
};

#endif

