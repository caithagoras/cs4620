/* File: semantic.h
 * ----------------
 * Defines routines for semantic analysis.
 */

#ifndef _H_semantic
#define _H_semantic

#include <map>
#include <vector>

#include "ast_stmt.h"
#include "ast_type.h"
#include "ast_decl.h"
#include "ast_expr.h"
using namespace std;

class Symbol {
 public:
  explicit Symbol(Decl *d):decl(d){}
  virtual ~Symbol(){}

 protected:
  Decl *decl;
  friend class Semantic;
};

class VarSymbol: public Symbol {
 public:
  explicit VarSymbol(VarDecl *varDecl):Symbol(varDecl){}
};

class FnSymbol: public Symbol {
 public:
  explicit FnSymbol(FnDecl *fnDecl):Symbol(fnDecl){}
};

class Scope {
 public:
  Scope();
  Scope(Scope *parent);
  void set_parent(Scope *parent);

 protected:
  map<string, Symbol> symbols;
  Scope *parent;
  friend class Semantic;
};

class Semantic {
 protected:
  Program *program;
  vector<Scope*> scopes;
  Scope* current;

  void enter_scope();
  void exit_scope();
  void insert_symbol(string ident, Symbol symbol);
  const Symbol* lookup(const Scope *scope, string s) const;
  const Symbol* lookup(string s) const;
  const Symbol* local_lookup(string s) const;

  void check(Program* program);
  void check(VarDecl *varDecl);
  void check(FnDecl *fnDecl);
  void check(Stmt *stmt);

 public:
  Semantic(Program *program);
  void analyze();
};

#endif

