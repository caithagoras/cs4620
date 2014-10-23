/* File: semantic.cc
 * -----------------
 * Semantic Analyzer
 */

#include <iostream>
#include <cstdio>
#include <utility>
#include <typeinfo>

#include "semantic.h"
#include "ast_stmt.h"
#include "ast_type.h"
#include "ast_decl.h"
#include "ast_expr.h"
#include "errors.h"

using namespace std;

Scope::Scope() {
  this->parent=NULL;
}

Scope::Scope(Scope *parent) {
  this->parent = parent;
}

Semantic::Semantic(Program *program) {
  this->program = program;
}

void Semantic::analyze(){
  check(program);
}

void Semantic::enter_scope() {
  Scope *scope=new Scope(current);
  current=scope;
  scopes.push_back(scope);
}

void Semantic::exit_scope() {
  current=current->parent;
}

void Semantic::insert_symbol(string ident, Symbol symbol) {
  current->symbols.insert(pair<string, Symbol>(ident, symbol));
}

const Symbol* Semantic::lookup(const Scope* scope, string s) const {
  if (scope->symbols.count(s) == 0)
    return NULL;
  else
    return &(scope->symbols.find(s)->second);
}

const Symbol* Semantic::lookup(string s) const {
  const Symbol *symbol;
  const Scope *p = current;
  while (p != NULL){
    symbol = lookup(p, s);
    if (symbol != NULL) return symbol;
    p = p->parent;
  }
  return NULL;
}

const Symbol* Semantic::local_lookup(string s) const {
  return lookup(current, s);
}

void Semantic::check(Program *program){
  enter_scope();
  for (int i=0; i<program->decls->NumElements(); i++){
    Decl *decl = program->decls->Nth(i);
    VarDecl *varDecl;
    FnDecl *fnDecl;
    ClassDecl *classDecl;
    InterfaceDecl *interfaceDecl;

    if ((varDecl=dynamic_cast<VarDecl*>(decl)) != NULL) {
      check(varDecl);
      continue;
    }

    if ((fnDecl=dynamic_cast<FnDecl*>(decl)) != NULL) {
      check(fnDecl);
      continue;
    }

    if ((classDecl=dynamic_cast<ClassDecl*>(decl)) != NULL) {
      continue;
    }

    if ((interfaceDecl=dynamic_cast<InterfaceDecl*>(decl)) != NULL) {
      continue;
    }
  }
  exit_scope();
}

void Semantic::check(VarDecl *varDecl) {
  string name = string(varDecl->id->name);
  const Symbol *declared;
  if ((declared=local_lookup(name)) != NULL)
    ReportError::DeclConflict(varDecl, declared->decl);
  else
    insert_symbol(name, VarSymbol(varDecl));
}

void Semantic::check(FnDecl *fnDecl) {
  string name = string(fnDecl->id->name);
  const Symbol *declared;
  if ((declared=local_lookup(name)) != NULL)
    ReportError::DeclConflict(fnDecl, declared->decl);
  else
    insert_symbol(name, FnSymbol(fnDecl));

  enter_scope();
  for (int i=0; i<fnDecl->formals->NumElements(); i++)
    check(fnDecl->formals->Nth(i));
  check(dynamic_cast<StmtBlock*>(fnDecl->body));
  exit_scope();
}

void Semantic::check(Stmt *stmt) {
  StmtBlock *stmtBlock;
  LoopStmt *loopStmt;
  IfStmt *ifStmt;

  if ((stmtBlock=dynamic_cast<StmtBlock*>(stmt)) != NULL) {
    enter_scope();
    for (int i=0; i<stmtBlock->decls->NumElements(); i++)
      check(stmtBlock->decls->Nth(i));
    for (int i=0; i<stmtBlock->stmts->NumElements(); i++)
      check(stmtBlock->stmts->Nth(i));
    exit_scope();
    return;
  }

  if ((loopStmt=dynamic_cast<LoopStmt*>(stmt)) != NULL){
    check(loopStmt->body);
    return;
  }

  if ((ifStmt=dynamic_cast<IfStmt*>(stmt)) != NULL){
    check(ifStmt->body);
    if (ifStmt->elseBody != NULL)
      check(ifStmt->elseBody);
    return;
  }
}
