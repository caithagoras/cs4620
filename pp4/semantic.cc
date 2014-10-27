/* File: semantic.cc
 * -----------------
 * Semantic Analyzer
 */

#include <iostream>
#include <cstdio>
#include <cstring>
#include <utility>
#include <typeinfo>
#include <cassert>

#include "semantic.h"
#include "ast_stmt.h"
#include "ast_type.h"
#include "ast_decl.h"
#include "ast_expr.h"
#include "errors.h"

using namespace std;

Scope::Scope(Scope *parent, ScopeType type) {
  this->parent = parent;
  this->type = type;
}

Semantic::Semantic(Program *program) {
  this->program = program;
  current = NULL;
  root = NULL;
}

void Semantic::enter_scope(ScopeType type) {
  Scope *scope = new Scope(current, type);
  current = scope;
  if (scopes.count(scope) == 0)
    scopes.insert(scope);
}

void Semantic::enter_scope(Scope *scope) {
  current = scope;
  if (scopes.count(scope) == 0)
    scopes.insert(scope);
}

void Semantic::exit_scope() {
  current = current->parent;
}

void Semantic::insert_symbol(string ident, Symbol symbol) {
  current->symbols.insert(pair<string, Symbol>(ident, symbol));
}

void Semantic::override(string ident, FnDecl* fnDecl) {
  assert(current->symbols.count(ident));
  Symbol symbol = current->symbols.find(ident)->second;
  assert(dynamic_cast<FnDecl*>(symbol.decl));
  current->symbols.erase(ident);
  Scope *scope = new Scope(current, fnScope);
  insert_symbol(ident, Symbol(fnDecl, scope));
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

void Semantic::build(Program *program){
  enter_scope(ScopeType(rootScope));
  root=current;
  for (int i=0; i<program->decls->NumElements(); i++){
    Decl *decl = program->decls->Nth(i);
    build(decl);
  }
  exit_scope();
}

void Semantic::build(Decl *decl){
  VarDecl *varDecl = dynamic_cast<VarDecl*>(decl);
  FnDecl *fnDecl = dynamic_cast<FnDecl*>(decl);
  ClassDecl *classDecl = dynamic_cast<ClassDecl*>(decl);
  InterfaceDecl *interfaceDecl =dynamic_cast<InterfaceDecl*>(decl);
  
  string name(decl->id->name);
  const Symbol *declared = local_lookup(name);
  bool conflict = true;
  if (declared)
    ReportError::DeclConflict(decl, declared->decl);
  else
    conflict = false;
  
  if (varDecl) {
    if (!conflict) insert_symbol(name, Symbol(decl));
  }
  else if (fnDecl) {
    Scope *scope = new Scope(current, ScopeType(fnScope));
    if (!conflict) insert_symbol(name, Symbol(decl, scope));
  }
  else if (classDecl) {
    Scope *scope = new Scope(current, ScopeType(classScope));
    if (!conflict) insert_symbol(name, Symbol(decl, scope));
  }
  else if (interfaceDecl) {
    Scope *scope = new Scope(current, ScopeType(interfaceScope));
    if (!conflict) insert_symbol(name, Symbol(decl, scope));
  }
}

void Semantic::build_scope_to_id_map() {
  for (map<string, Symbol>::iterator it = root->symbols.begin(); it != root->symbols.end(); it++)
    if (dynamic_cast<ClassDecl*>(it->second.decl) || dynamic_cast<InterfaceDecl*>(it->second.decl))
      scope_to_id.insert(pair<Scope*, string>(it->second.scope, it->first));
}

void Semantic::build_id_to_type_map() {
  for (map<Scope*, string>::iterator it = scope_to_id.begin(); it != scope_to_id.end(); it++)
    id_to_type.insert(pair<string, NamedType>(it->second, NamedType(lookup(root, it->second)->decl->id)));
}

void Semantic::check(Program *program){
  enter_scope(root);
  for (int i=0; i<program->decls->NumElements(); i++){
    Decl *decl = program->decls->Nth(i);
    VarDecl *varDecl = dynamic_cast<VarDecl*>(decl);
    FnDecl *fnDecl = dynamic_cast<FnDecl*>(decl);
    ClassDecl *classDecl = dynamic_cast<ClassDecl*>(decl);
    InterfaceDecl *interfaceDecl =dynamic_cast<InterfaceDecl*>(decl);
    
    if (varDecl)
      check(varDecl, false, true);
    if (fnDecl)
      check(fnDecl, false, true);
    else if (classDecl)
      check(classDecl, false);
    else if (interfaceDecl)
      check(interfaceDecl);
  }
  exit_scope();
}

void Semantic::check(VarDecl *varDecl, bool symbol_only, bool suppress_dup_error){
  string name(varDecl->id->name);
  if (local_lookup(name) == NULL)
    insert_symbol(name, Symbol(varDecl));
  
  const Symbol *declared = local_lookup(name);
  if (varDecl != declared->decl && !suppress_dup_error)
    ReportError::DeclConflict(varDecl, declared->decl);

  if (symbol_only) return;

  // check named variable type
  if (has_undefined_named_type(varDecl->type))
    varDecl->type = Type::errorType;
}

void Semantic::check(FnDecl *fnDecl, bool symbol_only, bool suppress_dup_error){
  string name(fnDecl->id->name);
  if (local_lookup(name) == NULL){
    Scope *scope = new Scope(current, ScopeType(fnScope));
    insert_symbol(name, Symbol(fnDecl, scope));
  }

  const Symbol *declared = local_lookup(name);
  if (fnDecl != declared->decl && !suppress_dup_error)
    ReportError::DeclConflict(fnDecl, declared->decl);

  if (symbol_only) return;
  
  if (fnDecl == declared->decl)
    enter_scope(declared->scope);
  else
    enter_scope(ScopeType(fnScope));

  // check return type
  if (has_undefined_named_type(fnDecl->returnType))
    fnDecl->returnType = Type::errorType;

  // check parameter list
  for (int i=0; i<fnDecl->formals->NumElements(); i++)
    check(dynamic_cast<VarDecl*>(fnDecl->formals->Nth(i)), false, false);

  // check statement
  if (fnDecl->body != NULL)
    check(dynamic_cast<StmtBlock*>(fnDecl->body));

  exit_scope();
}

void Semantic::check(ClassDecl *classDecl, bool load_only) {
  bool need_to_load = !loaded.count(classDecl);
  if (!need_to_load && load_only) return;
  loaded.insert(classDecl);

  const Symbol *declared = local_lookup(classDecl->id->name);
  if (classDecl == declared->decl)
    enter_scope(declared->scope);
  else
    enter_scope(ScopeType(classScope));

  map<string, FnDecl*> inherited_fn;
  // Handle inherited class
  if (classDecl->extends != NULL) {
    const Symbol *extendsSymbol = lookup(classDecl->extends->id->name);
    if (extendsSymbol == NULL || dynamic_cast<ClassDecl*>(extendsSymbol->decl) == NULL) {
      if (!load_only)
        ReportError::IdentifierNotDeclared(classDecl->extends->id, reasonT(LookingForClass));
    }
    else {
      Scope *saved = current;
      enter_scope(root);
      check(dynamic_cast<ClassDecl*>(extendsSymbol->decl), true);
      enter_scope(saved);
      for (map<string, Symbol>::iterator it = extendsSymbol->scope->symbols.begin(); it != extendsSymbol->scope->symbols.end(); it++) {
        insert_symbol(it->first, it->second);
        FnDecl *fnDecl = dynamic_cast<FnDecl*>(it->second.decl);
        if (fnDecl)
          inherited_fn.insert(pair<string, FnDecl*>(it->first, fnDecl));
      }
    }
  }

  // Handle implemented interefaces
  for (int i=0; i<classDecl->implements->NumElements(); i++) {
    const Symbol *implementsSymbol = lookup(classDecl->implements->Nth(i)->id->name);
    if (implementsSymbol == NULL || dynamic_cast<InterfaceDecl*>(implementsSymbol->decl) == NULL) {
      if (!load_only)
        ReportError::IdentifierNotDeclared(classDecl->implements->Nth(i)->id, reasonT(LookingForInterface));
    }
    else {
      Scope *saved = current;
      enter_scope(root);
      check(dynamic_cast<InterfaceDecl*>(implementsSymbol->decl));
      enter_scope(saved);
      for (map<string, Symbol>::iterator it = implementsSymbol->scope->symbols.begin(); it != implementsSymbol->scope->symbols.end(); it++) {
        insert_symbol(it->first, it->second);
        inherited_fn.insert(pair<string, FnDecl*>(it->first, static_cast<FnDecl*>(it->second.decl)));
      }
    }
  }

  // Add declartions to symbol table if not done yet 
  if (need_to_load)
    for (int i=0; i<classDecl->members->NumElements(); i++) {
      VarDecl *varDecl = dynamic_cast<VarDecl*>(classDecl->members->Nth(i));
      FnDecl *fnDecl = dynamic_cast<FnDecl*>(classDecl->members->Nth(i));
      if (varDecl)
        check(varDecl, true, false);
      else if (fnDecl) {
        if (inherited_fn.count(fnDecl->id->name)) {
          if (is_matched_prototype(fnDecl, inherited_fn.find(fnDecl->id->name)->second))
            override(fnDecl->id->name, fnDecl);
          else
            ReportError::OverrideMismatch(fnDecl);
        }
        else
          check(fnDecl, true, false);
      }
    }

  // Check each declartions for semantic errors if not read_only
  if (!load_only) {
    for (int i=0; i<classDecl->members->NumElements(); i++) {
      VarDecl *varDecl = dynamic_cast<VarDecl*>(classDecl->members->Nth(i));
      FnDecl *fnDecl = dynamic_cast<FnDecl*>(classDecl->members->Nth(i));
      if (varDecl)
        check(varDecl, false, true);
      else if (fnDecl)
        check(fnDecl, false, true);
    }
  }

  exit_scope();
}

void Semantic::check(InterfaceDecl *interfaceDecl){
  if (loaded.count(interfaceDecl)) return;

  const Symbol *declared = local_lookup(interfaceDecl->id->name);
  if (interfaceDecl == declared->decl)
    enter_scope(declared->scope);
  else
    enter_scope(ScopeType(interfaceScope));

  for (int i=0; i<interfaceDecl->members->NumElements(); i++){
    FnDecl *member = dynamic_cast<FnDecl*>(interfaceDecl->members->Nth(i));
    const Symbol *fnSymbol = local_lookup(member->id->name);
    if (fnSymbol)
      ReportError::DeclConflict(member, fnSymbol->decl);
    else
      insert_symbol(member->id->name, Symbol(member));
  }

  exit_scope();
  loaded.insert(interfaceDecl);
}

void Semantic::check(Stmt *stmt) {
  StmtBlock *stmtBlock = dynamic_cast<StmtBlock*>(stmt);
  ForStmt *forStmt = dynamic_cast<ForStmt*>(stmt);
  WhileStmt *whileStmt = dynamic_cast<WhileStmt*>(stmt);
  IfStmt *ifStmt = dynamic_cast<IfStmt*>(stmt);
  BreakStmt *breakStmt = dynamic_cast<BreakStmt*>(stmt);
  ReturnStmt *returnStmt = dynamic_cast<ReturnStmt*>(stmt);
  PrintStmt *printStmt = dynamic_cast<PrintStmt*>(stmt);
  Expr *expr = dynamic_cast<Expr*>(stmt);

  if (stmtBlock)
    check(stmtBlock);
  else if (forStmt)
    check(forStmt);
  else if (whileStmt)
    check(whileStmt);
  else if (ifStmt)
    check(ifStmt);
  else if (breakStmt)
    check(breakStmt);
  else if (returnStmt)
    check(returnStmt);
  else if(printStmt)
    check(printStmt);
  else if(expr)
    check(expr);
}

void Semantic::check(StmtBlock *stmtBlock) {
  enter_scope(ScopeType(blockScope));
  for (int i=0; i<stmtBlock->decls->NumElements(); i++)
    check(stmtBlock->decls->Nth(i), false, false);
  for (int i=0; i<stmtBlock->stmts->NumElements(); i++)
    check(stmtBlock->stmts->Nth(i));
  exit_scope();
}

void Semantic::check(ForStmt *forStmt) {
  enter_scope(ScopeType(loopScope));
  check(forStmt->init);
  const Type* test_type = check(forStmt->test);
  if (*test_type != *Type::boolType && *test_type != *Type::errorType)
    ReportError::TestNotBoolean(forStmt->test);
  check(forStmt->step);
  check(forStmt->body);
  exit_scope();
}

void Semantic::check(WhileStmt *whileStmt) {
  enter_scope(ScopeType(loopScope));
  const Type* test_type = check(whileStmt->test);
  if (*test_type != *Type::boolType && *test_type != *Type::errorType)
    ReportError::TestNotBoolean(whileStmt->test);
  check(whileStmt->body);
  exit_scope();
}

void Semantic::check(IfStmt *ifStmt) {
  enter_scope(ScopeType(ifScope));
  const Type* test_type = check(ifStmt->test);
  if (*test_type != *Type::boolType && *test_type != *Type::errorType)
    ReportError::TestNotBoolean(ifStmt->test);
  check(ifStmt->body);
  exit_scope();
}

void Semantic::check(BreakStmt *breakStmt) {
  Scope *p = current;
  while (p->type != ScopeType(rootScope)) {
    if (p->type == ScopeType(loopScope))
      return;
    p=p->parent;
  }
  ReportError::BreakOutsideLoop(breakStmt);
}

void Semantic::check(ReturnStmt *returnStmt) {
}

void Semantic::check(PrintStmt *printStmt) {
  for (int i=0; i<printStmt->args->NumElements(); i++) {
    const Type* arg_type = check(printStmt->args->Nth(i));
    if (*arg_type != *Type::intType && *arg_type != *Type::boolType && *arg_type != *Type::stringType && *arg_type != *Type::errorType)
      ReportError::PrintArgMismatch(printStmt->args->Nth(i), i+1, arg_type);
  }
}

const Type* Semantic::check(Expr *expr) {
  if (dynamic_cast<IntConstant*>(expr))
    return Type::intType;
  else if (dynamic_cast<DoubleConstant*>(expr))
    return Type::doubleType;
  else if (dynamic_cast<BoolConstant*>(expr))
    return Type::boolType;
  else if (dynamic_cast<StringConstant*>(expr))
    return Type::stringType;
  else if (dynamic_cast<NullConstant*>(expr))
    return Type::nullType;
  else if (dynamic_cast<ReadIntegerExpr*>(expr))
    return Type::intType;
  else if (dynamic_cast<ReadLineExpr*>(expr))
    return Type::stringType;
  else if (dynamic_cast<CompoundExpr*>(expr))
    return check(dynamic_cast<CompoundExpr*>(expr));
  else if (dynamic_cast<LValue*>(expr))
    return check(dynamic_cast<LValue*>(expr));
  else if (dynamic_cast<This*>(expr))
    return check(dynamic_cast<This*>(expr));
  else if (dynamic_cast<NewExpr*>(expr))
    return check(dynamic_cast<NewExpr*>(expr));
  else if (dynamic_cast<NewArrayExpr*>(expr))
    return check(dynamic_cast<NewArrayExpr*>(expr));

  return Type::errorType;
}

const Type* Semantic::check(CompoundExpr* expr) {
  if (expr->left == NULL){
    const Type* type_rhs = check(expr->right);
    return check_compatibility(expr->op, type_rhs);
  }
  else{
    const Type* type_lhs = check(expr->left);
    const Type* type_rhs = check(expr->right);
    return check_compatibility(expr->op, type_lhs, type_rhs);
  }
}

const Type* Semantic::check(LValue* expr){
  FieldAccess *fieldAccess = dynamic_cast<FieldAccess*>(expr);
  if (fieldAccess && !fieldAccess->base){
    const Symbol *symbol = lookup(fieldAccess->field->name);
    if (!symbol){
      ReportError::IdentifierNotDeclared(fieldAccess->field, reasonT(LookingForVariable));
      return Type::errorType;
    }

    VarDecl *varDecl = dynamic_cast<VarDecl*>(symbol->decl);
    if (!varDecl){
      ReportError::IdentifierNotDeclared(fieldAccess->field, reasonT(LookingForVariable));
      return Type::errorType;
    }

    if (varDecl->type == NULL)
      return Type::errorType;
    else
      return varDecl->type;
  }
  return Type::errorType;    
}

const Type* Semantic::check(This* expr){
  Scope *p = current;
  while (p->type != ScopeType(rootScope)){
    if (p->type == ScopeType(classScope) || p->type == ScopeType(interfaceScope)) {
      string id = scope_to_id.find(p)->second;
      return &(id_to_type.find(id)->second);
    }
    p=p->parent;
  }
  ReportError::ThisOutsideClassScope(expr);
  return Type::errorType;
}

const Type* Semantic::check(NewExpr *expr){
  const Symbol* type_symbol = lookup(expr->cType->id->name);
  if (type_symbol && dynamic_cast<ClassDecl*>(type_symbol->decl))
    return expr->cType;

  ReportError::IdentifierNotDeclared(expr->cType->id, reasonT(LookingForClass));
  return Type::errorType;
}
const Type* Semantic::check(NewArrayExpr *expr){
  if (*check(expr->size) != *Type::intType)
    ReportError::NewArraySizeNotInteger(expr->size);
  if (has_undefined_named_type(expr->elemType))
    return Type::errorType;
  array_types.push_back(ArrayType(yyltype(), expr->elemType));
  return &(array_types.back());
}

bool Semantic::has_undefined_named_type(const Type *type) {
  const Type *core_type = type;
  while (dynamic_cast<const ArrayType*>(core_type)) {
    core_type = dynamic_cast<const ArrayType*>(core_type) -> elemType;
  }
  const NamedType *named_type = dynamic_cast<const NamedType*>(core_type);
  if (named_type) {
    const Symbol *type_symbol = lookup(named_type->id->name);
    if (!type_symbol || (dynamic_cast<ClassDecl*>(type_symbol->decl)==NULL && dynamic_cast<InterfaceDecl*>(type_symbol->decl)==NULL)){
      ReportError::IdentifierNotDeclared(named_type->id, reasonT(LookingForType));
      return true;
    }
  }
  return false;
}

const Type* Semantic::check_compatibility(const Operator *op, const Type* rhs){
  if (!strcmp(op->tokenString, "-")){
    if (rhs == Type::intType || rhs == Type::doubleType) return rhs;
    if (rhs == Type::errorType) return Type::errorType;
    ReportError::IncompatibleOperand(op, rhs);
    return Type::errorType;
  }
  if (!strcmp(op->tokenString, "!")){
    if (rhs == Type::boolType) return rhs;
    if (rhs == Type::errorType) return Type::boolType;
    ReportError::IncompatibleOperand(op, rhs);
    return Type::boolType;
  }
  return Type::errorType;
}

const Type* Semantic::check_compatibility(const Operator *op, const Type* lhs, const Type* rhs) {
  if (!strcmp(op->tokenString, "+")
      || !strcmp(op->tokenString, "-")
      || !strcmp(op->tokenString, "*")
      || !strcmp(op->tokenString, "/")
      || !strcmp(op->tokenString, "%")){
    if (*lhs == *Type::intType && *rhs == *Type::intType) return Type::intType;
    if (*lhs == *Type::doubleType && *rhs == *Type::doubleType) return Type::doubleType;
    if (*lhs == *Type::errorType || *rhs == *Type::errorType) return Type::errorType;
    ReportError::IncompatibleOperands(op, lhs, rhs);
    return Type::errorType;
  }

  if (!strcmp(op->tokenString, "<")
      || !strcmp(op->tokenString, "<=")
      || !strcmp(op->tokenString, ">")
      || !strcmp(op->tokenString, ">=")) {
    if (*lhs == *Type::intType && *rhs == *Type::intType) return Type::boolType;
    if (*lhs == *Type::doubleType && *rhs == *Type::doubleType) return Type::boolType;
    if (*lhs == *Type::errorType || *rhs == *Type::errorType) return Type::boolType;
    ReportError::IncompatibleOperands(op, lhs, rhs);
    return Type::boolType;
  }

  if (!strcmp(op->tokenString, "==")
      || !strcmp(op->tokenString, "!=")) {
    if (*lhs == *rhs) return Type::boolType;
    if (*rhs == *Type::nullType && dynamic_cast<const NamedType*>(lhs)) return Type::boolType;
    if (*lhs == *Type::errorType || *rhs == *Type::errorType) return Type::boolType;
    if (is_compatible_inheritance(lhs, rhs)) return Type::boolType;
    ReportError::IncompatibleOperands(op, lhs, rhs);
    return Type::boolType;
  }

  if (!strcmp(op->tokenString, "&&")
      || !strcmp(op->tokenString, "||")) {
    if (*lhs == *Type::boolType && *rhs == *Type::boolType) return Type::boolType;
    if (*lhs == *Type::errorType || *rhs == *Type::errorType) return Type::boolType;
    ReportError::IncompatibleOperands(op, lhs, rhs);
    return Type::boolType;
  }

  if (!strcmp(op->tokenString, "=")) {
    if (*lhs == *rhs) return lhs;
    if (*rhs == *Type::nullType && dynamic_cast<const NamedType*>(lhs)) return lhs;
    if (*lhs == *Type::errorType || *rhs == *Type::errorType) return lhs;
    if (is_compatible_inheritance(lhs, rhs)) return lhs;
    ReportError::IncompatibleOperands(op, lhs, rhs);
    return lhs;
  }

  return Type::errorType;
}

bool Semantic::is_compatible_inheritance(const Type *parent, const Type *derived) {
  const NamedType *p = dynamic_cast<const NamedType*>(parent);
  const NamedType *d = dynamic_cast<const NamedType*>(derived);
  if (!p || !d) return false;

  return find_parent(d, p->id->name);
}

bool Semantic::find_parent(const NamedType *derived, string parent) {
  if (string(derived->id->name) == parent) return true;

  const Symbol *d = lookup(root, derived->id->name);
  ClassDecl *classDecl = dynamic_cast<ClassDecl*>(d->decl); 
  if (classDecl) {
    if (classDecl->extends!=NULL && id_to_type.count(classDecl->extends->id->name))
      if (find_parent(&(id_to_type.find(classDecl->extends->id->name)->second), parent)) return true;
    for (int i=0; i<classDecl->implements->NumElements(); i++)
      if (id_to_type.count(classDecl->implements->Nth(i)->id->name))
        if (find_parent(&(id_to_type.find(classDecl->implements->Nth(i)->id->name)->second), parent)) return true;
  }
  return false;
}

bool Semantic::is_matched_prototype(FnDecl *d1, FnDecl *d2) {
  if (*d1->returnType != *d2->returnType)
    return false;
  if (d1->formals->NumElements() != d2->formals->NumElements())
    return false;
  for (int i=0; i<d1->formals->NumElements(); i++)
    if (*d1->formals->Nth(i)->type != *d2->formals->Nth(i)->type)
      return false;
  return true;
}

void Semantic::analyze() {
  build(program);
  build_scope_to_id_map();
  build_id_to_type_map();
  check(program);
}
