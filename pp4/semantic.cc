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

Scope::Scope(Scope *parent, ScopeType type, Decl* decl) {
  if (type == ScopeType(classScope) || type == ScopeType(interfaceScope) || type == ScopeType(fnScope))
    assert(decl != NULL);
  this->parent = parent;
  this->type = type;
  this->decl = decl;
}

Semantic::Semantic(Program *program) {
  this->program = program;
  current = NULL;
  root = NULL;
  init();
}

void Semantic::init() {
  // length() for array
  List<VarDecl*> *empty_parameter_list = new List<VarDecl*>();
  Decl *length_decl = new FnDecl(new Identifier(yyltype(), "length()"), Type::intType, empty_parameter_list);
  fn_array_length = new Symbol(length_decl);
}

void Semantic::enter_scope(ScopeType type) {
  enter_scope(type, NULL);
}

void Semantic::enter_scope(ScopeType type, Decl *decl) {
  Scope *scope = new Scope(current, type, decl);
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
  Scope *scope = new Scope(current, fnScope, fnDecl);
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
    Scope *scope = new Scope(current, ScopeType(fnScope), fnDecl);
    if (!conflict) insert_symbol(name, Symbol(decl, scope));
  }
  else if (classDecl) {
    Scope *scope = new Scope(current, ScopeType(classScope), classDecl);
    if (!conflict) insert_symbol(name, Symbol(decl, scope));
  }
  else if (interfaceDecl) {
    Scope *scope = new Scope(current, ScopeType(interfaceScope), interfaceDecl);
    if (!conflict) insert_symbol(name, Symbol(decl, scope));
  }
}

void Semantic::build_id_to_type_map() {
  for (map<string, Symbol>::iterator it = root->symbols.begin(); it != root->symbols.end(); it++) {
    ClassDecl *classDecl = dynamic_cast<ClassDecl*>(it->second.decl);
    InterfaceDecl *interfaceDecl = dynamic_cast<InterfaceDecl*>(it->second.decl);

    if (classDecl || interfaceDecl)
      id_to_type.insert(pair<string, NamedType>(it->first, NamedType(lookup(root, it->first)->decl->id)));
  }
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
    Scope *scope = new Scope(current, ScopeType(fnScope), fnDecl);
    insert_symbol(name, Symbol(fnDecl, scope));
  }

  const Symbol *declared = local_lookup(name);
  if (fnDecl != declared->decl && !suppress_dup_error)
    ReportError::DeclConflict(fnDecl, declared->decl);

  if (symbol_only) return;
  
  if (fnDecl == declared->decl)
    enter_scope(declared->scope);
  else
    enter_scope(ScopeType(fnScope), fnDecl);

  // check return type
  if (has_undefined_named_type(fnDecl->returnType))
    fnDecl->returnType = Type::errorType;

  // check parameter list
  for (int i=0; i<fnDecl->formals->NumElements(); i++)
    check(dynamic_cast<VarDecl*>(fnDecl->formals->Nth(i)), true, false);
  for (int i=0; i<fnDecl->formals->NumElements(); i++)
    check(dynamic_cast<VarDecl*>(fnDecl->formals->Nth(i)), false, true);

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
    enter_scope(ScopeType(classScope), classDecl);

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
  
  // load-only checks ends
  if (load_only) {
    exit_scope();
    return;
  }

  // Check each declartions for semantic errors
  for (int i=0; i<classDecl->members->NumElements(); i++) {
    VarDecl *varDecl = dynamic_cast<VarDecl*>(classDecl->members->Nth(i));
    FnDecl *fnDecl = dynamic_cast<FnDecl*>(classDecl->members->Nth(i));
    if (varDecl)
      check(varDecl, false, true);
    else if (fnDecl)
      check(fnDecl, false, true);
  }

  // Check if interfaces implemented
  for (int i=0; i<classDecl->implements->NumElements(); i++) {
    string interface_name(classDecl->implements->Nth(i)->id->name);
    const Symbol *implementsSymbol = lookup(interface_name);
    if (implementsSymbol == NULL || dynamic_cast<InterfaceDecl*>(implementsSymbol->decl) == NULL)
      continue;

    for (map<string, Symbol>::iterator it = implementsSymbol->scope->symbols.begin(); it != implementsSymbol->scope->symbols.end(); it++) {
      const Symbol* actual_fn_symbol = local_lookup(it->first);
      FnDecl *actual_fn = dynamic_cast<FnDecl*>(actual_fn_symbol->decl);
      if (actual_fn->body == NULL) {
        ReportError::InterfaceNotImplemented(classDecl, classDecl->implements->Nth(i));
        break;
      }
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
    enter_scope(ScopeType(interfaceScope), interfaceDecl);

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
    check(stmtBlock->decls->Nth(i), true, false);
  for (int i=0; i<stmtBlock->decls->NumElements(); i++)
    check(stmtBlock->decls->Nth(i), false, true);
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
  if (ifStmt->elseBody != NULL)
    check(ifStmt->elseBody);
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
  const Type *actual_return_type = check(returnStmt->expr);
  Scope *p = current;
  while (p->type != ScopeType(fnScope))
    p = p->parent;
  Type* expected_return_type = dynamic_cast<FnDecl*>(p->decl)->returnType;
  if (check_compatibility(expected_return_type, actual_return_type)) return;
  if (is_compatible_inheritance(expected_return_type, actual_return_type)) return;
  ReportError::ReturnMismatch(returnStmt, actual_return_type, expected_return_type);
}

void Semantic::check(PrintStmt *printStmt) {
  for (int i=0; i<printStmt->args->NumElements(); i++) {
    const Type* arg_type = check(printStmt->args->Nth(i));
    if (*arg_type != *Type::intType && *arg_type != *Type::boolType && *arg_type != *Type::stringType && *arg_type != *Type::errorType)
      ReportError::PrintArgMismatch(printStmt->args->Nth(i), i+1, arg_type);
  }
}

const Type* Semantic::check(Expr *expr) {
  if (dynamic_cast<EmptyExpr*>(expr))
    return Type::voidType;
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
  else if (dynamic_cast<Call*>(expr))
    return check(dynamic_cast<Call*>(expr));  

  return Type::errorType;
}

const Type* Semantic::check(CompoundExpr* expr) {
  if (expr->left == NULL) {
    const Type* type_rhs = check(expr->right);
    return check_compatibility(expr->op, type_rhs);
  }
  else {
    const Type* type_lhs = check(expr->left);
    const Type* type_rhs = check(expr->right);
    return check_compatibility(expr->op, type_lhs, type_rhs);
  }
}

const Type* Semantic::check(LValue* expr) {
  FieldAccess *field_access = dynamic_cast<FieldAccess*>(expr);
  ArrayAccess *array_access = dynamic_cast<ArrayAccess*>(expr);
  
  if (field_access && field_access->base == NULL) {
    const Symbol *symbol = lookup(field_access->field->name);
    if (!symbol){
      ReportError::IdentifierNotDeclared(field_access->field, reasonT(LookingForVariable));
      return Type::errorType;
    }

    VarDecl *varDecl = dynamic_cast<VarDecl*>(symbol->decl);
    if (!varDecl){
      ReportError::IdentifierNotDeclared(field_access->field, reasonT(LookingForVariable));
      return Type::errorType;
    }

    return varDecl->type;
  }

  if (field_access && field_access->base != NULL) {
    const Type* base_type = check(field_access->base);
    if (*base_type == *Type::errorType) return Type::errorType;
    const NamedType* base_named_type = dynamic_cast<const NamedType*>(base_type);
    if (!base_named_type) {
      ReportError::FieldNotFoundInBase(field_access->field, base_type);
      return Type::errorType;
    }
    const Symbol* field_symbol = lookup(lookup(root, base_named_type->id->name)->scope, field_access->field->name);
    if (field_symbol == NULL || dynamic_cast<VarDecl*>(field_symbol->decl) == NULL) {
      ReportError::FieldNotFoundInBase(field_access->field, base_type);
      return Type::errorType;
    }
    Scope *p = current;
    bool in_scope = false;
    while (p != root) {
      if (p->type == ScopeType(classScope)) {
        if (strcmp(p->decl->id->name, base_named_type->id->name) == 0)
          in_scope = true;
        break;
      }
      p = p->parent;
    }

    if (!in_scope) {
      ReportError::InaccessibleField(field_access->field, base_type);
      return Type::errorType;
    }
    return dynamic_cast<VarDecl*>(field_symbol->decl)->type;
  }
  
  if (array_access) {
    const ArrayType *base_type = dynamic_cast<const ArrayType*>(check(array_access->base));
    if (!base_type)
      ReportError::BracketsOnNonArray(array_access->base);
    if (*check(array_access->subscript) != *Type::intType)
      ReportError::SubscriptNotInteger(array_access->subscript);
    if (base_type) return base_type->elemType;
    return Type::errorType;
  }

  assert(0);
}

const Type* Semantic::check(This* expr) {
  Scope *p = current;
  while (p->type != ScopeType(rootScope)){
    if (p->type == ScopeType(classScope) || p->type == ScopeType(interfaceScope)) {
      string id = p->decl->id->name;
      return &(id_to_type.find(id)->second);
    }
    p=p->parent;
  }
  ReportError::ThisOutsideClassScope(expr);
  return Type::errorType;
}

const Type* Semantic::check(NewExpr *expr) {
  const Symbol* type_symbol = lookup(expr->cType->id->name);
  if (type_symbol && dynamic_cast<ClassDecl*>(type_symbol->decl))
    return expr->cType;

  ReportError::IdentifierNotDeclared(expr->cType->id, reasonT(LookingForClass));
  return Type::errorType;
}

const Type* Semantic::check(NewArrayExpr *expr) {
  if (*check(expr->size) != *Type::intType)
    ReportError::NewArraySizeNotInteger(expr->size);
  if (has_undefined_named_type(expr->elemType))
    return Type::errorType;
  array_types.push_back(ArrayType(yyltype(), expr->elemType));
  return &(array_types.back());
}

const Type* Semantic::check(Call *expr) {
  const Type *base_type;
  const Symbol* called_fn;

  vector<const Type*> actual_types;
  for (int i=0; i<expr->actuals->NumElements(); i++)
    actual_types.push_back(check(expr->actuals->Nth(i)));

  if (expr->base != NULL) {
    base_type = check(expr->base);
    if (base_type == Type::errorType) return Type::errorType;
    const ArrayType *base_array_type = dynamic_cast<const ArrayType*>(base_type);
    
    // Check length() method for array
    if (base_array_type && strcmp(expr->field->name, "length")==0)
      called_fn = fn_array_length;
    else {
      const NamedType *base_named_type = dynamic_cast<const NamedType*>(base_type);
      if (!base_named_type) {
        ReportError::FieldNotFoundInBase(expr->field, base_type);
        return Type::errorType;
      }
      Scope *base_scope = lookup(root, base_named_type->id->name)->scope;
      called_fn = lookup(base_scope, expr->field->name);
    }
  }
  else
    called_fn = lookup(expr->field->name);

  // Check if function exists
  if (called_fn == NULL || !dynamic_cast<FnDecl*>(called_fn->decl)) {
    if (expr->base == NULL)
      ReportError::IdentifierNotDeclared(expr->field, reasonT(LookingForFunction));
    else
      ReportError::FieldNotFoundInBase(expr->field, base_type);
    return Type::errorType;
  }

  // Check # of arguments
  FnDecl *fnDecl = dynamic_cast<FnDecl*>(called_fn->decl);
  if (fnDecl->formals->NumElements() != expr->actuals->NumElements())
    ReportError::NumArgsMismatch(expr->field, fnDecl->formals->NumElements(), expr->actuals->NumElements());

  int num_args = min(fnDecl->formals->NumElements(), expr->actuals->NumElements());

  // Check types of arguments
  for (int i=0; i<num_args; i++) {
    const Type *formal_type = fnDecl->formals->Nth(i)->type;
    const Type *actual_type = actual_types[i];
    if (check_compatibility(formal_type, actual_type)) continue;
    if (is_compatible_inheritance(formal_type, actual_type)) continue;
    ReportError::ArgMismatch(expr->actuals->Nth(i), i+1, actual_type, formal_type);
  }

  return fnDecl->returnType;
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

const Type* Semantic::check_compatibility(const Operator *op, const Type* rhs) {
  if (!strcmp(op->tokenString, "-")) {
    if (rhs == Type::intType || rhs == Type::doubleType) return rhs;
    if (rhs == Type::errorType) return Type::errorType;
    ReportError::IncompatibleOperand(op, rhs);
    return Type::errorType;
  }
  if (!strcmp(op->tokenString, "!")) {
    if (rhs == Type::boolType) return rhs;
    if (rhs == Type::errorType) return Type::boolType;
    ReportError::IncompatibleOperand(op, rhs);
    return Type::boolType;
  }
  return Type::errorType;
}

const Type* Semantic::check_compatibility(const Operator *op, const Type* lhs, const Type* rhs) {
  const Type* resolved_type;
  resolved_type = check_compatibility(lhs, rhs);
  if (!strcmp(op->tokenString, "+")
      || !strcmp(op->tokenString, "-")
      || !strcmp(op->tokenString, "*")
      || !strcmp(op->tokenString, "/")
      || !strcmp(op->tokenString, "%")) {
    if (resolved_type != NULL && (*resolved_type == *Type::intType || *resolved_type == *Type::doubleType || *resolved_type == *Type::errorType))
      return resolved_type;
    ReportError::IncompatibleOperands(op, lhs, rhs);
    return Type::errorType;
  }

  if (!strcmp(op->tokenString, "<")
      || !strcmp(op->tokenString, "<=")
      || !strcmp(op->tokenString, ">")
      || !strcmp(op->tokenString, ">=")) {
    if (resolved_type != NULL && (*resolved_type == *Type::intType || *resolved_type == *Type::doubleType || *resolved_type == *Type::errorType))
      return Type::boolType;
    ReportError::IncompatibleOperands(op, lhs, rhs);
    return Type::boolType;
  }

  if (!strcmp(op->tokenString, "==")
      || !strcmp(op->tokenString, "!=")) {
    if (resolved_type != NULL) return Type::boolType;
    if (is_compatible_inheritance(lhs, rhs) || is_compatible_inheritance(rhs, lhs)) return Type::boolType;
    ReportError::IncompatibleOperands(op, lhs, rhs);
    return Type::boolType;
  }

  if (!strcmp(op->tokenString, "&&")
      || !strcmp(op->tokenString, "||")) {
    if (resolved_type != NULL && *resolved_type == *Type::boolType) return Type::boolType;
    ReportError::IncompatibleOperands(op, lhs, rhs);
    return Type::boolType;
  }

  if (!strcmp(op->tokenString, "=")) {
    if (resolved_type != NULL) return resolved_type;
    if (is_compatible_inheritance(lhs, rhs)) return lhs;
    ReportError::IncompatibleOperands(op, lhs, rhs);
    return lhs;
  }

  return Type::errorType;
}

const Type* Semantic::check_compatibility(const Type* lhs, const Type* rhs) {
  if (*lhs == *Type::errorType || *rhs == *Type::errorType) return Type::errorType;
  if (*lhs == *rhs) return lhs;
  if (*lhs == *Type::nullType && dynamic_cast<const NamedType*>(rhs)) return rhs;
  if (*rhs == *Type::nullType && dynamic_cast<const NamedType*>(lhs)) return lhs;
  return NULL;
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
  build_id_to_type_map();
  check(program);
}
