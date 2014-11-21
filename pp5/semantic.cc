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

#include "ast_stmt.h"
#include "ast_type.h"
#include "ast_decl.h"
#include "ast_expr.h"
#include "codegen.h"
#include "errors.h"
#include "semantic.h"
#include "tac.h"

using namespace std;

/***** Scope *****/

Scope::Scope(Scope *parent, ScopeType type, Decl* decl) {
  if (type == ScopeType(classScope) || type == ScopeType(interfaceScope) || type == ScopeType(fnScope))
    assert(decl != NULL);
  this->parent = parent;
  this->type = type;
  this->decl = decl;
}

/***** Symbol *****/
void Symbol::set_location_fp_relative(int offset) {
  location = new Location(fpRelative, offset, decl->id->name);
}

void Symbol::set_location_gp_relative(int offset) {
  location = new Location(gpRelative, offset, decl->id->name);
}

void Symbol::set_location_class_member(int offset) {
  location = new Location(NULL, offset);
}

/***** Commnon Internal ****/

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

Symbol* Semantic::lookup(Scope* scope, string s) const {
  if (scope->symbols.count(s) == 0)
    return NULL;
  else
    return &(scope->symbols.find(s)->second);
}

Symbol* Semantic::lookup(string s) const {
  Symbol *symbol;
  Scope *p = current;
  while (p != NULL){
    symbol = lookup(p, s);
    if (symbol != NULL) return symbol;
    p = p->parent;
  }
  return NULL;
}

Symbol* Semantic::local_lookup(string s) const {
  return lookup(current, s);
}

/***** Semantic Analyzer Internal *****/

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
    else if (fnDecl)
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
  stmtBlock->set_scope(current);
  for (int i=0; i<stmtBlock->decls->NumElements(); i++)
    check(stmtBlock->decls->Nth(i), true, false);
  for (int i=0; i<stmtBlock->decls->NumElements(); i++)
    check(stmtBlock->decls->Nth(i), false, true);
  for (int i=0; i<stmtBlock->stmts->NumElements(); i++)
    check(stmtBlock->stmts->Nth(i));
  exit_scope();
}

void Semantic::check(ForStmt *forStmt) {
  check(forStmt->init);
  const Type* test_type = check(forStmt->test);
  if (*test_type != *Type::boolType && *test_type != *Type::errorType)
    ReportError::TestNotBoolean(forStmt->test);
  check(forStmt->step);
  check(forStmt->body);
}

void Semantic::check(WhileStmt *whileStmt) {
  const Type* test_type = check(whileStmt->test);
  if (*test_type != *Type::boolType && *test_type != *Type::errorType)
    ReportError::TestNotBoolean(whileStmt->test);
  check(whileStmt->body);
}

void Semantic::check(IfStmt *ifStmt) {
  const Type* test_type = check(ifStmt->test);
  if (*test_type != *Type::boolType && *test_type != *Type::errorType)
    ReportError::TestNotBoolean(ifStmt->test);
  check(ifStmt->body);
  if (ifStmt->elseBody != NULL)
    check(ifStmt->elseBody);
}

void Semantic::check(BreakStmt *breakStmt) {
  Node* p = breakStmt->parent;
  while ((void*)p != (void*)program && !dynamic_cast<LoopStmt*>(p))
    p = p->parent;
  if (!dynamic_cast<LoopStmt*>(p))
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
  else if (dynamic_cast<FieldAccess*>(expr))
    return check(dynamic_cast<FieldAccess*>(expr));
  else if (dynamic_cast<ArrayAccess*>(expr))
    return check(dynamic_cast<ArrayAccess*>(expr));
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

const Type* Semantic::check(FieldAccess *field_access) {
  if (field_access->base == NULL) {
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

  if (!in_scope)
    ReportError::InaccessibleField(field_access->field, base_type);
  return dynamic_cast<VarDecl*>(field_symbol->decl)->type;
}

const Type* Semantic::check(ArrayAccess *array_access) {
  const ArrayType *base_type = dynamic_cast<const ArrayType*>(check(array_access->base));
  if (!base_type)
    ReportError::BracketsOnNonArray(array_access->base);
  if (*check(array_access->subscript) != *Type::intType)
    ReportError::SubscriptNotInteger(array_access->subscript);
  if (base_type) return base_type->elemType;
  return Type::errorType;
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

/**** Code Generator Internal ****/

bool Semantic::check_main() {
  for (map<string, Symbol>::iterator it = root->symbols.begin(); it != root->symbols.end(); it++) {
    if (strcmp(it->second.decl->id->name, "main") == 0 && dynamic_cast<FnDecl*>(it->second.decl) != NULL)
      return true;
  }
  ReportError::MainFunctionNotDefined();
  return false;
}

void Semantic::emit(Program *program) {
  enter_scope(root);
  for (int i=0; i<program->decls->NumElements(); i++) {
    Decl *decl = program->decls->Nth(i);
    VarDecl *varDecl = dynamic_cast<VarDecl*>(decl);
    FnDecl *fnDecl = dynamic_cast<FnDecl*>(decl);
    ClassDecl *classDecl = dynamic_cast<ClassDecl*>(decl);
    
    if (varDecl)
      emit(varDecl, true, &gp_offset);
    else if (fnDecl)
      emit(fnDecl);
    else if (classDecl)
      emit(classDecl);
  }
  exit_scope();
}

void Semantic::emit(VarDecl *varDecl, bool is_global, int *offset) {
  Symbol *symbol = local_lookup(varDecl->id->name);
  if (is_global) {
    symbol->set_location_gp_relative(*offset);
    *offset += 4;
  }
  else {
    symbol->set_location_fp_relative(*offset);
    *offset -= 4;
  }
}

void Semantic::emit(FnDecl *fnDecl) {
  char *fn_name = fnDecl->id->name;
  Scope *scope = local_lookup(fn_name)->scope;
  enter_scope(scope);

  // Generate Label
  char *label = get_function_label(fnDecl);
  instructions.GenLabel(label);
  free(label);

  // Set Location for Parameters - The parameters at fp+4 for functions, and fp+8 for methods
  if ((void*)fnDecl->parent == (void*)program) {
    for (int i=0; i<fnDecl->formals->NumElements(); i++) {
      Symbol *symbol = local_lookup(fnDecl->formals->Nth(i)->id->name);
      symbol -> set_location_fp_relative((i+1)*4);
    }
  }
  else {
    for (int i=0; i<fnDecl->formals->NumElements(); i++) {
      Symbol *symbol = local_lookup(fnDecl->formals->Nth(i)->id->name);
      symbol -> set_location_fp_relative((i+2)*4);
    }
  }

  // Generate BeginFunc
  BeginFunc *begin_func = instructions.GenBeginFunc();

  // Generate Statement
  int fp_offset = -8;
  emit(fnDecl->body, &fp_offset);

  // Set frame size of BeginFunc
  begin_func->SetFrameSize(-fp_offset-8);
  
  // Generate EndFunc
  instructions.GenEndFunc();

  exit_scope();
}

void Semantic::emit(ClassDecl *classDecl) {
  char *class_name = classDecl->id->name;
  Scope *scope = local_lookup(class_name)->scope;
  enter_scope(scope);

  // Initialize vtable and location for fields
  vector<FnDecl*> vtable;
  int field_offset = 4;
  init_current_class(classDecl, vtable, &field_offset);

  // Initialize method-to-index map
  for (int i=0; i<vtable.size(); i++)
    current->vtable_index.insert(pair<FnDecl*, int>(vtable[i], i));
  
  // Emit methods
  for (int i=0; i<classDecl->members->NumElements(); i++) {
    FnDecl *fnDecl = dynamic_cast<FnDecl*>(classDecl->members->Nth(i));
    if (fnDecl) emit(fnDecl);
  }

  // Emit vtable
  emit_vtable(class_name, vtable);

  exit_scope();
}

void Semantic::init_current_class(ClassDecl *classDecl, vector<FnDecl*> &vtable, int *field_offset) {
  if (classDecl->extends != NULL) {
    ClassDecl *parent_class = dynamic_cast<ClassDecl*>(lookup(root, classDecl->extends->id->name)->decl);
    init_current_class(parent_class, vtable, field_offset);
  }
  for (int i=0; i<classDecl->members->NumElements(); i++) {
    VarDecl *varDecl = dynamic_cast<VarDecl*>(classDecl->members->Nth(i));
    FnDecl *fnDecl = dynamic_cast<FnDecl*>(classDecl->members->Nth(i));

    if (varDecl) {
      Symbol *symbol = local_lookup(varDecl->id->name);
      symbol->set_location_class_member(*field_offset);
      *field_offset += 4;
    }
    if (fnDecl) {
      char *fn_name = fnDecl->id->name;
      bool override = false;
      for (int i=0; i<vtable.size(); i++)
        if (strcmp(vtable[i]->id->name, fn_name) == 0) {
          vtable[i] = fnDecl;
          override = true;
          break;
        }
      if (!override)
        vtable.push_back(fnDecl);
    }
  }
}

void Semantic::emit_vtable(char* class_name, const vector<FnDecl*> &vtable) {
  List<const char*> *labels = new List<const char*>();
  for (int i=0; i<vtable.size(); i++)
    labels->Append(get_function_label(vtable[i]));

  instructions.GenVTable(class_name, labels);
}

void Semantic::emit(Stmt *stmt, int *fp_offset) {
  StmtBlock *stmtBlock = dynamic_cast<StmtBlock*>(stmt);
  ForStmt *forStmt = dynamic_cast<ForStmt*>(stmt);
  WhileStmt *whileStmt = dynamic_cast<WhileStmt*>(stmt);
  IfStmt *ifStmt = dynamic_cast<IfStmt*>(stmt);
  BreakStmt *breakStmt = dynamic_cast<BreakStmt*>(stmt);
  ReturnStmt *returnStmt = dynamic_cast<ReturnStmt*>(stmt);
  PrintStmt *printStmt = dynamic_cast<PrintStmt*>(stmt);
  Expr *expr = dynamic_cast<Expr*>(stmt);

  if (stmtBlock)
    emit(stmtBlock, fp_offset);
  else if (forStmt)
    emit(forStmt, fp_offset);
  else if (whileStmt)
    emit(whileStmt, fp_offset);
  else if (ifStmt)
    emit(ifStmt, fp_offset);
  else if (breakStmt)
    emit(breakStmt, fp_offset);
  else if (returnStmt)
    emit(returnStmt, fp_offset);
  else if(printStmt)
    emit(printStmt, fp_offset);
  else if (expr) {
    CompoundExpr *compoundExpr = dynamic_cast<CompoundExpr*>(expr);
    if (compoundExpr && strcmp(compoundExpr->op->tokenString, "=") == 0)
      emit(expr, fp_offset, false);
    else
      emit(expr, fp_offset, true);
  }
}

void Semantic::emit(StmtBlock *stmtBlock, int *fp_offset) {
  enter_scope(stmtBlock->scope);
  for (int i=0; i<stmtBlock->decls->NumElements(); i++) {
    emit(stmtBlock->decls->Nth(i), false, fp_offset);
  }
  for (int i=0; i<stmtBlock->stmts->NumElements(); i++)
    emit(stmtBlock->stmts->Nth(i), fp_offset);
  exit_scope();
}

void Semantic::emit(ForStmt *forStmt, int *fp_offset) {
  char *start_label = instructions.NewLabel();
  char *finish_label = instructions.NewLabel();  
  forStmt->finish_label = finish_label;

  emit(forStmt->init, fp_offset);
  instructions.GenLabel(start_label);
  Location *location_test = emit(forStmt->test, fp_offset, true);
  instructions.GenIfZ(location_test, finish_label);
  emit(forStmt->body, fp_offset);
  emit(forStmt->step, fp_offset);
  instructions.GenGoto(start_label);
  instructions.GenLabel(finish_label);
}

void Semantic::emit(WhileStmt *whileStmt, int *fp_offset) {
  char *start_label = instructions.NewLabel();
  char *finish_label = instructions.NewLabel();
  whileStmt->finish_label = finish_label;
  
  instructions.GenLabel(start_label);
  Location *location_test = emit(whileStmt->test, fp_offset, true);
  instructions.GenIfZ(location_test, finish_label);
  emit(whileStmt->body, fp_offset);
  instructions.GenGoto(start_label);
  instructions.GenLabel(finish_label);
}

void Semantic::emit(IfStmt *ifStmt, int *fp_offset) {
  Location *location_test = emit(ifStmt->test, fp_offset, true);
  char *l0 = instructions.NewLabel();
  char *l1;
  instructions.GenIfZ(location_test, l0);
  emit(ifStmt->body, fp_offset);
  if (ifStmt->elseBody != NULL) {
    l1 = instructions.NewLabel();
    instructions.GenGoto(l1);
  }
  instructions.GenLabel(l0);
  if (ifStmt->elseBody != NULL) {  
    emit(ifStmt->elseBody, fp_offset);
    instructions.GenLabel(l1);
  }
}

void Semantic::emit(BreakStmt *breakStmt, int *fp_offset) {
  Node* p = breakStmt->parent;
  while (!dynamic_cast<LoopStmt*>(p))
    p = p->parent;
  instructions.GenGoto(dynamic_cast<LoopStmt*>(p)->finish_label);
}

void Semantic::emit(ReturnStmt *returnStmt, int *fp_offset) {
  Location *location = emit(returnStmt->expr, fp_offset, true);
  instructions.GenReturn(location);
}

void Semantic::emit(PrintStmt *printStmt, int *fp_offset) {
  for (int i=0; i<printStmt->args->NumElements(); i++) {
    Location *location = emit(printStmt->args->Nth(i), fp_offset, true);

    const Type *type = check(printStmt->args->Nth(i));

    if (*type == *Type::intType)
      instructions.GenBuiltInCall(PrintInt, location, NULL, fp_offset);
    else if (*type == *Type::stringType)
      instructions.GenBuiltInCall(PrintString, location, NULL, fp_offset);
    else if (*type == *Type::boolType)
      instructions.GenBuiltInCall(PrintBool, location, NULL, fp_offset);
  }
}

Location* Semantic::emit(Expr *expr, int *fp_offset, bool assignable_location) {
  Location *location = NULL;

  if (dynamic_cast<EmptyExpr*>(expr))
    ;
  else if (dynamic_cast<IntConstant*>(expr))
    location = instructions.GenLoadConstant(dynamic_cast<IntConstant*>(expr)->value, fp_offset);
  else if (dynamic_cast<DoubleConstant*>(expr))
    ;
  else if (dynamic_cast<BoolConstant*>(expr)) 
    location = instructions.GenLoadConstant(dynamic_cast<BoolConstant*>(expr)->value ? 1 : 0, fp_offset);
  else if (dynamic_cast<StringConstant*>(expr))
    location = instructions.GenLoadConstant(dynamic_cast<StringConstant*>(expr)->value, fp_offset);
  else if (dynamic_cast<NullConstant*>(expr))
    location = instructions.GenLoadConstant(0, fp_offset);
  else if (dynamic_cast<ReadIntegerExpr*>(expr))
    location = instructions.GenBuiltInCall(ReadInteger, NULL, NULL, fp_offset);
  else if (dynamic_cast<ReadLineExpr*>(expr))
    location = instructions.GenBuiltInCall(ReadLine, NULL, NULL, fp_offset);
  else if (dynamic_cast<CompoundExpr*>(expr))
    location = emit(dynamic_cast<CompoundExpr*>(expr), fp_offset);
  else if (dynamic_cast<FieldAccess*>(expr))
    location = emit(dynamic_cast<FieldAccess*>(expr), fp_offset);
  else if (dynamic_cast<ArrayAccess*>(expr))
    location = emit(dynamic_cast<ArrayAccess*>(expr), fp_offset);
  else if (dynamic_cast<This*>(expr))
    location = instructions.ThisPtr;
  else if (dynamic_cast<NewExpr*>(expr))
    location = emit(dynamic_cast<NewExpr*>(expr), fp_offset);
  else if (dynamic_cast<NewArrayExpr*>(expr))
    location = emit(dynamic_cast<NewArrayExpr*>(expr), fp_offset);
  else if (dynamic_cast<Call*>(expr))
    location = emit(dynamic_cast<Call*>(expr), fp_offset);

  if (assignable_location && location != NULL && location->GetSegment() == memoryAddr)
    return instructions.GenLoad(location->GetBase(), location->GetOffset(), fp_offset);

  return location;
}

Location* Semantic::emit(CompoundExpr *expr, int *fp_offset) {
  if (expr->left == NULL) {
    Location *location_rhs = emit(expr->right, fp_offset, true);
    return emit(expr->op, location_rhs, fp_offset);
  }
  else {
    if (!strcmp(expr->op->tokenString, "=")) {
      Location *location_lhs = emit(expr->left, fp_offset, false);
      Location *location_rhs = emit(expr->right, fp_offset, true);
      return emit_assignment(location_lhs, location_rhs);
    }
    else {
      Location *location_lhs = emit(expr->left, fp_offset, true);
      Location *location_rhs = emit(expr->right, fp_offset, true);
      return emit(expr->op, location_lhs, location_rhs, fp_offset);
    }
  }
}

Location* Semantic::emit(Operator *op, Location *rhs, int *fp_offset) {
  Location *zero = instructions.GenLoadConstant(0, fp_offset);

  if (!strcmp(op->tokenString, "-"))
    return instructions.GenBinaryOp("-", zero, rhs, fp_offset);

  if (!strcmp(op->tokenString, "!"))
    return instructions.GenBinaryOp("==", rhs, zero, fp_offset);

  return NULL;
}

Location* Semantic::emit(Operator *op, Location *lhs, Location *rhs, int *fp_offset) {
  if (!strcmp(op->tokenString, "+")
      || !strcmp(op->tokenString, "-")
      || !strcmp(op->tokenString, "*")
      || !strcmp(op->tokenString, "/")
      || !strcmp(op->tokenString, "%")
      || !strcmp(op->tokenString, "<")
      || !strcmp(op->tokenString, "==")
      || !strcmp(op->tokenString, "&&")
      || !strcmp(op->tokenString, "||"))
    return instructions.GenBinaryOp(op->tokenString, lhs, rhs, fp_offset);

  if (!strcmp(op->tokenString, "<=")) {
    Location *less_than_op = instructions.GenBinaryOp("<", lhs, rhs, fp_offset);
    Location *equal_op = instructions.GenBinaryOp("==", lhs, rhs, fp_offset);
    return instructions.GenBinaryOp("||", less_than_op, equal_op, fp_offset);
  }

  if (!strcmp(op->tokenString, ">"))
    return instructions.GenBinaryOp("<", rhs, lhs, fp_offset);

  if (!strcmp(op->tokenString, ">=")) {
    Location *less_than_op = instructions.GenBinaryOp("<", rhs, lhs, fp_offset);
    Location *equal_op = instructions.GenBinaryOp("==", rhs, lhs, fp_offset);
    return instructions.GenBinaryOp("||", less_than_op, equal_op, fp_offset);
  }

  if (!strcmp(op->tokenString, "!=")) {
    Location *equal_op = instructions.GenBinaryOp("==", lhs, rhs, fp_offset);
    Location *zero = instructions.GenLoadConstant(0, fp_offset);
    return instructions.GenBinaryOp("==", equal_op, zero, fp_offset);
  }

  assert(0);
}

Location* Semantic::emit_assignment(Location *lhs, Location *rhs) {
  if (lhs->GetSegment() == memoryAddr)
    instructions.GenStore(lhs->GetBase(), rhs, lhs->GetOffset());
  else
    instructions.GenAssign(lhs, rhs);
  return lhs;
}

Location* Semantic::emit(FieldAccess *expr, int *fp_offset) {
  if (expr->base == NULL) {
    Location *location = lookup(expr->field->name)->location;
    if (location->GetSegment() == memoryAddr)
      return new Location(instructions.ThisPtr, location->GetOffset());
    return location;
  }

  Location *base_location = emit(expr->base, fp_offset, true);
  const NamedType *base_type = dynamic_cast<const NamedType*>(check(expr->base));
  Scope* class_scope = lookup(root, base_type->id->name)->scope;
  Location *member_location = lookup(class_scope, expr->field->name)->location;
  return new Location(base_location, member_location->GetOffset());
}

Location* Semantic::emit(ArrayAccess *expr, int *fp_offset) {
  Location *base = emit(expr->base, fp_offset, true);
  Location *index = emit(expr->subscript, fp_offset, true);

  char *l0 = instructions.NewLabel();
  Location *zero = instructions.GenLoadConstant(0, fp_offset);
  Location *less_than_zero = instructions.GenBinaryOp("<", index, zero, fp_offset);
  Location *array_size = instructions.GenLoad(base, -4, fp_offset);
  Location *less_than_size = instructions.GenBinaryOp("<", index, array_size, fp_offset);
  Location *ge_size = instructions.GenBinaryOp("==", less_than_size, zero, fp_offset);
  Location *error_condition = instructions.GenBinaryOp("||", less_than_zero, ge_size, fp_offset);
  instructions.GenIfZ(error_condition, l0);
  Location *error_msg = instructions.GenLoadConstant("Decaf runtime error: Array subscript out of bounds\\n", fp_offset);
  instructions.GenBuiltInCall(PrintString, error_msg, NULL, fp_offset);
  instructions.GenBuiltInCall(Halt, NULL, NULL, fp_offset);
  instructions.GenLabel(l0);
  Location *four = instructions.GenLoadConstant(4, fp_offset);
  Location *offset = instructions.GenBinaryOp("*", four, index, fp_offset);
  Location *address = instructions.GenBinaryOp("+", base, offset, fp_offset);
  Location *ref = new Location(address, 0);
  
  return ref;
}

Location* Semantic::emit(NewExpr *expr, int *fp_offset) {
  Scope* class_scope = lookup(root, expr->cType->id->name)->scope;
  int field_count = 0;
  for (map<string, Symbol>::iterator it = class_scope->symbols.begin(); it != class_scope->symbols.end(); it++)
    if (dynamic_cast<VarDecl*>(it->second.decl))
      field_count++;

  Location *alloc_size = instructions.GenLoadConstant(4*(field_count+1), fp_offset);
  Location *alloc = instructions.GenBuiltInCall(Alloc, alloc_size, NULL, fp_offset);
  Location *vtable = instructions.GenLoadLabel(class_scope->decl->id->name, fp_offset);
  instructions.GenStore(alloc, vtable, 0);
  return alloc;
}

Location* Semantic::emit(NewArrayExpr *expr, int *fp_offset) {
  Location *size = emit(expr->size, fp_offset, true);

  char *l0 = instructions.NewLabel();
  Location *zero = instructions.GenLoadConstant(0, fp_offset);
  Location *size_test = instructions.GenBinaryOp("<", size, zero, fp_offset);
  instructions.GenIfZ(size_test, l0);
  Location *error_msg = instructions.GenLoadConstant("Decaf runtime error: Array size is <= 0\\n", fp_offset);
  instructions.GenBuiltInCall(PrintString, error_msg, NULL, fp_offset);
  instructions.GenBuiltInCall(Halt, NULL, NULL, fp_offset);
  instructions.GenLabel(l0);
  Location *one = instructions.GenLoadConstant(1, fp_offset);
  Location *size_p1 = instructions.GenBinaryOp("+", one, size, fp_offset);
  Location *four = instructions.GenLoadConstant(4, fp_offset);
  Location *alloc_size = instructions.GenBinaryOp("*", size_p1, four, fp_offset);
  Location *alloc = instructions.GenBuiltInCall(Alloc, alloc_size, NULL, fp_offset);
  instructions.GenStore(alloc, size, 0);
  Location *new_array = instructions.GenBinaryOp("+", alloc, four, fp_offset);
  
  return new_array;
}

Location* Semantic::emit(Call *expr, int *fp_offset) {
  vector<Location*> actuals;
  for (int i=0; i<expr->actuals->NumElements(); i++)
    actuals.push_back(emit(expr->actuals->Nth(i), fp_offset, true));

  Location *location_return;
  
  if (expr->base == NULL) {
    FnDecl *fnDecl = dynamic_cast<FnDecl*>(lookup(expr->field->name)->decl);

    // Emit function call
    if ((void*)fnDecl->parent == (void*)program) {
      for (int i=actuals.size()-1; i>=0; i--)
        instructions.GenPushParam(actuals[i]);
      location_return = instructions.GenLCall(get_function_label(fnDecl), *fnDecl->returnType != *Type::voidType, fp_offset);
      instructions.GenPopParams(4*actuals.size());
    }
    // Emit method call with implicit base
    else {
      Location *base_location = instructions.ThisPtr;
      
      Scope* class_scope = lookup(root, dynamic_cast<ClassDecl*>(fnDecl->parent)->id->name)->scope;
      FnDecl *method = dynamic_cast<FnDecl*>(lookup(class_scope, expr->field->name)->decl);

      int fn_index = class_scope->vtable_index.find(method)->second;
      Location *vtable = instructions.GenLoad(base_location, 0, fp_offset);
      Location *fn_addr = instructions.GenLoad(vtable, fn_index*4, fp_offset);
    
      for (int i=actuals.size()-1; i>=0; i--)
        instructions.GenPushParam(actuals[i]);
      instructions.GenPushParam(base_location);
      location_return = instructions.GenACall(fn_addr, *method->returnType != *Type::voidType, fp_offset);
      instructions.GenPopParams(4*actuals.size()+4);
    }
  }
  else {
    Location *base_location = emit(expr->base, fp_offset, true);
    const Type* base_type = check(expr->base);

    // Check if the function call is array.length()
    if (dynamic_cast<const ArrayType*>(base_type))
      return emit_array_length(base_location, fp_offset);

    // Emit method call
    const NamedType *base_named_type = dynamic_cast<const NamedType*>(base_type);
    Scope* class_scope = lookup(root, base_named_type->id->name)->scope;
    FnDecl *method = dynamic_cast<FnDecl*>(lookup(class_scope, expr->field->name)->decl);

    int fn_index = class_scope->vtable_index.find(method)->second;
    Location *vtable = instructions.GenLoad(base_location, 0, fp_offset);
    Location *fn_addr = instructions.GenLoad(vtable, fn_index*4, fp_offset);
    
    for (int i=actuals.size()-1; i>=0; i--)
      instructions.GenPushParam(actuals[i]);
    instructions.GenPushParam(base_location);
    location_return = instructions.GenACall(fn_addr, *method->returnType != *Type::voidType, fp_offset);
    instructions.GenPopParams(4*actuals.size()+4);
  }

  return location_return;
}

Location* Semantic::emit_array_length(Location *array_base, int *fp_offset) {
  Location *array_length = instructions.GenLoad(array_base, -4, fp_offset);
  return array_length;
}

/***** External *****/

Semantic::Semantic(Program *program) {
  this->program = program;
  current = NULL;
  root = NULL;
  List<VarDecl*> *empty_parameter_list = new List<VarDecl*>();
  Decl *length_decl = new FnDecl(new Identifier(yyltype(), "length()"), Type::intType, empty_parameter_list);
  fn_array_length = new Symbol(length_decl);

  gp_offset = 0;
}

void Semantic::analyze() {
  build(program);
  build_id_to_type_map();
  check(program);
}

void Semantic::generate() {
  if (!check_main()) return;
  emit(program);
  instructions.DoFinalCodeGen();
}

char* Semantic::get_function_label(FnDecl *fnDecl) {
  char *fn_name = fnDecl->id->name;
  char *label;

  if ((void*)fnDecl->parent == (void*)program) {
    if (strcmp(fn_name, "main") == 0)
      label = strdup(fn_name);
    else {
      label = (char*)malloc(sizeof(char)*(strlen(fn_name)+1));
      sprintf(label, "_%s", fn_name);
    }
  }
  else {
    char *class_name = dynamic_cast<ClassDecl*>(fnDecl->parent)->id->name;
    label = (char*)malloc(sizeof(char)*(strlen(fn_name)+strlen(class_name)+2));    
    sprintf(label, "_%s.%s", class_name, fn_name);
  }

  return label;
}
