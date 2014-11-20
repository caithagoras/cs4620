/* File: ast_stmt.h
 * ----------------
 * The Stmt class and its subclasses are used to represent
 * statements in the parse tree.  For each statment in the
 * language (for, if, return, etc.) there is a corresponding
 * node class for that construct. 
 *
 * pp3: You will need to extend the Stmt classes to implement
 * semantic analysis for rules pertaining to statements.
 */


#ifndef _H_ast_stmt
#define _H_ast_stmt

#include "list.h"
#include "ast.h"

class Decl;
class Expr;
class VarDecl;
class Scope;
class Semantic;
  
class Program : public Node{
 protected:
  List<Decl*> *decls;
  friend class Semantic;
  
 public:
  Program(List<Decl*> *declList);
  void CheckAndEmit();
};

class Stmt : public Node {
 public:
 Stmt() : Node() {}
 Stmt(yyltype loc) : Node(loc) {}
  virtual ~Stmt() {}
};

class StmtBlock : public Stmt {
  friend class Semantic;

 protected:
  List<VarDecl*> *decls;
  List<Stmt*> *stmts;
  Scope *scope;
    
 public:
  StmtBlock(List<VarDecl*> *variableDeclarations, List<Stmt*> *statements);
  void set_scope(Scope *scope);
};

  
class ConditionalStmt : public Stmt {
  friend class Semantic;
  
 protected:
  Expr *test;
  Stmt *body;

 public:
  ConditionalStmt(Expr *testExpr, Stmt *body);
};

class LoopStmt : public ConditionalStmt {
  friend class Semantic;

 protected:
  char *finish_label;

 public:
 LoopStmt(Expr *testExpr, Stmt *body)
   : ConditionalStmt(testExpr, body) {}
};

class ForStmt : public LoopStmt {
  friend class Semantic;
 protected:
  Expr *init, *step;
  
 public:
  ForStmt(Expr *init, Expr *test, Expr *step, Stmt *body);
};

class WhileStmt : public LoopStmt {
  friend class Semantic;
 public:
 WhileStmt(Expr *test, Stmt *body) : LoopStmt(test, body) {}
};

class IfStmt : public ConditionalStmt {
  friend class Semantic;

 protected:
  Stmt *elseBody;
  
 public:
  IfStmt(Expr *test, Stmt *thenBody, Stmt *elseBody);
};

class BreakStmt : public Stmt {
 public:
 BreakStmt(yyltype loc) : Stmt(loc) {}
};

class ReturnStmt : public Stmt  
{
  friend class Semantic;
  protected:
    Expr *expr;
  
  public:
    ReturnStmt(yyltype loc, Expr *expr);
};

class PrintStmt : public Stmt
{
  friend class Semantic;
  protected:
    List<Expr*> *args;
    
  public:
    PrintStmt(List<Expr*> *arguments);
};


#endif
