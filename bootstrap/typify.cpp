/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.

   - look up all name references and complete their namespaces
 */

#include "apt.h"
#include "errcodes.h"
#include "log.h"
#include "predefined.h"
#include "properties.h"
#include "scope.h"
#include "str.h"
#include "symbol.h"
#include "traverse.h"
#include "typectx.h"
#include "typify.h"

#include <set>


using namespace heather;

//----------------------------------------------------------------------------

Typifier::Typifier()
  : fPhase(kTypify)
{
}


void
Typifier::typifyRecursively(AptNode* node)
{
  fPhase = kTypify;
  typifyNode(node);

  fPhase = kCheck;
  typifyNode(node);
}


void
Typifier::typifyNode(AptNode* node)
{
  node->typify(this);
}


void
Typifier::typify(CompileUnitNode* node)
{
  typifyNodeList(node->children());
}


void
Typifier::typifyNodeList(NodeList& nl)
{
  for (size_t i = 0; i < nl.size(); i++)
    typifyNode(nl[i]);
}


//------------------------------------------------------------------------------

void
Typifier::typify(SymbolNode* node)
{
  // TODO
  if (fPhase == kTypify) {
    const AptNode* var = node->scope()->lookupVarOrFunc(node->name(), true);
    if (var != NULL) {
      node->setType(var->type());
      return;
    }

    Type type = node->scope()->lookupType(node->name(), true);
    if (type.isDef()) {
      node->setType(type);
      return;
    }
  }
}


void
Typifier::typify(ArraySymbolNode* node)
{
  //TODO
}


//------------------------------------------------------------------------------

void
Typifier::typify(DefNode* node)
{
  typifyNode(node->defNode());
  if (fPhase == kTypify)
    node->setType(node->type());
}


void
Typifier::typify(LetNode* node)
{
  typifyNode(node->defNode());
  if (fPhase == kTypify)
    node->setType(node->type());
}


void
Typifier::setupBindingNodeType(BindingNode* node, const char* errdesc)
{
  assert(node->scope() != NULL);

  if (node->type().isDef() && node->type().isOpen())
    return;

  String typenm = ( node->type().isDef()
                    ? node->type().typeName()
                    : Names::kAnyTypeName );
  Type bindty = ( node->type().isDef()
                  ? node->scope()->lookupType(node->type())
                  : node->scope()->lookupType(Names::kAnyTypeName, true) );
  if (!bindty.isDef()) {
    errorf(node->srcpos(), E_UndefinedType,
           "undefined type '%s' in %s",
           (const char*)StrHelper(typenm),
           errdesc);
  }
  else {
    assert(bindty.isDef());
    // if (bindty.isOpen()) {
    //   if (node->type().isDef())
    //     bindty = node->scope()->normalizeType(bindty, node->type());
    // }
    node->setType(bindty);

    if (node->initExpr() != NULL) {
      if (bindty.isAny()) {
        // infer the vardef type from the init expression
        node->setType(node->initExpr()->type());
      }
      else if (!node->initExpr()->type().isDef()) {
        errorf(node->initExpr()->srcpos(), E_TypeMismatch,
               "Undefined type in %s initialization", errdesc);
      }
      else if (!isContravariant(bindty, node->initExpr()->type(),
                                node->scope(), node->srcpos())) {
        errorf(node->initExpr()->srcpos(), E_TypeMismatch,
               "type mismatch in %s initialization", errdesc);
      }
      else {
        // infer the vardef type from the init expression
        node->setType(node->initExpr()->type());
      }
    }
  }
}


void
Typifier::typify(VardefNode* node)
{
  if (node->initExpr() != NULL)
    typifyNode(node->initExpr());

  if (fPhase == kTypify) {
    setupBindingNodeType(node, "variable");
  }
}


void
Typifier::setupFunctionNodeType(FunctionNode* node)
{
  FunctionParamVector params;
  for (size_t i = 0; i < node->params().size(); i++) {
    ParamNode* prmnd = dynamic_cast<ParamNode*>(node->params()[i].obj());
    assert(prmnd != NULL);

    if (prmnd->flags() == kPosArg) {
      params.push_back(FunctionParameter(FunctionParameter::kParamPos,
                                         false, String(),
                                         prmnd->type()));
    }
    else if (prmnd->flags() == kSpecArg) {
      params.push_back(FunctionParameter(FunctionParameter::kParamPos,
                                         true, String(),
                                         prmnd->type()));
    }
    else if (prmnd->flags() == kNamedArg) {
      params.push_back(FunctionParameter(FunctionParameter::kParamNamed,
                                         false, prmnd->key(),
                                         prmnd->type()));
    }
    else if (prmnd->flags() == kRestArg) {
      params.push_back(FunctionParameter(FunctionParameter::kParamRest,
                                         false, String(),
                                         prmnd->type()));
    }
    else {
      assert(0 && "undefined parameter type?");
    }
  }

  if (!node->retType().isDef() || node->retType().isAny()) {
    // try to infer the return type from the body's type
    if (node->body() != NULL) {
      node->setRetType(node->body()->type());
    }
  }
  else if (!node->retType().isOpen()) {
    String typenm = ( node->retType().isDef()
                      ? node->retType().typeName()
                      : Names::kAnyTypeName );
    Type retty = ( node->retType().isDef()
                   ? node->scope()->lookupType(node->retType())
                   : node->scope()->lookupType(Names::kAnyTypeName, true) );
    if (!retty.isDef()) {
      errorf(node->srcpos(), E_UndefinedType,
             "undefined return type '%s'",
             (const char*)StrHelper(typenm));
    }
    else
      node->setRetType(retty);
  }

  FunctionSignature sign(false, String(), node->retType(), params);
  node->setType(Type::newFunction(sign));
}


namespace heather
{
  class FindReturnTraverseDelegate : public TraverseDelegate
  {
  public:
    virtual bool preApply(AptNode* node)
    {
      // don't dive into nested functions
      if (dynamic_cast<FunctionNode*>(node) != NULL)
        return false;

      ApplyNode* apply = dynamic_cast<ApplyNode*>(node);
      if (apply != NULL && apply->isSimpleCall() &&
          apply->simpleCallName() == Names::kLangReturn)
        fReturns.push_back(apply);
      return true;
    }

    virtual void postApply(AptNode* node)
    {
    }

    NodeList fReturns;
  };
};


void
Typifier::checkFunctionReturnType(FunctionNode* node)
{
  if (node->body() != NULL) {
    if (!isContravariant(node->retType(), node->body()->type(),
                         node->scope(), node->srcpos()))
    {
      errorf(node->srcpos(), E_TypeMismatch,
             "function's body type does not match its return type");
    }


    FindReturnTraverseDelegate delegate;
    Traversator(delegate).traverseNode(node);

    for (size_t i = 0; i < delegate.fReturns.size(); i++) {
      AptNode* ret = delegate.fReturns[i];
      if (!isContravariant(node->retType(), ret->type(), ret->scope(),
                           ret->srcpos()))
      {
        errorf(ret->srcpos(), E_TypeMismatch,
               "return's type does not match outer block type");
      }
    }
  }
}


void
Typifier::typify(FuncDefNode* node)
{
  typifyNodeList(node->params());
  if (node->body() != NULL)
    typifyNode(node->body());

  if (fPhase == kTypify)
    setupFunctionNodeType(node);
  else if (fPhase == kCheck)
    checkFunctionReturnType(node);
}


void
Typifier::typify(FunctionNode* node)
{
  typifyNodeList(node->params());
  if (node->body() != NULL)
    typifyNode(node->body());

  if (fPhase == kTypify)
    setupFunctionNodeType(node);
  else if (fPhase == kCheck)
    checkFunctionReturnType(node);
}


void
Typifier::typify(SlotdefNode* node)
{
  // TODO
}


void
Typifier::typify(BlockNode* node)
{
  assert(!node->children().empty());

  typifyNodeList(node->children());

  if (fPhase == kTypify) {
    node->setType(node->children().back()->type());

    // look whether we have a on-exit handler; try to infer its parameter
    for (size_t i = 0; i < node->children().size(); ++i) {
      OnNode* onnd = dynamic_cast<OnNode*>(node->children()[i].obj());
      if (onnd != NULL && onnd->key() == Names::kExitKeyword) {
        assert(!onnd->params().empty());

        ParamNode* firstPrm = dynamic_cast<ParamNode*>(onnd->params()[0].obj());
        assert(firstPrm != NULL);

        if (firstPrm->type().isAny()) {
          // infer parameters type from block type
          firstPrm->setType(node->type());

          // retypify the on-node in the kTypify-phase
          typify(onnd);
        }
      }
    }
  }
  else if (fPhase == kCheck) {
    for (size_t i = 0; i < node->children().size(); ++i) {
      OnNode* onnd = dynamic_cast<OnNode*>(node->children()[i].obj());
      if (onnd != NULL && onnd->key() == Names::kExitKeyword) {
        if (!isContravariant(node->type(), onnd->body()->type(), node->scope(),
                             onnd->body()->srcpos()))
        {
          errorf(onnd->body()->srcpos(), E_TypeMismatch,
                 "on-exit handler type does not match outer block type");
        }
      }
    }
  }
}


void
Typifier::typify(ParamNode* node)
{
  if (node->initExpr() != NULL)
    typifyNode(node->initExpr());

  if (fPhase == kTypify) {
    setupBindingNodeType(node, "parameter");
  }
}


void
Typifier::checkArgParamType(TypeCtx& localCtx,
                            const ParamNode* param,
                            AptNode* arg,
                            int idx)
{
  if (param->type().isOpen()) {
    param->type().matchGenerics(localCtx, arg->type(),
                                arg->scope(), arg->srcpos());
  }
  else {
    if (!isContravariant(param->type(), arg->type(), arg->scope(),
                         arg->srcpos()))
    {
      errorf(arg->srcpos(), E_TypeMismatch,
             "type mismatch for argument %d", idx);
    }
  }
}


Typifier::KeyargReturn
Typifier::findKeyedArg(const NodeList& args, size_t argidx, const String& key)
{
  KeyargReturn retval = { NULL, 0 };

  for (size_t i = argidx; i < args.size(); i++) {
    const KeyargNode* keyarg = dynamic_cast<const KeyargNode*>(args[i].obj());
    if (keyarg == NULL) {
      return retval;
    }

    if (keyarg->key() == key) {
      retval.fKeyarg = keyarg;
      retval.fIdx = i;
      return retval;
    }
  }
  return retval;
}


void
Typifier::typifyMatchAndCheckParameters(ApplyNode* node,
                                        const FunctionNode* funcNode,
                                        const NodeList& funcParams)
{
/*
  def param.is-generic-type():
    if isRef() && isGeneric(): -> true
    if isRef() && constraints.containsGenericTypeRef(): -> true

  def check-arg-param-type():
    if param.is-generic-type:
      if generic-type is known yet:
        if not isSameType(generics-table().type(), arg.type():
          error(type mismatch)
      else:
        register-generic-type(generic-type)
    else if param.type().contains-generic-type:
      ty = match-generic-type-recursivly(params.type(), arg.type()
      if ty is known yet:
        if not isSameType(ty, arg.type():
          error(type mismatch)
      else:
        register-generic-type(ty)
    else:
      if not isContravariant(param.type, arg.type):
        error

  def create-sequence-type-from-rest-arg():
    ...

  for-each param in params:
    if param.isPositional():
      if not has-arg:
        error(wrong number of args)
      else:
        check-arg-param-type(arg)
    else if param.isNamed():
      if has-named-arg:
        check-arg-param-type(arg)
      else:
        check-arg-param-type(init-expr)
    else if param.isRest():
      create-sequence-type-from-rest-arg()

  if more args than params (and not rest-param):
    error(wrong number of args)
  */

  NodeList args = node->children();

  TypeCtx localCtx;
  size_t argidx = 0;
  std::set<int> argIndicesUsed;
  for (size_t i = 0; i < funcParams.size(); ++i) {
    const ParamNode* param = dynamic_cast<const ParamNode*>(funcParams[i].obj());

    if (param != NULL) {
      if (param->flags() == kPosArg || param->flags() == kSpecArg) {
        AptNode* arg = args[argidx].obj();

        if (i >= args.size()) {
          errorf(node->srcpos(), E_BadArgNumber, "not enough arguments");
          return;
        }
        checkArgParamType(localCtx, param, arg, i);
        argidx++;
      }
      else if (param->flags() == kNamedArg) {
        Typifier::KeyargReturn keyval = findKeyedArg(args, argidx, param->key());
        if (keyval.fKeyarg == NULL) {
          checkArgParamType(localCtx, param, param->initExpr(), i);
        }
        else {
          checkArgParamType(localCtx, param, keyval.fKeyarg->value(), keyval.fIdx);
          argIndicesUsed.insert(keyval.fIdx);
        }
      }
      else if (param->flags() == kRestArg) {
        // TypeVector types;
        // for (size_t j = 0; j < args.size(); j++) {
        //   if (argIndicesUser.find(j) != argIndicesUser.end()) {
        //     // argument not yet used
        //     types.push_back(args[j]->type());
        //   }
        // }
        // Type seq = Type::newSequence(types, true);
        argidx = args.size();
      }
      else {
        assert(0);
      }
    }
  }

  if (argidx < args.size()) {
    errorf(node->srcpos(), E_BadArgNumber,
           "Too much arguments");
  }

  if (funcNode->retType().isOpen()) {
    Type retty = funcNode->retType().replaceGenerics(localCtx);
    // fprintf(stderr, "RETTY: %s\n", (const char*)StrHelper(retty.toString()));

    if (retty.isDef())
      node->setType(retty);
    else
      errorf(node->srcpos(), E_TypeMismatch,
             "function has unmatched generic return type.");

    // Type retty = funcNode->retType();
    // if (localCtx.hasType(retty.typeName())) {
    //   node->setType(localCtx.lookupType(retty.typeName()));
    // }
    // else {
    //   errorf(node->srcpos(), E_TypeMismatch,
    //          "function has unmatched generic return type.");
    // }
 }
  else {
    node->setType(funcNode->retType());
  }
}


void
Typifier::typify(ApplyNode* node)
{
  typifyNode(node->base());
  typifyNodeList(node->children());

  if (fPhase == kTypify) {
    if (node->isSimpleCall()) {
      const FunctionNode* funcNode = (
        dynamic_cast<const FunctionNode*>(node->scope()
                                          ->lookupFunction(node->simpleCallName(), true)) );
      if (funcNode != NULL)
        typifyMatchAndCheckParameters(node, funcNode, funcNode->params());
    }
  }
}


void
Typifier::typify(ArrayNode* node)
{
  typifyNodeList(node->children());
}


void
Typifier::typify(AssignNode* node)
{
  typifyNode(node->lvalue());
  typifyNode(node->rvalue());
}


void
Typifier::typify(BinaryNode* node)
{
  typifyNode(node->left());
  typifyNode(node->right());
}


void
Typifier::typify(NegateNode* node)
{
  typifyNode(node->base());
}


void
Typifier::typify(IfNode* node)
{
  typifyNode(node->test());
  typifyNode(node->consequent());
  if (node->alternate())
    typifyNode(node->alternate());
}


void
Typifier::typify(KeyargNode* node)
{
  typifyNode(node->value());
}


void
Typifier::typify(MatchNode* node)
{
  typifyNode(node->expr());
  for (size_t i = 0; i < node->mappings().size(); i++) {
    typifyNode(node->mappings()[i].fConsequent);
  }
}


void
Typifier::typify(SelectNode* node)
{
  typifyNode(node->test());
  if (node->comparator() != NULL)
    typifyNode(node->comparator());

  for (size_t i = 0; i < node->mappings().size(); i++) {
    if (node->mappings()[i].fTestValues.empty()) {
      typifyNode(node->mappings()[i].fConsequent);
    }
    else {
      for (size_t j = 0; j < node->mappings()[i].fTestValues.size(); j++)
        typifyNode(node->mappings()[i].fTestValues[j]);
    }
    typifyNode(node->mappings()[i].fConsequent);
  }
}


void
Typifier::typify(OnNode* node)
{
  typifyNodeList(node->params());
  typifyNode(node->body());
}


void
Typifier::typify(RangeNode* node)
{
  typifyNode(node->from());
  typifyNode(node->to());
  if (node->by() != NULL)
    typifyNode(node->by());
}


void
Typifier::typify(ThenWhileNode* node)
{
  typifyNode(node->first());
  typifyNode(node->step());
  typifyNode(node->test());
}


void
Typifier::typify(TypeDefNode* node)
{
  // TODO
}


void
Typifier::typify(WhileNode* node)
{
  typifyNode(node->test());
  typifyNode(node->body());
}


void
Typifier::typify(VectorNode* node)
{
  typifyNodeList(node->children());
}


void
Typifier::typify(DictNode* node)
{
  typifyNodeList(node->children());
}


void
Typifier::typify(CastNode* node)
{
  typifyNode(node->base());
}


//------------------------------------------------------------------------------

namespace heather
{
  void
  typifyNodeType(AptNode* node, const Type& type, const String& defaultTypeName)
  {
    Type ty = ( type.isDef()
                ? node->scope()->lookupType(type)
                : node->scope()->lookupType(defaultTypeName, true) );
    if (!ty.isDef()) {
      errorf(node->srcpos(), E_UndefinedType,
             "undefined type '%s'", (const char*)StrHelper(defaultTypeName));
      node->setType(Type::newAny(true));
    }
    else
      node->setType(ty);
  }
};

void
Typifier::typify(BoolNode* node)
{
  if (fPhase == kTypify) {
    typifyNodeType(node, Type(), Names::kBoolTypeName);
  }
}


void
Typifier::typify(CharNode* node)
{
  if (fPhase == kTypify) {
    typifyNodeType(node, Type(), Names::kCharTypeName);
  }
}


void
Typifier::typify(RationalNode* node)
{
  if (fPhase == kTypify) {
    typifyNodeType(node, node->type(), Names::kRationalTypeName);
  }
}


void
Typifier::typify(RealNode* node)
{
  if (fPhase == kTypify) {
    typifyNodeType(node, node->type(), Names::kRealTypeName);
  }
}


void
Typifier::typify(IntNode* node)
{
  if (fPhase == kTypify) {
    typifyNodeType(node, node->type(), Names::kIntTypeName);
  }
}


void
Typifier::typify(StringNode* node)
{
  if (fPhase == kTypify) {
    typifyNodeType(node, Type(), Names::kStringTypeName);
  }
}


void
Typifier::typify(KeywordNode* node)
{
  if (fPhase == kTypify) {
    typifyNodeType(node, Type(), Names::kKeywordTypeName);
  }
}


void
Typifier::typify(UnitConstNode* node)
{
  // TODO
  typifyNode(node->value());
}
