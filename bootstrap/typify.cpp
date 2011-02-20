/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
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
#include "xmlout.h"
#include "port.h"

#include <set>


using namespace herschel;

//----------------------------------------------------------------------------

TypifyPass::TypifyPass(int level)
  : AptNodeCompilePass(level, true)
{}


AptNode*
TypifyPass::doApply(AptNode* src)
{
  Ptr<AptNode> node = src;
  Ptr<Typifier> ty = new Typifier;
  ty->typifyRecursively(node);
  return node.release();
}


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
Typifier::annotateTypeConv(AptNode* node, const Type& dstType)
{
  /*
    plain <- plain     (bitcast)
    plain <- atom      atom_2_x
    atom  <- atom      ok
    atom  <- any       type_check
    atom  <- plain     wrap_atom
   */
  if (dstType.isPlainType()) {
    if (node->type().isPlainType()) {
      // ok.  maybe some bitcast between int/short/long, etc.
    }
    else {
      // req. atom_2_x
      node->setTypeConv(kAtom2PlainConv);
    }
  }
  else {
    if (node->type().isPlainType()) {
      // req. wrap_atom
      node->setTypeConv(kPlain2AtomConv);
    }
    else if (node->type().isAny()) {
      // requires type_check
      node->setTypeConv(kTypeCheckConv);
    }
    else {
      // OK
    }
  }

  node->setDstType(dstType);
}


void
Typifier::setBodyLastDstType(AptNode* body, const Type& dstType)
{
  body->setDstType(dstType);
  if (BlockNode* block = dynamic_cast<BlockNode*>(body)) {
    assert(!block->children().empty());

    setBodyLastDstType(block->children().back(), dstType);
  }
}


//------------------------------------------------------------------------------

void
Typifier::typify(SymbolNode* node)
{
  if (fPhase == kTypify) {
    const AptNode* var = node->scope()->lookupVarOrFunc(node->name(), true);
    if (var != NULL) {
      if (const FuncDefNode* fdn = dynamic_cast<const FuncDefNode*>(var))
        node->setLinkage(fdn->linkage());

      node->setType(var->type());
      return;
    }

    Type type0 = node->scope()->lookupType(node->name(), true);
    Type type1 = node->scope()->normalizeType(type0);

    Type type2 = degeneralizeType(node->srcpos(), type1, node->generics());
    if (type2.isDef())
      node->setType(Type::newClassOf(type2));
  }
}


void
Typifier::typify(ArrayTypeNode* node)
{
  if (fPhase == kTypify) {
    typifyNode(node->typeNode());

    SymbolNode* symnd = dynamic_cast<SymbolNode*>(node->typeNode());
    Type type = ( symnd != NULL
                  ? symnd->type()
                  : node->typeNode()->type() );
    Type type1 = node->scope()->normalizeType(type);
    node->setType(Type::newClassOf(Type::newArray(type1, 0, true)));
  }
}


void
Typifier::typify(TypeNode* node)
{
  if (fPhase == kTypify) {
    assert(node->type().isDef());
    Type type1 = node->scope()->normalizeType(node->type());
    node->setType(Type::newClassOf(type1));
  }
}


//------------------------------------------------------------------------------

void
Typifier::typify(DefNode* node)
{
  typifyNode(node->defNode());
  if (fPhase == kTypify)
    node->setType(node->defNode()->type());
}


void
Typifier::typify(LetNode* node)
{
  typifyNode(node->defNode());
  if (fPhase == kTypify)
    node->setType(node->defNode()->type());
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
    node->setType(Type::newAny());
    if (node->initExpr() != NULL)
      node->initExpr()->setDstType(Type::newAny());
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
        node->initExpr()->setDstType(node->initExpr()->type());
      }
      else if (!node->initExpr()->type().isDef()) {
        errorf(node->initExpr()->srcpos(), E_TypeMismatch,
               "Undefined type in %s initialization", errdesc);
        node->initExpr()->setDstType(Type::newAny());
      }
      else if (!isContravariant(bindty, node->initExpr()->type(),
                                node->scope(), node->srcpos())) {
        errorf(node->initExpr()->srcpos(), E_TypeMismatch,
               "type mismatch in %s initialization", errdesc);
        node->initExpr()->setDstType(Type::newAny());
      }
      else {
        annotateTypeConv(node->initExpr(), node->type());
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
    else {
      // TODO: make warnings like this optional
      // warningf(node->srcpos(), E_UndefinedType,
      //          "undefined return type on function defaults to lang|Any");
      node->setRetType(Type::newAny());
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
      node->setRetType(Type::newAny());
    }
    else {
      node->setRetType(retty);
    }
  }

  FunctionSignature sign(false, String(), node->retType(), params);
  node->setType(Type::newFunction(sign));
}


namespace herschel
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
                         node->scope(), node->srcpos()) &&
        !containsAny(node->body()->type(), node->srcpos()))
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
  if (node->body() != NULL) {
    typifyNode(node->body());

    if (fPhase == kTypify) {
      if (!node->body()->type().isDef())
        node->body()->setType(Type::newAny());
    }
  }

  if (fPhase == kTypify) {
    setupFunctionNodeType(node);
    if (node->name() == String("app|main")) {
      if (!node->retType().isAny()) {
        if (node->retType().typeName() != String("lang|Int32"))
        {
          errorf(node->srcpos(), E_TypeMismatch,
                 "return type of app|main() must be lang|Int32");
        }
      }

      node->setRetType(Type::newTypeRef("lang|Int32"));
    }

    if (node->body() != NULL) {
      annotateTypeConv(node->body(), node->retType());
      setBodyLastDstType(node->body(), node->retType());
    }
  }
  else if (fPhase == kCheck) {
    checkFunctionReturnType(node);

    if (node->isMethod()) {
      // for methods check that the function signature matches the generic
      // function implementation.  The following conditions are checked in
      // annotate.cpp already.
      const FuncDefNode* genericDef = NULL;
      const AptNode* var = node->scope()->lookupVarOrFunc(node->name(), true);
      if (var != NULL &&
          (genericDef = dynamic_cast<const FuncDefNode*>(var)) != NULL &&
          genericDef->isGeneric())
      {
        if (!isContravariant(genericDef->type(), node->type(),
                             node->scope(), node->srcpos()))
        {
          errorf(node->srcpos(), E_TypeMismatch,
                 "method does not match generic function definition");
          // tyerror(genericDef->type(), "Generic");
          // tyerror(node->type(), "This");
        }
      }
    }
  }
}


void
Typifier::typify(FunctionNode* node)
{
  typifyNodeList(node->params());
  if (node->body() != NULL) {
    typifyNode(node->body());

    if (!node->body()->type().isDef())
      node->body()->setType(Type::newAny());
  }

  if (fPhase == kTypify) {
    setupFunctionNodeType(node);
    if (node->body() != NULL)
      annotateTypeConv(node->body(), node->retType());
  }
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
    if (!param->type().matchGenerics(localCtx, arg->type(),
                                     arg->scope(), arg->srcpos())) {
      errorf(arg->srcpos(), E_TypeMismatch,
             "type mismatch for argument %d", idx);
      return;
    }
  }
  else {
    if (!isContravariant(param->type(), arg->type(), arg->scope(),
                         arg->srcpos()) &&
        !containsAny(arg->type(), arg->srcpos()))
    {
      errorf(arg->srcpos(), E_TypeMismatch,
             "type mismatch for argument %d", idx);
      return;
    }
  }

  annotateTypeConv(arg, param->type());
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


Type
Typifier::typifyMatchAndCheckParameters(const SrcPos& srcpos,
                                        const NodeList& args,
                                        const FunctionNode* funcNode)
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

  const NodeList& funcParams = funcNode->params();

  TypeCtx localCtx;
  size_t argidx = 0;
  std::set<int> argIndicesUsed;
  for (size_t i = 0; i < funcParams.size(); ++i) {
    const ParamNode* param = dynamic_cast<const ParamNode*>(funcParams[i].obj());

    if (param != NULL) {
      if (param->flags() == kPosArg || param->flags() == kSpecArg) {
        AptNode* arg = args[argidx].obj();

        if (i >= args.size()) {
          errorf(srcpos, E_BadArgNumber, "not enough arguments");
          return Type();
        }
        checkArgParamType(localCtx, param, arg, i);
        argidx++;
      }
      else if (param->flags() == kNamedArg) {
        Typifier::KeyargReturn keyval = findKeyedArg(args, argidx, param->key());
        if (keyval.fKeyarg == NULL) {
          // if the function prototype has been parsed as interface, the init
          // expressions are not passed and therefore we don't need to check
          // them here.
          if (param->initExpr() != NULL)
            checkArgParamType(localCtx, param, param->initExpr(), i);
        }
        else {
          checkArgParamType(localCtx, param, keyval.fKeyarg->value(), keyval.fIdx);
          argIndicesUsed.insert(keyval.fIdx);
        }
      }
      else if (param->flags() == kRestArg) {
        // TODO: type of rest arguments
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
    errorf(srcpos, E_BadArgNumber,
           "Too much arguments");
  }

  if (funcNode->retType().isOpen()) {
    Type retty = funcNode->retType().replaceGenerics(localCtx);
    // fprintf(stderr, "RETTY: %s\n", (const char*)StrHelper(retty.toString()));

    if (retty.isDef())
      return retty;
    else
      errorf(srcpos, E_TypeMismatch,
             "function has unmatched generic return type.");

    // Type retty = funcNode->retType();
    // if (localCtx.hasType(retty.typeName())) {
    //   node->setType(localCtx.lookupType(retty.typeName()));
    // }
    // else {
    //   errorf(node->srcpos(), E_TypeMismatch,
    //          "function has unmatched generic return type.");
    // }

    return Type();
  }
  else
    return funcNode->retType();
}


void
Typifier::typifyMatchAndCheckParameters(ApplyNode* node,
                                        const FunctionNode* funcNode)
{
  Type type = typifyMatchAndCheckParameters(node->srcpos(), node->children(),
                                            funcNode);
  if (type.isDef())
    node->setType(type);
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
      if (funcNode != NULL) {
        // Ptr<XmlRenderer> out = new XmlRenderer(new FilePort(stdout));
        // out->render(const_cast<FunctionNode*>(funcNode));
        typifyMatchAndCheckParameters(node, funcNode);
      }
    }
    else {
      ArrayTypeNode* typeNode = dynamic_cast<ArrayTypeNode*>(node->base());
      if (typeNode != NULL) {
        node->setType(typeNode->type());
      }
      else {
        if (SymbolNode* symNode = dynamic_cast<SymbolNode*>(node->base())) {
          node->setType(symNode->type());
        }
        else if (TypeNode* typeNode = dynamic_cast<TypeNode*>(node->base())) {
          node->setType(typeNode->type());
        }
        else {
          // fprintf(stderr, "APPLY: %s\n", (const char*)StrHelper(node->base()->type().toString()));
          // Ptr<XmlRenderer> out = new XmlRenderer(new FilePort(stderr));
          // out->render(node->base());
          assert(0 && "Unhandled apply base node");
        }
      }
    }
    // fprintf(stderr, "APPLY: %s\n", (const char*)StrHelper(node->type().toString()));
  }
}


void
Typifier::typify(AssignNode* node)
{
  typifyNode(node->lvalue());
  typifyNode(node->rvalue());

  if (fPhase == kTypify) {
    Type ltype = node->lvalue()->type();
    Type rtype = node->rvalue()->type();

    if (!ltype.isDef()) {
      // infer the vardef type from rvalue expression
      node->lvalue()->setType(node->rvalue()->type());
    }
    else if (!rtype.isDef()) {
      errorf(node->rvalue()->srcpos(), E_TypeMismatch,
             "Undefined type in assignment right hand value");
    }
    else if (!isContravariant(ltype, rtype, node->scope(), node->srcpos()) &&
             !containsAny(rtype, node->srcpos()))
    {
      errorf(node->rvalue()->srcpos(), E_TypeMismatch,
             "type mismatch in assignment");
    }
    else if (!ltype.isDef()) {
      // infer the vardef type from rvalue expression
      node->lvalue()->setType(rtype);
    }

    node->setType(rtype);

    annotateTypeConv(node->rvalue(), ltype);
    annotateTypeConv(node->lvalue(), ltype);
    annotateTypeConv(node, rtype);
  }
}


String
Typifier::operatorNameByOp(OperatorType type) const
{
  switch (type) {
  case kOpAppend:       return String("append");
  case kOpBitAnd:       return String("bitand");
  case kOpBitOr:        return String("bitor");
  case kOpBitXor:       return String("bitxor");
  case kOpCompare:      return String("compare");
  case kOpDivide:       return String("divide");
  case kOpEqual:        return String("equal?");
  case kOpExponent:     return String("exponent");
  case kOpFold:         return String("fold");
  case kOpGreater:      return String("greater?");
  case kOpGreaterEqual: return String("greater-equal?");
  case kOpIn:           return String("in");
  case kOpIsa:          return String("isa?");
  case kOpLess:         return String("less?");
  case kOpLessEqual:    return String("less-equal?");
  case kOpLogicalAnd:   return String("logand");
  case kOpLogicalOr:    return String("logor");
  case kOpMinus:        return String("subtract");
  case kOpMod:          return String("mod");
  case kOpRem:          return String("rem");
  case kOpMultiply:     return String("multiply");
  case kOpPlus:         return String("add");
  case kOpRemove:       return String("remove");
  case kOpShiftLeft:    return String("shift-left");
  case kOpShiftRight:   return String("shift-right");
  case kOpUnequal:      return String("unequal?");

  case kOpInvalid:
  case kOpAssign:       return String("=");
  case kOpAs:           return String("cast-to");
  case kOpBy:           return String("by");
  case kOpMapTo:        return String("map-to");
  case kOpRange:        return String("..");
  case kOpThen:         return String("then");
  case kOpWhile:        return String("while");
    assert(0);
  }

  return String();
}


bool
Typifier::checkBinaryFunctionCall(BinaryNode* node,
                                  const String& funcName,
                                  AptNode* leftArg, AptNode* rightArg)
{
  const FunctionNode* funcNode = dynamic_cast<const FunctionNode*>(
    node->scope()->lookupFunction(funcName, true));

  if (funcNode != NULL) {
    // Ptr<XmlRenderer> out = new XmlRenderer(new FilePort(stdout));
    // out->render(const_cast<FunctionNode*>(funcNode));
    Type type = typifyMatchAndCheckParameters(node->srcpos(),
                                              newNodeList(leftArg, rightArg),
                                              funcNode);
    if (type.isDef()) {
      node->setType(type);
      return true;
    }
  }

  return false;
}


void
Typifier::typify(BinaryNode* node)
{
  Type retty;

  if (fPhase == kTypify) {
    typifyNode(node->left());
    typifyNode(node->right());

    Type leftty = node->left()->type();
    Type rightty = node->right()->type();

    switch (node->op()) {
    case kOpInvalid:
      assert(0 && "type not determined yet");

    case kOpPlus:
    case kOpMinus:
    case kOpMultiply:
    case kOpDivide:
    case kOpMod:
    case kOpRem:
    case kOpExponent:
      if (leftty.isAny() || rightty.isAny()) {
        node->setType(Type::newAny());
        annotateTypeConv(node->left(), node->type());
        annotateTypeConv(node->right(), node->type());
        return;
      }
      if (leftty.isNumber() || rightty.isNumber()) {
        node->setType(Type::newTypeRef(Names::kNumberTypeName, true));
        annotateTypeConv(node->left(), node->type());
        annotateTypeConv(node->right(), node->type());
        return;
      }

      if (leftty.isComplex() || rightty.isComplex()) {
        node->setType(Type::newTypeRef(Names::kComplexTypeName, true));
        annotateTypeConv(node->left(), node->type());
        annotateTypeConv(node->right(), node->type());
        return;
      }

      if (leftty.isAnyFloat()) {
        if (rightty.isAnyFloat())
          node->setType(maxFloatType(leftty, rightty));
        else
          node->setType(leftty);
        annotateTypeConv(node->left(), node->type());
        annotateTypeConv(node->right(), node->type());
        return;
      }
      if (rightty.isAnyFloat()) {
        if (leftty.isAnyFloat())
          node->setType(maxFloatType(leftty, rightty));
        else
          node->setType(rightty);
        annotateTypeConv(node->left(), node->type());
        annotateTypeConv(node->right(), node->type());
        return;
      }

      if (leftty.isRational() || rightty.isRational()) {
        node->setType(Type::newTypeRef(Names::kRationalTypeName, true));
        annotateTypeConv(node->left(), node->type());
        annotateTypeConv(node->right(), node->type());
        return;
      }

      if (leftty.isAnyInt() && rightty.isAnyInt()) {
        node->setType(maxIntType(leftty, rightty));
        annotateTypeConv(node->left(), node->type());
        annotateTypeConv(node->right(), node->type());
        return;
      }


      if (checkBinaryFunctionCall(node, operatorNameByOp(node->op()),
                                  node->left(), node->right()))
        return;

      tyerror(leftty, "left");
      tyerror(rightty, "right");
      // TODO: try to lookup a method which enables add(leftty, rightty) and use
      // it's returntype
      errorf(node->srcpos(), E_BinaryTypeMismatch,
             "incompatible types in binary operation");
      node->setType(Type::newAny());
      break;

    case kOpEqual:
    case kOpGreater:
    case kOpGreaterEqual:
    case kOpIn:
    case kOpLess:
    case kOpLessEqual:
    case kOpUnequal:
      if ( leftty.isAny() || rightty.isAny() ||
           (leftty.isAnySignedInt() && rightty.isAnySignedInt()) ||
           (leftty.isAnyUInt() && rightty.isAnyUInt()) ||
           (leftty.isAnyFloat() && rightty.isAnyFloat()) ||
           (leftty.isRational() && rightty.isRational()) ||
           (leftty.isComplex() && rightty.isComplex()) ||
           (leftty.isNumber() && rightty.isNumber()) ||
           isSameType(leftty, rightty, node->scope(), node->srcpos()) )
      {
        // any is always ok.
        node->setType(Type::newBool());
        annotateTypeConv(node->left(), leftty);
        annotateTypeConv(node->right(), leftty);
        return;
      }

      // TODO: check that left and right type are comparable
      errorf(node->srcpos(), E_BinaryTypeMismatch,
             "incompatible types in binary comparison");
      node->setType(Type::newAny());
      break;

    case kOpCompare:
      // TODO: check that left and right type are comparable
      node->setType(Type::newInt32());
      annotateTypeConv(node->left(), leftty);
      annotateTypeConv(node->right(), leftty);
      break;

    case kOpIsa:
      if (rightty.isAny() || rightty.isClassOf()) {
        node->setType(Type::newBool());
        annotateTypeConv(node->left(), node->type());
        annotateTypeConv(node->right(), node->type());
        return;
      }
      // TODO: try to lookup a method which enables isa(leftty, rightty) and
      // use it's returntype
      errorf(node->srcpos(), E_BinaryTypeMismatch,
             "incompatible right side type in isa operation");
      node->setType(Type::newAny());
      break;

    case kOpAppend:
      if (leftty.isString() || leftty.isAny()) {
        if (rightty.isString() || rightty.isChar() || rightty.isAny()) {
          node->setType(leftty);
          annotateTypeConv(node->left(), leftty);
          annotateTypeConv(node->right(), leftty);
          return;
        }
      }
      // TODO: try to lookup a method which enables append(leftty, rightty)
      // and use it's returntype
      errorf(node->srcpos(), E_BinaryTypeMismatch,
             "incompatible types in append() operation");
      node->setType(Type::newAny());
      break;

    case kOpFold:
      if (leftty.isString() || leftty.isAny()) {
        // accept everything on the right hand side
        node->setType(leftty);
        annotateTypeConv(node->left(), leftty);
        annotateTypeConv(node->right(), leftty);
        return;
      }
      // TODO: try to lookup a method which enables fold(leftty, rightty) and
      // use it's returntype
      errorf(node->srcpos(), E_BinaryTypeMismatch,
             "incompatible types in fold() operation");
      node->setType(Type::newAny());
      break;

    case kOpRemove:
      // TODO: try to lookup a method which enables remove(leftty, rightty)
      // and use it's returntype
      errorf(node->srcpos(), E_BinaryTypeMismatch,
             "incompatible types in remove() operation");
      node->setType(Type::newAny());
      break;

    case kOpLogicalAnd:
    case kOpLogicalOr:
      if (leftty.isBool() && rightty.isBool()) {
        node->setType(Type::newBool());
        annotateTypeConv(node->left(), leftty);
        annotateTypeConv(node->right(), leftty);
        return;
      }
      errorf(node->srcpos(), E_BinaryTypeMismatch,
             "bool types required in logical 'and'/'or' operations");
      node->setType(Type::newAny());
      break;

    case kOpBitAnd:
    case kOpBitOr:
    case kOpBitXor:
      if ((leftty.isAnyUInt() || leftty.isAny()) &&
          (rightty.isAnyUInt() || rightty.isAny()) ) {
        node->setType(leftty);
        annotateTypeConv(node->left(), leftty);
        annotateTypeConv(node->right(), leftty);
      }
      else {
        errorf(node->srcpos(), E_BinaryTypeMismatch,
               "AND, OR, XOR operations require unsigned integer types on both sides");
        node->setType(Type::newAny());
      }
      break;

    case kOpShiftLeft:
    case kOpShiftRight:
      if (leftty.isAnyUInt() || leftty.isAny()) {
        if (rightty.isAnyInt() || rightty.isAny()) {
          node->setType(leftty);
          annotateTypeConv(node->left(), leftty);
          annotateTypeConv(node->right(), leftty);
        }
        else {
          errorf(node->srcpos(), E_BinaryTypeMismatch,
                 "bit operations require integer types on right side");
          node->setType(Type::newAny());
        }
      }
      else {
        errorf(node->srcpos(), E_BinaryTypeMismatch,
               "bit operations require unsigned integer types on left side");
        node->setType(Type::newAny());
      }
      break;

    case kOpMapTo:
    case kOpRange:
    case kOpBy:
    case kOpAs:
    case kOpAssign:
    case kOpThen:
    case kOpWhile:
      assert(0 && "???");
    }
  }
}


void
Typifier::typify(NegateNode* node)
{
  typifyNode(node->base());
  if (fPhase == kTypify) {
    node->setType(node->base()->type());
  }
}


void
Typifier::typify(IfNode* node)
{
  typifyNode(node->test());
  annotateTypeConv(node->test(), Type::newBool());

  typifyNode(node->consequent());
  if (node->alternate())
    typifyNode(node->alternate());

  if (fPhase == kTypify) {
    if (node->alternate() != NULL) {
      Type cotype = node->consequent()->type();
      Type alttype = node->alternate()->type();

      if (isCovariant(cotype, alttype, node->scope(), node->srcpos())) {
        node->setType(alttype);
        annotateTypeConv(node->consequent(), alttype);
        annotateTypeConv(node->alternate(), alttype);
      }
      else if (isCovariant(alttype, cotype, node->scope(), node->srcpos())) {
        node->setType(cotype);
        annotateTypeConv(node->consequent(), cotype);
        annotateTypeConv(node->alternate(), cotype);
      }
      else {
        // if the if expression is not in tail position, the branch type
        // mismatch doesn't matter.
        if (node->isInTailPos() || node->isSingleTypeRequired()) {
          errorf(node->srcpos(), E_IfConsqTypeMismatch,
                 "types for if consequent and alternate branch do not match");
        }
        node->setType(Type::newAny(true));
        annotateTypeConv(node->consequent(), Type::newAny());
        annotateTypeConv(node->alternate(), Type::newAny());
      }
    }
    else {
      if (node->isInTailPos() || node->isSingleTypeRequired()) {
        // if the if expression is in tail position we should definitely have
        // have an alternate branch.
        errorf(node->srcpos(), E_IfAltTypeMismatch,
               "unspecified alternate branch do not match type with consequent");
        node->setType(Type::newAny(true));
      }
      else {
        node->setType(node->consequent()->type());
      }
    }
  }
  // TODO
  // else if (fPhase == kCheck) {
  //   if (!isSameType(Type::newBool(true), node->test()->type())) {
  //     errorf(node->test(), E_BoolTypeExpected,
  //            "Bool type in if test expected");
  //   }
  // }
}


void
Typifier::typify(KeyargNode* node)
{
  typifyNode(node->value());
  node->setType(node->value()->type());
}


void
Typifier::typify(MatchNode* node)
{
  // TODO
  typifyNode(node->expr());
  for (size_t i = 0; i < node->mappings().size(); i++) {
    typifyNode(node->mappings()[i].fConsequent);
  }
}


void
Typifier::typify(SelectNode* node)
{
  // TODO
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
  // TODO
  typifyNodeList(node->params());
  typifyNode(node->body());
}


void
Typifier::typify(RangeNode* node)
{
  typifyNode(node->from());
  typifyNode(node->to());
  if (node->by() != NULL) {
    typifyNode(node->by());
  }

  if (fPhase == kTypify) {
    Type fromType = node->from()->type();
    Type toType = node->to()->type();
    bool fromIsOpen = fromType.isOpen();
    bool toIsOpen = toType.isOpen();

    if ((fromIsOpen || toIsOpen) && (fromIsOpen != toIsOpen))
    {
      errorf(node->srcpos(), E_RangeTypeMismatch,
             "partial open types in range declaration defeats generics usage");
      node->setType(newRangeType(Type::newAny(true)));
      return;
    }

    if (!isSameType(fromType, toType, node->scope(), node->srcpos()))
    {
      errorf(node->srcpos(), E_RangeTypeMismatch, "type of range is ambiguous");
      node->setType(newRangeType(Type::newAny(true)));
      return;
    }

    if (node->by() != NULL) {
      Type byType = node->by()->type();
      bool byIsOpen= byType.isOpen();

      if (byIsOpen && byIsOpen != fromIsOpen) {
        errorf(node->srcpos(), E_RangeTypeMismatch,
               "partial open types in range declaration defeats generics usage");
        node->setType(newRangeType(Type::newAny(true)));
        return;
      }

      if (!isSameType(fromType, byType, node->scope(), node->by()->srcpos()))
      {
        errorf(node->srcpos(), E_RangeTypeMismatch,
               "step type does not match range type");
        node->setType(newRangeType(Type::newAny(true)));
        return;
      }
    }

    node->setType(newRangeType(node->from()->type()));
  }
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

  if (fPhase == kTypify) {
    annotateTypeConv(node->test(), Type::newBool());
  }

  if (node->isInTailPos() || node->isSingleTypeRequired()) {
    // the while expression should never be in tail position
    warningf(node->srcpos(), E_WhileTypeMismatch,
             "while in tail position enforces Any type");
    node->setType(Type::newAny(true));
  }
  else
    node->setType(node->body()->type());

  annotateTypeConv(node->body(), node->type());
}


static bool
mapCommonType(Type& resultType, AptNode* node)
{
  Type ty0 = node->type();
  if (!resultType.isDef()) {
    resultType = ty0;
  }
  else if (!isSameType(ty0, resultType, node->scope(), node->srcpos())) {
    resultType = Type::newAny(true);
    return false;
  }

  return true;
}


void
Typifier::typify(VectorNode* node)
{
  NodeList& nl = node->children();
  typifyNodeList(nl);

  Type valueType;
  TypeVector generics;
  for (size_t i = 0; i < nl.size(); i++) {
    if (!mapCommonType(valueType, nl[i]))
      break;
  }

  if (!valueType.isDef())
    valueType = Type::newAny(true);

  for (size_t i = 0; i < nl.size(); i++)
    annotateTypeConv(nl[i], valueType);

  node->setType(Type::newTypeRef(Names::kVectorTypeName,
                                 newTypeVector(valueType),
                                 newTypeConstVector(),
                                 true));
}


void
Typifier::typify(DictNode* node)
{
  Type keyType;
  Type valueType;

  NodeList& nl = node->children();
  for (size_t i = 0; i < nl.size(); i++) {
    BinaryNode* pair = dynamic_cast<BinaryNode*>(nl[i].obj());
    assert(pair != NULL);
    assert(pair->op() == kOpMapTo);

    typifyNode(pair->left());
    typifyNode(pair->right());

    mapCommonType(keyType, pair->left());
    mapCommonType(valueType, pair->right());
  }

  if (!keyType.isDef())
    keyType = Type::newAny(true);
  if (!valueType.isDef())
    valueType = Type::newAny(true);

  for (size_t i = 0; i < nl.size(); i++) {
    BinaryNode* pair = dynamic_cast<BinaryNode*>(nl[i].obj());
    assert(pair != NULL);
    assert(pair->op() == kOpMapTo);
    annotateTypeConv(pair->right(), valueType);
    annotateTypeConv(pair->left(), keyType);
  }

  node->setType(Type::newTypeRef(Names::kMapTypeName,
                                 newTypeVector(keyType, valueType),
                                 newTypeConstVector(),
                                 true));
}


void
Typifier::typify(ArrayNode* node)
{
  NodeList& nl = node->children();
  typifyNodeList(nl);

  Type valueType;
  TypeVector generics;
  for (size_t i = 0; i < nl.size(); i++) {
    if (!mapCommonType(valueType, nl[i]))
      break;
  }

  if (!valueType.isDef())
    valueType = Type::newAny(true);

  for (size_t i = 0; i < nl.size(); i++)
    annotateTypeConv(nl[i], valueType);

  node->setType(Type::newArray(valueType, nl.size(), true));
}


void
Typifier::typify(CastNode* node)
{
  typifyNode(node->base());

  if (fPhase == kTypify) {
    if (!node->type().isOpen()) {
      Type type = node->scope()->lookupType(node->type());
      if (!type.isDef()) {
        errorf(node->srcpos(), E_UndefinedType,
               "undefined type '%s'", (const char*)StrHelper(node->type().toString()));
        node->setType(Type::newAny(true));
      }
      else {
        if (isInvariant(node->base()->type(), type, node->scope(), node->srcpos())) {
          errorf(node->srcpos(), E_InvariantType, "Cast to invariant type");
          node->setType(Type::newAny(true));
        }
        else
          node->setType(type);
      }
    }
  }
}


//------------------------------------------------------------------------------

namespace herschel
{
  void
  typifyNodeType(AptNode* node, const Type& type, const String& defaultTypeName,
                 bool maybeImaginary)
  {
    Type ty = ( type.isDef()
                ? node->scope()->lookupType(type)
                : node->scope()->lookupType(defaultTypeName, true) );

    if (!ty.isDef()) {
      errorf(node->srcpos(), E_UndefinedType,
             "undefined type '%s'", (const char*)StrHelper(defaultTypeName));
      node->setType(Type::newAny(true));
    }
    else {
      if (maybeImaginary && ty.isAnyNumber()) {
        BaseNumberNode* nnd = dynamic_cast<BaseNumberNode*>(node);
        if (nnd != NULL)
          ty.setIsImaginary(nnd->isImaginary());
      }
      node->setType(ty);
    }
  }
};

void
Typifier::typify(BoolNode* node)
{
  if (fPhase == kTypify) {
    typifyNodeType(node, Type(), Names::kBoolTypeName, false);
  }
}


void
Typifier::typify(CharNode* node)
{
  if (fPhase == kTypify) {
    typifyNodeType(node, Type(), Names::kCharTypeName, false);
  }
}


void
Typifier::typify(RationalNode* node)
{
  if (fPhase == kTypify) {
    typifyNodeType(node, node->type(), Names::kRationalTypeName,
                   true);
  }
}


void
Typifier::typify(RealNode* node)
{
  if (fPhase == kTypify) {
    typifyNodeType(node, node->type(), Names::kFloat32TypeName, true);
  }
}


void
Typifier::typify(IntNode* node)
{
  if (fPhase == kTypify) {
    typifyNodeType(node, node->type(), Names::kInt32TypeName, true);
  }
}


void
Typifier::typify(StringNode* node)
{
  if (fPhase == kTypify) {
    typifyNodeType(node, Type(), Names::kStringTypeName, false);
  }
}


void
Typifier::typify(KeywordNode* node)
{
  if (fPhase == kTypify) {
    typifyNodeType(node, Type(), Names::kKeywordTypeName, false);
  }
}


void
Typifier::typify(UnitConstNode* node)
{
  // TODO
  typifyNode(node->value());
}
