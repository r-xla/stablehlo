#include <cassert>

#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/Support/LogicalResult.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/Dialect/Quant/IR/Quant.h>
#include <mlir/IR/Attributes.h>
#include <mlir/IR/Block.h>
#include <mlir/IR/Builders.h>
#include <mlir/IR/BuiltinAttributes.h>
#include <mlir/IR/BuiltinOps.h>
#include <mlir/IR/BuiltinTypes.h>
#include <mlir/IR/Location.h>
#include <mlir/IR/MLIRContext.h>
#include <mlir/IR/OwningOpRef.h>
#include <mlir/IR/Value.h>
#include <mlir/IR/Verifier.h>
#include <mlir/Support/LLVM.h>
#include <stablehlo/dialect/StablehloOps.h>
#include <stablehlo/reference/Api.h>
#include <stablehlo/reference/Configuration.h>

// [[Rcpp::export()]]
int test() {
  mlir::MLIRContext context;

  /** create module **/
  mlir::OwningOpRef<mlir::ModuleOp> module =
      mlir::ModuleOp::create(mlir::UnknownLoc::get(&context));
  module->getContext()->loadDialect<mlir::func::FuncDialect>();
  module->getContext()->loadDialect<mlir::stablehlo::StablehloDialect>();
  module->getContext()->loadDialect<mlir::quant::QuantDialect>();
  module->setName("test_module");

  /** create function **/
  // create function argument and result types.
  auto tensorType =
      mlir::RankedTensorType::get({3, 4}, mlir::Float32Type::get(&context));
  auto func_type =
      mlir::FunctionType::get(&context, {tensorType, tensorType}, {tensorType});

  // create the function and map arguments.
  llvm::ArrayRef<mlir::NamedAttribute> attrs;
  auto function = mlir::func::FuncOp::create(mlir::UnknownLoc::get(&context),
                                             "main", func_type, attrs);
  function.setVisibility(mlir::func::FuncOp::Visibility::Public);
  module->push_back(function);

  // create function block with add operations.
  mlir::Block* block = function.addEntryBlock();
  llvm::SmallVector<mlir::Value, 4> arguments(block->args_begin(),
                                              block->args_end());
  mlir::OpBuilder block_builder = mlir::OpBuilder::atBlockEnd(block);
  mlir::Location loc = block_builder.getUnknownLoc();

  llvm::SmallVector<mlir::NamedAttribute, 10> attributes;
  mlir::Operation* op =
      block_builder.create<mlir::stablehlo::AddOp>(loc, arguments, attributes)
          .getOperation();
  block_builder.create<mlir::func::ReturnOp>(loc, op->getResult(0));

  /** verify and dump the module **/
  assert(mlir::succeeded(mlir::verify(module.get())));

  /* interpret the function "main" with concrete inputs **/
  auto getConstValue = [&](double val) {
    return mlir::DenseElementsAttr::get(
        tensorType,
        block_builder.getFloatAttr(tensorType.getElementType(), val));
  };

  auto inputValue1 = getConstValue(10.0);
  auto inputValue2 = getConstValue(20.0);
  auto expectedValue = getConstValue(30.0);

  mlir::stablehlo::InterpreterConfiguration config;
  auto results = evalModule(*module, {inputValue1, inputValue2}, config);
  return failed(results) || (*results)[0] != expectedValue;
}