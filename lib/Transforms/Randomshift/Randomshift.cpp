//===- Randomshift.cpp ----------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include <string>
#include <vector>
using namespace llvm;

namespace {
  // Hello2 - The second implementation with getAnalysisUsage implemented.
  struct Randomshift : public FunctionPass {
    static char ID; // Pass identification, replacement for typeid
    std::vector<StringRef> sensitive_variables;
    bool randomshift_initialized = false;
    Randomshift() : FunctionPass(ID) {}

    // find annotation from llvm.global.annotations
    bool isSensitiveGlobal(Module *module, Value * variable) {
      errs() << "hasSensitiveAnnotation\n";
      GlobalVariable *global_annotations = module->getGlobalVariable("llvm.global.annotations");

      /*  global_annotations is
       *
       *  @llvm.global.annotations = appending global [1 x { i8*, i8*, i8*, i32
       *  }] [{ i8*, i8*, i8*, i32 } { i8* bitcast ([12 x i32]* @FT0 to i8*),
       *  i8* getelementptr inbounds ([21 x i8], [21 x i8]* @.str.4, i32 0, i32
       *  0), i8* getelementptr inbounds ([11 x i8], [11 x i8]* @.str.5, i32 0,
       *  i32 0), i32 7 }], section "llvm.metadata"
       */


      for (Value *annotation_op : global_annotations->operands()){
        ConstantArray *CA = dyn_cast<ConstantArray>(annotation_op);

        // CS is a unit of annotation. One CS is for FT0, another CS is for FT1
        // should iterate CA->getOperands()
        for (Value *CAop : CA->operands()){
          ConstantStruct *CS = dyn_cast<ConstantStruct>(CAop);

          /*
           * CS->getOperand(0) => i8* bitcast ([12 x i32]* @FT0 to i8*)
           * CS->getOperand(1) => i8* getelementptr inbounds ([21 x i8], [21 x
           * i8]* @.str.4, i32 0, i32 0)
           */

          errs() << "test\n";
          errs() << *CS->getOperand(0) << "\n";
          errs() << *CS->getOperand(1) << "\n";
          errs() << *CS->getOperand(2) << "\n";
          errs() << *CS->getOperand(3) << "\n";
          errs() << "variable name\n";
          errs() << variable->getName() << "\n";
          if (variable->getName().equals(CS->getOperand(0)->getOperand(0)->getName())){
            errs() << "match found\n";
            errs() << variable->getName() << "\n";

            if (GlobalVariable *annotation = dyn_cast<GlobalVariable>(CS->getOperand(1)->getOperand(0))){
              if (ConstantDataArray *CDA = dyn_cast<ConstantDataArray>(annotation->getInitializer())){
                StringRef annotation_str = CDA->getAsCString();
                if (annotation_str.equals("sensitive")){
                  errs() << "annotation sensitive found\n";
                  return true;
                }
              }
            }
          }
        }
      }
      return false;
    }

    bool isSensitiveLocal(Value * variable) {
      errs() << "isSensitiveLocal\n";
      for (std::vector<StringRef>::iterator it = sensitive_variables.begin();
              it != sensitive_variables.end(); ++it){
        errs() << *it << "\n";
        if ( (*it).contains(variable->getName()) ){
          return true;
        }
      }
      return false;
    }

    /* Declare global variable 'randomshift' to zero and assign random value.
     * equivalent to
     *
     * uint64_t randomshift = 0;
     *
     * some_function(...){
     *   srand(time(NULL));
     *   randomshift = rand();
     *   ...
     * }
     */
    void initialize_randomshift(Instruction * ii, LLVMContext &ctx, Module *module) {
      ConstantInt *zero = ConstantInt::get(Type::getInt64Ty(ctx), 0, false);
      IRBuilder<> builder(ctx);

      GlobalVariable *random_shift = new GlobalVariable(/*Module*/*module,
              /*Type*/Type::getInt64Ty(ctx),
              /*isConstant*/false,
              /*Linkage*/GlobalValue::ExternalLinkage,
              /*Initializer*/zero,
              /*Name*/"random_shift");
      random_shift->setAlignment(8);

      /*
      // time call
      std::vector<Type *> timeArgs;
      timeArgs.push_back(Type::getInt64PtrTy(ctx));
      ArrayRef<Type *> timeArgsRef(timeArgs);
      FunctionType *timeType = FunctionType::get(Type::getInt64Ty(ctx), timeArgsRef, false); //it should be getInt64Ty, but currently, to make call srand, i temporaily changed argument type
      Constant *c = module->getOrInsertFunction("time", timeType);
      Function *time = cast<Function>(c);
      Constant *null = Constant::getNullValue(Type::getInt64PtrTy(ctx));

      CallInst *time_call = builder.CreateCall(time, null);
      time_call->setName("time");
      time_call->insertAfter(ii);

      // srand call
      std::vector<Type *> srandArgs;
      srandArgs.push_back(Type::getInt32Ty(ctx));
      ArrayRef<Type *> srandArgsRef(srandArgs);
      FunctionType *srandType = FunctionType::get(Type::getInt64PtrTy(ctx), srandArgsRef, false); // Is it okay to change return value type and argument type?? Real return type of srand is void *, but currently in code, it's int64ptr
      Constant *s = module->getOrInsertFunction("srand", srandType);
      Function *srand = cast<Function>(s);

      // should cast 64bit integer(return value of time) to 32bit
      TruncInst * new_trunc = new TruncInst(time_call, Type::getInt32Ty(ctx), "casting", time_call->getNextNode());

      // CastInst *cast = CastInst::Create(
      CallInst *srand_call = builder.CreateCall(srand, new_trunc);
      srand_call->setName("srand");
      srand_call->insertAfter(new_trunc);

      // rand call
      FunctionType *randType = FunctionType::get(Type::getInt32Ty(ctx), true);
      Constant *r = module->getOrInsertFunction("rand", randType);
      Function *rand = cast<Function>(r); // problem
      CallInst *rand_call = builder.CreateCall(rand);
      rand_call->setName("rand");
      rand_call->insertAfter(srand_call);

      // convert rand() return value to 64bit integer
      SExtInst *new_SExt = new SExtInst(rand_call, Type::getInt64Ty(ctx), "convert" ,rand_call->getNextNode());
      StoreInst *new_store = new StoreInst(new_SExt, random_shift, false, 4, new_SExt->getNextNode());
      */


      std::vector<Type *> sgx_read_randArgs;
      sgx_read_randArgs.push_back(Type::getInt64PtrTy(ctx));
      sgx_read_randArgs.push_back(Type::getInt32Ty(ctx));
      ArrayRef<Type *> sgx_read_randArgsRef(sgx_read_randArgs);
      FunctionType *sgx_read_randType = FunctionType::get(Type::getInt32Ty(ctx), sgx_read_randArgsRef, false);
      Constant *sg = module->getOrInsertFunction("sgx_read_rand", sgx_read_randType);
      Function *sgx_read_rand = cast<Function>(sg);

      std::vector<Value *> sgxreadrandArgs;
      sgxreadrandArgs.push_back(random_shift);
      //sgxreadrandArgs.push_back(ConstantInt::get(Type::getInt32Ty(ctx), 8, false));
      sgxreadrandArgs.push_back(ConstantInt::get(Type::getInt32Ty(ctx), 1, false));
      CallInst *sgx_read_rand_call = builder.CreateCall(sgx_read_rand, sgxreadrandArgs);
      sgx_read_rand_call->setName("sgx_read_rand_call");
      sgx_read_rand_call->insertAfter(ii);

      randomshift_initialized = true;
      return;
    }

    bool runOnFunction(Function &F) override {
      for (Function::iterator bb = F.begin(), bbe = F.end(); bb != bbe; ++bb){
        BasicBlock &b = *bb;
        auto &ctx = F.getContext();
        auto *module = b.getModule();
        IRBuilder<> builder(ctx);

        for (BasicBlock::iterator i = b.begin(), ie = b.end(); i != ie; ++i){
          Instruction * ii = &*i;
          if (!randomshift_initialized) {
            initialize_randomshift(ii, ctx, module);
          }

          auto *gep = dyn_cast<GetElementPtrInst>(ii);
          if (gep && !gep->getName().contains("arraydecay")){
              /*
              auto *array_type = cast<PointerType>(gep->getOperand(0)->getType()->getScalarType())->getElementType();
              uint64_t array_size = array_type->getArrayNumElements(); // if array_type == [100 x i64], array_size = 100
              GlobalVariable *random_shift = module->getGlobalVariable("random_shift");
              */
              bool is_randomshift_target = false;

              if (dyn_cast<GlobalVariable>(gep->getOperand(0))){
                errs() << "Operand 0 is Global!\n";
                if (isSensitiveGlobal(module, gep->getOperand(0))){
                  errs() << "Have Sensitive Annotation!\n";
                  errs() << *gep->getOperand(0);
                  is_randomshift_target = true;
                }
              }
              else{ // if it is local variable
                if (isSensitiveLocal(gep->getOperand(0))){
                  is_randomshift_target = true;
                }
              }


              if (is_randomshift_target){
                auto *array_type = cast<PointerType>(gep->getOperand(0)->getType()->getScalarType())->getElementType();
                uint64_t array_size = array_type->getArrayNumElements(); // if array_type == [100 x i64], array_size = 100
                GlobalVariable *random_shift = module->getGlobalVariable("random_shift");

                // Operand(0) is array
                auto *original_index = gep->getOperand(2); // type is 64bit
                LoadInst *new_load = new LoadInst(random_shift, "random_shift", ii);

                // create add instruction and insert it before ii.  new_int = index + random_shift
                BinaryOperator *new_add = BinaryOperator::Create(Instruction::Add, new_load, original_index, "addtmp", ii);

                ConstantInt *array_size_int = ConstantInt::get(Type::getInt64Ty(ctx), array_size);
                BinaryOperator *new_rem = BinaryOperator::Create(Instruction::SRem, new_add, array_size_int, "remtmp", ii);

                //errs() << *cast<PointerType>(gep->getPointerOperand()->getType()->getScalarType())->getElementType() << "\n";
                gep->setOperand(2, new_rem); // Operand(2) is index of gep
            }
          }
          /*
           * Annotation of local variable is
           * call void @llvm.var.annotation(i8* %helloarray1, i8* getelementptr
           * inbounds ([6 x i8], [6 x i8]* @.str, i32 0, i32 0), i8*
           * getelementptr inbounds ([11 x i8], [11 x i8]* @.str.1, i32 0, i32
           * 0), i32 13)
           */
          else if (CallInst *CI = dyn_cast<CallInst>(ii)){
            Function *func = CI->getCalledFunction();
            StringRef func_name = func->getName();

            if (func_name.contains("llvm.var.annotation")){ // check whether it is annotation call or not
              errs() << "function name contains llvm.var.annotation\n";
              //auto *tmp = CI->getOperand(1);
              /*
               * i8* getelementptr inbounds ([11 x i8], [11 x i8]* @.str.1, i32
               * 0, i32 0)
               *
               * is ConstantExpr type. So use GEPOperator
               */

              GEPOperator *gep_operator = dyn_cast<GEPOperator>(CI->getOperand(1));
              GlobalVariable *annotation_str = dyn_cast<GlobalVariable>(gep_operator->getPointerOperand());
              ConstantDataArray *CDA = dyn_cast<ConstantDataArray>(annotation_str->getInitializer());
              if (CDA->getAsCString().equals("sensitive")){
                StringRef variable_name = CI->getOperand(0)->getName();
                //errs() << variable_name << "\n";
                sensitive_variables.push_back(variable_name);
              }
            }

          } // end of if (CallInst *CI = dyn_cast<CallInst>(ii))
        }
      }
      return true;
    }

    // We don't modify the program, so we preserve all analyses.
    void getAnalysisUsage(AnalysisUsage &AU) const override {
      AU.setPreservesAll();
    }
  };
}

char Randomshift::ID = 0;
static RegisterPass<Randomshift>
Y("randomshift", "Randomshift pass");

static void registerRandomshiftPass(const PassManagerBuilder &,
        legacy::PassManagerBase &PM){
    PM.add(new Randomshift());
}

static RegisterStandardPasses RegisterRandomshiftPass(
        PassManagerBuilder::EP_EarlyAsPossible, registerRandomshiftPass);
