//===--- HexagonDepITypes.h -----------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

namespace llvm {
namespace HexagonII {
enum Type {
  TypeALU32_2op = 0,
  TypeALU32_3op = 1,
  TypeALU32_ADDI = 2,
  TypeALU64 = 3,
  TypeCJ = 4,
  TypeCR = 6,
  TypeCVI_HIST = 10,
  TypeCVI_VA = 16,
  TypeCVI_VA_DV = 17,
  TypeCVI_VINLANESAT = 18,
  TypeCVI_VM_LD = 19,
  TypeCVI_VM_NEW_ST = 20,
  TypeCVI_VM_ST = 21,
  TypeCVI_VM_STU = 22,
  TypeCVI_VM_TMP_LD = 23,
  TypeCVI_VM_VP_LDU = 24,
  TypeCVI_VP = 25,
  TypeCVI_VP_VS = 26,
  TypeCVI_VS = 27,
  TypeCVI_VX = 29,
  TypeCVI_VX_DV = 30,
  TypeCVI_VX_LATE = 31,
  TypeDUPLEX = 33,
  TypeENDLOOP = 34,
  TypeEXTENDER = 35,
  TypeJ = 36,
  TypeLD = 37,
  TypeM = 38,
  TypeMAPPING = 39,
  TypeNCJ = 40,
  TypePSEUDO = 41,
  TypeST = 42,
  TypeSUBINSN = 43,
  TypeS_2op = 44,
  TypeS_3op = 45,
  TypeV2LDST = 48,
  TypeV4LDST = 49
};
}
}
