
#define WANT_M_I1_LABELS
#define WANT_M_I2_LABELS
#define WANT_M_I666_LABELS

#include "input-defs.h"


#ifndef M_I1_FOOF_LABEL
#error "No label for M::I1/foof"
#endif
#ifdef M_I1_FOOF_SUBLABEL
#error "Sublabel defined for M::I1/foof"
#endif

#if !defined(M_I2_YFFY_LABEL) || !defined(M_I2_UGGU_LABEL) || !defined(M_I2_KDDK_LABEL)
#error "Label missing for M::I2/yffy, M::I2/uggu, or M::I2/kddk"
#elif M_I2_YFFY_LABEL != 0xb00b
#error "M::I2/yffy label isn't 0xb00b"
#endif

#if !defined(M_I2_YFFY_SUBLABEL) || !defined(M_I2_UGGU_SUBLABEL) || !defined(M_I2_KDDK_SUBLABEL)
#error "Sublabel missing for M::I2/yffy, M::I2/uggu, or M::I2/kddk"
#elif M_I2_KDDK_SUBLABEL != 0xabcd
#error "M::I2/kddk sublabel isn't 0xabcd"
#endif

#ifdef M_I666_FORE_LABEL
#error "Label defined for M::I666/fore"
#endif

int main(void) { return 0; }
