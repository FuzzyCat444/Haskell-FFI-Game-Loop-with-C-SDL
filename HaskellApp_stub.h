#include "HsFFI.h"
#ifdef __cplusplus
extern "C" {
#endif
extern void hs_windowTitleCallback(HsPtr a1);
extern void hs_windowSizeCallback(HsPtr a1);
extern HsStablePtr hs_initGameStateCallback(void);
extern void hs_cleanupCallback(HsStablePtr a1);
extern HsStablePtr hs_updateGameStateCallback(HsStablePtr a1);
extern void hs_renderGameStateCallback(HsStablePtr a1, HsPtr a2);
extern HsPtr hs_imageResourcesCallback(void);
extern HsInt32 hs_maxSpritesCallback(void);
#ifdef __cplusplus
}
#endif

