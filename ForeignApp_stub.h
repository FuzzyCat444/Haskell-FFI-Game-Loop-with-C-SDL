#include "HsFFI.h"
#ifdef __cplusplus
extern "C" {
#endif
extern void hs_windowTitleCallback(HsPtr a1);
extern void hs_windowSizeCallback(HsPtr a1);
extern HsStablePtr hs_initGameStateCallback(void);
extern void hs_cleanupCallback(HsStablePtr a1);
extern HsStablePtr hs_updateGameStateCallback(HsStablePtr a1);
extern void hs_doIOCallback(HsStablePtr a1);
extern void hs_renderGameStateCallback(HsStablePtr a1, HsPtr a2);
extern void hs_playSoundsCallback(HsStablePtr a1, HsPtr a2);
extern void hs_musicCallback(HsStablePtr a1, HsPtr a2);
extern void hs_writeLogsCallback(HsStablePtr a1);
extern HsBool hs_shouldQuitCallback(HsStablePtr a1);
extern HsPtr hs_imageResourcesCallback(void);
extern HsPtr hs_soundResourcesCallback(void);
extern HsPtr hs_musicResourcesCallback(void);
extern HsInt32 hs_maxSpritesCallback(void);
extern HsInt32 hs_maxSoundsCallback(void);
extern HsStablePtr hs_eventCallback(HsStablePtr a1, HsInt32 a2, HsInt32 a3, HsInt32 a4, HsDouble a5, HsDouble a6, HsDouble a7, HsDouble a8, HsDouble a9);
#ifdef __cplusplus
}
#endif

