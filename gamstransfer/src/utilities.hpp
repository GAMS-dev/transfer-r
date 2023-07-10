#include "gdxcc.h"
#include "gclgms.h"
using namespace Rcpp;
class gt_gdx
{
  public:
    gdxHandle_t gdx;
    gt_gdx() {
      gdx = NULL;
    }
    void init_library(std::string mySysDir) {
      char  Msg[GMS_SSSIZE];
      int rc;
      // open GDX file and start reading
      rc = gdxCreateD(&gdx, mySysDir.c_str(), Msg, sizeof(Msg));
      if (!rc) stop("gdxCreateD: GDX init failed: %s", Msg);
    }
    ~gt_gdx() {
      // close the file and return
      if (gdx) {
        if (gdxClose(gdx)) stop("CPP_readSuper:gdxClose GDX error (gdxClose)");
        if (!gdxFree(&gdx)) stop("CPP_readSuper:gdxFree GDX error (gdxFree)");
      }

      if (gdxLibraryLoaded()) {
        if (!gdxLibraryUnload()) stop("CPP_readSuper:gdxLibraryUnload GDX error (gdxLibraryUnload)");
      }

    }
};