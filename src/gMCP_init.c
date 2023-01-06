#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .C calls */
extern void cgMCP(void *, void *, void *, void *, void *, void *, void *, void *);
extern void graphmult(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void graphproc(void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"cgMCP",     (DL_FUNC) &cgMCP,      8},
    {"graphmult", (DL_FUNC) &graphmult, 14},
    {"graphproc", (DL_FUNC) &graphproc,  9},
    {NULL, NULL, 0}
};

void R_init_gMCP(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

