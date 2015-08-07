void
PrettyDump(PrettyDumper &pd, const [[Type]] &val, const char* const caption)
{
    PrettyDumperHeader header(pd, (void*)&val, sizeof(val), caption,
                              "[[Type]]: ");
    if (header.skipit)
        return;
    
    // fprintf(pd.fp, "zapp");
}

#define TheType [[Type]]
#define TheTypeName "[[Type]]"
#include "PrettyDumper/PrettyDumperDBDef.C"
