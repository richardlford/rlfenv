void
PrettyDump(PrettyDumper &pd, const [[Type]] &val, const char* const caption)
{
    val.PrettyDumpBody(pd, caption);
}

#define TheType [[Type]]
#define TheTypeName "[[Type]]"
#include "PrettyDumper/PrettyDumperDBDef.C"
