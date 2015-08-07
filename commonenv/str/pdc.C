void
[[Type]]::PrettyDumpBody(PrettyDumper &pd, const char* const caption) const
{
  PrettyDumperHeader header(pd, (void*)this, sizeof(*this), caption,
                            "[[Type]]: ");
  if (header.skipit)
    return;
  
  // PrettyDump(pd, zapp, zapp);
}

#define TheType [[Type]]
#define TheTypeName "[[Type]]"
#include "PrettyDumper/PrettyDumperClassDumpDef.C"
