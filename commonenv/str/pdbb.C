void
PrettyDump(PrettyDumper &pd, const [[Type]] &val, const char* const caption)
{
    PrettyDumperHeader header(pd, (void*)&val, sizeof(val), caption,
                              "[[Type]]: ");
    if (header.skipit)
        return;
    
    // fprintf(pd.fp, "zapp");
}
