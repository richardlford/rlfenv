void
PrettyDump(PrettyDumper &pd, [[Type]] &val, const char* const caption)
{
  PrettyDumperHeader header(pd, (void*)this, sizeof(*this), caption,
			    "[[Type]]: ");
  switch (val) {
  case [[c1]]:
    {
      fputs("[[c1]]", pd.fp);;
    }
    break;
  case [[c2]]: 
    {
      fputs("[[c2]]", pd.fp);;
    }
    break;
  case [[c3]]: 
    {
      fputs("[[c3]]", pd.fp);;
    }
    break;
  case [[c4]]: 
    {
      fputs("[[c4]]", pd.fp);;
    }
    break;
  case [[c5]]: 
    {
      fputs("[[c5]]", pd.fp);;
    }
    break;
  case [[c6]]: 
    {
      fputs("[[c6]]", pd.fp);;
    }
    break;
  case [[c7]]: 
    {
      fputs("[[c7]]", pd.fp);;
    }
    break;
  case [[c8]]: 
    {
      fputs("[[c8]]", pd.fp);;
    }
    break;
  default: 
    {
      fputs("*Invalid*", pd.fp);
    }
    break;
  }
}
